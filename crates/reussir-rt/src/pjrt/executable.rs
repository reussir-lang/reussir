//! Verified compilation with bounded loaded and serialized executable caches.

use super::{
    Error, Result,
    api::{Api, call, non_null},
    artifact::Memory,
    cache_config::{self, Reussir},
    client::Client,
    donation::DonationInputs,
    sys::*,
};
use compcache::{Key, NoExtraData, Produced, QuickCache, Store, StoreBuilder};
use serde::Serialize;
use std::{
    path::Path,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

mod execute;

/// An owned executable. The compiler ABI holds strong references to this object;
/// cache eviction cannot destroy it until every caller releases its reference.
pub struct Executable {
    native: NonNull<PJRT_LoadedExecutable>,
    owner: Arc<CompilationClient>,
    estimated_bytes: AtomicU64,
    pub(super) plan: LaunchPlan,
}

/// Execution metadata produced by compilation and persisted with its executable.
#[derive(Clone, Debug, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(super) struct LaunchPlan {
    pub inputs: Vec<usize>,
    pub non_donatable: Vec<i64>,
}

// SAFETY: PJRT executable/client operations are thread-safe. The last strong
// reference destroys the executable, and its owner keeps the client alive.
unsafe impl Send for Executable {}
unsafe impl Sync for Executable {}

impl std::fmt::Debug for Executable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Executable").field(&self.native).finish()
    }
}

impl Executable {
    /// Borrow the native executable for the lifetime of this strong reference.
    /// The caller must not delete or destroy the returned PJRT object.
    pub fn as_ptr(&self) -> *mut PJRT_LoadedExecutable {
        self.native.as_ptr()
    }

    pub(super) fn set_weight(&self, bytes: usize) {
        self.estimated_bytes
            .store((bytes as u64).max(128), Ordering::Relaxed);
    }
    pub(super) fn weight(&self) -> u64 {
        self.estimated_bytes.load(Ordering::Relaxed)
    }

    fn serialize(&self) -> Result<Vec<u8>> {
        let api = self.owner.api;
        let executable = call!(
            api,
            PJRT_LoadedExecutable_GetExecutable {
                loaded_executable: self.as_ptr()
            }
        )?
        .executable;
        non_null(executable, "executable")?;
        let serialized = call!(api, PJRT_Executable_Serialize { executable });
        let cleanup = call!(api, PJRT_Executable_Destroy { executable });
        let serialized = serialized?;
        // Copy before releasing the backend's serialization allocation.
        let bytes = unsafe {
            copy_bytes(
                serialized.serialized_bytes,
                serialized.serialized_bytes_size,
            )
        };
        if let Some(delete) = serialized.serialized_executable_deleter {
            unsafe { delete(serialized.serialized_executable) };
        }
        cleanup?;
        bytes
    }
}

impl Drop for Executable {
    fn drop(&mut self) {
        if let Err(error) = call!(
            self.owner.api,
            PJRT_LoadedExecutable_Destroy {
                executable: self.as_ptr()
            }
        ) {
            tracing::warn!(%error, "PJRT executable cleanup failed");
        }
    }
}

/// Shares client ownership with loaded executables; the plugin stays resident.
pub(super) struct CompilationClient {
    pub api: Api,
    pub client: Client,
}

// SAFETY: API table is immutable and resident; PJRT clients are thread-safe.
unsafe impl Send for CompilationClient {}
unsafe impl Sync for CompilationClient {}

impl Drop for CompilationClient {
    fn drop(&mut self) {
        if let Err(error) = unsafe { self.client.destroy(self.api) } {
            tracing::warn!(%error, "PJRT client cleanup failed");
        }
    }
}

impl CompilationClient {
    fn compile(
        self: &Arc<Self>,
        code: &[u8],
        options: &[u8],
        plan: LaunchPlan,
    ) -> Result<Arc<Executable>> {
        let program = PJRT_Program {
            struct_size: PJRT_Program_STRUCT_SIZE as usize,
            code: code.as_ptr().cast_mut().cast(),
            code_size: code.len(),
            format: c"mlir".as_ptr(),
            format_size: 4,
            ..Default::default()
        };
        let compiled = call!(
            self.api,
            PJRT_Client_Compile {
                client: self.client.as_ptr(),
                program: &program,
                compile_options: options.as_ptr().cast(),
                compile_options_size: options.len(),
            }
        )?;
        self.loaded(compiled.executable, plan)
    }

    fn loaded(
        self: &Arc<Self>,
        native: *mut PJRT_LoadedExecutable,
        plan: LaunchPlan,
    ) -> Result<Arc<Executable>> {
        Ok(Arc::new(Executable {
            native: non_null(native, "loaded executable")?,
            owner: self.clone(),
            estimated_bytes: AtomicU64::new(128),
            plan,
        }))
    }

    pub(super) fn deserialize(
        self: &Arc<Self>,
        bytes: &[u8],
        plan: LaunchPlan,
    ) -> Result<Arc<Executable>> {
        let loaded = call!(
            self.api,
            PJRT_Executable_DeserializeAndLoad {
                client: self.client.as_ptr(),
                serialized_executable: bytes.as_ptr().cast(),
                serialized_executable_size: bytes.len(),
            }
        )?;
        self.loaded(loaded.loaded_executable, plan)
    }

    /// Backend compatibility is part of the key, not a cache/config version.
    /// Persistent compilation requires target identity and serialization APIs.
    pub fn namespace(&self, plugin: &Path) -> Result<[u8; 32]> {
        let api = self.api;
        // These optional fields are within the prefix validated by Api::new.
        let supported = unsafe {
            (*api.0.as_ptr())
                .PJRT_LoadedExecutable_GetExecutable
                .is_some()
                && (*api.0.as_ptr()).PJRT_Executable_Destroy.is_some()
                && (*api.0.as_ptr()).PJRT_Executable_Serialize.is_some()
                && (*api.0.as_ptr())
                    .PJRT_Executable_DeserializeAndLoad
                    .is_some()
                && (*api.0.as_ptr()).PJRT_Client_TopologyDescription.is_some()
                && (*api.0.as_ptr())
                    .PJRT_TopologyDescription_Serialize
                    .is_some()
                && (*api.0.as_ptr()).PJRT_Client_PlatformVersion.is_some()
        };
        if !supported {
            return Err(Error::PersistenceUnsupported);
        }
        let mut hasher = blake3::Hasher::new();
        hasher
            .update_reader(std::fs::File::open(plugin).map_err(|e| Error::Io(Arc::new(e)))?)
            .map_err(|e| Error::Io(Arc::new(e)))?;
        let plugin_digest = hasher.finalize();
        hasher = blake3::Hasher::new();
        hasher.update(plugin_digest.as_bytes());
        let version = call!(
            api,
            PJRT_Client_PlatformVersion {
                client: self.client.as_ptr()
            }
        )?;
        let version =
            unsafe { copy_bytes(version.platform_version, version.platform_version_size) }?;
        hasher
            .update(&(version.len() as u64).to_le_bytes())
            .update(&version);
        let topology = call!(
            api,
            PJRT_Client_TopologyDescription {
                client: self.client.as_ptr()
            }
        )?
        .topology;
        let topology = call!(api, PJRT_TopologyDescription_Serialize { topology })?;
        let bytes =
            unsafe { copy_bytes(topology.serialized_bytes, topology.serialized_bytes_size) };
        if let Some(delete) = topology.serialized_topology_deleter {
            unsafe { delete(topology.serialized_topology) };
        }
        let bytes = bytes?;
        hasher
            .update(&(bytes.len() as u64).to_le_bytes())
            .update(&bytes);
        let flags = std::env::var_os("XLA_FLAGS").unwrap_or_default();
        hasher.update(flags.as_encoded_bytes());
        Ok(*hasher.finalize().as_bytes())
    }
}

unsafe fn copy_bytes(pointer: *const std::ffi::c_char, length: usize) -> Result<Vec<u8>> {
    if length == 0 {
        return Ok(Vec::new());
    }
    let pointer = non_null(pointer.cast_mut(), "serialized bytes")?;
    Ok(unsafe { std::slice::from_raw_parts(pointer.as_ptr().cast::<u8>(), length) }.to_vec())
}

fn verify_checksum(bytecode: &[u8], checksum: &[u8]) -> Result<blake3::Hash> {
    let expected = blake3::Hash::from_hex(checksum)
        .map_err(|_| Error::local("requires a 64-character hexadecimal BLAKE3 checksum"))?;
    let actual = blake3::hash(bytecode);
    if actual != expected {
        return Err(Error::local("BLAKE3 checksum does not match bytecode"));
    }
    Ok(actual)
}

pub(super) struct CompilationCache {
    store: Store<Memory, NoExtraData>,
    memory: Arc<QuickCache<Arc<Executable>>>,
    namespace: [u8; 32],
    pub(super) owner: Arc<CompilationClient>,
}

impl CompilationCache {
    pub fn new(owner: Arc<CompilationClient>, namespace: [u8; 32]) -> Result<Self> {
        let config = cache_config::load()?;
        let builder = StoreBuilder::with_defaults::<Reussir>()?.config(config.clone());
        Self::open(owner, namespace, config, builder)
    }

    fn open(
        owner: Arc<CompilationClient>,
        namespace: [u8; 32],
        config: compcache::Config,
        builder: StoreBuilder<Memory, NoExtraData>,
    ) -> Result<Self> {
        let memory = Memory::new(owner.clone(), &config.memory)?;
        let entries = memory.entries.clone();
        let store = builder.in_memory(memory).open()?;
        Ok(Self {
            store,
            memory: entries,
            namespace,
            owner,
        })
    }

    pub(super) fn compile_program(
        &self,
        code: &[u8],
        checksum: &[u8],
        options: &[u8],
        arguments: &[reussir_stablehlo::Argument],
        donation: &DonationInputs,
        output_count: usize,
    ) -> Result<Arc<Executable>> {
        let digest = verify_checksum(code, checksum)?;
        self.get_or_compile(code, digest, options, arguments, donation, output_count)
    }

    fn get_or_compile(
        &self,
        code: &[u8],
        digest: blake3::Hash,
        options: &[u8],
        arguments: &[reussir_stablehlo::Argument],
        donation: &DonationInputs,
        output_count: usize,
    ) -> Result<Arc<Executable>> {
        let key = self.artifact_key(digest, options, arguments, donation, output_count)?;
        self.get_or_build(key, options, || {
            let refined =
                reussir_stablehlo::refine_shapes(code, arguments).map_err(|source| match source
                    .kind
                {
                    reussir_stablehlo::ErrorKind::ResidualDynamism => {
                        Error::ResidualDynamism(Arc::new(source))
                    }
                    _ => Error::Stablehlo(Arc::new(source)),
                })?;
            if refined.result_types.len() != output_count {
                return Err(Error::local("refined result count mismatch"));
            }
            let selected = donation.select(&refined.result_types)?;
            let inputs: Vec<_> = selected
                .mapping
                .iter()
                .enumerate()
                .filter_map(|(j, &rep)| (j == rep as usize).then_some(j))
                .collect();
            let non_donatable = inputs
                .iter()
                .enumerate()
                .filter_map(|(new, &old)| {
                    (!selected.donors.contains(&(old as u32))).then_some(new as i64)
                })
                .collect();
            let bytecode = if selected.donors.is_empty() && selected.aliases.is_empty() {
                refined.bytecode
            } else {
                reussir_stablehlo::apply_donation(&refined.bytecode, &selected)
                    .map_err(|source| Error::Stablehlo(Arc::new(source)))?
            };
            Ok((
                bytecode,
                LaunchPlan {
                    inputs,
                    non_donatable,
                },
            ))
        })
    }

    fn get_or_build(
        &self,
        key: Key,
        options: &[u8],
        prepare: impl Fn() -> Result<(Vec<u8>, LaunchPlan)>,
    ) -> Result<Arc<Executable>> {
        // A materialization failure means cached bytes were corrupt or rejected
        // by the plugin. Repair once; actual compilation errors are not retried.
        for attempt in 0..2 {
            let cached = self.store.get_or_insert_payload(key, || {
                let (code, plan) = prepare()?;
                let executable = self.owner.compile(&code, options, plan)?;
                let serialized = executable
                    .serialize()
                    .and_then(|bytes| {
                        let record = super::artifact::encode(&executable.plan, &bytes)?;
                        executable.set_weight(record.len());
                        Ok(Some(record))
                    })
                    .map_err(compcache::Error::producer);
                Ok(Produced {
                    payload: executable,
                    serialized,
                })
            });
            match cached {
                Err(compcache::Error::Materialize(_)) if attempt == 0 => {
                    tracing::warn!("PJRT artifact rejected; recompiling");
                    self.store.force_evict_blob(&key)?;
                }
                result => {
                    let result = result?;
                    if let Some(error) = result.cache_error {
                        tracing::warn!(%error, "PJRT executable usable but persistence failed");
                    }
                    return Ok(result.payload);
                }
            }
        }
        unreachable!("the final cache attempt returns its result")
    }

    fn artifact_key(
        &self,
        digest: blake3::Hash,
        options: &[u8],
        arguments: &[reussir_stablehlo::Argument],
        donation: &DonationInputs,
        output_count: usize,
    ) -> compcache::Result<Key> {
        #[derive(Serialize)]
        enum Argument<'a> {
            Shape(&'a [i64]),
            Constant(i64),
        }
        #[derive(Serialize)]
        struct Recipe<'a> {
            backend: &'a [u8; 32],
            bytecode: &'a [u8; 32],
            options: &'a [u8],
            arguments: Vec<Argument<'a>>,
            donation: &'a DonationInputs,
            output_count: usize,
        }
        Key::from_serde(
            "reussir-pjrt-executable-v1",
            &Recipe {
                backend: &self.namespace,
                bytecode: digest.as_bytes(),
                options,
                arguments: arguments
                    .iter()
                    .map(|arg| match arg {
                        reussir_stablehlo::Argument::Shape(dims) => Argument::Shape(dims),
                        reussir_stablehlo::Argument::Constant(value) => Argument::Constant(*value),
                    })
                    .collect(),
                donation,
                output_count,
            },
        )
    }

    /// All writes are synchronous. Clear cached ownership before native plugin
    /// teardown; external executable Arcs keep their native client alive.
    pub fn shutdown(&self) {
        drop(self.memory.clear());
    }
}

impl Drop for CompilationCache {
    fn drop(&mut self) {
        self.shutdown();
    }
}

#[cfg(test)]
mod tests;
