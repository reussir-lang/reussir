//! Verified compilation with bounded loaded and serialized executable caches.

use super::{
    Error, Result,
    api::{Api, call, non_null},
    artifact::Artifacts,
    cache_config::CacheConfig,
    client::Client,
    context,
    sys::*,
};
use foyer::{Cache, CacheBuilder, S3FifoConfig};
use std::{future::Future, path::Path, ptr::NonNull, sync::Arc};

/// An owned executable. The compiler ABI holds strong references to this object;
/// cache eviction cannot destroy it until every caller releases its reference.
pub struct Executable {
    native: NonNull<PJRT_LoadedExecutable>,
    owner: Arc<CompilationClient>,
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

    pub(super) fn compile(bytecode: &[u8], checksum: &[u8], options: &[u8]) -> Result<Arc<Self>> {
        let digest = verify_checksum(bytecode, checksum)?;
        context::get()?
            .executables
            .get_or_compile(bytecode, digest, options)
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
    fn compile(self: &Arc<Self>, code: &[u8], options: &[u8]) -> Result<Arc<Executable>> {
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
        self.loaded(compiled.executable)
    }

    fn loaded(self: &Arc<Self>, native: *mut PJRT_LoadedExecutable) -> Result<Arc<Executable>> {
        Ok(Arc::new(Executable {
            native: non_null(native, "loaded executable")?,
            owner: self.clone(),
        }))
    }

    fn deserialize(self: &Arc<Self>, bytes: &[u8]) -> Result<Arc<Executable>> {
        let loaded = call!(
            self.api,
            PJRT_Executable_DeserializeAndLoad {
                client: self.client.as_ptr(),
                serialized_executable: bytes.as_ptr().cast(),
                serialized_executable_size: bytes.len(),
            }
        )?;
        self.loaded(loaded.loaded_executable)
    }

    /// Backend compatibility is part of the key, not a cache/config version.
    /// If the plugin cannot describe its target or serialize, use memory only.
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
            return Err(Error::local(
                "plugin does not support persistent executable caching",
            ));
        }
        let mut hasher = blake3::Hasher::new();
        hasher
            .update_reader(std::fs::File::open(plugin).map_err(|e| Error::local(e.to_string()))?)
            .map_err(|e| Error::local(e.to_string()))?;
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

#[derive(Clone, Hash, PartialEq, Eq)]
struct CacheKey {
    digest: blake3::Hash,
    options: Vec<u8>,
}

pub(super) struct CompilationCache {
    entries: Cache<CacheKey, Arc<Executable>>,
    artifacts: Option<Arc<Artifacts>>,
    namespace: [u8; 32],
    owner: Arc<CompilationClient>,
    runtime: Option<tokio::runtime::Runtime>,
}

impl CompilationCache {
    pub fn new(
        owner: Arc<CompilationClient>,
        config: &CacheConfig,
        namespace: Option<[u8; 32]>,
    ) -> Result<Self> {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .thread_name("reussir-pjrt-cache")
            .enable_all()
            .build()
            .map_err(|e| Error::local(e.to_string()))?;
        let artifacts = if config.disk.enabled && namespace.is_some() {
            let _enter = runtime.enter();
            match futures::executor::block_on(Artifacts::open(&config.disk)) {
                Ok(artifacts) => Some(artifacts),
                Err(error) => {
                    tracing::warn!(%error, "PJRT disk cache disabled");
                    None
                }
            }
        } else {
            None
        };
        Ok(Self {
            entries: CacheBuilder::new(config.memory.loaded_entries)
                .with_shards(1)
                .with_eviction_config(S3FifoConfig::default())
                .build(),
            artifacts,
            namespace: namespace.unwrap_or_default(),
            owner,
            runtime: Some(runtime),
        })
    }

    fn wait<F: Future>(&self, future: F) -> F::Output {
        // A separate runtime drives I/O even if the FFI caller is itself on a
        // Tokio worker. No nested Runtime::block_on and no caller executor needed.
        let _enter = self.runtime.as_ref().unwrap().enter();
        futures::executor::block_on(future)
    }

    fn get_or_compile(
        &self,
        code: &[u8],
        digest: blake3::Hash,
        options: &[u8],
    ) -> Result<Arc<Executable>> {
        let key = CacheKey {
            digest,
            options: options.into(),
        };
        if let Some(entry) = self.entries.get(&key) {
            return Ok(entry.value().clone());
        }
        let owner = self.owner.clone();
        let artifacts = self.artifacts.clone();
        let mut artifact_key = self.namespace.to_vec();
        artifact_key.extend_from_slice(digest.as_bytes());
        artifact_key.extend_from_slice(options);
        let code = code.to_vec();
        let options = options.to_vec();
        let _enter = self.runtime.as_ref().unwrap().enter();
        self.wait(self.entries.get_or_fetch(&key, || async move {
            let stored = if let Some(artifacts) = &artifacts {
                match artifacts.cache.get(&artifact_key).await {
                    Ok(entry) => entry,
                    Err(error) => {
                        tracing::warn!(%error, "PJRT artifact read failed");
                        None
                    }
                }
            } else {
                None
            };
            // Backend compilation/serialization may block for a long time.
            // Keep it off foyer's I/O workers; same-key requests are coalesced.
            tokio::task::spawn_blocking(move || {
                if let Some(entry) = stored {
                    let bytes = entry.value();
                    if bytes.len() >= 32 && blake3::hash(&bytes[32..]).as_bytes() == &bytes[..32] {
                        match owner.deserialize(&bytes[32..]) {
                            Ok(executable) => {
                                tracing::debug!("PJRT executable restored from artifact cache");
                                return Ok(executable);
                            }
                            Err(error) => {
                                tracing::warn!(%error, "PJRT artifact rejected; recompiling")
                            }
                        }
                    }
                    if let Some(artifacts) = &artifacts {
                        artifacts.cache.remove(&artifact_key);
                    }
                }
                let executable = owner.compile(&code, &options)?;
                if let Some(artifacts) = &artifacts {
                    match executable.serialize() {
                        Ok(bytes) => {
                            let mut record = blake3::hash(&bytes).as_bytes().to_vec();
                            record.extend_from_slice(&bytes);
                            artifacts.cache.insert(artifact_key, record);
                        }
                        Err(error) => {
                            tracing::warn!(%error, "PJRT executable is only cached in memory")
                        }
                    }
                }
                Ok::<_, Error>(executable)
            })
            .await
            .map_err(|e| Error::local(e.to_string()))?
        }))
        .map(|entry| entry.value().clone())
        .map_err(|e| {
            e.downcast_ref::<Error>()
                .cloned()
                .unwrap_or_else(|| Error::local(e.to_string()))
        })
    }

    pub fn close(&self) {
        if let Some(artifacts) = &self.artifacts {
            self.wait(artifacts.close());
        }
    }
}

impl Drop for CompilationCache {
    fn drop(&mut self) {
        self.close();
        self.entries.clear();
        // Closing the store already waits for I/O. Avoid Tokio's blocking
        // Runtime destructor when an embedding drops us on its async worker.
        self.runtime.take().unwrap().shutdown_background();
    }
}

#[cfg(test)]
mod tests;
