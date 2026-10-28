//! Leased, disk-only compiled bundled libraries. Consumers use the cache files
//! directly and retain their reader leases through compilation and linking.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use compcache::{
    ArtifactWriter, Bypass, Config, Key, ManagedDirectory, StoreBuilder, StoreDefaults,
};
use indicatif::MultiProgress;
use serde::Serialize;

use crate::{compile, core_src, deps, fresh, manifest::TargetKind, plan, resolve::Graph};

struct Rene;
impl StoreDefaults for Rene {
    const APP_NAME: &'static str = "rene";
    const CACHE_DIR_ENV: Option<&'static str> = Some("RENE_CACHE_DIR");
    fn config() -> Config {
        let mut config = Config::default();
        config.memory.enabled = false;
        config
    }
}

/// One handle to the global on-disk cache. Reader leases live until all work
/// using this handle has finished; entries themselves outlive every build.
pub struct Cache {
    pub(crate) store: compcache::Store<Bypass, ManagedDirectory>,
    pub(crate) runtimes: compcache::Store<Bypass, ManagedDirectory>,
    leases: std::sync::Mutex<Vec<compcache::RuntimeLease>>,
}

impl Cache {
    pub fn open() -> Result<Arc<Self>, String> {
        let store = StoreBuilder::<Bypass, ManagedDirectory>::with_defaults::<Rene>()
            .and_then(|builder| builder.config(Rene::config()).open())
            .map_err(cache_error)?;
        // Explicit dynamic-runtime links may outlive the build process. Keep
        // globally installed runtimes until an explicit cache cleanup, while
        // ordinary static library entries retain automatic pruning.
        let mut runtime_config = Rene::config();
        runtime_config.disk.prune_on_write = false;
        let runtimes = StoreBuilder::<Bypass, ManagedDirectory>::new()
            .path(store.root().join("runtimes"))
            .config(runtime_config)
            .open()
            .map_err(cache_error)?;
        Ok(Arc::new(Self {
            store,
            runtimes,
            leases: Default::default(),
        }))
    }

    pub(crate) fn retain(&self, lease: compcache::RuntimeLease) {
        self.leases
            .lock()
            .expect("cache lease mutex poisoned")
            .push(lease);
    }
}

/// Complete external-tool output under compcache's producer contract. Walk
/// once, count logical bytes, sync files, then use its portable directory flush.
pub(crate) fn sync_tree(root: &Path) -> compcache::Result<u64> {
    let mut bytes = 0u64;
    for entry in walkdir::WalkDir::new(root).contents_first(true) {
        let entry = entry.map_err(compcache::Error::producer)?;
        if entry.file_type().is_file() {
            let file = std::fs::OpenOptions::new()
                .write(true)
                .open(entry.path())
                .map_err(compcache::Error::producer)?;
            file.sync_all().map_err(compcache::Error::producer)?;
            bytes = bytes
                .checked_add(file.metadata().map_err(compcache::Error::producer)?.len())
                .ok_or(compcache::Error::SizeOverflow)?;
        } else if entry.file_type().is_dir() {
            ArtifactWriter::new(entry.path()).finish()?;
        }
    }
    Ok(bytes)
}

/// Library inputs, independent of project output locations and runtime bake
/// instances. Static-library flags cover optimization, sanitizers, target CPU
/// and features, reuse ABI, and layout settings; link-only flags are excluded.
#[derive(Serialize)]
struct Recipe {
    compiler: String,
    rustc_version: String,
    package: String,
    bundle: String,
    config_hash: String,
    sources: BTreeMap<PathBuf, [u8; 32]>,
    target: String,
    flags: Vec<String>,
    reports: bool,
    upstream: BTreeMap<String, String>,
}

pub(crate) struct Request {
    key: Key,
    name: String,
    target: String,
    reports: bool,
    commands: Vec<plan::PlannedCommand>,
    rustc: PathBuf,
}

impl Request {
    pub(crate) fn new(
        graph: &Graph,
        name: &str,
        opts: &plan::Options<'_>,
        rustc: &Path,
        rustc_version: &str,
        commands: Vec<plan::PlannedCommand>,
        sources: &[deps::SourceFile],
    ) -> Result<Self, String> {
        let deps_dir = opts.build_dir.join(opts.profile_name).join("deps");
        let compiler = which::which(deps::resolve_rrc()).map_err(|e| e.to_string())?;
        let source_root = graph.nodes[name]
            .dir
            .canonicalize()
            .map_err(|e| e.to_string())?;
        let recipe = Recipe {
            compiler: file_digest(&compiler)?,
            rustc_version: rustc_version.to_owned(),
            package: name.to_owned(),
            bundle: core_src::content_hash(),
            config_hash: deps::config_hash(&graph.nodes[name].loaded.dump),
            sources: sources
                .iter()
                .map(|source| {
                    let path = Path::new(&source.path)
                        .canonicalize()
                        .map_err(|e| e.to_string())?;
                    let relative = path.strip_prefix(&source_root).map_err(|e| e.to_string())?;
                    Ok((relative.to_owned(), source.record.hash))
                })
                .collect::<Result<_, String>>()?,
            target: opts.target.to_owned(),
            flags: compile::profile_flags(opts.profile, opts.target, TargetKind::Staticlib, None),
            reports: opts.token_reuse_remarks,
            upstream: fresh::graph_upstream_digests(
                graph,
                &deps_dir,
                &plan::transitive_deps(graph)[name],
                opts.target,
            ),
        };
        Ok(Self {
            key: Key::from_serde("rene-bundled-library-v2", &recipe).map_err(|e| e.to_string())?,
            name: name.to_owned(),
            target: opts.target.to_owned(),
            reports: opts.token_reuse_remarks,
            commands,
            rustc: rustc.to_owned(),
        })
    }

    /// Acquire an immutable library directory, compiling on a miss. Run on a
    /// blocking worker under the process pool's admission permit.
    pub(crate) fn acquire(
        self,
        cache: Arc<Cache>,
        color: bool,
        progress: &MultiProgress,
    ) -> Result<(plan::Artifacts, bool), String> {
        let produced = std::cell::Cell::new(false);
        let lease = cache
            .store
            .get_or_insert_runtime(self.key, &|directory: &Path| {
                produced.set(true);
                tracing::debug!(package = %self.name, "baking bundled library");
                let mut outputs = Vec::new();
                for command in &self.commands {
                    let mut command = command.clone();
                    command.out = directory.join(command.out.file_name().unwrap());
                    for i in 1..command.args.len() {
                        if matches!(command.args[i - 1].as_str(), "-o" | "--token-reuse-remarks")
                            && let Some(file) = Path::new(&command.args[i]).file_name()
                        {
                            command.args[i] = directory.join(file).display().to_string();
                        }
                    }
                    outputs.push(command.out.clone());
                    if self.reports && command.emit != "rri" {
                        outputs.push(plan::token_reuse_report_path(&command.out));
                    }
                    let completed = compile::Invocation::from_planned(command, &self.rustc, color)
                        .run_blocking()
                        .map_err(producer_error)?;
                    progress
                        .suspend(|| completed.report())
                        .map_err(producer_error)?;
                }
                let mut bytes = 0u64;
                for path in outputs {
                    let file = std::fs::OpenOptions::new()
                        .write(true)
                        .open(&path)
                        .map_err(compcache::Error::producer)?;
                    file.sync_all().map_err(compcache::Error::producer)?;
                    bytes = bytes
                        .checked_add(file.metadata().map_err(compcache::Error::producer)?.len())
                        .ok_or(compcache::Error::SizeOverflow)?;
                }
                // Outputs are flat files already synced above. ArtifactWriter's
                // finish supplies compcache's portable directory flush, including
                // writable directory handles on Windows and F_FULLFSYNC on macOS.
                ArtifactWriter::new(directory).finish()?;
                Ok(bytes)
            })
            .map_err(cache_error)?;
        let rri = lease.path().join(format!("{}.rri", self.name));
        let archive = plan::archive_path(lease.path(), &self.name, &self.target);
        let report = self
            .reports
            .then(|| plan::token_reuse_report_path(&archive));
        tracing::debug!(package = %self.name, path = %lease.path().display(), "using cached bundled library in place");
        cache.retain(lease);
        Ok((
            plan::Artifacts {
                rri,
                archive,
                report,
            },
            produced.get(),
        ))
    }
}

fn cache_error(error: compcache::Error) -> String {
    format!("compiler cache (RENE_CACHE_DIR): {error}")
}
fn producer_error(error: String) -> compcache::Error {
    compcache::Error::producer(std::io::Error::other(error))
}
fn file_digest(path: &Path) -> Result<String, String> {
    let mut file = std::fs::File::open(path).map_err(|e| e.to_string())?;
    let mut hash = blake3::Hasher::new();
    std::io::copy(&mut file, &mut hash).map_err(|e| e.to_string())?;
    Ok(hash.finalize().to_hex().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::Profile;

    #[test]
    fn recipe_flags_cover_codegen_and_reuse_abi_but_not_linking() {
        let base = Profile::default();
        let flags = |p: &Profile| {
            compile::profile_flags(p, "x86_64-unknown-linux-gnu", TargetKind::Staticlib, None)
        };
        for changed in [
            Profile {
                opt: Some("aggressive".into()),
                ..base.clone()
            },
            Profile {
                sanitizers: vec!["address".into()],
                ..base.clone()
            },
            Profile {
                reuse_across_call: Some(true),
                ..base.clone()
            },
            Profile {
                nullary_variant_encoding: Some("arch-independent".into()),
                ..base.clone()
            },
        ] {
            assert_ne!(flags(&base), flags(&changed));
        }
        let linking = Profile {
            linker: Some("other-linker".into()),
            link_args: vec!["-lm".into()],
            ..base.clone()
        };
        assert_eq!(flags(&base), flags(&linking));
    }
}
