//! Disk-only compilation cache for rene's bundled libraries.
//!
//! compcache owns publication, cross-process exclusion, recovery and pruning.
//! A producer compiles inside its managed directory and streams only the
//! declared outputs through the counted, durable writer. Readers retain a
//! lease until their private copies are complete; linkers never read a path
//! that cache pruning can remove.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use compcache::{
    ArtifactWriter, Bypass, Config, Key, ManagedDirectory, RuntimeLease, StoreBuilder,
    StoreDefaults,
};
use indicatif::MultiProgress;
use serde::Serialize;

use crate::{compile, core_src, deps, fresh, plan, resolve::Graph, rt};

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

/// Every input which may change a bundled library's emitted bytes. Output and
/// bundled-source locations are logical paths so different projects can share
/// the result. User flags and external tool/library paths remain literal.
#[derive(Serialize)]
struct Recipe {
    rene: &'static str,
    package: String,
    bundle: String,
    config_hash: String,
    sources: BTreeMap<PathBuf, [u8; 32]>,
    compiler: String,
    rustc: PathBuf,
    rustc_version: String,
    runtime_bundle: String,
    target: String,
    commands: Vec<Vec<String>>,
    upstream: BTreeMap<String, String>,
    environment: BTreeMap<String, Option<String>>,
}

pub(crate) struct Request {
    key: Key,
    commands: Vec<plan::PlannedCommand>,
    outputs: Vec<PathBuf>,
    rustc: PathBuf,
}

impl Request {
    pub(crate) fn new(
        graph: &Graph,
        name: &str,
        opts: &plan::Options<'_>,
        runtime: &rt::RtArtifacts,
        commands: Vec<plan::PlannedCommand>,
        sources: &[deps::SourceFile],
    ) -> Result<Self, String> {
        let deps_dir = opts.build_dir.join(opts.profile_name).join("deps");
        let compiler = which::which(deps::resolve_rrc())
            .map_err(|e| format!("cannot locate rrc for the bundled library cache: {e}"))?;
        let source_root = graph.nodes[name]
            .dir
            .canonicalize()
            .map_err(|e| format!("cannot resolve bundled source root: {e}"))?;
        let recipe = Recipe {
            rene: env!("CARGO_PKG_VERSION"),
            package: name.to_owned(),
            bundle: core_src::content_hash(),
            config_hash: deps::config_hash(&graph.nodes[name].loaded.dump),
            sources: sources
                .iter()
                .map(|source| {
                    let path = Path::new(&source.path)
                        .canonicalize()
                        .map_err(|e| format!("cannot resolve bundled source: {e}"))?;
                    let relative = path
                        .strip_prefix(&source_root)
                        .map_err(|e| format!("bundled source is outside its package: {e}"))?;
                    Ok((relative.to_owned(), source.record.hash))
                })
                .collect::<Result<_, String>>()?,
            compiler: file_digest(&compiler)?,
            rustc: runtime.rustc.clone(),
            rustc_version: runtime.rustc_version.clone(),
            runtime_bundle: rt::bundle_hash(),
            target: opts.target.to_owned(),
            commands: commands
                .iter()
                .map(|command| normalize(command, runtime))
                .collect(),
            upstream: fresh::upstream_digests(
                &deps_dir,
                &plan::transitive_deps(graph)[name],
                opts.target,
            ),
            environment: [
                "RUSTFLAGS",
                "CARGO_ENCODED_RUSTFLAGS",
                "REUSSIR_RUSTC_DEPS",
                "RENE_RT_PLATFORM_ALLOCATOR",
            ]
            .into_iter()
            .map(|name| (name.to_owned(), std::env::var(name).ok()))
            .collect(),
        };
        let key = Key::from_serde("rene-bundled-library-v1", &recipe).map_err(|e| e.to_string())?;
        let mut outputs = Vec::new();
        for command in &commands {
            outputs.push(command.out.clone());
            if opts.token_reuse_remarks && command.emit != "rri" {
                outputs.push(plan::token_reuse_report_path(&command.out));
            }
        }
        Ok(Self {
            key,
            commands,
            outputs,
            rustc: runtime.rustc.clone(),
        })
    }

    /// Run on a blocking worker under the process pool's admission permit.
    pub(crate) fn build(
        self,
        color: bool,
        progress: &MultiProgress,
    ) -> Result<Restoration, String> {
        let lease = self.build_inner(color, progress).map_err(|e| {
            format!(
                "bundled library disk cache (set RENE_CACHE_DIR to a writable local directory): {e}"
            )
        })?;
        Ok(Restoration {
            lease,
            outputs: self.outputs,
            key: self.key,
        })
    }

    fn build_inner(&self, color: bool, progress: &MultiProgress) -> Result<RuntimeLease, String> {
        // Deliberately use the built-in settings: this integration needs no
        // external Nickel executable or separate user configuration file.
        let store = StoreBuilder::<Bypass, ManagedDirectory>::with_defaults::<Rene>()
            .and_then(|builder| builder.config(Rene::config()).open())
            .map_err(|e| e.to_string())?;
        let lease = store
            .get_or_insert_runtime(self.key, &|directory: &Path| {
                tracing::debug!(key = %self.key, "baking bundled library");
                // A blocking task can outlive a cancelled future. Keep all of
                // its compiler writes inside the managed object, never in the
                // project whose lock the caller may release after an error.
                let work = directory.join("compile");
                std::fs::create_dir(&work).map_err(compcache::Error::producer)?;
                for command in &self.commands {
                    let mut staged = command.clone();
                    staged.out = work.join(command.out.file_name().unwrap());
                    for i in 1..staged.args.len() {
                        if matches!(staged.args[i - 1].as_str(), "-o" | "--token-reuse-remarks")
                            && let Some(file) = Path::new(&staged.args[i]).file_name()
                        {
                            staged.args[i] = work.join(file).display().to_string();
                        }
                    }
                    let completed = compile::Invocation::from_planned(staged, &self.rustc, color)
                        .run_blocking()
                        .map_err(producer_error)?;
                    progress
                        .suspend(|| completed.report())
                        .map_err(producer_error)?;
                }
                let mut writer = ArtifactWriter::new(directory);
                for path in &self.outputs {
                    let mut file = std::fs::File::open(work.join(path.file_name().unwrap()))
                        .map_err(compcache::Error::producer)?;
                    writer.copy_file(
                        path.file_name().expect("planned artifact has a name"),
                        &mut file,
                    )?;
                }
                std::fs::remove_dir_all(&work).map_err(compcache::Error::producer)?;
                writer.finish()
            })
            .map_err(|e| e.to_string())?;
        Ok(lease)
    }
}

/// The worker returns ownership of a lease, with no writes to project paths.
/// Restoration runs on the caller's build thread after awaiting the worker;
/// dropping an in-flight dependency can therefore never leave a background
/// worker writing into an unlocked project directory.
pub(crate) struct Restoration {
    lease: RuntimeLease,
    outputs: Vec<PathBuf>,
    key: Key,
}

impl Restoration {
    pub(crate) fn restore(self) -> Result<(), String> {
        // Copy instead of hard-linking: a later local rebuild must not mutate
        // the immutable cached copy. Hold the reader lease through every copy.
        for path in &self.outputs {
            let cached = self
                .lease
                .path()
                .join(path.file_name().expect("planned artifact has a name"));
            std::fs::copy(&cached, path).map_err(|e| {
                format!(
                    "cannot restore `{}` from `{}`: {e}",
                    path.display(),
                    cached.display()
                )
            })?;
        }
        tracing::debug!(key = %self.key, "restored bundled library from disk cache");
        Ok(())
    }
}

fn producer_error(error: String) -> compcache::Error {
    compcache::Error::producer(std::io::Error::other(error))
}

fn file_digest(path: &Path) -> Result<String, String> {
    let mut file = std::fs::File::open(path)
        .map_err(|e| format!("cannot read compiler `{}`: {e}", path.display()))?;
    let mut hash = blake3::Hasher::new();
    std::io::copy(&mut file, &mut hash).map_err(|e| e.to_string())?;
    Ok(hash.finalize().to_hex().to_string())
}

fn normalize(command: &plan::PlannedCommand, runtime: &rt::RtArtifacts) -> Vec<String> {
    let mut args = command.args.clone();
    // Only rewrite values of known path-bearing flags. In particular, an
    // arbitrary extra flag containing a build path must keep its identity.
    for i in 1..args.len() {
        args[i] = match args[i - 1].as_str() {
            "--package-root" => "<bundled-source>".into(),
            "-o" | "--token-reuse-remarks" => Path::new(&args[i])
                .file_name()
                .map(|file| file.to_string_lossy().into_owned())
                .unwrap_or_else(|| args[i].clone()),
            "--extern" => args[i]
                .split_once('=')
                .map(|(name, path)| {
                    format!(
                        "{name}=<deps>/{}",
                        Path::new(path)
                            .file_name()
                            .unwrap_or(std::ffi::OsStr::new(path))
                            .to_string_lossy()
                    )
                })
                .unwrap_or_else(|| args[i].clone()),
            "--extern-src" => args[i]
                .split_once('=')
                .map(|(name, _)| format!("{name}=<bundled-source>"))
                .unwrap_or_else(|| args[i].clone()),
            "--polyffi-libdir" => runtime
                .deps_dirs
                .iter()
                .position(|path| path == Path::new(&args[i]))
                .map(|index| format!("<runtime-deps-{index}>"))
                .unwrap_or_else(|| args[i].clone()),
            _ => args[i].clone(),
        };
    }
    args
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalization_preserves_user_flags_and_external_library_paths() {
        let runtime = rt::RtArtifacts {
            target: "test".into(),
            rustc: "rustc".into(),
            rustc_version: "test".into(),
            rust_libdir: "/sysroot".into(),
            deps_dirs: vec!["/build/rt/deps".into()],
            staticlib: "rt.a".into(),
            rlib: "rt.rlib".into(),
        };
        let command = plan::PlannedCommand {
            emit: "rri",
            target: None,
            out: "/build/core.rri".into(),
            args: [
                "--package-root",
                "/build/core/src",
                "-o",
                "/build/core.rri",
                "--polyffi-libdir",
                "/build/rt/deps",
                "--polyffi-libdir",
                "/sysroot",
                "--link-arg",
                "/build/custom.a",
            ]
            .map(str::to_owned)
            .to_vec(),
        };
        assert_eq!(
            normalize(&command, &runtime),
            [
                "--package-root",
                "<bundled-source>",
                "-o",
                "core.rri",
                "--polyffi-libdir",
                "<runtime-deps-0>",
                "--polyffi-libdir",
                "/sysroot",
                "--link-arg",
                "/build/custom.a"
            ]
        );
    }
}
