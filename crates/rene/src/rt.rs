//! Baking the bundled `reussir-rt` runtime in the global compiler cache.
//!
//! The `rene` binary embeds the runtime *source* as a zstd-compressed tar
//! (packed by `build.rs` — source, not binaries, is what keeps the bundle
//! cross-platform: rlibs and symbol hashes are version-locked to the rustc
//! that produced them, so the runtime must be built by the user's toolchain;
//! see docs/design/system-rust-runtime.md). One immutable runtime is shared by
//! every project with the same toolchain, target and runtime settings. Project
//! records hold its paths; the cache handle holds its reader lease through use.

use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::db::BuildDir;
use crate::tables;

/// The runtime's directory name inside the build directory.
pub const RT_DIR: &str = "reussir-rt";

/// The bundled runtime source (tar.zst), packed by `build.rs`.
static RT_BUNDLE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/reussir-rt.tar.zst"));

/// The rcgmp integer runtime archive built for rene's own target, or empty
/// when rene was built without it (packed by `build.rs`).
static RCGMP_RUNTIME: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/rcgmp-runtime.a"));

/// The file name `rrc`'s link step looks up in the runtime library directories.
const RCGMP_RUNTIME_ARCHIVE: &str = "libRCGMPRuntime.a";

/// Blake3 hex digest of the bundles — the staleness key for the baked runtime.
pub fn bundle_hash() -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(RT_BUNDLE);
    hasher.update(RCGMP_RUNTIME);
    hasher.finalize().to_hex().to_string()
}

/// Where a bake for `target` stages the rcgmp runtime: the dependency
/// directory in which `rrc` finds the runtime rlib, which it searches for the
/// archive too. `None` for targets rene carries no archive for.
fn rcgmp_runtime_path(target: &str, deps_dirs: &[PathBuf]) -> Option<PathBuf> {
    if RCGMP_RUNTIME.is_empty() || target != env!("RENE_RCGMP_RUNTIME_TARGET") {
        return None;
    }
    deps_dirs
        .iter()
        .find(|dir| dir.join("libreussir_rt.rlib").is_file())
        .map(|dir| dir.join(RCGMP_RUNTIME_ARCHIVE))
}

/// Unpack the bundled source into `dest` (creating `dest/reussir-rt/…`).
pub fn unpack(dest: &Path) -> std::io::Result<()> {
    let decoder = zstd::Decoder::new(RT_BUNDLE)?;
    tar::Archive::new(decoder).unpack(dest)
}

/// The baked runtime: the target, toolchain that built it, and artifacts
/// consumers need. Stored as JSON under [`tables::rt_artifacts_key`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtArtifacts {
    /// The machine target this runtime was baked for.
    pub target: String,
    /// The rustc that baked the runtime — also the one `rrc`'s link step and
    /// polyffi texture compiles must use (`REUSSIR_RUSTC`), so the runtime
    /// and everything linked against it agree on one toolchain.
    pub rustc: PathBuf,
    /// `rustc --version` of that toolchain.
    pub rustc_version: String,
    /// The toolchain's target libdir (`rustc --print target-libdir`) — the
    /// "detected rust lib" directory polyffi links the standard library from.
    pub rust_libdir: PathBuf,
    /// Cargo dependency directories needed to consume the runtime. With an
    /// explicit `--target`, target libraries and host-built proc macros live
    /// in different trees; polyffi's `rustc -L` needs both.
    pub deps_dirs: Vec<PathBuf>,
    /// The static library final executables link the runtime from.
    pub staticlib: PathBuf,
    pub rlib: PathBuf,
}

impl RtArtifacts {
    /// The library directories to pass to `rrc` (`--polyffi-libdir`): the
    /// target standard library and every Cargo dependency directory needed
    /// by the freshly baked runtime.
    pub fn libdirs(&self) -> impl Iterator<Item = &Path> {
        std::iter::once(self.rust_libdir.as_path())
            .chain(self.deps_dirs.iter().map(PathBuf::as_path))
    }
}

/// The resolved `cargo`/`rustc` pair used for baking.
#[derive(Clone)]
struct Toolchain {
    cargo: PathBuf,
    rustc: PathBuf,
    version: String,
    /// The resolved target the bake builds for, and so the key of cargo's
    /// target-specific configuration.
    target: String,
    rust_libdir: PathBuf,
}

impl Toolchain {
    /// `REUSSIR_CARGO`/`REUSSIR_RUSTC` first (the explicit override rrc also
    /// honors), then cargo's own `CARGO`/`RUSTC`, then `PATH` — which
    /// respects rustup shims, so a `rust-toolchain.toml` in the user's
    /// project still picks the toolchain naturally.
    async fn resolve(requested_target: Option<&str>) -> Result<Self, String> {
        let mut cargo = which::which(pick_program("REUSSIR_CARGO", "CARGO", "cargo"))
            .map_err(|e| e.to_string())?;
        let mut rustc = which::which(pick_program("REUSSIR_RUSTC", "RUSTC", "rustc"))
            .map_err(|e| e.to_string())?;
        // Resolve rustup's project selection before changing cwd to the global
        // cache. Otherwise its shim would select the cache directory's default.
        let is_rustup = |program: &Path| {
            program
                .canonicalize()
                .ok()
                .and_then(|p| p.file_stem().map(|s| s == "rustup"))
                .unwrap_or(false)
        };
        if is_rustup(&rustc) {
            let root = PathBuf::from(capture(&rustc, &["--print", "sysroot"]).await?);
            rustc = root
                .join("bin")
                .join(format!("rustc{}", std::env::consts::EXE_SUFFIX));
            if is_rustup(&cargo) {
                cargo = root
                    .join("bin")
                    .join(format!("cargo{}", std::env::consts::EXE_SUFFIX));
            }
        }
        let verbose_version = capture(&rustc, &["-vV"]).await?;
        let version = verbose_version
            .lines()
            .next()
            .unwrap_or_default()
            .to_owned();
        let host = verbose_version
            .lines()
            .find_map(|line| line.strip_prefix("host: "))
            .ok_or_else(|| {
                format!(
                    "`{} -vV` reported no `host:` line:\n{verbose_version}",
                    rustc.display()
                )
            })?
            .trim()
            .to_owned();
        let target = match requested_target {
            Some(target) => validate_target(target)?,
            None => validate_target(&host)?,
        };
        let rust_libdir = PathBuf::from(
            capture(
                &rustc,
                &["--print", "target-libdir", "--target", target.as_str()],
            )
            .await?,
        );
        Ok(Toolchain {
            cargo,
            rustc,
            version,
            target,
            rust_libdir,
        })
    }
}

fn pick_program(specific: &str, generic: &str, default: &str) -> PathBuf {
    std::env::var_os(specific)
        .or_else(|| std::env::var_os(generic))
        .map_or_else(|| PathBuf::from(default), PathBuf::from)
}

/// Resolve the target used by a command that does not bake the runtime:
/// an explicit/profile target wins, otherwise use the selected rustc's host.
pub async fn resolve_target(requested: Option<&str>) -> Result<String, String> {
    if let Some(target) = requested {
        return validate_target(target);
    }
    let rustc = pick_program("REUSSIR_RUSTC", "RUSTC", "rustc");
    let verbose_version = capture(&rustc, &["-vV"]).await?;
    verbose_version
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .ok_or_else(|| {
            format!(
                "`{} -vV` reported no `host:` line:\n{verbose_version}",
                rustc.display()
            )
        })
        .and_then(|host| validate_target(host.trim()))
}

/// Target triples become one build-directory path component and one status
/// key component. Accept Rust-style triples, including dotted ARM targets,
/// while rejecting path syntax and empty/special components.
fn validate_target(target: &str) -> Result<String, String> {
    if target.ends_with(".json") {
        return Err(format!(
            "invalid target triple `{target}`: custom target JSON specifications are not supported"
        ));
    }
    let valid = !target.is_empty()
        && target != "."
        && target != ".."
        && target
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'));
    if valid {
        Ok(target.to_owned())
    } else {
        Err(format!(
            "invalid target triple `{target}`: expected only ASCII letters, digits, `-`, `_`, or `.`"
        ))
    }
}

/// Whether the bundled runtime must use its platform allocator instead of
/// mimalloc for `target`.
///
/// mimalloc's backend is C and does not support WebAssembly targets. WASI
/// supplies the libc allocator used by reussir-rt's no-default-features
/// backend; freestanding wasm targets are classified the same way so they
/// fail, if unsupported, at that platform allocator boundary rather than in
/// mimalloc's native C build.
fn target_should_disable_mimalloc(target: &str) -> bool {
    matches!(target.split('-').next(), Some("wasm32" | "wasm64"))
}

/// Whether `RENE_RT_PLATFORM_ALLOCATOR` forces the platform-allocator backend
/// on every target.
///
/// mimalloc's private heap is invisible to the sanitizers, so a program built
/// through rene cannot be checked with ASan/LSan/MSan while the runtime keeps
/// its default backend — the allocation and free events never reach
/// compiler-rt. This is the escape hatch for that: it selects the same
/// `--no-default-features` backend the wasm targets take, whose libc/UCRT
/// allocation the sanitizers do interpose.
///
/// Any value other than `0` enables it. It changes which allocator a built
/// program uses, so it is a diagnostic knob, not a supported build mode.
fn platform_allocator_forced() -> bool {
    std::env::var_os("RENE_RT_PLATFORM_ALLOCATOR").is_some_and(|value| value != "0")
}

async fn capture(program: &Path, args: &[&str]) -> Result<String, String> {
    let out = compio::process::Command::new(program)
        .args(args)
        .stdout(Stdio::piped())
        .expect("pipe stdout")
        .stderr(Stdio::piped())
        .expect("pipe stderr")
        .output()
        .await
        .map_err(|e| format!("cannot run `{} {}`: {e}", program.display(), args.join(" ")))?;
    if !out.status.success() {
        return Err(format!(
            "`{} {}` failed: {}",
            program.display(),
            args.join(" "),
            String::from_utf8_lossy(&out.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_owned())
}

#[derive(Serialize)]
struct RuntimeRecipe<'a> {
    bundle: &'a str,
    rustc: &'a Path,
    rustc_version: &'a str,
    target: &'a str,
    rust_libdir: &'a Path,
    cargo: &'a Path,
    platform_allocator: bool,
    sanitizers: &'a [String],
    rustflags: Option<std::ffi::OsString>,
    encoded_flags: Option<std::ffi::OsString>,
    codegen_environment: std::collections::BTreeMap<String, std::ffi::OsString>,
}

/// Acquire the globally shared runtime for this toolchain and target. The
/// cache owns its reader lease through every compiler and final linker.
pub async fn prepare(
    cache: &std::sync::Arc<crate::cache::Cache>,
    dir: &BuildDir,
    requested_target: Option<&str>,
    linker: Option<&Path>,
    sanitizers: &[String],
    progress: &indicatif::MultiProgress,
) -> Result<RtArtifacts, String> {
    let toolchain = Toolchain::resolve(requested_target).await?;
    let hash = bundle_hash();
    let key = compcache::Key::from_serde(
        "rene-rust-runtime-v1",
        &RuntimeRecipe {
            bundle: &hash,
            rustc: &toolchain.rustc,
            rustc_version: &toolchain.version,
            target: &toolchain.target,
            rust_libdir: &toolchain.rust_libdir,
            cargo: &toolchain.cargo,
            platform_allocator: target_should_disable_mimalloc(&toolchain.target)
                || platform_allocator_forced()
                || !sanitizers.is_empty(),
            sanitizers,
            rustflags: std::env::var_os("RUSTFLAGS"),
            encoded_flags: std::env::var_os("CARGO_ENCODED_RUSTFLAGS"),
            codegen_environment: std::env::vars_os()
                .filter_map(|(key, value)| {
                    let key = key.into_string().ok()?;
                    (key.starts_with("CARGO_PROFILE_RELEASE_")
                        || (key.starts_with("CARGO_TARGET_") && key.ends_with("_RUSTFLAGS"))
                        || matches!(key.as_str(), "CC" | "CXX" | "CFLAGS" | "CXXFLAGS")
                        || key.starts_with("CC_")
                        || key.starts_with("CFLAGS_"))
                    .then_some((key, value))
                })
                .collect(),
        },
    )
    .map_err(|e| e.to_string())?;
    let cache = std::sync::Arc::clone(cache);
    let linker = linker.map(Path::to_owned);
    let sanitizers = sanitizers.to_vec();
    let progress = progress.clone();
    let artifacts = compio::runtime::spawn_blocking(move || -> Result<RtArtifacts, String> {
        let lease = cache
            .runtimes
            .get_or_insert_runtime(key, &|directory: &Path| {
                tracing::debug!(target = %toolchain.target, "baking shared reussir-rt");
                unpack(directory).map_err(compcache::Error::producer)?;
                let src_dir = directory.join(RT_DIR);
                let bar = progress.add(
                    indicatif::ProgressBar::new_spinner().with_message("baking shared reussir-rt"),
                );
                bar.enable_steady_tick(Duration::from_millis(120));
                let runtime =
                    compio::runtime::Runtime::new().map_err(compcache::Error::producer)?;
                let built = runtime.block_on(cargo_build(
                    &toolchain,
                    &src_dir,
                    linker.as_deref(),
                    &sanitizers,
                ));
                bar.finish_and_clear();
                progress.remove(&bar);
                let (rlib, staticlib, deps_dirs) =
                    built.map_err(|e| compcache::Error::producer(std::io::Error::other(e)))?;
                if let Some(path) = rcgmp_runtime_path(&toolchain.target, &deps_dirs) {
                    std::fs::write(path, RCGMP_RUNTIME).map_err(compcache::Error::producer)?;
                }
                let relative = |path: &Path| {
                    let canonical = path.canonicalize().map_err(compcache::Error::producer)?;
                    canonical
                        .strip_prefix(directory)
                        .map(Path::to_owned)
                        .map_err(|e| {
                            compcache::Error::producer(std::io::Error::other(format!(
                                "runtime artifact outside cache: {e}"
                            )))
                        })
                };
                let record = RtArtifacts {
                    target: toolchain.target.clone(),
                    rustc: toolchain.rustc.clone(),
                    rustc_version: toolchain.version.clone(),
                    rust_libdir: toolchain.rust_libdir.clone(),
                    rlib: relative(&rlib)?,
                    staticlib: relative(&staticlib)?,
                    deps_dirs: deps_dirs
                        .iter()
                        .map(|p| relative(p))
                        .collect::<compcache::Result<_>>()?,
                };
                std::fs::write(
                    directory.join("runtime.json"),
                    serde_json::to_vec(&record).map_err(compcache::Error::producer)?,
                )
                .map_err(compcache::Error::producer)?;
                crate::cache::sync_tree(directory)
            })
            .map_err(|e| format!("runtime cache (RENE_CACHE_DIR): {e}"))?;
        let mut record: RtArtifacts = serde_json::from_slice(
            &std::fs::read(lease.path().join("runtime.json")).map_err(|e| e.to_string())?,
        )
        .map_err(|e| e.to_string())?;
        record.rlib = lease.path().join(&record.rlib);
        record.staticlib = lease.path().join(&record.staticlib);
        record.deps_dirs = record
            .deps_dirs
            .iter()
            .map(|p| lease.path().join(p))
            .collect();
        cache.retain(lease);
        Ok(record)
    })
    .await
    .map_err(|e| format!("runtime cache worker failed: {e:?}"))??;
    let record = serde_json::to_string(&artifacts).map_err(|e| e.to_string())?;
    dir.set_status(&[
        (&tables::rt_source_hash_key(&artifacts.target), &hash),
        (&tables::rt_artifacts_key(&artifacts.target), &record),
    ])
    .map_err(|e| e.to_string())?;
    Ok(artifacts)
}

/// Run `cargo build --release` in `src_dir`, reading the JSON message
/// stream for the artifact locations (no directory guessing). The stream is
/// parsed once cargo exits — live progress reaches the user through the
/// inherited stderr regardless. Returns the runtime's (rlib, staticlib).
async fn cargo_build(
    toolchain: &Toolchain,
    src_dir: &Path,
    linker: Option<&Path>,
    sanitizers: &[String],
) -> Result<(PathBuf, PathBuf, Vec<PathBuf>), String> {
    let mut cmd = compio::process::Command::new(&toolchain.cargo);
    cmd.args(["build", "--release", "--target", toolchain.target.as_str()]);
    // The bundled mimalloc backend builds C code and is not yet a WebAssembly
    // allocator. WASI supplies the libc malloc family used by the runtime's
    // size-recovering fallback, so select that backend for wasm targets.
    if target_should_disable_mimalloc(&toolchain.target)
        || platform_allocator_forced()
        || !sanitizers.is_empty()
    {
        cmd.arg("--no-default-features");
    }
    cmd.arg("--message-format=json-render-diagnostics")
        .current_dir(src_dir)
        // Pin the rustc cargo delegates to, so the recorded version and
        // libdir describe the compiler that actually built the artifacts.
        .env("RUSTC", &toolchain.rustc)
        // Cargo's new build-dir layout drops target/<triple>/release/deps —
        // the directory this bake hands rrc as --polyffi-libdir. The
        // unpacked runtime tree sits outside the workspace, so the repo's
        // .cargo/config.toml opt-out does not reach it; pin the old layout
        // here until the bake hands rrc explicit artifact paths instead.
        .env("CARGO_UNSTABLE_BUILD_DIR_NEW_LAYOUT", "false")
        .env("CARGO_TARGET_DIR", src_dir.join("target"))
        .env("CARGO_BUILD_BUILD_DIR", src_dir.join("target"));
    if !sanitizers.is_empty() {
        let mut flags: Vec<String> = if let Ok(encoded) = std::env::var("CARGO_ENCODED_RUSTFLAGS") {
            encoded.split('\x1f').map(str::to_owned).collect()
        } else {
            std::env::var("RUSTFLAGS")
                .unwrap_or_default()
                .split_whitespace()
                .map(str::to_owned)
                .collect()
        };
        for sanitizer in sanitizers
            .iter()
            .filter(|s| matches!(s.as_str(), "address" | "memory" | "thread"))
        {
            flags.push(format!("-Zsanitizer={sanitizer}"));
        }
        cmd.env("CARGO_ENCODED_RUSTFLAGS", flags.join("\x1f"));
    }
    if let Some(linker) = linker {
        // Cargo's target-specific linker configuration, as an environment
        // variable: `CARGO_TARGET_<TARGET>_LINKER`. Unlike RUSTFLAGS this
        // composes with whatever the user's environment already carries.
        let key = format!(
            "CARGO_TARGET_{}_LINKER",
            toolchain
                .target
                .chars()
                .map(|ch| {
                    if ch.is_ascii_alphanumeric() {
                        ch.to_ascii_uppercase()
                    } else {
                        '_'
                    }
                })
                .collect::<String>()
        );
        cmd.env(key, linker);
    }
    // Both streams are captured: the JSON messages on stdout carry the
    // artifact locations, and cargo's human progress on stderr stays out of
    // the progress bars — replayed below only if the bake fails.
    let out = cmd
        .stdout(Stdio::piped())
        .expect("pipe stdout")
        .stderr(Stdio::piped())
        .expect("pipe stderr")
        .output()
        .await
        .map_err(|e| format!("cannot run `{}`: {e}", toolchain.cargo.display()))?;

    let mut rlib = None;
    let mut staticlib = None;
    let mut deps_dirs = std::collections::BTreeSet::new();
    let stdout = String::from_utf8_lossy(&out.stdout);
    for line in stdout.lines() {
        let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        if msg["reason"] != "compiler-artifact" {
            continue;
        }
        let name = msg["target"]["name"].as_str().unwrap_or_default();
        tracing::debug!(%name, "compiled");
        for file in msg["filenames"].as_array().into_iter().flatten() {
            let Some(path) = file.as_str() else { continue };
            let path = Path::new(path);
            if let Some(parent) = path.parent()
                && parent.file_name() == Some("deps".as_ref())
            {
                // Includes both target dependencies and host-built proc
                // macros when Cargo separates them for `--target`.
                deps_dirs.insert(parent.to_owned());
            }
            if name != "reussir-rt" && name != "reussir_rt" {
                continue;
            }
            match path.extension().and_then(|e| e.to_str()) {
                Some("rlib") => rlib = Some(path.to_owned()),
                // `.a` everywhere but MSVC, `.lib` there.
                Some("a") | Some("lib") => staticlib = Some(path.to_owned()),
                _ => {}
            }
        }
    }
    if !out.status.success() {
        return Err(format!(
            "building reussir-rt failed:\n{}",
            String::from_utf8_lossy(&out.stderr).trim_end()
        ));
    }
    match (rlib, staticlib) {
        (Some(rlib), Some(staticlib)) => {
            // Cargo may report the uplifted runtime rlib
            // (`…/release/lib….rlib`) rather than its hashed `deps/` copy.
            // Ensure that target dependency directory is present even when
            // no dependency artifact message named it.
            let parent = rlib.parent().unwrap_or(Path::new("."));
            let runtime_deps = if parent.file_name() == Some("deps".as_ref()) {
                parent.to_owned()
            } else {
                parent.join("deps")
            };
            deps_dirs.insert(runtime_deps);
            Ok((rlib, staticlib, deps_dirs.into_iter().collect()))
        }
        _ => Err("cargo succeeded but did not report the runtime's rlib and staticlib".to_owned()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocator_backend_follows_the_target_architecture() {
        for target in [
            "wasm32-wasip1",
            "wasm32-unknown-unknown",
            "wasm64-unknown-unknown",
        ] {
            assert!(target_should_disable_mimalloc(target), "{target}");
        }
        for target in [
            "x86_64-unknown-linux-gnu",
            "aarch64-apple-darwin",
            "wasmtime-unknown-linux-gnu",
        ] {
            assert!(!target_should_disable_mimalloc(target), "{target}");
        }
    }

    #[test]
    fn the_bundle_unpacks_to_a_standalone_crate() {
        let tmp = tempfile::tempdir().unwrap();
        unpack(tmp.path()).unwrap();

        let manifest = std::fs::read_to_string(tmp.path().join(RT_DIR).join("Cargo.toml")).unwrap();
        // Workspace inheritance must be fully resolved…
        assert!(!manifest.contains("workspace = true"), "{manifest}");
        assert!(manifest.contains("edition = \"2024\""), "{manifest}");
        // …the crate must be its own workspace root (or an enclosing
        // workspace — e.g. a user project in one — captures and rejects it)…
        assert!(manifest.contains("[workspace]"), "{manifest}");
        // …and the nightly-only optimization feature dropped, so the user's
        // stable toolchain can build the runtime. (Prose comments mentioning
        // nightly survive; only the feature string must be gone.)
        assert!(!manifest.contains("\"nightly\""), "{manifest}");

        assert!(tmp.path().join(RT_DIR).join("src").join("lib.rs").is_file());
        // Versions are pinned through the bundled lock file (no vendoring;
        // the host cargo fetches, the lock decides versions).
        assert!(tmp.path().join(RT_DIR).join("Cargo.lock").is_file());

        // Even a feature-off Cargo build must resolve optional path dependencies.
        let pjrt = tmp.path().join("reussir-pjrt-sys");
        let manifest = std::fs::read_to_string(pjrt.join("Cargo.toml")).unwrap();
        assert!(!manifest.contains("workspace = true"), "{manifest}");
        assert!(pjrt.join("build.rs").is_file());
        assert!(pjrt.join("header.rs").is_file());
        assert!(pjrt.join("src/lib.rs").is_file());
        let stablehlo = tmp.path().join("reussir-stablehlo");
        let manifest = std::fs::read_to_string(stablehlo.join("Cargo.toml")).unwrap();
        assert!(!manifest.contains("workspace = true"), "{manifest}");
        // The workspace patch, not the crates.io release, provides LLVM 23.
        assert!(manifest.contains("reussir-lang/mlir-sys"), "{manifest}");
        assert!(stablehlo.join("build.rs").is_file());
        assert!(stablehlo.join("src/lib.rs").is_file());
        assert!(
            tmp.path()
                .join("reussir-backend-sys/native_archive.rs")
                .is_file()
        );
    }

    #[test]
    fn target_triples_are_safe_path_components() {
        for target in [
            "x86_64-unknown-linux-gnu",
            "wasm32-unknown-unknown",
            "thumbv8m.main-none-eabi",
        ] {
            assert_eq!(validate_target(target).unwrap(), target);
        }
        for target in ["", ".", "..", "../wasm32", "x/y", "target.json"] {
            assert!(validate_target(target).is_err(), "{target}");
        }
    }

    #[test]
    fn rcgmp_runtime_is_staged_beside_the_runtime_rlib() {
        let root = tempfile::tempdir().unwrap();
        let (host_deps, target_deps) = (root.path().join("host"), root.path().join("target"));
        for dir in [&host_deps, &target_deps] {
            std::fs::create_dir(dir).unwrap();
        }
        std::fs::write(target_deps.join("libreussir_rt.rlib"), []).unwrap();
        let deps_dirs = [host_deps, target_deps.clone()];

        assert_eq!(rcgmp_runtime_path("wasm32-wasip1", &deps_dirs), None);
        let own = rcgmp_runtime_path(env!("RENE_RCGMP_RUNTIME_TARGET"), &deps_dirs);
        if RCGMP_RUNTIME.is_empty() {
            assert_eq!(own, None);
        } else {
            assert_eq!(own, Some(target_deps.join("libRCGMPRuntime.a")));
        }
    }

    #[test]
    fn bundle_hash_is_stable_hex() {
        let h = bundle_hash();
        assert_eq!(h.len(), 64);
        assert_eq!(h, bundle_hash());
    }
}
