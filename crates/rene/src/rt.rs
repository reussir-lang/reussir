//! Baking the bundled `reussir-rt` runtime in the per-user cache.
//!
//! The `rene` binary embeds the runtime *source* as a zstd-compressed tar
//! (packed by `build.rs` — source, not binaries, is what keeps the bundle
//! cross-platform: rlibs and symbol hashes are version-locked to the rustc
//! that produced them, so the runtime must be built by the user's toolchain;
//! see docs/design/system-rust-runtime.md). `prepare` bakes it in a
//! [`Cache`] entry keyed by everything the bake depends on: the bundle's
//! content, the target, the toolchain, the allocator backend, and the
//! environment's rustflags. It unpacks the source into the entry and runs
//! Cargo with `--target <target>` there, yielding the rlib+deps directory
//! polymorphic FFI links against (`rustc -L`) and the static library final
//! executables link. Every build of every package on the machine thus
//! shares one bake per toolchain and target. Each build directory still
//! records the bake it uses, which its freshness checks and `inspect` read.
//!
//! The bake does not depend on where it runs. The toolchain is resolved
//! once, in the package's directory, and its binaries are pinned by path:
//! rustup picks a toolchain by the directory its proxy runs in, which need
//! not be one the project's `rust-toolchain.toml` covers. Cargo's target
//! directory is pinned inside the bake, whatever `CARGO_TARGET_DIR` or the
//! user's cargo configuration say.

use std::path::{Path, PathBuf};
use std::process::Stdio;

use serde::{Deserialize, Serialize};

use crate::cache::{self, Acquired, Cache, EntryId, RecordedFile, Request, Slot, Usage};
use crate::db::BuildDir;
use crate::tables;

/// The runtime crate's directory name inside the unpacked bundle.
pub const RT_DIR: &str = "reussir-rt";

/// The unpacked source tree inside a bake's cache entry.
const SRC_DIR: &str = "src";

/// Cargo's target directory inside a bake's cache entry.
const TARGET_DIR: &str = "target";

/// The bake's record inside its cache entry, a JSON [`BakeRecord`]. Written
/// last and atomically: its presence marks the entry complete.
const RECORD: &str = "artifacts.json";

/// The bundled runtime source (tar.zst), packed by `build.rs`.
static RT_BUNDLE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/reussir-rt.tar.zst"));

/// The rcgmp integer runtime archive built for rene's own target, or empty
/// when rene was built without it (packed by `build.rs`).
static RCGMP_RUNTIME: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/rcgmp-runtime.a"));

/// The file name `rrc`'s link step looks up in the runtime library directories.
const RCGMP_RUNTIME_ARCHIVE: &str = "libRCGMPRuntime.a";

/// Blake3 hex digest of the bundles, part of a bake's cache key.
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

/// The toolchain a bake runs, and so the one everything linked against
/// the runtime must use.
struct Toolchain {
    /// The compiler the selected rustc resolves to in the package
    /// directory: `<sysroot>/bin/rustc`, or the selection itself when the
    /// sysroot has no such binary.
    rustc: PathBuf,
    sysroot: PathBuf,
    /// `rustc --version` of the toolchain.
    version: String,
    /// The configured cargo (`REUSSIR_CARGO`, `CARGO`), else the sysroot's,
    /// else `cargo` on `PATH`.
    cargo: PathBuf,
    /// The resolved target the bake builds for, and so the key of cargo's
    /// target-specific configuration.
    target: String,
    rust_libdir: PathBuf,
}

impl Toolchain {
    /// `REUSSIR_CARGO`/`REUSSIR_RUSTC` first (the explicit override rrc also
    /// honors), then cargo's own `CARGO`/`RUSTC`, then `PATH` — which
    /// respects rustup shims, so a `rust-toolchain.toml` in the user's
    /// project still picks the toolchain naturally. The selected rustc runs
    /// in `package_dir`, the directory whose toolchain file applies, and
    /// names its sysroot, whose binaries the bake then runs by path.
    async fn resolve(requested_target: Option<&str>, package_dir: &Path) -> Result<Self, String> {
        let selected = pick_program("REUSSIR_RUSTC", "RUSTC", "rustc");
        let verbose_version = capture(&selected, &["-vV"], package_dir).await?;
        let version = verbose_version
            .lines()
            .next()
            .unwrap_or_default()
            .to_owned();
        let target = match requested_target {
            Some(target) => validate_target(target)?,
            None => validate_target(&host(&selected, &verbose_version)?)?,
        };
        let printed = capture(
            &selected,
            &[
                "--print",
                "sysroot",
                "--print",
                "target-libdir",
                "--target",
                &target,
            ],
            package_dir,
        )
        .await?;
        let mut lines = printed.lines().map(str::trim);
        let (Some(sysroot), Some(rust_libdir)) = (lines.next(), lines.next()) else {
            return Err(format!(
                "`{}` printed no sysroot and target libdir:\n{printed}",
                selected.display()
            ));
        };
        let sysroot = PathBuf::from(sysroot);
        let binary = |name: &str| {
            let path = sysroot
                .join("bin")
                .join(format!("{name}{}", std::env::consts::EXE_SUFFIX));
            path.is_file().then_some(path)
        };
        let rustc = binary("rustc").unwrap_or(selected);
        let cargo = configured_program("REUSSIR_CARGO", "CARGO")
            .or_else(|| binary("cargo"))
            .unwrap_or_else(|| PathBuf::from("cargo"));
        Ok(Toolchain {
            rustc,
            rust_libdir: PathBuf::from(rust_libdir),
            sysroot,
            version,
            cargo,
            target,
        })
    }
}

/// The program `specific`, else `generic`, names, if either is set.
fn configured_program(specific: &str, generic: &str) -> Option<PathBuf> {
    std::env::var_os(specific)
        .or_else(|| std::env::var_os(generic))
        .map(PathBuf::from)
}

fn pick_program(specific: &str, generic: &str, default: &str) -> PathBuf {
    configured_program(specific, generic).unwrap_or_else(|| PathBuf::from(default))
}

/// The host triple in `rustc -vV`'s report.
fn host(rustc: &Path, verbose_version: &str) -> Result<String, String> {
    verbose_version
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .map(|host| host.trim().to_owned())
        .ok_or_else(|| {
            format!(
                "`{} -vV` reported no `host:` line:\n{verbose_version}",
                rustc.display()
            )
        })
}

/// Resolve the target used by a command that does not bake the runtime:
/// an explicit/profile target wins, otherwise use the host of the rustc
/// `package_dir` selects.
pub async fn resolve_target(requested: Option<&str>, package_dir: &Path) -> Result<String, String> {
    if let Some(target) = requested {
        return validate_target(target);
    }
    let rustc = pick_program("REUSSIR_RUSTC", "RUSTC", "rustc");
    let verbose_version = capture(&rustc, &["-vV"], package_dir).await?;
    validate_target(&host(&rustc, &verbose_version)?)
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

/// Run `program` with `args` in `dir`, returning its trimmed stdout.
async fn capture(program: &Path, args: &[&str], dir: &Path) -> Result<String, String> {
    let out = compio::process::Command::new(program)
        .args(args)
        .current_dir(dir)
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

/// Ensure the runtime is baked for the target and the toolchain
/// `package_dir` selects, and record the bake in `dir`. The bake is a
/// [`Cache`] entry used in place: a complete one as it is, otherwise baked
/// by whoever holds its lock while concurrent builds wait for it.
///
/// `linker`, when given, is pinned for the bake's own links (the runtime
/// dylib) — the same pinning `rrc --linker` gives the driver-level links,
/// needed where rustc's discovery resolves the wrong tool (a vcvars shell
/// whose PATH carries a coreutils `link` ahead of MSVC's). It is not part of
/// the bake's key: whichever working linker linked the runtime, the
/// artifacts are equivalent.
pub async fn prepare(
    dir: &BuildDir,
    cache: &Cache,
    package_dir: &Path,
    requested_target: Option<&str>,
    linker: Option<&Path>,
    progress: &indicatif::MultiProgress,
) -> Result<RtArtifacts, String> {
    let toolchain = Toolchain::resolve(requested_target, package_dir).await?;
    let platform_allocator =
        target_should_disable_mimalloc(&toolchain.target) || platform_allocator_forced();
    let notify = |msg: &str| progress.suspend(|| tracing::info!("{msg}"));
    let request = Request {
        id: EntryId::new(cache::kind::RT, bake_key(&toolchain, platform_allocator)),
        usage: Usage::InPlace,
        what: "baking reussir-rt",
        notify: &notify,
    };
    let artifacts = match cache
        .acquire(&request, |entry| cached_bake(entry, &toolchain))
        .await?
    {
        Acquired::Ready(artifacts) => {
            tracing::debug!("reussir-rt is up to date");
            artifacts
        }
        Acquired::Build(slot) => {
            bake(slot, &toolchain, platform_allocator, linker, progress).await?
        }
    };

    let record = serde_json::to_string(&artifacts).map_err(|e| e.to_string())?;
    let key = tables::rt_artifacts_key(&artifacts.target);
    if dir.status(&key).map_err(|e| e.to_string())?.as_deref() != Some(record.as_str()) {
        dir.set_status(&[(key.as_str(), record.as_str())])
            .map_err(|e| e.to_string())?;
    }
    Ok(artifacts)
}

/// The cache key of a bake: everything its artifacts depend on. The
/// rustc's path is part of it, not just its version: a binary at another
/// path must never inherit a bake merely because it reports the same
/// version string. The rustflags environment changes the code cargo emits,
/// so it is part of the key too.
fn bake_key(toolchain: &Toolchain, platform_allocator: bool) -> String {
    let mut env: Vec<(String, String)> = std::env::vars_os()
        .filter_map(|(name, value)| {
            let name = name.into_string().ok()?;
            let relevant = matches!(
                name.as_str(),
                "RUSTFLAGS" | "CARGO_ENCODED_RUSTFLAGS" | "CARGO_BUILD_RUSTFLAGS"
            ) || (name.starts_with("CARGO_TARGET_") && name.ends_with("_RUSTFLAGS"))
                || name.starts_with("CARGO_PROFILE_RELEASE_");
            relevant.then(|| (name, value.to_string_lossy().into_owned()))
        })
        .collect();
    env.sort();
    let env = env
        .iter()
        .map(|(name, value)| format!("{name}={value}"))
        .collect::<Vec<_>>()
        .join("\0");
    cache::key([
        b"reussir-rt bake v1".as_slice(),
        bundle_hash().as_bytes(),
        toolchain.target.as_bytes(),
        toolchain.rustc.as_os_str().as_encoded_bytes(),
        toolchain.sysroot.as_os_str().as_encoded_bytes(),
        toolchain.version.as_bytes(),
        toolchain.cargo.as_os_str().as_encoded_bytes(),
        &[u8::from(platform_allocator)],
        env.as_bytes(),
    ])
}

/// A bake's record in its cache entry ([`RECORD`]).
#[derive(Serialize, Deserialize)]
struct BakeRecord {
    artifacts: RtArtifacts,
    /// The sysroot of the toolchain that baked it.
    sysroot: PathBuf,
    /// The files the bake vouches for, with their sizes: the runtime's rlib
    /// and staticlib, and the staged rcgmp archive.
    files: Vec<RecordedFile>,
}

/// The complete bake in `entry`, if it is usable: its record parses,
/// describes this toolchain, names nothing outside the entry, and every
/// file it vouches for is still there, whole.
fn cached_bake(entry: &Path, toolchain: &Toolchain) -> Option<RtArtifacts> {
    let record = std::fs::read_to_string(entry.join(RECORD)).ok()?;
    let record = serde_json::from_str::<BakeRecord>(&record).ok()?;
    let artifacts = &record.artifacts;
    let valid = artifacts.target == toolchain.target
        && artifacts.rustc == toolchain.rustc
        && artifacts.rustc_version == toolchain.version
        && record.sysroot == toolchain.sysroot
        && within(artifacts, entry)
        && record.files.iter().all(|file| file.intact(entry))
        && rcgmp_runtime_path(&artifacts.target, &artifacts.deps_dirs)
            .is_none_or(|path| path.is_file())
        && artifacts.libdirs().all(Path::is_dir);
    if !valid {
        tracing::debug!(entry = %entry.display(), "the cached reussir-rt bake is incomplete");
    }
    valid.then_some(record.artifacts)
}

/// Whether every cargo artifact of the bake lies inside its entry. Cargo
/// writes wherever its configuration says; artifacts outside the entry
/// could be another bake's to overwrite.
fn within(artifacts: &RtArtifacts, entry: &Path) -> bool {
    [&artifacts.rlib, &artifacts.staticlib]
        .into_iter()
        .chain(&artifacts.deps_dirs)
        .all(|path| path.starts_with(entry))
}

/// Bake into the slot's entry from scratch, in place: the cargo target
/// directory records absolute paths, so the bake cannot be staged elsewhere
/// and moved. The record is written last.
async fn bake(
    slot: Slot,
    toolchain: &Toolchain,
    platform_allocator: bool,
    linker: Option<&Path>,
    progress: &indicatif::MultiProgress,
) -> Result<RtArtifacts, String> {
    slot.clear()?;
    let entry = slot.dir().to_owned();
    let src_root = entry.join(SRC_DIR);
    std::fs::create_dir(&src_root)
        .map_err(|e| format!("cannot create `{}`: {e}", src_root.display()))?;
    tracing::debug!(dest = %src_root.display(), "extracting bundled reussir-rt source");
    unpack(&src_root).map_err(|e| format!("cannot unpack the runtime bundle: {e}"))?;

    tracing::debug!(
        cargo = %toolchain.cargo.display(),
        rustc = %toolchain.version,
        entry = %entry.display(),
        "building reussir-rt"
    );
    // The bake is the build's one slow cold step (a real cargo build, with
    // the network on a fresh directory); a spinner carries that while
    // cargo's own output is captured — and replayed if the bake fails.
    let bar = progress.add(
        indicatif::ProgressBar::new_spinner()
            .with_message("baking reussir-rt (once per toolchain and target)"),
    );
    bar.enable_steady_tick(std::time::Duration::from_millis(120));
    let result = cargo_build(
        toolchain,
        &src_root.join(RT_DIR),
        &entry.join(TARGET_DIR),
        platform_allocator,
        linker,
    )
    .await;
    bar.finish_and_clear();
    progress.remove(&bar);
    let (rlib, staticlib, deps_dirs) = result?;
    let mut files = vec![rlib.clone(), staticlib.clone()];
    if let Some(path) = rcgmp_runtime_path(&toolchain.target, &deps_dirs) {
        std::fs::write(&path, RCGMP_RUNTIME)
            .map_err(|e| format!("cannot stage `{}`: {e}", path.display()))?;
        files.push(path);
    }

    let artifacts = RtArtifacts {
        target: toolchain.target.clone(),
        rustc: toolchain.rustc.clone(),
        rustc_version: toolchain.version.clone(),
        rust_libdir: toolchain.rust_libdir.clone(),
        deps_dirs,
        staticlib,
        rlib,
    };
    if !within(&artifacts, &entry) {
        return Err(format!(
            "cargo put reussir-rt's artifacts outside its cache entry `{}`",
            entry.display()
        ));
    }
    let record = BakeRecord {
        sysroot: toolchain.sysroot.clone(),
        files: files
            .iter()
            .map(|file| RecordedFile::measure(&entry, file))
            .collect::<Result<_, _>>()?,
        artifacts,
    };
    let json = serde_json::to_string(&record).map_err(|e| e.to_string())?;
    slot.commit(RECORD, json.as_bytes())?;
    tracing::debug!(staticlib = %record.artifacts.staticlib.display(), "reussir-rt ready");
    Ok(record.artifacts)
}

/// Run `cargo build --release` in `src_dir`, into `target_dir`, reading the
/// JSON message stream for the artifact locations (no directory guessing).
/// The stream is parsed once cargo exits — live progress reaches the user
/// through the inherited stderr regardless. Returns the runtime's (rlib,
/// staticlib) and its dependency directories.
async fn cargo_build(
    toolchain: &Toolchain,
    src_dir: &Path,
    target_dir: &Path,
    platform_allocator: bool,
    linker: Option<&Path>,
) -> Result<(PathBuf, PathBuf, Vec<PathBuf>), String> {
    let mut cmd = compio::process::Command::new(&toolchain.cargo);
    cmd.args(["build", "--release", "--target", toolchain.target.as_str()]);
    // The bundled mimalloc backend builds C code and is not yet a WebAssembly
    // allocator. WASI supplies the libc malloc family used by the runtime's
    // size-recovering fallback, so select that backend for wasm targets.
    // (`RENE_RT_PLATFORM_ALLOCATOR` forces the same backend everywhere.)
    if platform_allocator {
        cmd.arg("--no-default-features");
    }
    cmd.arg("--message-format=json-render-diagnostics")
        // The bake owns every artifact it hands out: the target directory,
        // and cargo's separate build directory, stay inside the cache entry
        // whatever `CARGO_TARGET_DIR` or the user's cargo configuration say.
        .arg("--target-dir")
        .arg(target_dir)
        .env("CARGO_BUILD_BUILD_DIR", target_dir)
        .current_dir(src_dir)
        // Pin the rustc cargo delegates to, so the recorded version and
        // libdir describe the compiler that actually built the artifacts.
        .env("RUSTC", &toolchain.rustc)
        // Cargo's new build-dir layout drops target/<triple>/release/deps —
        // the directory this bake hands rrc as --polyffi-libdir. The
        // unpacked runtime tree sits outside the workspace, so the repo's
        // .cargo/config.toml opt-out does not reach it; pin the old layout
        // here until the bake hands rrc explicit artifact paths instead.
        .env("CARGO_UNSTABLE_BUILD_DIR_NEW_LAYOUT", "false");
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

    /// A cached bake serves only the toolchain it names, only while it owns
    /// every artifact it names, and only while each file it vouches for is
    /// whole.
    #[test]
    fn a_cached_bake_is_ready_only_while_whole() {
        let tmp = tempfile::tempdir().unwrap();
        let entry = tmp.path().join("rt/key");
        let release = entry.join("target/x86_64-unknown-test/release");
        let rust_libdir = tmp.path().join("sysroot/lib");
        for dir in [&release.join("deps"), &rust_libdir] {
            std::fs::create_dir_all(dir).unwrap();
        }
        let rlib = release.join("deps/libreussir_rt-0.rlib");
        let staticlib = release.join("libreussir_rt.a");
        std::fs::write(&rlib, "rlib").unwrap();
        std::fs::write(&staticlib, "staticlib").unwrap();
        let toolchain = |version: &str| Toolchain {
            rustc: tmp.path().join("sysroot/bin/rustc"),
            sysroot: tmp.path().join("sysroot"),
            version: version.to_owned(),
            cargo: PathBuf::from("cargo"),
            target: "x86_64-unknown-test".to_owned(),
            rust_libdir: rust_libdir.clone(),
        };
        let ours = toolchain("rustc 1.0.0-test");
        let artifacts = RtArtifacts {
            target: ours.target.clone(),
            rustc: ours.rustc.clone(),
            rustc_version: ours.version.clone(),
            rust_libdir: rust_libdir.clone(),
            deps_dirs: vec![release.join("deps")],
            staticlib: staticlib.clone(),
            rlib: rlib.clone(),
        };
        let record = |artifacts: &RtArtifacts| {
            let record = BakeRecord {
                artifacts: artifacts.clone(),
                sysroot: ours.sysroot.clone(),
                files: [&rlib, &staticlib]
                    .map(|file| RecordedFile::measure(&entry, file).unwrap())
                    .to_vec(),
            };
            std::fs::write(entry.join(RECORD), serde_json::to_string(&record).unwrap()).unwrap();
        };
        record(&artifacts);
        assert!(cached_bake(&entry, &ours).is_some());
        assert!(cached_bake(&entry, &toolchain("rustc 2.0.0-test")).is_none());

        // A staticlib cut short, as a crash before it reached the disk
        // leaves it.
        std::fs::write(&staticlib, "static").unwrap();
        assert!(cached_bake(&entry, &ours).is_none());
        std::fs::write(&staticlib, "staticlib").unwrap();

        // A dependency directory outside the entry: another bake's to
        // overwrite.
        let mut escaped = artifacts.clone();
        escaped.deps_dirs.push(rust_libdir.clone());
        record(&escaped);
        assert!(cached_bake(&entry, &ours).is_none());
    }

    #[test]
    fn bundle_hash_is_stable_hex() {
        let h = bundle_hash();
        assert_eq!(h.len(), 64);
        assert_eq!(h, bundle_hash());
    }
}
