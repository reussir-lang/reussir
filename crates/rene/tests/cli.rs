//! End-to-end tests of the `rene` binary: build-directory locking, the clean
//! protocol, the source-graph table (`inspect`, and the three conditions that
//! rebuild it), and the runtime bake — driven through fake `rrc`/`cargo`/
//! `rustc` by default, with the real toolchain under `--ignored`.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use rene::db::BuildDir;
use rene::tables;

fn demo_manifest() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("demo-pkg")
        .join("rene.ncl")
}

/// A scratch package the test may mutate: a manifest plus a two-file source
/// graph (`src/lib.rr` declaring `mod math;`). Returns the manifest path.
/// (Unix-gated with its callers, the fake-toolchain tests; on Windows it
/// would be dead code under `-D warnings`.)
#[cfg(unix)]
fn package(dir: &Path, name: &str) -> PathBuf {
    let src = dir.join("src");
    std::fs::create_dir_all(&src).unwrap();
    std::fs::write(
        src.join("lib.rr"),
        "mod math;\npub fn entry(n: u64) -> u64 { math::add(n, 10) }\n",
    )
    .unwrap();
    std::fs::write(
        src.join("math.rr"),
        "pub fn add(a: u64, b: u64) -> u64 { a + b }\n",
    )
    .unwrap();
    write_manifest(dir, name, "0.1.0")
}

/// (Re)write the scratch package's manifest.
#[cfg(unix)]
fn write_manifest(dir: &Path, name: &str, version: &str) -> PathBuf {
    let path = dir.join("rene.ncl");
    std::fs::write(
        &path,
        format!("{{ package = {{ name = \"{name}\", version = \"{version}\" }} }}\n"),
    )
    .unwrap();
    path
}

fn rene(args: &[&str]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_rene"));
    cmd.args(args);
    // Keep the per-user cache out of the user's home. Tests that look into
    // the cache use their own ([`Fakes::cache`]).
    cmd.env(
        rene::cache::CACHE_DIR_ENV,
        Path::new(env!("CARGO_TARGET_TMPDIR")).join("rene-cache"),
    );
    cmd
}

fn run(cmd: &mut Command) -> Output {
    cmd.output().expect("failed to spawn rene")
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

#[test]
fn build_fails_cleanly_without_a_manifest() {
    let tmp = tempfile::tempdir().unwrap();
    let out = run(rene(&["build"]).current_dir(tmp.path()));
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("rene.ncl"),
        "stderr: {}",
        stderr(&out)
    );
}

#[test]
fn build_help_distinguishes_products_from_the_machine_target() {
    let out = run(&mut rene(&["build", "--help"]));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let help = String::from_utf8(out.stdout).unwrap();
    assert!(help.contains("--bin <NAME>"), "{help}");
    assert!(help.contains("--lib <NAME>"), "{help}");
    assert!(help.contains("--target <TRIPLE>"), "{help}");
    assert!(help.contains("--token-reuse-remarks"), "{help}");
}

#[test]
fn build_waits_for_a_build_dir_held_by_another_instance() {
    use std::process::Stdio;

    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("reussir-build");
    let held = BuildDir::open(&root).unwrap();
    // Once the waiter acquires the lock it will stop at this marker, before
    // touching the real toolchain. That makes the test about queueing alone.
    held.set_status(&[(tables::CLEANING_KEY, "true")]).unwrap();

    let mut child = rene(&["build", "--manifest-path"])
        .arg(demo_manifest())
        .arg("--build-dir")
        .arg(&root)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    std::thread::sleep(std::time::Duration::from_millis(100));
    assert!(child.try_wait().unwrap().is_none(), "build did not wait");

    drop(held);
    let out = child.wait_with_output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("rene clean") && !stderr(&out).contains("in use"),
        "stderr: {}",
        stderr(&out)
    );
}

#[test]
fn build_refuses_a_build_dir_with_a_pending_clean() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("reussir-build");
    // An interrupted clean: marker stamped, directory still there.
    let dir = BuildDir::open(&root).unwrap();
    dir.set_status(&[(tables::CLEANING_KEY, "true")]).unwrap();
    drop(dir);

    let out = run(rene(&["build", "--manifest-path"])
        .arg(demo_manifest())
        .arg("--build-dir")
        .arg(&root));
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("rene clean"),
        "stderr: {}",
        stderr(&out)
    );
}

#[test]
fn clean_is_a_noop_without_a_build_dir_and_removes_one_when_free() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("reussir-build");

    let out = run(rene(&["clean", "--build-dir"]).arg(&root));
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let dir = BuildDir::open(&root).unwrap();
    dir.set_status(&[("some", "state")]).unwrap();
    drop(dir);
    std::fs::write(root.join("artifact"), b"x").unwrap();

    let out = run(rene(&["clean", "--build-dir"]).arg(&root));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(!root.exists());
}

#[test]
fn clean_errors_out_when_the_db_is_in_use() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("reussir-build");
    let _held = BuildDir::open(&root).unwrap();

    let out = run(rene(&["clean", "--build-dir"]).arg(&root));
    assert_eq!(out.status.code(), Some(1));
    assert!(stderr(&out).contains("in use"), "stderr: {}", stderr(&out));
    assert!(root.exists());
}

/// A fake toolchain: stand-in `rrc`, `cargo`, and `rustc` scripts that record
/// every invocation, so a test can assert exactly what a build re-ran.
#[cfg(unix)]
struct Fakes {
    /// The directory holding the scripts and their run logs.
    dir: PathBuf,
    rrc: PathBuf,
    cargo: PathBuf,
    rustc: PathBuf,
    /// The libdir the fake `rustc` reports as its target libdir.
    libdir: PathBuf,
    /// The per-user cache [`Fakes::rene`] runs against.
    cache: PathBuf,
}

#[cfg(unix)]
impl Fakes {
    fn new(dir: &Path) -> Self {
        use std::os::unix::fs::PermissionsExt;

        let libdir = dir.join("fake-rust-libdir");
        std::fs::create_dir_all(&libdir).unwrap();

        let script = |name: &str, body: String| -> PathBuf {
            let path = dir.join(name);
            std::fs::write(&path, format!("#!/bin/sh\n{body}")).unwrap();
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
            path
        };

        let rustc = script(
            "fake-rustc",
            fake_rustc("9.9.9 (fake)", &dir.join("fake-sysroot"), &libdir),
        );
        // Counts invocations, records the linker rene pinned through cargo's
        // target-specific environment (keyed by the fake host triple above)
        // and the rustc it pinned, fabricates the artifacts in the target
        // directory (`--target-dir`, else `CARGO_TARGET_DIR`, else `target`
        // under the cwd, which is the unpacked source dir), and reports them
        // the way `--message-format=json` does. With `FAKE_CARGO_GATE` set,
        // it says it started (`<gate>.started`) and waits for the gate file.
        let cargo = script(
            "fake-cargo",
            r#"log="$(dirname "$0")"
echo run >> "$log/cargo-runs"
echo "$*" >> "$log/cargo-args"
echo "linker=$CARGO_TARGET_X86_64_UNKNOWN_FAKE_LINKER" >> "$log/cargo-env"
echo "$RUSTC" >> "$log/cargo-rustc"
target=""; target_dir="${CARGO_TARGET_DIR:-$PWD/target}"; prev=""
for arg in "$@"; do
  [ "$prev" = "--target" ] && target="$arg"
  [ "$prev" = "--target-dir" ] && target_dir="$arg"
  prev="$arg"
done
[ -n "$target" ] || exit 2
if [ -n "$FAKE_CARGO_GATE" ]; then
  touch "$FAKE_CARGO_GATE.started"
  i=0
  while [ ! -e "$FAKE_CARGO_GATE" ] && [ "$i" -lt 6000 ]; do sleep 0.01; i=$((i + 1)); done
fi
mkdir -p "$target_dir/$target/release/deps" "$target_dir/release/deps"
proc_macro="$target_dir/release/deps/libnum_enum_derive-fake.so"
rlib="$target_dir/$target/release/deps/libreussir_rt-0000.rlib"
staticlib="$target_dir/$target/release/libreussir_rt.a"
touch "$proc_macro" "$rlib" "$staticlib"
printf '{"reason":"compiler-artifact","target":{"name":"num_enum_derive"},"filenames":["%s"]}\n' "$proc_macro"
printf '{"reason":"compiler-artifact","target":{"name":"reussir-rt"},"filenames":["%s","%s"]}\n' "$rlib" "$staticlib"
"#
            .to_owned(),
        );
        // Stands in for `rrc`. A `--scan-deps` invocation counts itself and
        // reports the package root's `.rr` files as the source graph, crate
        // root first — the same JSON shape rrc emits, so a file added to the
        // package shows up in the next scan. Anything else is a compile:
        // it logs the full argv (plus the REUSSIR_RUSTC rene handed it) and
        // fabricates the `-o` artifact.
        let rrc = script(
            "fake-rrc",
            r#"root=""; input=""; out=""; report=""; package=""; color=auto; prev=""; scan=no
for arg in "$@"; do
  [ "$prev" = "--package-root" ] && root="$arg"
  [ "$prev" = "--package-name" ] && package="$arg"
  [ "$prev" = "-o" ] && out="$arg"
  [ "$prev" = "--token-reuse-remarks" ] && report="$arg"
  [ "$prev" = "--color" ] && color="$arg"
  [ "$arg" = "--scan-deps" ] && scan=yes
  case "$arg" in *.rr) input="$arg";; esac
  prev="$arg"
done
if [ "$scan" = no ]; then
  # Concurrent compiles append to one log; the shell may emit a line in
  # more than one write(2), which interleaves under contention (seen on
  # macOS). mkdir is the portable atomic lock.
  log="$(dirname "$0")/rrc-compiles"
  until mkdir "$log.lock" 2>/dev/null; do sleep 0.001; done
  echo "$* rustc=$REUSSIR_RUSTC" >> "$log"
  rmdir "$log.lock"
  if [ "$FAKE_RRC_FRAGMENTED_DIAGNOSTIC" = yes ] && [ "$package" != core ]; then
    if [ "$color" = always ]; then
      sync="$(dirname "$0")/diagnostic-sync"
      id=$(basename "$out")
      mkdir -p "$sync"
      touch "$sync/ready.$id"
      while :; do
        set -- "$sync"/ready.*
        [ -e "$1" ] && [ "$#" -ge 2 ] && break
        sleep 0.01
      done
      printf '\033[' >&2
      touch "$sync/escape.$id"
      while :; do
        set -- "$sync"/escape.*
        [ -e "$1" ] && [ "$#" -ge 2 ] && break
        sleep 0.01
      done
      printf '31mDIAGNOSTIC\033[0m\n' >&2
    else
      printf 'DIAGNOSTIC\n' >&2
    fi
    exit 1
  fi
  echo compiled > "$out"
  if [ -n "$report" ]; then
    mkdir -p "$(dirname "$report")"
    printf '{"schema":"reussir.token-reuse.v1","locations":[],"decisions":[]}\n' > "$report"
  fi
  exit 0
fi
log="$(dirname "$0")/rrc-runs"
until mkdir "$log.lock" 2>/dev/null; do sleep 0.001; done
echo run >> "$log"
rmdir "$log.lock"
if [ -n "$input" ]; then
  printf '{"package":"pkg","files":[{"path":"%s","module":["pkg"]}]}\n' "$input"
  exit 0
fi
printf '{"package":"pkg","files":[{"path":"%s","module":["pkg"]}' "$root/lib.rr"
for f in "$root"/*.rr; do
  case "$f" in */lib.rr) continue;; esac
  [ -f "$f" ] || continue
  base=$(basename "$f" .rr)
  printf ',{"path":"%s","module":["pkg","%s"]}' "$f" "$base"
done
printf ']}\n'
"#
            .to_owned(),
        );

        Fakes {
            dir: dir.to_owned(),
            rrc,
            cargo,
            rustc,
            libdir,
            cache: dir.join("cache"),
        }
    }

    /// How many times the named fake ran (`rrc`, `cargo`).
    fn runs(&self, tool: &str) -> usize {
        match std::fs::read_to_string(self.dir.join(format!("{tool}-runs"))) {
            Ok(log) => log.lines().count(),
            Err(_) => 0,
        }
    }

    /// The compile invocations the fake `rrc` served, one argv line each.
    fn compiles(&self) -> Vec<String> {
        match std::fs::read_to_string(self.dir.join("rrc-compiles")) {
            Ok(log) => log.lines().map(str::to_owned).collect(),
            Err(_) => Vec::new(),
        }
    }

    /// [`compiles`](Self::compiles) without the implicit `core` package's
    /// invocations — most expectations are about the packages a test
    /// declares, with core's two compiles (interface + archive) asserted
    /// where the injection itself is the subject.
    fn package_compiles(&self) -> Vec<String> {
        self.compiles()
            .into_iter()
            .filter(|l| !l.contains("--package-name core"))
            .collect()
    }

    /// The runtime bakes in the cache, one entry directory each.
    fn rt_entries(&self) -> Vec<PathBuf> {
        cache_entries(&self.cache.join("rt"))
    }

    /// The cargo bake invocations, one argv line each.
    fn cargo_invocations(&self) -> Vec<String> {
        match std::fs::read_to_string(self.dir.join("cargo-args")) {
            Ok(log) => log.lines().map(str::to_owned).collect(),
            Err(_) => Vec::new(),
        }
    }

    /// A `rene` command wired to the fake toolchain and the test's own
    /// cache.
    fn command(&self, args: &[&str]) -> Command {
        let mut cmd = rene(args);
        cmd.env("REUSSIR_RRC", &self.rrc)
            .env("REUSSIR_CARGO", &self.cargo)
            .env("REUSSIR_RUSTC", &self.rustc)
            .env(rene::cache::CACHE_DIR_ENV, &self.cache);
        cmd
    }

    /// A `rene` invocation wired to the fake toolchain.
    fn rene(&self, args: &[&str], manifest: &Path, build_dir: &Path) -> Output {
        run(self
            .command(args)
            .arg("--manifest-path")
            .arg(manifest)
            .arg("--build-dir")
            .arg(build_dir))
    }
}

/// The body of a fake `rustc` reporting `version` and the host
/// `x86_64-unknown-fake`, with `sysroot` and `libdir` for `--print`.
#[cfg(unix)]
fn fake_rustc(version: &str, sysroot: &Path, libdir: &Path) -> String {
    format!(
        r#"case "$1" in
--version) echo 'rustc {version}';;
-vV) printf 'rustc {version}\nhost: x86_64-unknown-fake\n';;
--print)
  prev=""
  for arg in "$@"; do
    if [ "$prev" = --print ]; then
      case "$arg" in
      sysroot) echo '{}';;
      target-libdir) echo '{}';;
      esac
    fi
    prev="$arg"
  done;;
esac
"#,
        sysroot.display(),
        libdir.display()
    )
}

/// The entry directories of one cache kind, sorted (lock files and staging
/// or trash siblings excluded).
#[cfg(unix)]
fn cache_entries(kind: &Path) -> Vec<PathBuf> {
    let Ok(entries) = std::fs::read_dir(kind) else {
        return Vec::new();
    };
    let mut dirs: Vec<PathBuf> = entries
        .map(|entry| entry.unwrap())
        .filter(|entry| {
            let name = entry.file_name().into_string().unwrap();
            entry.path().is_dir() && rene::cache::parse_sibling(&name).is_none()
        })
        .map(|entry| entry.path())
        .collect();
    dirs.sort();
    dirs
}

/// `rene inspect --frozen`'s report, parsed.
#[cfg(unix)]
fn frozen_report(fakes: &Fakes, manifest: &Path, build_dir: &Path) -> serde_json::Value {
    let out = fakes.rene(&["inspect", "--frozen"], manifest, build_dir);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    serde_json::from_slice(&out.stdout).expect("inspect --frozen prints JSON")
}

/// The full bake driven through the fake toolchain: cheap, and it pins the
/// toolchain-facing contract (JSON artifact messages in, libdir listing out,
/// status recorded, second build reuses the bake).
#[cfg(unix)]
#[test]
fn build_bakes_the_runtime_and_prints_the_libdirs() {
    let tmp_dir = tempfile::tempdir().unwrap();
    // Canonicalized: macOS tempdirs sit behind the `/var -> /private/var`
    // symlink, and the deps line rene prints comes from cargo's artifact
    // report, whose paths the fake cargo roots at the resolved `$PWD`.
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    // Stdout is exactly the `--polyffi-libdir` directories: the detected
    // Rust lib, Cargo's host proc-macro deps, then its target deps.
    let stdout = String::from_utf8(out.stdout).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    let target = "x86_64-unknown-fake";
    // The bake lives in the cache, cargo's target directory inside it.
    let bakes = fakes.rt_entries();
    assert_eq!(bakes.len(), 1, "{bakes:#?}");
    let target_deps = bakes[0].join(format!("target/{target}/release/deps"));
    let host_deps = bakes[0].join("target/release/deps");
    assert_eq!(
        lines,
        [
            fakes.libdir.display().to_string(),
            host_deps.display().to_string(),
            target_deps.display().to_string(),
        ]
    );

    // The bundle really was unpacked, and the bake recorded, in the cache
    // and in the build directory, which holds nothing of the bake itself.
    assert!(bakes[0].join("src/reussir-rt/Cargo.toml").is_file());
    assert!(bakes[0].join("artifacts.json").is_file());
    assert!(!root.join(target).exists());
    let dir = BuildDir::open(&root).unwrap();
    assert!(
        dir.status(&tables::rt_artifacts_key(target))
            .unwrap()
            .is_some()
    );
    drop(dir);

    // A second build must reuse the bake: same output, no new cargo run.
    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(String::from_utf8(out.stdout).unwrap(), stdout);
    assert_eq!(fakes.runs("cargo"), 1, "the second build re-ran cargo");
    assert!(fakes.cargo_invocations()[0].contains("--target x86_64-unknown-fake"));
    // The sysroot of the fake has no `bin/rustc`, so the selection itself is
    // what cargo ran.
    let pinned = std::fs::read_to_string(tmp.join("cargo-rustc")).unwrap();
    assert_eq!(pinned.trim(), fakes.rustc.display().to_string());

    // A compiler at a different explicit path is a different toolchain
    // selection even when it reports the same version. Do not restore the
    // old cached path and hand that stale value to rrc.
    let alternate_rustc = tmp.join("alternate-rustc");
    std::fs::copy(&fakes.rustc, &alternate_rustc).unwrap();
    let with_alternate = || {
        run(fakes
            .command(&["build", "--manifest-path"])
            .arg(&manifest)
            .arg("--build-dir")
            .arg(&root)
            .env("REUSSIR_RUSTC", &alternate_rustc))
    };
    let out = with_alternate();
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(fakes.runs("cargo"), 2, "the rustc path did not rebake");
    let out = with_alternate();
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(fakes.runs("cargo"), 2, "the new rustc path was not cached");
    assert_eq!(fakes.rt_entries().len(), 2);
}

/// Builds of different packages in different build directories share one
/// runtime bake through the cache, and a bake whose staticlib vanished is
/// baked again in the same entry.
#[cfg(unix)]
#[test]
fn packages_share_one_runtime_bake() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let build = |name: &str| {
        let pkg = tmp.join(name);
        let out = fakes.rene(&["build"], &package(&pkg, name), &pkg.join("reussir-build"));
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        String::from_utf8(out.stdout).unwrap()
    };
    let libdirs = build("first");
    assert_eq!(build("second"), libdirs);
    assert_eq!(fakes.runs("cargo"), 1, "the second package rebaked");
    let bakes = fakes.rt_entries();
    assert_eq!(bakes.len(), 1, "{bakes:#?}");

    let record: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(bakes[0].join("artifacts.json")).unwrap())
            .unwrap();
    std::fs::remove_file(record["artifacts"]["staticlib"].as_str().unwrap()).unwrap();
    assert_eq!(build("second"), libdirs);
    assert_eq!(fakes.runs("cargo"), 2, "a broken bake was trusted");
    assert_eq!(fakes.rt_entries(), bakes);
}

/// Write a package `name` under `dir` that declares one executable, so a
/// build runs the dependency pipeline (and compiles `core`).
#[cfg(unix)]
fn executable_package(dir: &Path, name: &str) -> PathBuf {
    let manifest = package(dir, name);
    std::fs::write(
        &manifest,
        format!(
            "{{ package = {{ name = \"{name}\", version = \"0.1.0\" }}, \
             targets.{name}.kind = 'executable }}\n"
        ),
    )
    .unwrap();
    manifest
}

/// Builds of different packages in different build directories compile the
/// bundled core once, through the cache; each build directory still gets
/// its own copy of core's artifacts. Another profile is another entry, and
/// an entry missing a file is rebuilt, not trusted.
#[cfg(unix)]
#[test]
fn packages_share_compiled_core_through_the_cache() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let core_compiles = || {
        let compiles = fakes.compiles();
        compiles.len() - fakes.package_compiles().len()
    };
    let build = |name: &str, profile: &str| {
        let pkg = tmp.join(name);
        let manifest = executable_package(&pkg, name);
        let root = pkg.join("reussir-build");
        let out = fakes.rene(&["build", "--profile", profile], &manifest, &root);
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        root
    };
    let built = [build("first", "dev"), build("second", "dev")];
    // Interface and archive, once for both packages.
    assert_eq!(core_compiles(), 2, "{:#?}", fakes.compiles());
    for root in &built {
        assert!(
            root.join("dev/deps/core.rri").is_file(),
            "{}",
            root.display()
        );
    }
    assert_eq!(cache_entries(&fakes.cache.join("deps")).len(), 1);

    build("first", "release");
    assert_eq!(core_compiles(), 4);
    let entries = cache_entries(&fakes.cache.join("deps"));
    assert_eq!(entries.len(), 2);
    for entry in &entries {
        std::fs::remove_file(entry.join("core.rri")).unwrap();
    }
    std::fs::remove_dir_all(&built[1]).unwrap();
    build("second", "dev");
    assert_eq!(core_compiles(), 6);
    assert_eq!(cache_entries(&fakes.cache.join("deps")), entries);
    assert_eq!(fakes.runs("cargo"), 1);

    // Restoring takes no lock, so concurrent restores never queue.
    let restores = ["third", "fourth"].map(|name| {
        let pkg = tmp.join(name);
        fakes
            .command(&["build", "--manifest-path"])
            .arg(executable_package(&pkg, name))
            .arg("--build-dir")
            .arg(pkg.join("reussir-build"))
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap()
    });
    for restore in restores {
        let out = restore.wait_with_output().unwrap();
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        assert!(!stderr(&out).contains("waiting for"), "{}", stderr(&out));
    }
    assert_eq!(core_compiles(), 6);
}

/// Write a package `name` under `dir` that declares one executable and
/// depends on the bundled std.
#[cfg(unix)]
fn std_package(dir: &Path, name: &str) -> PathBuf {
    let manifest = package(dir, name);
    std::fs::write(
        &manifest,
        format!(
            "{{ package = {{ name = \"{name}\", version = \"0.1.0\" }}, \
             dependencies.std = {{ bundled = true, version = \"^0.1\" }}, \
             targets.{name}.kind = 'executable }}\n"
        ),
    )
    .unwrap();
    manifest
}

/// `std = { bundled = true }` builds the std bundled in rene: unpacked only
/// once a package asks for it, and compiled once for every package that
/// does, after the core it depends on.
#[cfg(unix)]
#[test]
fn bundled_std_is_compiled_once_for_every_package() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let std_compiles = || {
        fakes
            .compiles()
            .into_iter()
            .filter(|line| line.contains("--package-name std"))
            .collect::<Vec<_>>()
    };
    let plain = tmp.join("plain");
    let manifest = executable_package(&plain, "plain");
    let out = fakes.rene(&["build"], &manifest, &plain.join("reussir-build"));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(!fakes.cache.join("std-src").exists());

    for name in ["first", "second"] {
        let pkg = tmp.join(name);
        let manifest = std_package(&pkg, name);
        let root = pkg.join("reussir-build");
        let out = fakes.rene(&["build"], &manifest, &root);
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        assert!(root.join("dev/deps/std.rri").is_file());
    }
    let compiles = std_compiles();
    assert_eq!(compiles.len(), 2, "{compiles:#?}");
    // The std sources come from the cache, and std builds against core, both
    // as built-ins.
    let std_src = cache_entries(&fakes.cache.join("std-src"));
    assert_eq!(std_src.len(), 1);
    let root = format!("--package-root {}", std_src[0].join("src").display());
    for line in &compiles {
        assert!(line.contains(&root), "{line}");
        assert!(line.contains("--extern core="), "{line}");
        assert!(line.contains("--builtin"), "{line}");
    }
    // core and std, each once.
    assert_eq!(cache_entries(&fakes.cache.join("deps")).len(), 2);

    // The root's targets see std.
    let manifest = tmp.join("second/rene.ncl");
    let root = tmp.join("second/reussir-build");
    assert_eq!(
        frozen_report(&fakes, &manifest, &root)["state"],
        "up-to-date"
    );
    let out = fakes.rene(&["inspect", "--frozen", "--commands"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let plan = String::from_utf8(out.stdout).unwrap();
    assert!(plan.contains("std="), "{plan}");
}

/// Two renes needing the same bake and the same core at once run cargo once
/// and compile core once: the second waits for the first's bake, says so
/// once, and then shares the bake and core through the cache.
#[cfg(unix)]
#[test]
fn concurrent_builds_bake_once_and_compile_core_once() {
    use std::io::BufRead;
    use std::process::Stdio;

    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let gate = tmp.join("gate");
    let spawn = |name: &str| {
        let pkg = tmp.join(name);
        fakes
            .command(&["build", "--manifest-path"])
            .arg(executable_package(&pkg, name))
            .arg("--build-dir")
            .arg(pkg.join("reussir-build"))
            .env("FAKE_CARGO_GATE", &gate)
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap()
    };
    let first = spawn("first");
    let started = tmp.join("gate.started");
    for _ in 0..3000 {
        if started.exists() {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    assert!(started.exists(), "the first bake never started");

    // Release the first bake only once the second says it waits for it.
    let mut second = spawn("second");
    let (tx, rx) = std::sync::mpsc::channel();
    let reader = {
        let stderr = second.stderr.take().unwrap();
        std::thread::spawn(move || {
            let mut lines = Vec::new();
            for line in std::io::BufReader::new(stderr).lines() {
                let line = line.unwrap();
                if line.contains("waiting for another rene to finish baking reussir-rt") {
                    let _ = tx.send(());
                }
                lines.push(line);
            }
            lines
        })
    };
    rx.recv_timeout(std::time::Duration::from_secs(60))
        .expect("the second build never waited for the first");
    std::fs::write(&gate, "").unwrap();

    let first = first.wait_with_output().unwrap();
    assert!(first.status.success(), "stderr: {}", stderr(&first));
    assert!(!stderr(&first).contains("waiting for"));
    assert!(second.wait().unwrap().success());
    let lines = reader.join().unwrap();
    let waits = |what: &str| lines.iter().filter(|line| line.contains(what)).count();
    // The bake is waited for once. Whether core is waited for as well
    // depends on which rene reaches it first; it is compiled once either way.
    assert_eq!(waits("finish baking reussir-rt"), 1, "{lines:#?}");
    assert!(waits("waiting for") <= 2, "{lines:#?}");
    assert_eq!(fakes.runs("cargo"), 1);
    // Interface and archive, once for both packages, and a copy in each
    // build directory.
    let core_compiles = fakes.compiles().len() - fakes.package_compiles().len();
    assert_eq!(core_compiles, 2, "{:#?}", fakes.compiles());
    for name in ["first", "second"] {
        let rri = tmp.join(name).join("reussir-build/dev/deps/core.rri");
        assert!(rri.is_file(), "{}", rri.display());
    }
}

/// A package that brings its own `core` by path builds it locally, in each
/// build directory, like any other dependency: only the bundled core is
/// shared through the cache.
#[cfg(unix)]
#[test]
fn a_core_brought_by_path_is_built_locally() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let core = tmp.join("core");
    package(&core, "core");
    let pkg = tmp.join("app");
    let manifest = executable_package(&pkg, "app");
    std::fs::write(
        &manifest,
        "{ package = { name = \"app\", version = \"0.1.0\" }, \
         dependencies.core.path = \"../core\", targets.app.kind = 'executable }\n",
    )
    .unwrap();
    for build_dir in ["one", "two"] {
        let out = fakes.rene(&["build"], &manifest, &pkg.join(build_dir));
        assert!(out.status.success(), "stderr: {}", stderr(&out));
    }
    // Interface and archive, in each build directory.
    let core_compiles = fakes.compiles().len() - fakes.package_compiles().len();
    assert_eq!(core_compiles, 4, "{:#?}", fakes.compiles());
    assert!(cache_entries(&fakes.cache.join("deps")).is_empty());
}

/// rustup picks a toolchain by the directory its proxy runs in, which need
/// not be the package's. The toolchain is resolved in the package's
/// directory and its binaries are pinned: cargo runs that toolchain's cargo
/// and rustc, and rrc is handed the same rustc.
#[cfg(unix)]
#[test]
fn the_bake_uses_the_toolchain_of_the_package_directory() {
    use std::os::unix::fs::PermissionsExt;

    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let fakes = Fakes::new(&tmp);
    let executable = |path: &Path, body: &str| {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, format!("#!/bin/sh\n{body}")).unwrap();
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755)).unwrap();
    };
    // Two toolchains, each with its own rustc and a cargo that hands over to
    // the logging fake.
    for (name, version) in [("default", "9.9.9 (default)"), ("pinned", "8.8.8 (pinned)")] {
        let sysroot = tmp.join("toolchains").join(name);
        executable(
            &sysroot.join("bin/rustc"),
            &fake_rustc(version, &sysroot, &fakes.libdir),
        );
        executable(
            &sysroot.join("bin/cargo"),
            &format!("exec '{}' \"$@\"\n", fakes.cargo.display()),
        );
    }
    // A rustup-like proxy: a `rust-toolchain.toml` at or above its working
    // directory selects `pinned`.
    let proxy = tmp.join("proxy-rustc");
    executable(
        &proxy,
        &format!(
            r#"dir="$PWD"; toolchain=default
while [ "$dir" != / ]; do
  [ -f "$dir/rust-toolchain.toml" ] && {{ toolchain=pinned; break; }}
  dir=$(dirname "$dir")
done
exec '{}'/"$toolchain"/bin/rustc "$@"
"#,
            tmp.join("toolchains").display()
        ),
    );
    let pkg = tmp.join("pkg");
    let manifest = package(&pkg, "demo");
    std::fs::write(
        &manifest,
        "{ package = { name = \"demo\", version = \"0.1.0\" }, targets.demo.kind = 'executable }\n",
    )
    .unwrap();
    std::fs::write(pkg.join("rust-toolchain.toml"), "[toolchain]\n").unwrap();
    let root = pkg.join("reussir-build");

    // rene runs from outside the package, with no cargo configured.
    let out = run(fakes
        .command(&["build", "--manifest-path"])
        .arg(&manifest)
        .arg("--build-dir")
        .arg(&root)
        .current_dir(&tmp)
        .env_remove("CARGO")
        .env_remove("REUSSIR_CARGO")
        .env("REUSSIR_RUSTC", &proxy));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let pinned = tmp.join("toolchains/pinned/bin/rustc");
    let ran = std::fs::read_to_string(tmp.join("cargo-rustc")).unwrap();
    assert_eq!(ran.trim(), pinned.display().to_string());
    let dir = BuildDir::open(&root).unwrap();
    let record: serde_json::Value = serde_json::from_str(
        &dir.status(&tables::rt_artifacts_key("x86_64-unknown-fake"))
            .unwrap()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(record["rustc_version"], "rustc 8.8.8 (pinned)");
    assert_eq!(record["rustc"], pinned.display().to_string());
    let compiles = fakes.package_compiles();
    assert!(!compiles.is_empty());
    for line in &compiles {
        assert!(
            line.ends_with(&format!("rustc={}", pinned.display())),
            "{line}"
        );
    }
}

/// Cargo's outputs stay inside the bake's cache entry, whatever
/// `CARGO_TARGET_DIR` says: a target directory shared between bakes would
/// let one overwrite another's artifacts.
#[cfg(unix)]
#[test]
fn the_bake_keeps_its_outputs_whatever_cargo_target_dir_says() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);
    let shared = tmp.join("shared-target");

    let out = run(fakes
        .command(&["build", "--manifest-path"])
        .arg(&manifest)
        .arg("--build-dir")
        .arg(&root)
        .env("CARGO_TARGET_DIR", &shared));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(!shared.exists());
    let stdout = String::from_utf8(out.stdout).unwrap();
    let cargo_dirs: Vec<&str> = stdout.lines().skip(1).collect();
    assert_eq!(cargo_dirs.len(), 2, "{stdout}");
    let bakes = fakes.rt_entries();
    assert_eq!(bakes.len(), 1, "{bakes:#?}");
    assert!(
        cargo_dirs
            .iter()
            .all(|dir| Path::new(dir).starts_with(&bakes[0])),
        "{stdout}"
    );
}

/// The profile supplies a default target, the CLI overrides it, and the two
/// target-prefixed runtime bakes remain independently reusable.
#[cfg(unix)]
#[test]
fn build_resolves_the_target_and_keeps_each_runtime_bake() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    std::fs::write(
        &manifest,
        r#"
        {
          package = { name = "demo", version = "0.1.0" },
          targets.demo.kind = 'executable,
          profiles.dev.default_target_triple = "wasm32-unknown-unknown",
        }
        "#,
    )
    .unwrap();
    let fakes = Fakes::new(&tmp);

    let default = fakes.rene(&["build"], &manifest, &root);
    assert!(default.status.success(), "stderr: {}", stderr(&default));
    assert_eq!(
        String::from_utf8(default.stdout).unwrap().trim(),
        root.join("dev/demo.wasm").display().to_string()
    );

    let override_target = "aarch64-unknown-linux-gnu";
    let overridden = fakes.rene(&["build", "--target", override_target], &manifest, &root);
    assert!(
        overridden.status.success(),
        "stderr: {}",
        stderr(&overridden)
    );
    assert_eq!(
        String::from_utf8(overridden.stdout).unwrap().trim(),
        root.join("dev/demo").display().to_string()
    );

    // One cache entry per target.
    let bakes = fakes.rt_entries();
    assert_eq!(bakes.len(), 2, "{bakes:#?}");
    for bake in &bakes {
        assert!(bake.join("src/reussir-rt/Cargo.toml").is_file());
    }
    for target in ["wasm32-unknown-unknown", override_target] {
        let dir = BuildDir::open(&root).unwrap();
        assert!(
            dir.status(&tables::rt_artifacts_key(target))
                .unwrap()
                .is_some()
        );
    }
    // Each bake also pins its target directory, at the end.
    let invocations: Vec<String> = fakes
        .cargo_invocations()
        .iter()
        .map(|line| line.split(" --target-dir ").next().unwrap().to_owned())
        .collect();
    assert_eq!(
        invocations,
        [
            "build --release --target wasm32-unknown-unknown --no-default-features --message-format=json-render-diagnostics",
            "build --release --target aarch64-unknown-linux-gnu --message-format=json-render-diagnostics",
        ]
    );
    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 2, "{compiles:#?}");
    assert!(
        compiles[0].contains("--target-triple wasm32-unknown-unknown"),
        "{compiles:#?}"
    );
    assert!(
        compiles[1].contains("--target-triple aarch64-unknown-linux-gnu"),
        "{compiles:#?}"
    );

    let inspected = fakes.rene(
        &[
            "inspect",
            "--frozen",
            "--commands",
            "--target",
            override_target,
        ],
        &manifest,
        &root,
    );
    assert!(inspected.status.success(), "stderr: {}", stderr(&inspected));
    let report: serde_json::Value = serde_json::from_slice(&inspected.stdout).unwrap();
    assert_eq!(report["plan"]["target"], override_target);
    assert!(
        report["plan"]["env"]["REUSSIR_RUSTC"]
            .as_str()
            .unwrap()
            .contains("fake-rustc")
    );

    let default_again = fakes.rene(&["build"], &manifest, &root);
    assert!(
        default_again.status.success(),
        "stderr: {}",
        stderr(&default_again)
    );
    assert_eq!(fakes.runs("cargo"), 2, "the default target was rebaked");
    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 3, "{compiles:#?}");
    assert!(
        compiles[2].contains("--target-triple wasm32-unknown-unknown"),
        "{compiles:#?}"
    );
}

#[cfg(unix)]
#[test]
fn build_rejects_a_target_that_is_not_one_safe_triple_component() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build", "--target", "../escape"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("invalid target triple `../escape`"),
        "stderr: {}",
        stderr(&out)
    );
    assert_eq!(fakes.runs("cargo"), 0);
    assert!(!tmp.join("escape").exists());
}

/// The first build records the graph; a build that finds every recorded file
/// untouched must not invoke `rrc` at all.
#[cfg(unix)]
#[test]
fn build_is_a_no_op_when_no_source_changed() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(fakes.runs("rrc"), 1, "the first build must scan");

    for _ in 0..2 {
        // `-v`: the freshness narration lives at DEBUG now.
        let out = fakes.rene(&["-v", "build"], &manifest, &root);
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        assert!(
            stderr(&out).contains("up to date"),
            "stderr: {}",
            stderr(&out)
        );
    }
    assert_eq!(
        fakes.runs("rrc"),
        1,
        "an unchanged package must not be re-scanned"
    );
    assert_eq!(
        frozen_report(&fakes, &manifest, &root)["state"],
        "up-to-date"
    );
}

/// Trigger 3: a modified file. The rebuild re-scans the *whole* graph rather
/// than refreshing that file's entry — the edit may have changed the graph.
#[cfg(unix)]
#[test]
fn build_rescans_the_whole_graph_when_a_source_changes() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    assert!(
        fakes.rene(&["build"], &manifest, &root).status.success(),
        "the first build failed"
    );

    // Edit a file *and* declare a new module: exactly the case a per-file
    // mtime refresh would miss, because the graph itself grew.
    std::fs::write(
        tmp.join("src/lib.rr"),
        "mod math;\nmod extra;\npub fn entry(n: u64) -> u64 { math::add(n, 10) }\n",
    )
    .unwrap();
    std::fs::write(tmp.join("src/extra.rr"), "pub fn x() -> u64 { 7 }\n").unwrap();

    assert_eq!(
        frozen_report(&fakes, &manifest, &root)["state"],
        "file-changed"
    );

    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(
        fakes.runs("rrc"),
        2,
        "the changed package must be re-scanned"
    );

    // The refreshed table is the new graph, `extra.rr` included (the record
    // is a set of files, reported in path order).
    let report = frozen_report(&fakes, &manifest, &root);
    assert_eq!(report["state"], "up-to-date");
    let files: Vec<&str> = report["files"]
        .as_array()
        .unwrap()
        .iter()
        .map(|f| f["path"].as_str().unwrap())
        .collect();
    assert_eq!(
        files,
        [
            tmp.join("src/extra.rr").display().to_string(),
            tmp.join("src/lib.rr").display().to_string(),
            tmp.join("src/math.rr").display().to_string(),
        ]
    );
    // …and the next build is a no-op again.
    assert!(fakes.rene(&["build"], &manifest, &root).status.success());
    assert_eq!(fakes.runs("rrc"), 2);
}

/// Trigger 3, the other half: a file that vanished from the graph must not
/// linger in the table and keep invalidating later builds.
#[cfg(unix)]
#[test]
fn a_removed_file_drops_out_of_the_refreshed_graph() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    assert!(fakes.rene(&["build"], &manifest, &root).status.success());

    std::fs::remove_file(tmp.join("src/math.rr")).unwrap();
    std::fs::write(tmp.join("src/lib.rr"), "pub fn entry() -> u64 { 0 }\n").unwrap();
    assert_eq!(
        frozen_report(&fakes, &manifest, &root)["state"],
        "file-changed"
    );

    assert!(fakes.rene(&["build"], &manifest, &root).status.success());
    let report = frozen_report(&fakes, &manifest, &root);
    assert_eq!(report["state"], "up-to-date");
    assert_eq!(report["files"].as_array().unwrap().len(), 1);

    // The stale row is gone, so the build stays a no-op.
    assert!(fakes.rene(&["build"], &manifest, &root).status.success());
    assert_eq!(fakes.runs("rrc"), 2);
}

/// Trigger 2: the evaluated manifest changed. Nothing on disk moved, but the
/// configuration the graph was taken under did.
#[cfg(unix)]
#[test]
fn build_rescans_when_the_config_checksum_changes() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    assert!(fakes.rene(&["build"], &manifest, &root).status.success());
    let before = frozen_report(&fakes, &manifest, &root);
    assert_eq!(before["state"], "up-to-date");

    write_manifest(&tmp, "demo", "0.2.0");
    let after = frozen_report(&fakes, &manifest, &root);
    assert_eq!(after["state"], "config-changed");
    assert_ne!(before["config_hash"], after["config_hash"]);

    assert!(fakes.rene(&["build"], &manifest, &root).status.success());
    assert_eq!(
        fakes.runs("rrc"),
        2,
        "a changed config must force a re-scan"
    );
    assert_eq!(
        frozen_report(&fakes, &manifest, &root)["state"],
        "up-to-date"
    );
}

/// `inspect` reports the graph as JSON: module paths, sizes, and the Blake3
/// digest of every file. Without `--frozen` it refreshes a stale record.
#[cfg(unix)]
#[test]
fn inspect_reports_the_recorded_graph() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    // A plain `inspect` on an unrecorded package scans it — no runtime bake.
    let out = fakes.rene(&["inspect"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(fakes.runs("rrc"), 1);
    assert_eq!(fakes.runs("cargo"), 0, "inspect must not bake the runtime");

    let report: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(report["package"], "demo");
    assert_eq!(report["state"], "up-to-date");
    assert_eq!(report["config_hash"].as_str().unwrap().len(), 64);

    let files = report["files"].as_array().unwrap();
    assert_eq!(files.len(), 2);
    assert_eq!(files[0]["module"], serde_json::json!(["pkg"]));
    assert_eq!(files[1]["module"], serde_json::json!(["pkg", "math"]));
    for file in files {
        let path = file["path"].as_str().unwrap();
        let contents = std::fs::read(path).unwrap();
        assert_eq!(file["size"].as_u64().unwrap(), contents.len() as u64);
        assert_eq!(
            file["hash"].as_str().unwrap(),
            blake3::hash(&contents).to_hex().to_string(),
            "recorded digest must be Blake3 of the contents of {path}"
        );
    }

    // A second `inspect` finds nothing to do.
    assert!(fakes.rene(&["inspect"], &manifest, &root).status.success());
    assert_eq!(fakes.runs("rrc"), 1);
}

/// `--frozen` is a pure query: it reports an unrecorded package as
/// uninitialized without running `rrc` or creating a build directory.
#[cfg(unix)]
#[test]
fn inspect_frozen_neither_scans_nor_creates_a_build_directory() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);

    let report = frozen_report(&fakes, &manifest, &root);
    assert_eq!(report["state"], "uninitialized");
    assert_eq!(report["files"].as_array().unwrap().len(), 0);
    assert_eq!(fakes.runs("rrc"), 0);
    assert!(!root.exists(), "--frozen created `{}`", root.display());
}

/// The bundled core unpacks once into the per-user cache, which every build
/// directory then reads; a relative cache location is resolved once, so
/// what the build records stays valid wherever rene runs next.
#[cfg(unix)]
#[test]
fn the_core_sources_unpack_once_into_the_cache() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);
    for build_dir in ["first", "second"] {
        let out = run(rene(&["inspect", "--frozen", "--graph", "--manifest-path"])
            .arg(&manifest)
            .arg("--build-dir")
            .arg(tmp.join(build_dir))
            .current_dir(&tmp)
            .env(rene::cache::CACHE_DIR_ENV, "relative-cache")
            .env("REUSSIR_RRC", &fakes.rrc));
        assert!(out.status.success(), "stderr: {}", stderr(&out));
        let report: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
        let core = cache_entries(&tmp.join("relative-cache/core-src"));
        assert_eq!(core.len(), 1, "{core:#?}");
        assert_eq!(
            report["graph"]["nodes"]["core"]["dir"],
            core[0].display().to_string()
        );
        assert!(!tmp.join(build_dir).join("core-src").exists());
    }
}

/// An unusable cache location is not fatal: the build warns and caches in
/// its own build directory.
#[cfg(unix)]
#[test]
fn an_unusable_cache_falls_back_to_the_build_directory() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = package(&tmp, "demo");
    let fakes = Fakes::new(&tmp);
    let blocker = tmp.join("not-a-directory");
    std::fs::write(&blocker, "").unwrap();

    let out = run(fakes
        .command(&["build", "--manifest-path"])
        .arg(&manifest)
        .arg("--build-dir")
        .arg(&root)
        .env(rene::cache::CACHE_DIR_ENV, blocker.join("cache")));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(
        stderr(&out)
            .matches("caching shared dependencies in")
            .count(),
        1,
        "{}",
        stderr(&out)
    );
    assert_eq!(cache_entries(&root.join("cache/core-src")).len(), 1);
}

/// (Re)write the scratch package's manifest with targets of every kind and a
/// `release` profile refining the built-in.
#[cfg(unix)]
fn write_targets_manifest(dir: &Path) -> PathBuf {
    let path = dir.join("rene.ncl");
    std::fs::write(
        &path,
        r#"
        {
          package = { name = "demo", version = "0.1.0" },
          targets = {
            demo = { kind = 'executable },
            shared = { kind = 'dynlib },
            archive = { kind = 'staticlib },
          },
          profiles = {
            release = { lto = "thin", codegen_units = 2, link_args = ["-lm"] },
          },
        }
        "#,
    )
    .unwrap();
    path
}

/// The whole flag surface, through the fakes: every declared target compiles
/// with the profile's knobs spelled as rrc flags, the link-only knobs go to
/// the linked kinds alone, artifacts land under `<build-dir>/<profile>/`,
/// and stdout lists them in declaration order.
#[cfg(unix)]
#[test]
fn build_compiles_the_declared_targets_with_the_profile_knobs() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    package(&tmp, "demo");
    let manifest = write_targets_manifest(&tmp);
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build", "--profile", "release"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    // Stdout lists the artifacts in BTreeMap (declaration-name) order. The
    // fake rustc reports `x86_64-unknown-fake`, an ELF-like target regardless
    // of the platform running this test.
    let stdout = String::from_utf8(out.stdout).unwrap();
    let release = root.join("release");
    assert_eq!(
        stdout.lines().collect::<Vec<_>>(),
        [
            release.join("libarchive.a").display().to_string(),
            release.join("demo").display().to_string(),
            release.join("libshared.so").display().to_string(),
        ]
    );
    for line in stdout.lines() {
        assert!(Path::new(line).is_file(), "no artifact at {line}");
    }

    // The implicit core built once, ahead of everything: its interface and
    // archive, each with the reserved name lifted.
    let core: Vec<String> = fakes
        .compiles()
        .into_iter()
        .filter(|l| l.contains("--package-name core"))
        .collect();
    assert_eq!(core.len(), 2, "{core:#?}");
    assert!(core.iter().all(|l| l.contains("--builtin")), "{core:#?}");

    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 3, "{compiles:#?}");
    for line in &compiles {
        // The package, the profile's codegen knobs (the built-in's opt
        // refined by the manifest's lto/units), the PIC default, the baked
        // libdirs, and the toolchain rene pinned for the child.
        assert!(line.contains("--package-name demo"), "{line}");
        assert!(line.contains("-O aggressive"), "{line}");
        assert!(line.contains("--lto thin"), "{line}");
        assert!(line.contains("--codegen-units 2"), "{line}");
        assert!(line.contains("--relocation-mode pic"), "{line}");
        assert!(
            line.contains("--target-triple x86_64-unknown-fake"),
            "{line}"
        );
        assert!(line.contains("--polyffi-libdir"), "{line}");
        assert!(
            line.contains(&format!("rustc={}", fakes.rustc.display())),
            "{line}"
        );
    }
    let of_kind = |kind: &str| {
        compiles
            .iter()
            .find(|l| l.contains(&format!("--emit {kind}")))
            .unwrap_or_else(|| panic!("no {kind} compile in {compiles:#?}"))
    };
    // Link-only knobs reach the linked kinds and never the archive.
    assert!(of_kind("executable").contains("--link-arg=-lm"));
    assert!(of_kind("dynlib").contains("--link-arg=-lm"));
    assert!(!of_kind("staticlib").contains("--link-arg"));

    // A second build finds every product current: no new compiles, same
    // listing.
    let again = fakes.rene(&["build", "--profile", "release"], &manifest, &root);
    assert!(again.status.success(), "stderr: {}", stderr(&again));
    assert_eq!(String::from_utf8(again.stdout).unwrap(), stdout);
    assert_eq!(
        fakes.package_compiles().len(),
        3,
        "a fresh build re-compiled"
    );

    // A source edit re-fingerprints every product of the profile.
    std::fs::write(
        tmp.join("src/math.rr"),
        "pub fn add(a: u64, b: u64) -> u64 { b + a }\n",
    )
    .unwrap();
    let rebuilt = fakes.rene(&["build", "--profile", "release"], &manifest, &root);
    assert!(rebuilt.status.success(), "stderr: {}", stderr(&rebuilt));
    assert_eq!(fakes.package_compiles().len(), 6, "the edit must rebuild");
}

/// Target paths give independent executables in one package their own crate
/// roots; both the source scan and the compile plan use those files directly.
#[cfg(unix)]
#[test]
fn build_compiles_explicit_target_paths() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    std::fs::create_dir_all(tmp.join("src")).unwrap();
    for name in ["one", "two"] {
        std::fs::write(
            tmp.join("src").join(format!("{name}.rr")),
            "#[main]\npub fn entry() {}\n",
        )
        .unwrap();
    }
    let manifest = tmp.join("rene.ncl");
    std::fs::write(
        &manifest,
        r#"{
          package = { name = "multi", version = "0.1.0" },
          targets = {
            one = { kind = 'executable, path = "src/one.rr" },
            two = { kind = 'executable, path = "src/two.rr" },
          },
        }
        "#,
    )
    .unwrap();
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 2, "{compiles:#?}");
    for name in ["one", "two"] {
        let path = tmp.join("src").join(format!("{name}.rr"));
        assert!(
            compiles
                .iter()
                .any(|line| line.contains(&path.display().to_string())),
            "no compile rooted at {}: {compiles:#?}",
            path.display()
        );
    }
    assert!(
        compiles.iter().all(|line| !line.contains("--package-root")),
        "{compiles:#?}"
    );

    let dir = BuildDir::open(&root).unwrap();
    let sources = dir.sources().unwrap();
    assert_eq!(sources.len(), 2, "{sources:#?}");
    drop(dir);

    let again = fakes.rene(&["build"], &manifest, &root);
    assert!(again.status.success(), "stderr: {}", stderr(&again));
    assert_eq!(fakes.package_compiles().len(), 2, "fresh targets rebuilt");
}

/// `--linker` pins one linker end to end: cargo's target-specific
/// environment for the runtime bake, and `rrc --linker` for the driver-level
/// links — the Windows story, where rustc's own discovery under a vcvars
/// shell resolves coreutils' `link` instead of MSVC's.
#[cfg(unix)]
#[test]
fn build_pins_the_linker_through_the_bake_and_the_compiles() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    package(&tmp, "demo");
    let manifest = write_targets_manifest(&tmp);
    let fakes = Fakes::new(&tmp);
    // Any existing file serves as "the linker" — nothing executes it here.
    let linker = fakes.rustc.display().to_string();

    let out = fakes.rene(&["build", "--linker", &linker], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    // The bake saw it through CARGO_TARGET_<TARGET>_LINKER…
    let cargo_env = std::fs::read_to_string(tmp.join("cargo-env")).unwrap();
    assert_eq!(cargo_env.trim(), format!("linker={linker}"));

    // …and every linked compile got `--linker`; the archive did not.
    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 3, "{compiles:#?}");
    for line in &compiles {
        let linked = !line.contains("--emit staticlib");
        assert_eq!(
            line.contains(&format!("--linker {linker}")),
            linked,
            "{line}"
        );
    }
}

/// `--bin` and `--lib` narrow the build by target kind; unknown names,
/// kind mismatches, and unknown profiles are refused with their names.
#[cfg(unix)]
#[test]
fn build_selects_bins_and_libs_and_refuses_invalid_names() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    package(&tmp, "demo");
    let manifest = write_targets_manifest(&tmp);
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build", "--bin", "demo"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(
        stdout.lines().collect::<Vec<_>>(),
        [root.join("dev").join("demo").display().to_string()]
    );
    let compiles = fakes.package_compiles();
    assert_eq!(compiles.len(), 1, "{compiles:#?}");
    // The dev built-in: unoptimized, debug info on.
    assert!(compiles[0].contains("-O none"), "{}", compiles[0]);
    assert!(compiles[0].contains("-g"), "{}", compiles[0]);

    let out = fakes.rene(
        &["build", "--lib", "shared", "--lib", "archive"],
        &manifest,
        &root,
    );
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(
        stdout.lines().collect::<Vec<_>>(),
        [
            root.join("dev/libshared.so").display().to_string(),
            root.join("dev").join("libarchive.a").display().to_string(),
        ]
    );
    assert_eq!(fakes.package_compiles().len(), 3);

    let out = fakes.rene(&["build", "--bin", "bogus"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("no binary target named `bogus`"),
        "stderr: {}",
        stderr(&out)
    );

    let out = fakes.rene(&["build", "--lib", "bogus"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("no library target named `bogus`"),
        "stderr: {}",
        stderr(&out)
    );

    let out = fakes.rene(&["build", "--bin", "shared"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("select it with `--lib shared`"),
        "stderr: {}",
        stderr(&out)
    );

    let out = fakes.rene(&["build", "--lib", "demo"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("select it with `--bin demo`"),
        "stderr: {}",
        stderr(&out)
    );

    let out = fakes.rene(&["build", "--profile", "bogus"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("no profile named `bogus`"),
        "stderr: {}",
        stderr(&out)
    );
}

/// A package without a source root is a configuration error, named as such.
#[cfg(unix)]
#[test]
fn build_reports_a_package_without_sources() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    let manifest = write_manifest(&tmp, "empty", "0.1.0");
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build"], &manifest, &root);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("src") && stderr(&out).contains("lib.rr"),
        "stderr: {}",
        stderr(&out)
    );
}

/// `-j`/`--jobs` caps the compile-process pool. `-j 1` serializes the pool
/// entirely; the build must still produce every artifact.
#[cfg(unix)]
#[test]
fn build_honors_a_single_job_cap() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    package(&tmp, "demo");
    let manifest = write_targets_manifest(&tmp);
    let fakes = Fakes::new(&tmp);

    let out = fakes.rene(&["build", "--jobs", "1"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.lines().count(), 3, "stdout: {stdout}");
    for line in stdout.lines() {
        assert!(Path::new(line).is_file(), "no artifact at {line}");
    }
    assert_eq!(fakes.package_compiles().len(), 3);
}

/// Parallel rrc jobs write into private pipes. Even deliberately fragmented
/// ANSI sequences are replayed as whole diagnostic blocks, with rene's color
/// policy forwarded through the pipe.
#[cfg(unix)]
#[test]
fn build_keeps_parallel_colored_diagnostics_intact() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");
    package(&tmp, "demo");
    let manifest = tmp.join("rene.ncl");
    std::fs::write(
        &manifest,
        "{ package = { name = \"demo\", version = \"0.1.0\" },\n\
         \x20 targets.shared = { kind = 'dynlib },\n\
         \x20 targets.archive = { kind = 'staticlib } }\n",
    )
    .unwrap();
    let fakes = Fakes::new(&tmp);

    let out = run(fakes
        .command(&["--color", "always", "build", "--jobs", "2"])
        .arg("--manifest-path")
        .arg(&manifest)
        .arg("--build-dir")
        .arg(&root)
        .env("FAKE_RRC_FRAGMENTED_DIAGNOSTIC", "yes"));
    assert_eq!(out.status.code(), Some(1), "stderr: {}", stderr(&out));

    let colored_stderr = stderr(&out);
    let block = "\x1b[31mDIAGNOSTIC\x1b[0m\n";
    assert_eq!(
        colored_stderr.match_indices(block).count(),
        2,
        "parallel diagnostics were corrupted:\n{colored_stderr:?}"
    );
    assert!(
        !colored_stderr.contains("\x1b[\x1b["),
        "ANSI prefixes interleaved:\n{colored_stderr:?}"
    );

    // The default resolves against rene's stderr, not the child's pipe. The
    // test harness captures stderr, so the same failed build must be plain.
    let plain = run(fakes
        .command(&["build", "--jobs", "2"])
        .arg("--manifest-path")
        .arg(&manifest)
        .arg("--build-dir")
        .arg(&root)
        .env("FAKE_RRC_FRAGMENTED_DIAGNOSTIC", "yes"));
    assert_eq!(plain.status.code(), Some(1), "stderr: {}", stderr(&plain));
    assert!(
        !plain.stderr.contains(&b'\x1b'),
        "auto color polluted redirected stderr: {:?}",
        plain.stderr
    );
    assert_eq!(stderr(&plain).match_indices("DIAGNOSTIC\n").count(), 2);
}

/// The pool needs at least one process: `-j 0` is a usage error (exit 2),
/// reported by the parser before any build state is touched.
#[test]
fn build_rejects_a_zero_jobs_cap() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");

    let out = run(rene(&["build", "-j", "0"])
        .arg("--manifest-path")
        .arg(tmp.join("rene.ncl"))
        .arg("--build-dir")
        .arg(&root));
    assert_eq!(out.status.code(), Some(2), "stderr: {}", stderr(&out));
    assert!(stderr(&out).contains("--jobs"), "stderr: {}", stderr(&out));
    assert!(!root.exists(), "usage error must not create the build dir");
}

/// The real thing: bake `reussir-rt` with the host toolchain. Slow (a full
/// release build of the runtime) and network-dependent (crates.io + the
/// pinned mlir-sync git revision), hence ignored by default:
/// `cargo test -p rene -- --ignored`.
///
/// The source scan still runs through the fake `rrc`: this test is about the
/// runtime bake, and a real `rrc` needs the whole LLVM/MLIR build.
#[cfg(unix)]
#[test]
#[ignore = "builds reussir-rt with the host cargo (slow, needs network)"]
fn build_bakes_the_runtime_with_the_host_toolchain() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("reussir-build");
    let fakes = Fakes::new(tmp.path());

    let out = run(rene(&["build", "--manifest-path"])
        .arg(demo_manifest())
        .arg("--build-dir")
        .arg(&root)
        .env("REUSSIR_RRC", &fakes.rrc)
        .env(rene::cache::CACHE_DIR_ENV, &fakes.cache));
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let stdout = String::from_utf8(out.stdout).unwrap();
    let libdirs: Vec<PathBuf> = stdout.lines().map(PathBuf::from).collect();
    assert_eq!(libdirs.len(), 3, "stdout: {stdout}");
    // The detected rust libdir holds the standard library…
    assert!(libdirs[0].is_dir());
    // …while explicit `--target` keeps host proc macros and target libraries
    // in separate Cargo deps directories. One holds the runtime rlib polyffi
    // links against; ordering varies with the target triple's spelling.
    assert!(libdirs[1..].iter().all(|dir| dir.is_dir()));
    assert!(
        libdirs[1..].iter().any(|dir| {
            std::fs::read_dir(dir)
                .unwrap()
                .filter_map(Result::ok)
                .any(|entry| {
                    entry
                        .file_name()
                        .to_string_lossy()
                        .starts_with("libreussir_rt")
                })
        }),
        "no libreussir_rt in {:#?}",
        &libdirs[1..]
    );
}

/// The cross-package pipeline under the fake toolchain: the dependency's
/// interface and archive compile before the root's targets, the root sees
/// the cone (`--extern`/`--extern-src`) and links the archive
/// (`--link-lib`), records make the second build a no-op, and a touched
/// dependency source rebuilds it.
#[cfg(unix)]
#[test]
fn build_runs_the_dependency_pipeline() {
    let tmp_dir = tempfile::tempdir().unwrap();
    let tmp = tmp_dir.path().canonicalize().unwrap();
    let root = tmp.join("reussir-build");

    // The root package, depending on `util` by path.
    std::fs::create_dir_all(tmp.join("src")).unwrap();
    std::fs::write(
        tmp.join("src/lib.rr"),
        "pub fn entry(n: u64) -> u64 { n }\n",
    )
    .unwrap();
    let manifest = tmp.join("rene.ncl");
    std::fs::write(
        &manifest,
        "{ package = { name = \"app\", version = \"0.1.0\" },\n\
         \x20 dependencies.util = { path = \"vendor/util\", version = \"^1.0\" },\n\
         \x20 targets.demo = { kind = 'executable } }\n",
    )
    .unwrap();
    let util = tmp.join("vendor/util");
    std::fs::create_dir_all(util.join("src")).unwrap();
    std::fs::write(
        util.join("src/lib.rr"),
        "pub fn twice(n: u64) -> u64 { n + n }\n",
    )
    .unwrap();
    std::fs::write(
        util.join("rene.ncl"),
        "{ package = { name = \"util\", version = \"1.2.0\" } }\n",
    )
    .unwrap();

    let fakes = Fakes::new(&tmp);
    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let compiles = fakes.package_compiles();
    // Dependency interface, dependency archive, root target — in order.
    assert_eq!(compiles.len(), 3, "{compiles:#?}");
    assert!(compiles[0].contains("--package-name util"), "{compiles:#?}");
    assert!(compiles[0].contains("--emit rri"), "{compiles:#?}");
    assert!(compiles[1].contains("--package-name util"), "{compiles:#?}");
    assert!(compiles[1].contains("--emit staticlib"), "{compiles:#?}");
    assert!(compiles[2].contains("--package-name app"), "{compiles:#?}");
    assert!(compiles[2].contains("--emit executable"), "{compiles:#?}");
    // The root sees the dependency's interface and sources, and links its
    // archive.
    let deps_dir = root.join("dev").join("deps");
    assert!(
        compiles[2].contains(&format!(
            "--extern util={}",
            deps_dir.join("util.rri").display()
        )),
        "{compiles:#?}"
    );
    assert!(
        compiles[2].contains(&format!("--extern-src util={}", util.join("src").display())),
        "{compiles:#?}"
    );
    assert!(
        compiles[2].contains(&format!(
            "--link-lib {}",
            deps_dir.join("libutil.a").display()
        )),
        "{compiles:#?}"
    );
    // The dependency's compiles never see link flags or their own extern
    // (the implicit `core` is the one interface every compile gets).
    assert!(!compiles[0].contains("--link-lib"), "{compiles:#?}");
    assert!(!compiles[1].contains("--extern util"), "{compiles:#?}");

    // Everything is recorded: the second build compiles nothing.
    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(
        fakes.package_compiles().len(),
        3,
        "stderr: {}\n{:#?}",
        stderr(&out),
        fakes.package_compiles()
    );

    // A touched dependency source re-runs its two compiles. The root's
    // target does NOT re-run: the fake fabricates byte-identical artifacts,
    // and the root's fingerprint hashes upstream *content* — the early
    // cutoff, demonstrated. (With a real compiler the archive's bytes would
    // change and the root would relink.)
    std::fs::write(
        util.join("src/lib.rr"),
        "pub fn twice(n: u64) -> u64 { n * 2 }\n",
    )
    .unwrap();
    let out = fakes.rene(&["build"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let after = fakes.package_compiles();
    assert_eq!(after.len(), 5, "{after:#?}");
    assert!(after[3].contains("--emit rri"), "{after:#?}");
    assert!(after[4].contains("--emit staticlib"), "{after:#?}");
    assert!(
        after[3..]
            .iter()
            .all(|line| !line.contains("--package-name app")),
        "the root re-ran despite identical upstream bytes: {after:#?}"
    );

    // Reports are side outputs of artifact-producing compiles, never the interface
    // compile. Enabling them after a normal build refreshes each affected
    // record exactly once and uses collision-free paths beside the products.
    let out = fakes.rene(&["build", "--token-reuse-remarks"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let with_remarks = fakes.package_compiles();
    assert_eq!(with_remarks.len(), 8, "{with_remarks:#?}");
    assert!(
        !with_remarks[5].contains("--token-reuse-remarks"),
        "interface unexpectedly requested remarks: {with_remarks:#?}"
    );
    let dep_report = deps_dir.join("libutil.a.token-reuse.json");
    let root_report = root.join("dev/demo.token-reuse.json");
    assert!(
        with_remarks[6].contains(&format!("--token-reuse-remarks {}", dep_report.display())),
        "{with_remarks:#?}"
    );
    assert!(
        with_remarks[7].contains(&format!("--token-reuse-remarks {}", root_report.display())),
        "{with_remarks:#?}"
    );
    assert!(dep_report.is_file() && root_report.is_file());

    let out = fakes.rene(&["build", "--token-reuse-remarks"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert_eq!(fakes.package_compiles().len(), 8, "fresh reports rebuilt");

    // Deleting one tracked sidecar recompiles only its own product.
    std::fs::remove_file(&root_report).unwrap();
    let out = fakes.rene(&["build", "--token-reuse-remarks"], &manifest, &root);
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    let repaired = fakes.package_compiles();
    assert_eq!(repaired.len(), 9, "{repaired:#?}");
    assert!(repaired[8].contains("--package-name app"), "{repaired:#?}");
}

/// `rene new` end to end: the flags land in the scaffold, the manifest the
/// binary writes really evaluates, and stdout stays silent (it is reserved
/// for machine-readable listings).
#[test]
fn new_scaffolds_a_package_the_loader_accepts() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().join("hello");
    let out = run(rene(&["new", "--vcs", "none", "--lib"]).arg(&dir));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(out.stdout.is_empty(), "stdout is for machine output");

    let loaded = rene::manifest::load(&dir.join("rene.ncl")).unwrap();
    assert_eq!(loaded.manifest.package.name, "hello");
    assert_eq!(
        loaded.manifest.targets["hello"].kind,
        rene::manifest::TargetKind::Dynlib
    );
    assert!(!dir.join(".gitignore").exists(), "--vcs none writes none");

    // Occupied now: a rerun must refuse rather than overwrite.
    let out = run(rene(&["new", "--vcs", "none"]).arg(&dir));
    assert_eq!(out.status.code(), Some(1));
    assert!(
        stderr(&out).contains("not empty"),
        "stderr: {}",
        stderr(&out)
    );
}

/// Interactive mode reads the answers from stdin; typed answers override
/// the flag-provided defaults, and empty ones keep them.
#[test]
fn new_interactive_answers_override_the_flag_defaults() {
    use std::io::Write as _;
    use std::process::Stdio;

    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path().join("scaffold");
    // name: typed; version: kept; executable: declined (flag default was
    // yes); dynlib: accepted; staticlib: kept (no); triple: typed; git:
    // declined.
    let answers = "calc\n\nn\ny\n\nwasm32-wasip1\nn\n";
    let mut child = rene(&["new", "-i"])
        .arg(&dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(answers.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success(), "stderr: {}", stderr(&out));

    let manifest = rene::manifest::load(&dir.join("rene.ncl"))
        .unwrap()
        .manifest;
    assert_eq!(manifest.package.name, "calc");
    assert_eq!(manifest.package.version.as_deref(), Some("0.1.0"));
    assert_eq!(manifest.targets.len(), 1, "{:?}", manifest.targets);
    assert_eq!(
        manifest.targets["calc"].kind,
        rene::manifest::TargetKind::Dynlib
    );
    let dev = rene::manifest::resolve_profile(&manifest, "dev").unwrap();
    assert_eq!(dev.default_target_triple.as_deref(), Some("wasm32-wasip1"));
    assert!(!dir.join(".git").exists(), "git was declined");
}

/// The git default: a repository outside any work tree gets its own, a
/// scaffold nested inside one only gets the ignore file.
#[test]
fn new_inits_git_once_and_never_nested() {
    if !std::process::Command::new("git")
        .arg("--version")
        .output()
        .is_ok_and(|out| out.status.success())
    {
        eprintln!("skipping: no git on PATH");
        return;
    }
    let tmp = tempfile::tempdir().unwrap();
    let outer = tmp.path().join("outer");
    let out = run(rene(&["new"]).arg(&outer));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(outer.join(".git").exists());
    let ignore = std::fs::read_to_string(outer.join(".gitignore")).unwrap();
    assert!(ignore.contains("/reussir-build/"), "{ignore}");

    let nested = outer.join("vendor").join("inner");
    let out = run(rene(&["new"]).arg(&nested));
    assert!(out.status.success(), "stderr: {}", stderr(&out));
    assert!(
        !nested.join(".git").exists(),
        "must not nest a repository inside `outer`'s"
    );
    assert!(nested.join(".gitignore").exists());
}
