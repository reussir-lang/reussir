//! Bundles the `reussir-rt` crate source into the `rene` binary as a
//! zstd-compressed tar, so `rene build` can materialize and bake the runtime
//! inside any project's build directory with the user's own Rust toolchain
//! (docs/design/system-rust-runtime.md). Shipping *source* is what makes the
//! bundle cross-platform: the produced rlib/staticlib are version-locked to
//! the rustc that builds them, so they must be baked on the user's machine,
//! never prebuilt here.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use toml_edit::{DocumentMut, Item};

#[path = "../reussir-pjrt-sys/header.rs"]
mod pjrt_header;

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let crates_dir = manifest_dir.parent().unwrap();
    let rt_dir = crates_dir.join("reussir-rt");
    let workspace_dir = crates_dir.parent().unwrap();
    let workspace_toml = workspace_dir.join("Cargo.toml");
    let workspace_lock = workspace_dir.join("Cargo.lock");

    println!(
        "cargo::rerun-if-changed={}",
        rt_dir.join("Cargo.toml").display()
    );
    println!("cargo::rerun-if-changed={}", rt_dir.join("src").display());
    println!("cargo::rerun-if-changed={}", workspace_toml.display());
    println!("cargo::rerun-if-changed={}", workspace_lock.display());
    println!(
        "cargo::rerun-if-changed={}",
        workspace_dir.join(".cargo/config.toml").display()
    );

    let manifest = standalone_manifest(&rt_dir.join("Cargo.toml"), &workspace_toml);
    // Seed the standalone graph from the workspace pins. The packaging step
    // below prunes unreachable packages and verifies every surviving pin.
    let lock = fs::read_to_string(&workspace_lock).expect("failed to read Cargo.lock");

    // Unpack the runtime and its optional PjRt bindings side by side, preserving
    // the relative path dependency in the runtime's manifest.
    let mut tar = tar::Builder::new(Vec::new());
    append_text(&mut tar, "reussir-rt/Cargo.toml", &manifest);
    append_text(&mut tar, "reussir-rt/Cargo.lock", &lock);
    tar.append_dir_all("reussir-rt/src", rt_dir.join("src"))
        .expect("failed to archive reussir-rt/src");
    // Cargo resolves optional path dependencies even with their feature off.
    // Ship the bindings source beside the runtime.
    let pjrt_dir = crates_dir.join("reussir-pjrt-sys");
    println!("cargo::rerun-if-changed={}", pjrt_dir.display());
    append_text(
        &mut tar,
        "reussir-pjrt-sys/Cargo.toml",
        &standalone_manifest(&pjrt_dir.join("Cargo.toml"), &workspace_toml),
    );
    for file in ["build.rs", "header.rs", "README.md"] {
        append_text(
            &mut tar,
            &format!("reussir-pjrt-sys/{file}"),
            &fs::read_to_string(pjrt_dir.join(file)).expect("failed to read PjRt bindings source"),
        );
    }
    tar.append_dir_all("reussir-pjrt-sys/src", pjrt_dir.join("src"))
        .expect("failed to archive PjRt bindings source");
    if let Some(header) = pjrt_header::find() {
        // Use the header from the configured project dependency at build time.
        // The extracted crate can then build without the original XLA checkout.
        let license = header
            .ancestors()
            .skip(1)
            .map(|dir| dir.join("LICENSE"))
            .find(|path| path.is_file())
            .expect("bundling pjrt_c_api.h requires the upstream LICENSE alongside it or in its source checkout");
        for (source, name) in [(&header, "pjrt_c_api.h"), (&license, "LICENSE")] {
            println!("cargo::rerun-if-changed={}", source.display());
            append_text(
                &mut tar,
                &format!("reussir-pjrt-sys/bundled/{name}"),
                &fs::read_to_string(source).expect("failed to read PjRt header/license"),
            );
        }
    }
    // The PjRt runtime also depends on reussir-stablehlo. Its build script
    // links archives from an OpenXLA-enabled CMake build, so ship only what
    // cargo needs, including the helper the build script includes by path.
    let stablehlo_dir = crates_dir.join("reussir-stablehlo");
    println!("cargo::rerun-if-changed={}", stablehlo_dir.display());
    append_text(
        &mut tar,
        "reussir-stablehlo/Cargo.toml",
        &standalone_manifest(&stablehlo_dir.join("Cargo.toml"), &workspace_toml),
    );
    for file in [
        "reussir-stablehlo/build.rs",
        "reussir-backend-sys/native_archive.rs",
    ] {
        println!(
            "cargo::rerun-if-changed={}",
            crates_dir.join(file).display()
        );
        append_text(
            &mut tar,
            file,
            &fs::read_to_string(crates_dir.join(file))
                .expect("failed to read StableHLO build source"),
        );
    }
    tar.append_dir_all("reussir-stablehlo/src", stablehlo_dir.join("src"))
        .expect("failed to archive reussir-stablehlo source");
    let archive = tar.into_inner().expect("failed to finish the tar archive");

    bundle_vendored_runtime(&archive, &workspace_lock);

    bundle_rcgmp_runtime();
    bundle_library(workspace_dir, "core");
    bundle_library(workspace_dir, "std");
}

/// Bundle the rcgmp integer runtime archive that CMake built for rene's own
/// target, when the build enables it (an empty file otherwise). Unlike
/// `reussir-rt`, it is C++ with no Rust ABI to match, so a prebuilt archive
/// serves every toolchain; runtime bakes for that target stage it.
fn bundle_rcgmp_runtime() {
    println!("cargo::rerun-if-env-changed=REUSSIR_RCGMP_RUNTIME");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("rcgmp-runtime.a");
    match env::var_os("REUSSIR_RCGMP_RUNTIME") {
        Some(archive) => {
            println!("cargo::rerun-if-changed={}", Path::new(&archive).display());
            fs::copy(&archive, &out).expect("failed to bundle the rcgmp runtime archive");
        }
        None => fs::write(&out, []).expect("failed to write the empty rcgmp runtime bundle"),
    }
    println!(
        "cargo::rustc-env=RENE_RCGMP_RUNTIME_TARGET={}",
        env::var("TARGET").unwrap()
    );
}

/// Bundle a library package the same way as the runtime: a
/// zstd-compressed tar. Entries are appended in sorted order with fixed
/// metadata, so the archive bytes — and the content hash that keys the
/// unpacked directory — depend only on the sources.
fn bundle_library(workspace_dir: &Path, name: &str) {
    let core_dir = workspace_dir.join("library").join(name);
    println!("cargo::rerun-if-changed={}", core_dir.display());

    let mut files = Vec::new();
    files.push(core_dir.join("rene.ncl"));
    collect_files(&core_dir.join("src"), &mut files);
    files.sort();

    let mut tar = tar::Builder::new(Vec::new());
    for path in &files {
        let rel = path
            .strip_prefix(&core_dir)
            .unwrap()
            .to_str()
            .expect("library source paths are UTF-8")
            .replace('\\', "/");
        let text = fs::read_to_string(path).expect("failed to read a library source");
        append_text(&mut tar, &rel, &text);
    }
    let archive = tar.into_inner().expect("failed to finish the library tar");
    let compressed =
        zstd::encode_all(&archive[..], 19).expect("failed to zstd-compress the library bundle");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join(format!("{name}-src.tar.zst"));
    fs::write(&out, compressed).expect("failed to write the library bundle");
}

fn collect_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("failed to read library sources") {
        let path = entry.expect("failed to read a directory entry").path();
        if path.is_dir() {
            collect_files(&path, out);
        } else {
            out.push(path);
        }
    }
}

fn append_text(tar: &mut tar::Builder<Vec<u8>>, path: &str, text: &str) {
    let mut header = tar::Header::new_gnu();
    header.set_size(text.len() as u64);
    header.set_mode(0o644);
    // Fixed mtime: the archive bytes (and thus the runtime staleness hash)
    // depend only on content, not on when the bundle was packed.
    header.set_mtime(0);
    header.set_cksum();
    tar.append_data(&mut header, path, text.as_bytes())
        .expect("failed to append the rewritten manifest");
}

/// Rewrite the in-workspace `reussir-rt` manifest into a standalone one:
/// resolve every `workspace = true` inheritance against the workspace
/// manifest, and pin an empty `[workspace]` table so the unpacked copy stays
/// its own workspace root even when the build directory sits inside some
/// enclosing cargo workspace (a user project often does).
fn standalone_manifest(crate_toml: &Path, workspace_toml: &Path) -> String {
    let mut doc: DocumentMut = fs::read_to_string(crate_toml)
        .unwrap()
        .parse()
        .expect("reussir-rt/Cargo.toml is not valid TOML");
    let ws: DocumentMut = fs::read_to_string(workspace_toml)
        .unwrap()
        .parse()
        .expect("workspace Cargo.toml is not valid TOML");

    let inherits = |item: &Item| {
        item.get("workspace")
            .and_then(Item::as_value)
            .and_then(|v| v.as_bool())
            == Some(true)
    };

    let pkg = doc["package"]
        .as_table_mut()
        .expect("[package] must be a table");
    let keys: Vec<String> = pkg.iter().map(|(k, _)| k.to_owned()).collect();
    for key in keys {
        if inherits(&pkg[&key]) {
            pkg[&key] = ws["workspace"]["package"][&key].clone();
        }
    }

    for section in ["dependencies", "build-dependencies", "dev-dependencies"] {
        let Some(deps) = doc.get_mut(section).and_then(Item::as_table_like_mut) else {
            continue;
        };
        let names: Vec<String> = deps.iter().map(|(k, _)| k.to_owned()).collect();
        for name in names {
            let spec = deps.get(&name).unwrap();
            if !inherits(spec) {
                continue;
            }
            let overrides: Vec<_> = spec
                .as_table_like()
                .unwrap()
                .iter()
                .filter(|(key, _)| *key != "workspace")
                .map(|(key, item)| (key.to_owned(), item.clone()))
                .collect();
            // `[patch]` only applies inside the workspace, and the unpacked
            // crates are their own workspace roots: use the patch source.
            let patched = ws
                .get("patch")
                .and_then(|patch| patch.get("crates-io"))
                .and_then(|patches| patches.get(&name));
            let spec = patched.unwrap_or(&ws["workspace"]["dependencies"][&name]);
            let mut resolved = spec.clone();
            if !overrides.is_empty() {
                if let Some(version) = resolved.as_str() {
                    let mut table = toml_edit::Table::new();
                    table.insert("version", toml_edit::value(version));
                    resolved = Item::Table(table);
                }
                for (key, value) in overrides {
                    if key == "features" && resolved.get("features").is_some() {
                        let features = resolved["features"].as_array_mut().unwrap();
                        for feature in value.as_array().unwrap() {
                            if !features.iter().any(|old| old.as_str() == feature.as_str()) {
                                features.push(feature.clone());
                            }
                        }
                    } else {
                        resolved.as_table_like_mut().unwrap().insert(&key, value);
                    }
                }
            }
            deps.insert(&name, resolved);
        }

        // The `nightly` feature of mlir_sync only gates optimizations (see the
        // comment in reussir-rt/Cargo.toml). The workspace pins a nightly
        // toolchain, but the bundled runtime is built by the *user's* rust —
        // drop the feature so a stable toolchain works
        // (docs/design/system-rust-runtime.md §2).
        if let Some(features) = deps
            .get_mut("mlir_sync")
            .and_then(|s| s.get_mut("features"))
            .and_then(Item::as_array_mut)
        {
            features.retain(|f| f.as_str() != Some("nightly"));
        }
    }

    // Cut the crate loose from any workspace it might land under: cargo
    // walks parent directories for a workspace root, and a foreign root
    // rejects the crate ("believes it's in a workspace when it's not").
    doc["workspace"] = toml_edit::Item::Table(toml_edit::Table::new());

    doc.to_string()
}

/// Generate a self-contained Cargo source distribution at packaging time.
/// `cargo vendor` resolves every target and optional dependency, not only the
/// build host's selected features. Unlike the old workspace metadata query,
/// it fetches exactly the standalone runtime closure and retains those sources
/// in the artifact; runtime baking never contacts a registry or Git server.
fn bundle_vendored_runtime(sources: &[u8], workspace_lock: &Path) {
    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let staging = out.join("runtime-src");
    if staging.exists() {
        fs::remove_dir_all(&staging).expect("failed to clear runtime staging directory");
    }
    fs::create_dir_all(&staging).unwrap();
    tar::Archive::new(sources)
        .unpack(&staging)
        .expect("failed to stage runtime sources");
    let runtime = staging.join("reussir-rt");
    let cargo = || {
        let mut command = std::process::Command::new(env::var_os("CARGO").unwrap());
        command
            .current_dir(&runtime)
            // The host packages sources even for a Windows cross-build.
            .env("CARGO_ENCODED_RUSTFLAGS", "")
            .env("RUSTFLAGS", "");
        command
    };
    // Cargo must prune the workspace's superset lock for this new root. Do not
    // permit that normalization to update any reachable version/source/checksum.
    // Subsequent validation and every consumer use --locked --offline.
    let output = cargo()
        .args([
            "vendor",
            "--respect-source-config",
            "--versioned-dirs",
            "vendor",
        ])
        .output()
        .expect("cannot vendor runtime dependencies");
    assert!(
        output.status.success(),
        "cargo vendor: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let seed: DocumentMut = fs::read_to_string(workspace_lock).unwrap().parse().unwrap();
    let resolved: DocumentMut = fs::read_to_string(runtime.join("Cargo.lock"))
        .unwrap()
        .parse()
        .unwrap();
    for package in resolved["package"].as_array_of_tables().unwrap() {
        assert!(
            seed["package"]
                .as_array_of_tables()
                .unwrap()
                .iter()
                .any(|old| {
                    ["name", "version", "source", "checksum"]
                        .iter()
                        .all(|field| {
                            old.get(field).and_then(Item::as_str)
                                == package.get(field).and_then(Item::as_str)
                        })
                }),
            "runtime vendoring changed a workspace dependency pin: {package}"
        );
    }
    let mut config: DocumentMut = String::from_utf8(output.stdout).unwrap().parse().unwrap();
    // Relative to .cargo's parent, so extraction works at any path/on Wine.
    config["source"]["vendored-sources"]["directory"] = toml_edit::value("vendor");
    config["net"]["offline"] = toml_edit::value(true);
    fs::create_dir_all(runtime.join(".cargo")).unwrap();
    fs::write(runtime.join(".cargo/config.toml"), config.to_string()).unwrap();
    let checked = cargo()
        .args([
            "metadata",
            "--format-version=1",
            "--all-features",
            "--locked",
            "--offline",
        ])
        .output()
        .expect("cannot validate vendored runtime");
    assert!(
        checked.status.success(),
        "vendored runtime metadata: {}",
        String::from_utf8_lossy(&checked.stderr)
    );

    // Normalize ordering, timestamps, owners and permissions for all source
    // files (including binary vendor assets), not just generated manifests.
    let file = fs::File::create(out.join("reussir-rt.tar.zst")).unwrap();
    let encoder = zstd::Encoder::new(file, 9).unwrap();
    let mut tar = tar::Builder::new(encoder);
    let mut files = Vec::new();
    collect_files(&staging, &mut files);
    files.sort();
    for path in files {
        let mut header = tar::Header::new_gnu();
        let contents = fs::read(&path).expect("failed to read runtime source");
        header.set_size(contents.len() as u64);
        header.set_mode(0o644);
        header.set_mtime(0);
        header.set_cksum();
        tar.append_data(
            &mut header,
            path.strip_prefix(&staging).unwrap(),
            &contents[..],
        )
        .expect("failed to archive runtime source");
    }
    tar.into_inner().unwrap().finish().unwrap();
}
