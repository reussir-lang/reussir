//! Links the native library built by CMake (`ReussirStablehloRuntime`) and the
//! StableHLO archives it needs. MLIR and LLVM themselves come from mlir-sys.
#[path = "../reussir-backend-sys/native_archive.rs"]
mod native_archive;

fn main() {
    println!("cargo:rerun-if-env-changed=REUSSIR_STABLEHLO_LIB_DIR");
    let dir = std::env::var_os("REUSSIR_STABLEHLO_LIB_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| {
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../build/lib")
        });
    let manifest = dir.join("reussir-stablehlo-archives.txt");
    println!("cargo:rerun-if-changed={}", manifest.display());
    let archives = std::fs::read_to_string(&manifest).expect(
        "build ReussirStablehloRuntime with REUSSIR_ENABLE_OPENXLA=ON and set REUSSIR_STABLEHLO_LIB_DIR",
    );
    let mut names = vec!["ReussirStablehloRuntime"];
    names.extend(archives.lines());
    for name in &names {
        assert!(
            native_archive::archive_exists(&dir, name),
            "missing native archive {name}"
        );
    }
    native_archive::track(&dir, &names).expect("cannot fingerprint StableHLO archives");
    println!("cargo:rustc-link-search=native={}", dir.display());
    for name in &names {
        println!("cargo:rustc-link-lib=static={name}");
    }
}
