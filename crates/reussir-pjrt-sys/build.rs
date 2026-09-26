fn main() {
    let header = "vendor/pjrt_c_api.h";
    println!("cargo:rerun-if-changed={header}");
    bindgen::Builder::default()
        .header(header)
        .allowlist_type("PJRT_.*")
        .allowlist_var("PJRT_.*")
        .derive_default(true)
        .generate_comments(false)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("generate PjRt C API bindings (libclang is required)")
        .write_to_file(
            std::path::PathBuf::from(std::env::var_os("OUT_DIR").unwrap()).join("pjrt.rs"),
        )
        .expect("write PjRt bindings");
}
