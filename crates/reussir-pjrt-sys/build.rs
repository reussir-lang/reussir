use std::{env, path::PathBuf};

mod header;

fn main() {
    let header = header::find().expect(
        "set REUSSIR_PJRT_C_API_HEADER to pjrt_c_api.h, or REUSSIR_PJRT_PATH to its directory or an XLA checkout",
    );
    println!("cargo:rerun-if-changed={}", header.display());
    bindgen::Builder::default()
        .header(header.to_str().expect("PjRt header path must be UTF-8"))
        .allowlist_type("PJRT_.*")
        .allowlist_var("PJRT_.*")
        .derive_default(true)
        .generate_comments(false)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("generate PjRt C API bindings (libclang is required)")
        .write_to_file(PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("pjrt.rs"))
        .expect("write PjRt bindings");
}
