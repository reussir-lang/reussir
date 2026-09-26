use std::{env, path::PathBuf};

fn main() {
    println!("cargo:rerun-if-env-changed=REUSSIR_PJRT_C_API_HEADER");
    println!("cargo:rerun-if-env-changed=REUSSIR_PJRT_PATH");
    let header = env::var_os("REUSSIR_PJRT_C_API_HEADER")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let root = PathBuf::from(env::var_os("REUSSIR_PJRT_PATH").expect(
                "set REUSSIR_PJRT_C_API_HEADER to pjrt_c_api.h, or REUSSIR_PJRT_PATH to its directory or an XLA checkout",
            ));
            ["pjrt_c_api.h", "xla/pjrt/c/pjrt_c_api.h"]
                .into_iter()
                .map(|suffix| root.join(suffix))
                .find(|path| path.is_file())
                .expect("REUSSIR_PJRT_PATH must contain pjrt_c_api.h or xla/pjrt/c/pjrt_c_api.h")
        });
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
