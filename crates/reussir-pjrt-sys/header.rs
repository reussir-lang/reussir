use std::{env, path::PathBuf};

// Shared by bindgen and rene's source bundler so they select the same header.
pub fn find() -> Option<PathBuf> {
    println!("cargo:rerun-if-env-changed=REUSSIR_PJRT_C_API_HEADER");
    println!("cargo:rerun-if-env-changed=REUSSIR_PJRT_PATH");
    if let Some(header) = env::var_os("REUSSIR_PJRT_C_API_HEADER") {
        return Some(header.into());
    }
    if let Some(root) = env::var_os("REUSSIR_PJRT_PATH") {
        let root = PathBuf::from(root);
        return Some(
            ["pjrt_c_api.h", "xla/pjrt/c/pjrt_c_api.h"]
                .into_iter()
                .map(|suffix| root.join(suffix))
                .find(|path| path.is_file())
                .expect("REUSSIR_PJRT_PATH must contain pjrt_c_api.h or xla/pjrt/c/pjrt_c_api.h"),
        );
    }
    // Present in rene's extracted runtime bundle, not vendored in this repo.
    let bundled = PathBuf::from("bundled/pjrt_c_api.h");
    bundled.is_file().then_some(bundled)
}
