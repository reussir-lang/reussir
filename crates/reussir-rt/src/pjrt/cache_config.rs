//! Standard compcache locations/configuration for the Reussir application.
//!
//! `reussir/config.ncl` under the platform configuration directory configures
//! compcache. `REUSSIR_CONFIG_DIR` and `REUSSIR_CACHE_DIR` override its roots.

pub(super) struct Reussir;
impl compcache::StoreDefaults for Reussir {
    const APP_NAME: &'static str = "reussir";
    const CONFIG_DIR_ENV: Option<&'static str> = Some("REUSSIR_CONFIG_DIR");
    const CACHE_DIR_ENV: Option<&'static str> = Some("REUSSIR_CACHE_DIR");
}

// The live-payload strategy needs these memory settings before being passed to
// StoreBuilder::in_memory. Load the standard file once, then give the builder
// the same explicit configuration; cache-root resolution remains compcache's.
pub(super) fn load() -> super::Result<compcache::Config> {
    let directory = std::env::var_os("REUSSIR_CONFIG_DIR")
        .filter(|value| !value.is_empty())
        .map(std::path::PathBuf::from)
        .or_else(|| dirs::config_dir().map(|root| root.join("reussir")))
        .ok_or(super::Error::ConfigDirectoryUnavailable)?;
    read(&directory)
}

fn read(directory: &std::path::Path) -> super::Result<compcache::Config> {
    let path = directory.join("config.ncl");
    match std::fs::metadata(directory) {
        Ok(metadata) if !metadata.is_dir() => {
            return Err(super::Error::Io(std::sync::Arc::new(std::io::Error::from(
                std::io::ErrorKind::NotADirectory,
            ))));
        }
        Err(error) if error.kind() != std::io::ErrorKind::NotFound => {
            return Err(super::Error::Io(std::sync::Arc::new(error)));
        }
        _ => {}
    }
    match std::fs::metadata(&path) {
        Ok(_) => Ok(compcache::Config::from_nickel(path)?),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            Ok(compcache::Config::default())
        }
        Err(error) => Err(super::Error::Io(std::sync::Arc::new(error))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compcache::StoreDefaults;
    #[test]
    fn standard_app_defaults_and_invalid_config_directory() {
        assert_eq!(Reussir::APP_NAME, "reussir");
        let root = tempfile::tempdir().unwrap();
        let absent = read(&root.path().join("absent")).unwrap();
        assert_eq!(
            absent.memory.weight_capacity,
            compcache::Config::default().memory.weight_capacity
        );
        let file = root.path().join("file");
        std::fs::write(&file, b"not a directory").unwrap();
        assert!(matches!(read(&file), Err(super::super::Error::Io(_))));
    }
}
