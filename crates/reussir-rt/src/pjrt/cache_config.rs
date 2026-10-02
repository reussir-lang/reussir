//! Runtime cache configuration. No config file is required for normal use.

use super::{Error, Result};
use serde::Deserialize;
use std::path::{Path, PathBuf};

#[derive(Debug, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(super) struct CacheConfig {
    pub memory: MemoryConfig,
    pub disk: DiskConfig,
}

#[derive(Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(super) struct MemoryConfig {
    pub loaded_entries: usize,
}

impl Default for MemoryConfig {
    fn default() -> Self {
        Self {
            loaded_entries: 128,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum WritePolicy {
    #[default]
    OnEviction,
    OnInsertion,
}

#[derive(Debug, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(super) struct DiskConfig {
    pub enabled: bool,
    pub directory: Option<PathBuf>,
    pub capacity_bytes: usize,
    pub buffer_bytes: usize,
    pub block_bytes: usize,
    pub write_policy: WritePolicy,
    pub flush_on_shutdown: bool,
}

impl Default for DiskConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            directory: dirs::cache_dir().map(|dir| dir.join("reussir").join("pjrt")),
            capacity_bytes: 1024 * 1024 * 1024,
            buffer_bytes: 64 * 1024 * 1024,
            block_bytes: 16 * 1024 * 1024,
            write_policy: WritePolicy::OnEviction,
            flush_on_shutdown: true,
        }
    }
}

impl CacheConfig {
    pub fn load() -> Result<Self> {
        match std::env::var_os("REUSSIR_PJRT_CACHE_CONFIG") {
            Some(path) => Self::read(Path::new(&path)),
            None => Ok(Self::default()),
        }
    }

    fn read(path: &Path) -> Result<Self> {
        let text = std::fs::read_to_string(path)
            .map_err(|e| Error::local(format!("PJRT cache config {}: {e}", path.display())))?;
        let mut config = Self::parse(&text)?;
        if let Some(directory) = &mut config.disk.directory {
            if directory.is_relative() {
                *directory = path.parent().unwrap_or(Path::new(".")).join(&*directory);
            }
        }
        Ok(config)
    }

    fn parse(text: &str) -> Result<Self> {
        let config: Self =
            toml::from_str(text).map_err(|e| Error::local(format!("PJRT cache config: {e}")))?;
        if config.memory.loaded_entries == 0 {
            return Err(Error::local("PJRT cache loaded_entries must be positive"));
        }
        let disk = &config.disk;
        if disk.enabled
            && (disk.buffer_bytes < 8192
                || disk.buffer_bytes % 4096 != 0
                || disk.block_bytes < 16384
                || disk.block_bytes % 4096 != 0
                || disk.capacity_bytes / disk.block_bytes < 4
                || disk.capacity_bytes % disk.block_bytes != 0)
        {
            return Err(Error::local(
                "PJRT disk cache requires a buffer >= 8 KiB aligned to 4 KiB, a block size >= 16 KiB aligned to 4 KiB, and capacity of at least four whole blocks",
            ));
        }
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_paths_are_relative_to_config_and_read_errors_are_reported() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("cache.toml");
        assert!(CacheConfig::read(&path).is_err());
        std::fs::write(&path, "[disk]\ndirectory = 'artifacts'").unwrap();
        assert_eq!(
            CacheConfig::read(&path).unwrap().disk.directory.unwrap(),
            dir.path().join("artifacts")
        );
    }

    #[test]
    fn defaults_overrides_and_invalid_config() {
        let config = CacheConfig::parse("").unwrap();
        assert_eq!(config.memory.loaded_entries, 128);
        assert_eq!(
            config.disk.directory,
            dirs::cache_dir().map(|p| p.join("reussir/pjrt"))
        );
        assert!(config.disk.enabled);
        let config =
            CacheConfig::parse("[memory]\nloaded_entries = 4\n[disk]\nenabled = false").unwrap();
        assert_eq!(config.memory.loaded_entries, 4);
        assert!(!config.disk.enabled);
        for text in [
            "version = 1",
            "[memory]\nloaded_entries = 0",
            "[disk]\ncapacity_bytes = 1",
            "[disk]\nbuffer_bytes = 1",
            "[disk]\nblock_bytes = 3",
            "[disk]\nwrite_policy = 'typo'",
        ] {
            assert!(CacheConfig::parse(text).is_err(), "{text}");
        }
    }
}
