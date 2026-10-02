//! Serialized executable storage; no PJRT pointers enter this cache.

use super::{
    Error, Result,
    cache_config::{DiskConfig, WritePolicy},
};
use foyer::{
    BlockEngineConfig, DeviceBuilder, FsDeviceBuilder, HybridCache, HybridCacheBuilder,
    HybridCachePolicy, PsyncIoEngineConfig, S3FifoConfig,
};
use std::{
    fs::{self, File},
    sync::Arc,
};

pub(super) struct Artifacts {
    pub cache: HybridCache<Vec<u8>, Vec<u8>>,
    // Hold the directory lock until the store has closed and its tasks stopped.
    _lock: File,
}

impl Artifacts {
    pub async fn open(config: &DiskConfig) -> Result<Arc<Self>> {
        let directory = config
            .directory
            .as_ref()
            .ok_or_else(|| Error::local("no platform cache directory available"))?;
        fs::create_dir_all(directory).map_err(|e| Error::local(e.to_string()))?;
        let lock = File::options()
            .create(true)
            .truncate(false)
            .write(true)
            .open(directory.join(".lock"))
            .map_err(|e| Error::local(e.to_string()))?;
        lock.try_lock()
            .map_err(|e| Error::local(format!("PJRT cache directory is unavailable: {e}")))?;
        // Foyer resizes active partitions but does not remove surplus files.
        // Reset our disposable data when its physical layout changes, so a
        // smaller configured capacity also bounds an existing cache directory.
        let layout = format!("{}:{}", config.capacity_bytes, config.block_bytes);
        let layout_path = directory.join("layout");
        let data = directory.join("data");
        let previous = match fs::read_to_string(&layout_path) {
            Ok(layout) => Some(layout),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(Error::local(error.to_string())),
        };
        if previous.as_deref() != Some(&layout) {
            match fs::remove_dir_all(&data) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(Error::local(error.to_string())),
            }
            fs::write(layout_path, layout).map_err(|e| Error::local(e.to_string()))?;
        }
        let device = FsDeviceBuilder::new(data)
            .with_capacity(config.capacity_bytes)
            .build()
            .map_err(|e| Error::local(e.to_string()))?;
        let cache = HybridCacheBuilder::new()
            .with_name("reussir-pjrt-artifacts")
            .with_policy(match config.write_policy {
                WritePolicy::OnEviction => HybridCachePolicy::WriteOnEviction,
                WritePolicy::OnInsertion => HybridCachePolicy::WriteOnInsertion,
            })
            .with_flush_on_close(config.flush_on_shutdown)
            .memory(config.buffer_bytes)
            .with_shards(1)
            .with_eviction_config(S3FifoConfig::default())
            .with_weighter(|key: &Vec<u8>, value: &Vec<u8>| key.len().saturating_add(value.len()))
            .storage()
            .with_io_engine_config(PsyncIoEngineConfig::new())
            .with_engine_config(
                BlockEngineConfig::new(device)
                    .with_block_size(config.block_bytes)
                    .with_buffer_pool_size(config.buffer_bytes)
                    .with_submit_queue_size_threshold(config.buffer_bytes),
            )
            .build()
            .await
            .map_err(|e| Error::local(e.to_string()))?;
        Ok(Arc::new(Self { cache, _lock: lock }))
    }

    pub async fn close(&self) {
        if let Err(error) = self.cache.close().await {
            tracing::warn!(%error, "PJRT artifact cache close failed");
        }
    }
}
