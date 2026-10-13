//! Bounded, explicitly opened storage for compiler-generated FFI bitcode.
//!
//! Keys and invalidation belong to the caller. No compiler, PJRT, or environment
//! dependency is imposed here. Calls are synchronous; a dedicated owner thread
//! drives Foyer's Tokio runtime so callers need no executor (and may have one).

pub mod ffi;

use foyer::{
    BlockEngineConfig, DeviceBuilder, FsDeviceBuilder, HybridCacheBuilder, HybridCachePolicy,
    PsyncIoEngineConfig, S3FifoConfig,
};
use std::{
    fmt,
    fs::{self, File},
    panic::{AssertUnwindSafe, catch_unwind},
    path::PathBuf,
    sync::mpsc::{self, Receiver, Sender, SyncSender},
    thread::{self, JoinHandle},
};

/// Cache errors leave compilation and fallback policy to the caller.
#[derive(Debug)]
pub struct Error(String);

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}
impl std::error::Error for Error {}

fn error(value: impl fmt::Display) -> Error {
    Error(value.to_string())
}

pub type Result<T> = std::result::Result<T, Error>;

/// All capacities count bytes. Memory capacity weighs key and value payloads;
/// runtime metadata, I/O buffers, and caller-owned results are additional.
#[derive(Clone, Debug)]
pub struct Config {
    /// Dedicated, trusted cache directory; exclusively locked until close.
    pub directory: PathBuf,
    pub memory_bytes: usize,
    pub disk_bytes: usize,
    pub block_bytes: usize,
}

impl Config {
    pub fn new(directory: impl Into<PathBuf>) -> Self {
        Self {
            directory: directory.into(),
            memory_bytes: 64 * 1024 * 1024,
            disk_bytes: 1024 * 1024 * 1024,
            block_bytes: 16 * 1024 * 1024,
        }
    }

    fn validate(&self) -> Result<()> {
        if self.directory.as_os_str().is_empty()
            || self.memory_bytes < 8192
            || !self.memory_bytes.is_multiple_of(4096)
            || self.block_bytes < 16384
            || !self.block_bytes.is_multiple_of(4096)
            || self.disk_bytes / self.block_bytes < 4
            || !self.disk_bytes.is_multiple_of(self.block_bytes)
        {
            return Err(error(
                "cache requires a directory, memory >= 8 KiB aligned to 4 KiB, \
                 blocks >= 16 KiB aligned to 4 KiB, and at least four whole disk blocks",
            ));
        }
        Ok(())
    }

    fn max_entry_bytes(&self) -> usize {
        // Reserve a page for Foyer's record/serialization overhead.
        self.memory_bytes.min(self.block_bytes - 4096)
    }
}

enum Request {
    Get(Vec<u8>, Sender<Result<Option<Vec<u8>>>>),
    Insert(Vec<u8>, Vec<u8>, Sender<Result<()>>),
    Close(Sender<Result<()>>),
}

/// Synchronous, thread-safe cache handle. Drop flushes and joins its owner.
/// Explicit `close` reports flush failures; dropping intentionally ignores them.
pub struct Cache {
    requests: SyncSender<Request>,
    worker: Option<JoinHandle<()>>,
    max_entry_bytes: usize,
}

impl Cache {
    pub fn open(config: Config) -> Result<Self> {
        config.validate()?;
        let max_entry_bytes = config.max_entry_bytes();
        let (requests, receiver) = mpsc::sync_channel(16);
        let (ready, initialized) = mpsc::channel();
        let worker = thread::Builder::new()
            .name("reussir-ffi-cache".into())
            .spawn(move || {
                let result = catch_unwind(AssertUnwindSafe(|| run(config, receiver, &ready)));
                if let Err(failure) = result.unwrap_or_else(|_| Err(error("cache worker panicked")))
                {
                    // Before initialization this reports the error to open. After
                    // initialization reply-channel closure reports it to a call.
                    let _ = ready.send(Err(failure));
                }
            })
            .map_err(error)?;
        match initialized.recv().map_err(error).and_then(|result| result) {
            Ok(()) => Ok(Self {
                requests,
                worker: Some(worker),
                max_entry_bytes,
            }),
            Err(failure) => {
                let _ = worker.join();
                Err(failure)
            }
        }
    }

    pub fn get(&self, key: &[u8]) -> Result<Option<Vec<u8>>> {
        if key.len() > self.max_entry_bytes {
            return Ok(None);
        }
        let (reply, response) = mpsc::channel();
        self.requests
            .send(Request::Get(key.to_vec(), reply))
            .map_err(error)?;
        response.recv().map_err(error)?
    }

    /// Copies an entry into the cache. Entries too large for its memory/block
    /// budget are rejected. Success is admission, not a durability guarantee;
    /// eviction is allowed, and close flushes admitted pending disk writes.
    pub fn insert(&self, key: &[u8], value: &[u8]) -> Result<()> {
        if key.len().saturating_add(value.len()) > self.max_entry_bytes {
            return Err(error("entry exceeds cache memory or block budget"));
        }
        let (reply, response) = mpsc::channel();
        self.requests
            .send(Request::Insert(key.to_vec(), value.to_vec(), reply))
            .map_err(error)?;
        response.recv().map_err(error)?
    }

    /// Flush pending writes, release the directory lock, and stop all workers.
    pub fn close(mut self) -> Result<()> {
        self.stop()
    }

    fn stop(&mut self) -> Result<()> {
        let Some(worker) = self.worker.take() else {
            return Ok(());
        };
        let (reply, response) = mpsc::channel();
        let result = self
            .requests
            .send(Request::Close(reply))
            .map_err(error)
            .and_then(|()| response.recv().map_err(error))
            .and_then(|result| result);
        worker.join().map_err(|_| error("cache worker panicked"))?;
        result
    }
}

impl Drop for Cache {
    fn drop(&mut self) {
        let _ = self.stop();
    }
}

fn run(config: Config, requests: Receiver<Request>, ready: &Sender<Result<()>>) -> Result<()> {
    fs::create_dir_all(&config.directory).map_err(error)?;
    let lock = File::options()
        .create(true)
        .truncate(false)
        .write(true)
        .open(config.directory.join(".lock"))
        .map_err(error)?;
    lock.try_lock()
        .map_err(|failure| error(format!("cache directory unavailable: {failure}")))?;

    // Foyer keeps surplus partitions when shrinking. Reset only our private data
    // directory when the physical layout changes, under the exclusive lock.
    let layout = format!(
        "reussir-ffi-cache-v1:{}:{}",
        config.disk_bytes, config.block_bytes
    );
    let layout_path = config.directory.join("layout");
    let data = config.directory.join("data");
    let previous = match fs::read_to_string(&layout_path) {
        Ok(value) => Some(value),
        Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => None,
        Err(failure) => return Err(error(failure)),
    };
    if previous.as_deref() != Some(&layout) {
        match fs::remove_dir_all(&data) {
            Ok(()) => {}
            Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => {}
            Err(failure) => return Err(error(failure)),
        }
        fs::write(layout_path, layout).map_err(error)?;
    }
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .enable_all()
        .build()
        .map_err(error)?;
    let cache = runtime.block_on(async {
        let device = FsDeviceBuilder::new(data)
            .with_capacity(config.disk_bytes)
            .build()
            .map_err(error)?;
        HybridCacheBuilder::new()
            .with_name("reussir-ffi-cache")
            .with_policy(HybridCachePolicy::WriteOnInsertion)
            .with_flush_on_close(true)
            .memory(config.memory_bytes)
            .with_shards(1)
            .with_eviction_config(S3FifoConfig::default())
            .with_weighter(|key: &Vec<u8>, value: &Vec<u8>| key.len().saturating_add(value.len()))
            .storage()
            .with_io_engine_config(PsyncIoEngineConfig::new())
            .with_engine_config(
                BlockEngineConfig::new(device)
                    .with_block_size(config.block_bytes)
                    .with_buffer_pool_size(config.memory_bytes)
                    .with_submit_queue_size_threshold(config.memory_bytes),
            )
            .build()
            .await
            .map_err(error)
    })?;
    ready.send(Ok(())).map_err(error)?;
    let mut closed = None;
    while let Ok(request) = requests.recv() {
        match request {
            Request::Get(key, reply) => {
                let result = runtime.block_on(async {
                    cache
                        .get(&key)
                        .await
                        .map(|entry| entry.map(|entry| entry.value().clone()))
                        .map_err(error)
                });
                let _ = reply.send(result);
            }
            Request::Insert(key, value, reply) => {
                runtime.block_on(async { cache.insert(key, value) });
                let _ = reply.send(Ok(()));
            }
            Request::Close(reply) => {
                closed = Some(reply);
                break;
            }
        }
    }
    let result = runtime.block_on(cache.close()).map_err(error);
    drop(cache);
    drop(runtime);
    drop(lock);
    if let Some(reply) = closed {
        let _ = reply.send(result);
        Ok(())
    } else {
        result
    }
}
