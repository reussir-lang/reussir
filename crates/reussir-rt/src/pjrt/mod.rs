//! PjRt context, cached compilation, and buffer ownership (`pjrt` Cargo feature).
//!
//! Build with `REUSSIR_PJRT_PATH` or `REUSSIR_PJRT_C_API_HEADER` pointing to
//! upstream headers. The runtime loads `REUSSIR_PJRT_PLUGIN` on first use and
//! owns the client for the process lifetime. Transfers block until the caller
//! can reuse its host memory. Uploads accept signed host byte strides and device memory/layout metadata;
//! downloads and their size queries accept an explicit host layout. Device
//! indices refer to the client's addressable devices. Multi-device arrays keep
//! these native buffer handles in the compiler's RC descriptor.
//!
//! Compilation verifies the exact payload's BLAKE3 checksum before consulting
//! bounded S3-FIFO caches. Loaded executables use reference-counted ownership;
//! serialized executables persist in a size-limited disk cache. Configuration
//! comes from REUSSIR_PJRT_CACHE_CONFIG or platform cache-directory defaults.
//!
//! `REUSSIR_PJRT_CACHE_CONFIG` names a TOML file; omitted fields use these defaults:
//! ```toml
//! [memory]
//! loaded_entries = 128
//! [disk]
//! enabled = true
//! # directory = "/custom/cache" # relative paths resolve beside the config file
//! capacity_bytes = 1073741824
//! buffer_bytes = 67108864
//! block_bytes = 16777216
//! write_policy = "on_eviction" # or "on_insertion"
//! flush_on_shutdown = true
//! ```
//! The default directory is `reussir/pjrt` under `dirs::cache_dir()`:
//! `$XDG_CACHE_HOME` or `~/.cache` on Linux, `%LOCALAPPDATA%` on Windows,
//! and `~/Library/Caches` on macOS. No configuration version stamp is used.
//! Disk capacity must contain at least four whole blocks; blocks must be at
//! least 16 KiB and aligned to 4 KiB. Buffers must be at least 8 KiB and
//! aligned to 4 KiB. Foyer may skip artifacts larger than a
//! block or under write-buffer pressure. Changing capacity/block size resets
//! the disposable disk artifacts. `buffer_bytes` separately bounds the
//! artifact memory cache and disk write queue; foyer also uses I/O buffers and
//! indexes. Loaded-entry limits exclude references retained by callers and do
//! not constitute an RSS/device-memory limit.
//!
//! One process locks the directory for writing. A busy/unavailable directory,
//! unsupported serialization/topology API, or unreadable plugin binary falls
//! back to memory caching with a tracing warning. Invalid config is an error.
//! Persistent keys include plugin contents, platform version, serialized target
//! topology, XLA_FLAGS, verified payload digest, and exact compile-option bytes.
//! Corrupt or rejected artifacts are replaced by compilation; payload checksum
//! mismatches always fail, including on warm-cache hits.
//!
//! Normal process exit closes the artifact cache (and flushes hot entries when
//! configured). Compilation returns an owned handle; release it after execution.
//! Abort/forced termination can lose pending writes; cache misses recompile.
//!
//! `__reussir_pjrt_executable_execute` invokes an executable assigned to one
//! addressable device and blocks until its completion event resolves, including
//! for zero-result kernels. It borrows the executable and input buffers without
//! donation. The caller supplies output slots matching the executable's result
//! count and owns the returned buffers, released with the existing array API.
//! The executable and inputs can be released or reused when invocation returns.

mod api;
mod artifact;
mod buffer;
mod cache_config;
mod client;
mod context;
mod error;
mod executable;
pub mod ffi;
mod layout;
mod memory;

pub use buffer::Buffer;
use error::{Error, Result};
pub use executable::Executable;
use reussir_pjrt_sys as sys;
