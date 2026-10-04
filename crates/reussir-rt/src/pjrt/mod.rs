//! PjRt device buffers, cached compilation, and synchronous execution.
//!
//! - **Setup:** enable the `pjrt` Cargo feature and point `REUSSIR_PJRT_PATH` or
//!   `REUSSIR_PJRT_C_API_HEADER` to upstream headers; set `REUSSIR_PJRT_PLUGIN`
//!   to the runtime plugin.
//! - **Transfers:** support host strides and memory/layout selection; host memory
//!   can be reused when the transfer returns.
//! - **Compilation:** verifies the payload checksum and reuses cached executables.
//! - **Execution:** waits for all devices, consumes one compiled handle, borrows
//!   inputs without donation, and transfers ownership of output buffers.
//! - **Caching:** uses memory and disk caches, configurable through the TOML file
//!   named by `REUSSIR_PJRT_CACHE_CONFIG`; unavailable disk caching falls back to
//!   memory, while invalid configuration is an error.
//!
//! See [`ffi`] for ABI and ownership requirements, and `cache_config.rs` for
//! cache settings and defaults.

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
