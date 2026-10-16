//! PjRt device buffers and synchronous kernel launches.
//!
//! - **Setup:** enable the `pjrt` Cargo feature and point `REUSSIR_PJRT_PATH` or
//!   `REUSSIR_PJRT_C_API_HEADER` to upstream headers; set `REUSSIR_PJRT_PLUGIN`
//!   to the runtime plugin.
//! - **Transfers:** support host strides and memory/layout selection; host memory
//!   can be reused when the transfer returns.
//! - **Launch:** verifies the payload checksum, refines the kernel for the
//!   inputs' logical shapes, chooses donation from the actual buffers, compiles
//!   or reuses a cached executable, waits for all devices, and transfers
//!   ownership of output buffers.
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
mod donation;
mod error;
mod executable;
pub mod ffi;
mod layout;
mod memory;
mod transfer;

pub use buffer::Buffer;
use error::{Error, Result};
use reussir_pjrt_sys as sys;
