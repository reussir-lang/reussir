//! PjRt context and single-device buffer ownership (`pjrt` Cargo feature).
//!
//! Build with `REUSSIR_PJRT_PATH` or `REUSSIR_PJRT_C_API_HEADER` pointing to
//! upstream headers. The runtime loads `REUSSIR_PJRT_PLUGIN` on first use and
//! owns the client for the process lifetime. Transfers block until the caller
//! can reuse its host memory. Uploads accept signed host byte strides and device memory/layout metadata;
//! downloads and their size queries accept an explicit host layout. Device
//! indices refer to the client's addressable devices. Multi-device arrays keep
//! these native buffer handles in the compiler's RC descriptor.

mod api;
mod buffer;
mod client;
mod context;
mod error;
pub mod ffi;
mod layout;
mod memory;

pub use buffer::Buffer;
use error::{Error, Result};
use reussir_pjrt_sys as sys;
