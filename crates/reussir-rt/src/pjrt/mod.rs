//! PjRt context and single-device buffer ownership (`pjrt` Cargo feature).
//!
//! Build with `REUSSIR_PJRT_PATH` or `REUSSIR_PJRT_C_API_HEADER` pointing to
//! upstream headers. The runtime loads `REUSSIR_PJRT_PLUGIN` on first use and
//! owns the client for the process lifetime. Transfers block until the caller
//! can reuse its host memory. Shapes use dense major-to-minor order and
//! device indices refer to the client's addressable devices.

mod api;
mod buffer;
mod context;
mod error;
pub mod ffi;

use error::{Error, Result};
use reussir_pjrt_sys as sys;
