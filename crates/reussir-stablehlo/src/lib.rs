//! Runtime transformations of StableHLO programs, implemented with the pinned
//! upstream StableHLO passes in a native library built by CMake.
//!
//! Each call parses its program into an independent MLIR context. Building
//! the crate requires an OpenXLA-enabled CMake build: set
//! `REUSSIR_STABLEHLO_LIB_DIR` to its `lib` directory and `MLIR_SYS_230_PREFIX`
//! to the LLVM/MLIR prefix CMake used.

mod donation;
mod ffi;
mod refine;

pub use donation::{Donation, apply_donation};
pub use refine::{Argument, Refined, ResultType, refine_shapes};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidInput,
    RefinementFailed,
    ResidualDynamism,
    SerializationFailed,
}

impl ErrorKind {
    fn from_status(status: u32) -> Option<Self> {
        Some(match status {
            ffi::OK => return None,
            ffi::INVALID_INPUT => Self::InvalidInput,
            ffi::REFINEMENT_FAILED => Self::RefinementFailed,
            ffi::RESIDUAL_DYNAMISM => Self::ResidualDynamism,
            _ => Self::SerializationFailed,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("StableHLO {kind:?}: {diagnostic}")]
pub struct Error {
    pub kind: ErrorKind,
    pub diagnostic: String,
}

/// What one native call passes to its callbacks.
#[derive(Default)]
struct Output {
    bytes: Vec<u8>,
    result_types: Vec<ResultType>,
    diagnostic: Vec<u8>,
}

impl Output {
    fn user_data(&mut self) -> *mut std::ffi::c_void {
        (self as *mut Self).cast()
    }

    fn finish(self, status: u32) -> Result<Self, Error> {
        match ErrorKind::from_status(status) {
            None => Ok(self),
            Some(kind) => Err(Error {
                kind,
                diagnostic: String::from_utf8_lossy(&self.diagnostic).into_owned(),
            }),
        }
    }
}

/// # Safety
/// `data` is valid for `len` bytes and `output` is an [`Output::user_data`].
unsafe extern "C" fn collect_bytes(data: *const u8, len: usize, output: *mut std::ffi::c_void) {
    if len != 0 {
        let output = unsafe { &mut *output.cast::<Output>() };
        output
            .bytes
            .extend_from_slice(unsafe { std::slice::from_raw_parts(data, len) });
    }
}

/// # Safety
/// As for [`collect_bytes`].
unsafe extern "C" fn collect_diagnostic(
    data: *const u8,
    len: usize,
    output: *mut std::ffi::c_void,
) {
    if len != 0 {
        let output = unsafe { &mut *output.cast::<Output>() };
        output
            .diagnostic
            .extend_from_slice(unsafe { std::slice::from_raw_parts(data, len) });
    }
}
