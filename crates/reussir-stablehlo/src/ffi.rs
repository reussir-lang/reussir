//! Declarations matching `native/reussir_stablehlo.h`.

use std::ffi::c_void;

// Keep mlir-sys' native link closure in the final executable.
use mlir_sys as _;

pub const OK: u32 = 0;
pub const INVALID_INPUT: u32 = 1;
pub const REFINEMENT_FAILED: u32 = 2;
pub const RESIDUAL_DYNAMISM: u32 = 3;

#[repr(C)]
pub struct Argument {
    pub dimensions: *const i64,
    pub rank: usize,
    pub values: *const i64,
    pub value_count: usize,
    pub has_values: u8,
}

pub type Bytes = unsafe extern "C" fn(*const u8, usize, *mut c_void);
pub type ResultType = unsafe extern "C" fn(*const i64, usize, u32, *mut c_void);

unsafe extern "C" {
    pub fn reussir_stablehlo_refine_shapes(
        program: *const u8,
        program_size: usize,
        arguments: *const Argument,
        argument_count: usize,
        refined: Bytes,
        result_type: ResultType,
        diagnostic: Bytes,
        user_data: *mut c_void,
    ) -> u32;
}
