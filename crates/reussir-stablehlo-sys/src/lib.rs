//! Raw synchronous ABI. All nonempty pointers must be valid for their lengths;
//! callbacks must not unwind, and native code retains no caller memory.
use std::ffi::c_void;
// Keep mlir-sys' native link closure in the final executable.
use mlir_sys as _;
#[repr(C)]
pub struct Argument {
    pub dimensions: *const i64,
    pub rank: usize,
    pub values: *const i64,
    pub value_count: usize,
    pub has_values: u8,
}
pub type Bytes = unsafe extern "C" fn(*const u8, usize, *mut c_void);
pub type Shape = unsafe extern "C" fn(*const i64, usize, u64, *mut c_void);
unsafe extern "C" {
    pub fn reussir_stablehlo_specialize(
        program: *const u8,
        program_size: usize,
        arguments: *const Argument,
        argument_count: usize,
        output: Bytes,
        output_shape: Shape,
        diagnostic: Bytes,
        user_data: *mut c_void,
    ) -> u32;
    pub fn reussir_stablehlo_select(
        program: *const u8,
        program_size: usize,
        mapping: *const u32,
        argument_count: usize,
        donors: *const u32,
        donor_count: usize,
        aliases: *const u32,
        alias_count: usize,
        output: Bytes,
        diagnostic: Bytes,
        user_data: *mut c_void,
    ) -> u32;
}
