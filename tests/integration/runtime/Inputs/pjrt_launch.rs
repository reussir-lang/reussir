//! Launches kernel bytecode through the runtime's `__reussir_pjrt_launch`,
//! without donation, for the tests that drive compiled kernels directly.
//! Include it with `#[path = ".../pjrt_launch.rs"] mod pjrt_launch;`.

use std::{ffi::c_void, ptr};

#[repr(C)]
struct ShapeArgument {
    dimensions: *const i64,
    rank: usize,
}

#[repr(C)]
struct LaunchRequest {
    bytecode: *const u8,
    bytecode_size: usize,
    checksum: *const u8,
    options: *const u8,
    options_size: usize,
    arguments: *const ShapeArgument,
    input_count: usize,
    inputs: *const *const *mut c_void,
    devices: *const i32,
    device_count: usize,
    permissions: *const u8,
    mapping: *const u32,
    donors: *const u32,
    donor_count: usize,
    aliases: *const [u32; 2],
    alias_count: usize,
    outputs: *const *mut *mut c_void,
    output_count: usize,
}

unsafe extern "C" {
    fn __reussir_pjrt_launch(request: *const LaunchRequest);
}

/// Compile options for a single-device launch.
pub const OPTIONS: &[u8] =
    include_bytes!("../../../../crates/reussir-pjrt-sys/tests/fixtures/compile_options.pb");

/// One launch input: a buffer per launch device and its logical dimensions.
pub struct Input<'a> {
    pub shards: &'a [*mut c_void],
    pub dimensions: &'a [i64],
}

/// Launch `bytecode` on `devices` and return the owned shards of each of its
/// `outputs` results, in device order.
///
/// # Safety
/// Every shard must be a live buffer on its device; shards stay owned by the
/// caller.
pub unsafe fn launch(
    bytecode: &[u8],
    options: &[u8],
    devices: &[i32],
    inputs: &[Input],
    outputs: usize,
) -> Vec<Vec<*mut c_void>> {
    let checksum = blake3::hash(bytecode).to_hex();
    let shapes: Vec<_> = inputs
        .iter()
        .map(|input| ShapeArgument {
            dimensions: input.dimensions.as_ptr(),
            rank: input.dimensions.len(),
        })
        .collect();
    let descriptors: Vec<_> = inputs.iter().map(|input| input.shards.as_ptr()).collect();
    let permissions = vec![0u8; inputs.len()];
    let mapping: Vec<u32> = (0..inputs.len() as u32).collect();
    let mut results = vec![vec![ptr::null_mut(); devices.len()]; outputs];
    let slots: Vec<_> = results.iter_mut().map(|shards| shards.as_mut_ptr()).collect();
    let request = LaunchRequest {
        bytecode: bytecode.as_ptr(),
        bytecode_size: bytecode.len(),
        checksum: checksum.as_ptr(),
        options: options.as_ptr(),
        options_size: options.len(),
        arguments: shapes.as_ptr(),
        input_count: inputs.len(),
        inputs: descriptors.as_ptr(),
        devices: devices.as_ptr(),
        device_count: devices.len(),
        permissions: permissions.as_ptr(),
        mapping: mapping.as_ptr(),
        donors: ptr::null(),
        donor_count: 0,
        aliases: ptr::null(),
        alias_count: 0,
        outputs: slots.as_ptr(),
        output_count: outputs,
    };
    unsafe { __reussir_pjrt_launch(&request) };
    results
}
