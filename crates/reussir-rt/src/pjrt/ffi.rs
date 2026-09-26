//! Compiler-facing ABI. Errors use the runtime's aborting panic path.
//!
//! Array handles are native `PJRT_Buffer*` pointers, represented by transparent
//! `Buffer` handles.
//! There is no Reussir allocation or context pointer attached to a handle.
//! Allocation, upload and copy transfer ownership; deallocate consumes it.
//! All other operations borrow the handle. Release each owned handle once.
//! Calls and destruction of the same array must not race. The runtime owns
//! the context; generated code never carries or manages a context handle.
//! Device indices select addressable devices;
//! element types are PJRT_Buffer_Type values. Shapes and host data are dense
//! major-to-minor arrays in default device memory, with nonnegative dimensions.
//! Dimensions may be null only for rank-zero shapes.
//!
//! The runtime loads `REUSSIR_PJRT_PLUGIN` once on first use. Allocation leaves
//! data uninitialized. Upload requires aligned, initialized host storage of the
//! declared shape/type, immutable until return. Download requires initialized
//! device contents and aligned, exclusively writable host storage of at least
//! array_host_size bytes. Host storage can be reused once a transfer returns.

use std::{ffi::c_void, slice};

use super::{Buffer, Result};

fn checked<T>(result: Result<T>) -> T {
    result.unwrap_or_else(|error| unsafe {
        crate::panic::panic!("PjRt: {}", error);
    })
}

// A rank-zero shape is allowed to have a null dimensions pointer.
unsafe fn dimensions<'a>(dims: *const i64, rank: usize) -> &'a [i64] {
    if rank == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(dims, rank) }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_allocate(
    device: usize,
    element_type: u32,
    dims: *const i64,
    rank: usize,
) -> Buffer {
    checked(Buffer::allocate(device, element_type, unsafe {
        dimensions(dims, rank)
    }))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_deallocate(buffer: Buffer) {
    checked(unsafe { buffer.deallocate() });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_from_host(
    device: usize,
    element_type: u32,
    dims: *const i64,
    rank: usize,
    data: *const c_void,
) -> Buffer {
    checked(unsafe { Buffer::from_host(device, element_type, dimensions(dims, rank), data) })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_to_host(
    buffer: Buffer,
    data: *mut c_void,
    bytes: usize,
) {
    checked(unsafe { buffer.to_host(data, bytes) });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_copy_to_device(
    buffer: Buffer,
    device: usize,
) -> Buffer {
    checked(unsafe { buffer.copy_to_device(device) })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_wait_ready(buffer: Buffer) {
    checked(unsafe { buffer.wait_ready() });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_host_size(buffer: Buffer) -> usize {
    checked(unsafe { buffer.host_size() })
}
