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
//! element types are PJRT_Buffer_Type values. Shapes use logical dimension order
//! with nonnegative extents; host data uses dense major-to-minor order. Allocation
//! accepts optional memory-kind and tiled-layout metadata; upload uses
//! device-default memory.
//! Dimensions may be null only for rank-zero shapes.
//!
//! The runtime loads `REUSSIR_PJRT_PLUGIN` once on first use. Allocation leaves
//! data uninitialized. Upload requires aligned, initialized host storage of the
//! declared shape/type, immutable until return. Download requires initialized
//! device contents and aligned, exclusively writable host storage of at least
//! array_host_size bytes. Host storage can be reused once a transfer returns.

use std::{ffi::c_void, slice};

use super::{
    Buffer, Result,
    layout::{Allocation, Layout},
};

/// Optional allocation metadata borrowed for the duration of the call.
/// A null memory_kind selects device-default memory. A null minor_to_major
/// selects the backend's default layout; otherwise it points to rank indices.
/// tile_dim_sizes has num_tiles entries; tile_dims concatenates those tiles.
#[repr(C)]
#[derive(Default)]
pub struct AllocationOptions {
    pub memory_kind: *const u8,
    pub memory_kind_size: usize,
    pub minor_to_major: *const i64,
    pub tile_dims: *const i64,
    pub tile_dim_sizes: *const usize,
    pub num_tiles: usize,
}

impl AllocationOptions {
    // The compiler supplies valid, live arrays with the documented lengths.
    unsafe fn allocation(&self, rank: usize) -> Result<Allocation<'_>> {
        let memory_kind = if self.memory_kind.is_null() {
            None
        } else {
            Some(unsafe { slice::from_raw_parts(self.memory_kind, self.memory_kind_size) })
        };
        let layout = if self.minor_to_major.is_null() {
            None
        } else {
            let sizes = if self.num_tiles == 0 {
                &[]
            } else {
                unsafe { slice::from_raw_parts(self.tile_dim_sizes, self.num_tiles) }
            };
            let count = sizes
                .iter()
                .try_fold(0usize, |n, &size| n.checked_add(size))
                .ok_or_else(|| super::Error::local("PjRt tile dimension count overflow"))?;
            let tiles = unsafe { dimensions(self.tile_dims, count) };
            Some(Layout::new(
                unsafe { dimensions(self.minor_to_major, rank) },
                tiles,
                sizes,
            )?)
        };
        Ok(Allocation {
            memory_kind,
            layout,
        })
    }
}

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
    options: *const AllocationOptions,
) -> Buffer {
    let allocation = match unsafe { options.as_ref() } {
        Some(options) => checked(unsafe { options.allocation(rank) }),
        None => Allocation::default(),
    };
    checked(Buffer::allocate(
        device,
        element_type,
        unsafe { dimensions(dims, rank) },
        allocation,
    ))
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
