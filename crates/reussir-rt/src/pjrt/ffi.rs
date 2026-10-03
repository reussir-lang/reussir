//! Compiler-facing PJRT ABI. Backend errors abort through the runtime panic path.
//!
//! Array handles are native `PJRT_Buffer*`: allocation/upload/copy return ownership;
//! deallocate consumes it; other calls borrow it. Release each owned handle once.
//! Devices index the process-lifetime client's addressable devices. The runtime
//! loads `REUSSIR_PJRT_PLUGIN` on first use. Host transfers block until return.
//!
//! Safety: handles must be live; calls must not race with destruction. Metadata
//! and its nonempty arrays must be valid, aligned and immutable for the call.
//! Host data must remain live and aligned, immutable during upload and exclusively
//! writable during download. Every address selected by strides must be in bounds.

use std::{ffi::c_void, slice, sync::Arc};

use super::{
    Buffer, Executable, Result,
    layout::{Allocation, Layout},
    sys,
};

/// Destination placement for allocation/upload. Null options use PJRT defaults.
/// Device layout is independent of the upload's host strides.
#[repr(C)]
#[derive(Default)]
pub struct AllocationOptions {
    /// Memory-kind bytes, not NUL-terminated; null selects device-default memory.
    pub memory_kind: *const u8,
    /// Byte count; ignored when `memory_kind` is null.
    pub memory_kind_size: usize,
    /// `rank` dimensions, fastest first; null uses PJRT defaults and ignores tiles.
    pub minor_to_major: *const i64,
    /// Concatenated tiles: `sum(tile_dim_sizes)` entries; null if empty.
    pub tile_dims: *const i64,
    /// `num_tiles` dimension counts; null if empty.
    pub tile_dim_sizes: *const usize,
    /// Zero selects an untiled layout.
    pub num_tiles: usize,
}

/// Download layout; use the same value for size query and copy.
/// Null inherits device layout. Row-major: order `[rank-1, ..., 0]`, no tiles.
/// Non-null strides require all tiled fields to be null/zero; otherwise supply
/// a dimension permutation. Rank zero permits null array pointers.
/// The pinned PJRT wrapper rejects strided/tiled downloads; errors propagate.
#[repr(C)]
#[derive(Default)]
pub struct HostLayout {
    /// Must match the buffer rank.
    pub rank: usize,
    /// `rank` signed byte strides, or null to select tiled form.
    pub byte_strides: *const i64,
    /// Tiled form: permutation of `rank` dimensions, fastest first.
    pub minor_to_major: *const i64,
    /// Tiled form: concatenated tiles, as in [`AllocationOptions`].
    pub tile_dims: *const i64,
    /// Tiled form: `num_tiles` dimension counts.
    pub tile_dim_sizes: *const usize,
    /// Tiled form: zero selects an untiled layout.
    pub num_tiles: usize,
}

impl HostLayout {
    unsafe fn layout(&self) -> Result<Layout<'_>> {
        if !self.byte_strides.is_null() {
            if !self.minor_to_major.is_null()
                || !self.tile_dims.is_null()
                || !self.tile_dim_sizes.is_null()
                || self.num_tiles != 0
            {
                return Err(super::Error::local(
                    "PjRt host layout mixes strides and tiles",
                ));
            }
            return Ok(Layout::Strides(unsafe {
                dimensions(self.byte_strides, self.rank)
            }));
        }
        unsafe {
            tiled_layout(
                self.minor_to_major,
                self.rank,
                self.tile_dims,
                self.tile_dim_sizes,
                self.num_tiles,
            )
        }
    }
}

// Shared by allocation/upload device layouts and download host layouts.
unsafe fn tiled_layout<'a>(
    order: *const i64,
    rank: usize,
    tiles: *const i64,
    tile_sizes: *const usize,
    num_tiles: usize,
) -> Result<Layout<'a>> {
    if rank != 0 && order.is_null() {
        return Err(super::Error::local(
            "PjRt layout requires a dimension order",
        ));
    }
    let sizes = if num_tiles == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(tile_sizes, num_tiles) }
    };
    let count = sizes
        .iter()
        .try_fold(0usize, |n, &size| n.checked_add(size))
        .ok_or_else(|| super::Error::local("PjRt tile dimension count overflow"))?;
    Layout::new(
        unsafe { dimensions(order, rank) },
        unsafe { dimensions(tiles, count) },
        sizes,
    )
}

unsafe fn host_layout<'a>(options: *const HostLayout) -> Result<Option<Layout<'a>>> {
    unsafe { options.as_ref() }
        .map(|options| unsafe { options.layout() })
        .transpose()
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
            Some(unsafe {
                tiled_layout(
                    self.minor_to_major,
                    rank,
                    self.tile_dims,
                    self.tile_dim_sizes,
                    self.num_tiles,
                )
            }?)
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

/// Verify an MLIR payload's BLAKE3 checksum and return its cached compilation.
/// `checksum` contains exactly 64 ASCII hex bytes (either case, no terminator).
/// `options` contains a serialized PJRT CompileOptionsProto, including device
/// assignment. The runtime borrows all input bytes only until this call returns.
/// Acquires ownership before returning an opaque runtime executable. Pass it once
/// to __reussir_pjrt_executable_execute, which consumes that reference. Failures
/// use the runtime panic path.
///
/// # Safety
/// All nonempty inputs must be readable and immutable for their stated lengths.
/// Empty bytecode/options slices permit null pointers; checksum must be readable
/// for 64 bytes. This function does not execute the compiled program.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_compile(
    bytecode: *const u8,
    bytecode_size: usize,
    checksum: *const u8,
    options: *const u8,
    options_size: usize,
) -> *const Executable {
    let bytecode = if bytecode_size == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(bytecode, bytecode_size) }
    };
    let options = if options_size == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(options, options_size) }
    };
    Arc::into_raw(checked(Executable::compile(
        bytecode,
        unsafe { slice::from_raw_parts(checksum, 64) },
        options,
    )))
}

/// Invoke with one descriptor pointer per logical input/output array.
/// Each pointer addresses the allocation-handle field of an MLIR target-array
/// descriptor (after the RC header): `PJRT_Buffer *[d]`, where `d` comes from the
/// loaded executable's addressable devices. Shards follow that device order.
/// The runtime packs these arrays into PJRT's device lists and waits for all
/// device completions, then drops the consumed executable reference. Inputs are
/// borrowed without donation. Each output shard is owned and is released by
/// the existing target-array drop lowering.
///
/// # Safety
/// `executable` must carry one owned reference returned by compile. This call
/// consumes it on success or failure; it must not be reused afterward. `inputs`
/// and `outputs` have `num_inputs` and `num_outputs` descriptor pointers, counting
/// logical arrays, not shards. The input arrays must contain `d` live buffer
/// handles from the runtime's client, matching the executable's input sharding.
/// Their owners must remain alive and must not donate/destroy the buffers during
/// this call. Each output descriptor must provide exclusive writable storage for
/// `d` handles, disjoint from other outputs and inputs; slots may be uninitialized.
/// The compiler initializes the remaining descriptor metadata (offset/extents).
/// Only complete-buffer views with zero offset are supported. Either outer list
/// may be null when its logical-array count is zero. Inputs may be reused or
/// released after return. Compile again to acquire a reference for another
/// invocation. Failures use the runtime panic path after releasing the reference.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_executable_execute(
    executable: *const Executable,
    inputs: *const *const c_void,
    num_inputs: usize,
    outputs: *const *mut c_void,
    num_outputs: usize,
) {
    // The ABI carries opaque array-descriptor pointers. Their first field is
    // the existing allocation-handle array, not a standalone PJRT_Buffer.
    let inputs = if num_inputs == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(inputs.cast::<*const *mut sys::PJRT_Buffer>(), num_inputs) }
    };
    let executable = unsafe { Arc::from_raw(executable) };
    // execute owns the reference and drops it before returning its Result,
    // including on failure, before checked enters the aborting panic path.
    let results = checked(unsafe { executable.execute(inputs, num_outputs) });
    for (array, shards) in results.iter().enumerate() {
        unsafe {
            std::ptr::copy_nonoverlapping(
                shards.as_ptr(),
                (*outputs.add(array)).cast::<*mut sys::PJRT_Buffer>(),
                shards.len(),
            )
        };
    }
}

// A rank-zero shape is allowed to have a null dimensions pointer.
unsafe fn dimensions<'a>(dims: *const i64, rank: usize) -> &'a [i64] {
    if rank == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(dims, rank) }
    }
}

/// Return an owned, uninitialized device buffer. `element_type` is PJRT_Buffer_Type;
/// `dims` has `rank` nonnegative extents (null allowed for rank zero).
/// Null `options` uses PJRT defaults. Initialize contents before reading/copying.
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

/// Consume an owned handle. No copy of the handle may be used afterward.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_deallocate(buffer: Buffer) {
    checked(unsafe { buffer.deallocate() });
}

/// Return an owned buffer after PJRT finishes borrowing initialized host data.
/// `element_type` is PJRT_Buffer_Type; `dims` has `rank` nonnegative extents
/// (null allowed for rank zero). `byte_strides` has `rank` signed **byte** strides;
/// null selects row-major. Non-null `data` points to the logical first element,
/// including any view offset. `options` selects destination memory/layout.
/// Host storage is reusable on return; device readiness is a separate event.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_from_host(
    device: usize,
    element_type: u32,
    dims: *const i64,
    rank: usize,
    data: *const c_void,
    byte_strides: *const i64,
    options: *const AllocationOptions,
) -> Buffer {
    let allocation = match unsafe { options.as_ref() } {
        Some(options) => checked(unsafe { options.allocation(rank) }),
        None => Allocation::default(),
    };
    let strides = if byte_strides.is_null() {
        None
    } else {
        Some(unsafe { dimensions(byte_strides, rank) })
    };
    checked(unsafe {
        Buffer::from_host(
            device,
            element_type,
            dimensions(dims, rank),
            data,
            strides,
            allocation,
        )
    })
}

/// Download initialized device contents and wait for completion; borrows `buffer`.
/// Use `host_size(buffer, layout)` to size storage for this same layout.
/// Null layout inherits device layout; pass an explicit order for row-major output.
/// `data` addresses the logical first element and must be non-null even if empty.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_to_host(
    buffer: Buffer,
    data: *mut c_void,
    bytes: usize,
    layout: *const HostLayout,
) {
    let layout = checked(unsafe { host_layout(layout) });
    checked(unsafe { buffer.to_host(data, bytes, layout.as_ref()) });
}

/// Enqueue a device copy and return an owned handle; borrows initialized `buffer`.
/// Does not wait for completion. The backend tracks pending storage use.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_copy_to_device(
    buffer: Buffer,
    device: usize,
) -> Buffer {
    checked(unsafe { buffer.copy_to_device(device) })
}

/// Wait for pending work and report errors; borrows `buffer`.
/// Waiting does not initialize an uninitialized allocation.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_wait_ready(buffer: Buffer) {
    checked(unsafe { buffer.wait_ready() });
}

/// Required host bytes for `to_host` with the same buffer and layout; borrows both.
/// Null layout inherits device layout. Unsupported layouts report backend errors.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_pjrt_array_host_size(
    buffer: Buffer,
    layout: *const HostLayout,
) -> usize {
    let layout = checked(unsafe { host_layout(layout) });
    checked(unsafe { buffer.host_size(layout.as_ref()) })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn host_layout_rejects_ambiguous_or_missing_metadata() {
        let order = [1, 0];
        let strides = [12, -4];
        let mut layout = HostLayout {
            rank: 2,
            byte_strides: strides.as_ptr(),
            minor_to_major: order.as_ptr(),
            ..Default::default()
        };
        unsafe {
            assert!(layout.layout().is_err());
            layout.minor_to_major = std::ptr::null();
            assert!(matches!(layout.layout().unwrap(), Layout::Strides(s) if s == strides));
            layout.byte_strides = std::ptr::null();
            assert!(layout.layout().is_err());
            layout.rank = 0;
            assert!(layout.layout().is_ok());
            assert!(host_layout(std::ptr::null()).unwrap().is_none());
        }
    }

    #[test]
    fn host_layout_preserves_tiles_and_checks_overflow() {
        let order = [0, 1];
        let tiles = [8, 128, 2, 1];
        let sizes = [2, 2];
        let mut layout = HostLayout {
            rank: 2,
            minor_to_major: order.as_ptr(),
            tile_dims: tiles.as_ptr(),
            tile_dim_sizes: sizes.as_ptr(),
            num_tiles: 2,
            ..Default::default()
        };
        unsafe {
            let raw = layout.layout().unwrap().as_pjrt();
            let tiled = raw.__bindgen_anon_1.tiled;
            assert_eq!(tiled.minor_to_major_size, 2);
            assert_eq!(tiled.tile_dims, tiles.as_ptr());
            assert_eq!(tiled.tile_dim_sizes, sizes.as_ptr());
            assert_eq!(tiled.num_tiles, 2);
            let overflow = [usize::MAX, 1];
            layout.tile_dim_sizes = overflow.as_ptr();
            assert!(layout.layout().is_err());
        }
    }
}
