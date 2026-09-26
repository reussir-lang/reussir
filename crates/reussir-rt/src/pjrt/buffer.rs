//! Operations on native PjRt buffer handles. Ownership crosses the FFI directly:
//! allocation/upload/copy return one owned handle, and deallocate consumes it.
//! All handles belong to the runtime's process-lifetime client.

use std::{ffi::c_void, ptr::NonNull};

use super::{
    Result,
    api::{call, non_null},
    context,
    sys::*,
};

/// Allocate uninitialized storage in the device's default memory. Backends
/// without this operation return their `UNIMPLEMENTED` status.
pub(super) fn allocate(
    device: usize,
    element_type: PJRT_Buffer_Type,
    dims: &[i64],
) -> Result<NonNull<PJRT_Buffer>> {
    let context = context::get()?;
    let args = call!(
        context.api,
        PJRT_Client_CreateUninitializedBuffer {
            client: context.client.as_ptr(),
            device: context.device(device)?.as_ptr(),
            shape_element_type: element_type,
            shape_dims: dims.as_ptr(),
            shape_num_dims: dims.len(),
        }
    )?;
    let buffer = non_null(args.buffer, "buffer")?;
    tracing::debug!(
        ?buffer,
        device,
        element_type,
        ?dims,
        "allocated PjRt buffer"
    );
    Ok(buffer)
}

/// Upload dense host data. The allocation described by `element_type`/`dims`
/// must be aligned, initialized and immutable until this returns.
pub(super) unsafe fn from_host(
    device: usize,
    element_type: PJRT_Buffer_Type,
    dims: &[i64],
    data: *const c_void,
) -> Result<NonNull<PJRT_Buffer>> {
    let context = context::get()?;
    let host_buffer_semantics =
        PJRT_HostBufferSemantics_PJRT_HostBufferSemantics_kImmutableUntilTransferCompletes;
    let args = call!(
        context.api,
        PJRT_Client_BufferFromHostBuffer {
            client: context.client.as_ptr(),
            device: context.device(device)?.as_ptr(),
            type_: element_type,
            dims: dims.as_ptr(),
            num_dims: dims.len(),
            data,
            host_buffer_semantics,
        }
    )?;
    // Wait before returning, including when the plugin returned an invalid
    // buffer, so the caller can immediately reuse its host allocation.
    let ready = context.api.wait(args.done_with_host_buffer);
    let buffer = non_null(args.buffer, "buffer")?;
    if let Err(error) = ready {
        // Ownership has not reached the caller; release the failed upload.
        if let Err(cleanup) = unsafe { deallocate(buffer) } {
            tracing::warn!(?buffer, %cleanup, "PjRt failed-upload cleanup failed");
        }
        return Err(error);
    }
    tracing::debug!(?buffer, device, element_type, ?dims, "uploaded PjRt buffer");
    Ok(buffer)
}

/// Consume a live owned handle. No use of the handle may race with destruction.
pub(super) unsafe fn deallocate(buffer: NonNull<PJRT_Buffer>) -> Result<()> {
    let context = context::get()?;
    call!(
        context.api,
        PJRT_Buffer_Destroy {
            buffer: buffer.as_ptr()
        }
    )?;
    tracing::debug!(?buffer, "released PjRt buffer");
    Ok(())
}

/// Borrow a live handle and wait for allocation/transfer completion.
pub(super) unsafe fn wait_ready(buffer: NonNull<PJRT_Buffer>) -> Result<()> {
    let context = context::get()?;
    let args = call!(
        context.api,
        PJRT_Buffer_ReadyEvent {
            buffer: buffer.as_ptr()
        }
    )?;
    context.api.wait(args.event)
}

/// Borrow a live handle and query the required host storage size.
pub(super) unsafe fn host_size(buffer: NonNull<PJRT_Buffer>) -> Result<usize> {
    let context = context::get()?;
    let args = call!(
        context.api,
        PJRT_Buffer_ToHostBuffer {
            src: buffer.as_ptr()
        }
    )?;
    if !args.event.is_null() {
        context.api.wait(args.event)?;
    }
    Ok(args.dst_size)
}

/// Borrow a live initialized buffer and enqueue a copy to an addressable device.
pub(super) unsafe fn copy_to_device(
    buffer: NonNull<PJRT_Buffer>,
    device: usize,
) -> Result<NonNull<PJRT_Buffer>> {
    let context = context::get()?;
    let args = call!(
        context.api,
        PJRT_Buffer_CopyToDevice {
            buffer: buffer.as_ptr(),
            dst_device: context.device(device)?.as_ptr(),
        }
    )?;
    let copy = non_null(args.dst_buffer, "copied buffer")?;
    tracing::debug!(source = ?buffer, destination = ?copy, device, "enqueued PjRt device copy");
    Ok(copy)
}

/// Borrow a live initialized buffer and download it. `dst` must be non-null,
/// aligned and exclusively writable for `bytes` bytes until this returns.
/// The plugin checks that `bytes` is sufficient for the buffer's layout.
pub(super) unsafe fn to_host(
    buffer: NonNull<PJRT_Buffer>,
    dst: *mut c_void,
    bytes: usize,
) -> Result<()> {
    let context = context::get()?;
    let args = call!(
        context.api,
        PJRT_Buffer_ToHostBuffer {
            src: buffer.as_ptr(),
            dst,
            dst_size: bytes,
        }
    )?;
    context.api.wait(args.event)?;
    tracing::debug!(?buffer, bytes, "downloaded PjRt buffer");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
    fn allocation_and_transfer_errors() {
        match allocate(0, PJRT_Buffer_Type_PJRT_Buffer_Type_F32, &[2, 4]) {
            Ok(buffer) => unsafe {
                wait_ready(buffer).unwrap();
                assert_eq!(host_size(buffer).unwrap(), 32);
                deallocate(buffer).unwrap();
            },
            Err(error) => assert_eq!(
                error.code,
                Some(PJRT_Error_Code_PJRT_Error_Code_UNIMPLEMENTED)
            ),
        }
        assert!(allocate(usize::MAX, PJRT_Buffer_Type_PJRT_Buffer_Type_F32, &[1]).is_err());
        let value = 3.0f32;
        // An upstream error leaves the same buffer usable for a valid transfer.
        unsafe {
            let buffer = from_host(
                0,
                PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
                &[],
                (&value as *const f32).cast(),
            )
            .unwrap();
            let mut output = 0.0f32;
            let error = to_host(buffer, (&mut output as *mut f32).cast(), 1).unwrap_err();
            assert_eq!(
                error.code,
                Some(PJRT_Error_Code_PJRT_Error_Code_INVALID_ARGUMENT)
            );
            to_host(
                buffer,
                (&mut output as *mut f32).cast(),
                size_of_val(&output),
            )
            .unwrap();
            assert_eq!(output, value);
            deallocate(buffer).unwrap();
        }
    }
}
