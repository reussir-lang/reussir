//! Operations on native PjRt buffer handles. Ownership crosses the FFI directly:
//! allocation/upload/copy return one owned handle, and deallocate consumes it.
//! All handles belong to the runtime's process-lifetime client.

use std::{ffi::c_void, ptr::NonNull};

use super::{
    Result,
    api::{Event, call, non_null},
    context,
    layout::Allocation,
    sys::*,
};

/// A pointer-sized PjRt buffer handle. Copying the handle does not retain the
/// buffer; the owner must explicitly deallocate it once, after all uses.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct Buffer(NonNull<PJRT_Buffer>);

impl Buffer {
    pub fn as_ptr(self) -> *mut PJRT_Buffer {
        self.0.as_ptr()
    }

    /// Allocate uninitialized storage with the requested memory and layout. Backends
    /// without this operation return their `UNIMPLEMENTED` status.
    pub(super) fn allocate(
        device: usize,
        element_type: PJRT_Buffer_Type,
        dims: &[i64],
        allocation: Allocation<'_>,
    ) -> Result<Self> {
        let context = context::get()?;
        let device_handle = context.device(device)?;
        let memory = allocation
            .memory_kind
            .map(|kind| device_handle.memory(context.api, kind))
            .transpose()?;
        let mut layout = allocation.layout.as_ref().map(|layout| layout.as_pjrt());
        let args = call!(
            context.api,
            PJRT_Client_CreateUninitializedBuffer {
                client: context.client.as_ptr(),
                // The upstream wrapper prefers device-default memory whenever
                // device is non-null, even if memory is also supplied.
                device: if memory.is_some() {
                    std::ptr::null_mut()
                } else {
                    device_handle.as_ptr()
                },
                memory: memory.map_or(std::ptr::null_mut(), |memory| memory.as_ptr()),
                shape_layout: layout
                    .as_mut()
                    .map_or(std::ptr::null_mut(), std::ptr::from_mut),
                shape_element_type: element_type,
                shape_dims: dims.as_ptr(),
                shape_num_dims: dims.len(),
            }
        )?;
        let buffer = Self(non_null(args.buffer, "buffer")?);
        tracing::debug!(
            ?buffer,
            device,
            element_type,
            ?dims,
            ?memory,
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
    ) -> Result<Self> {
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
        let ready =
            Event::new(args.done_with_host_buffer).and_then(|event| event.wait(context.api));
        let buffer = Self(non_null(args.buffer, "buffer")?);
        if let Err(error) = ready {
            // Ownership has not reached the caller; release the failed upload.
            if let Err(cleanup) = unsafe { buffer.deallocate() } {
                tracing::warn!(?buffer, %cleanup, "PjRt failed-upload cleanup failed");
            }
            return Err(error);
        }
        tracing::debug!(?buffer, device, element_type, ?dims, "uploaded PjRt buffer");
        Ok(buffer)
    }

    /// Consume a live owned handle. No use of the handle may race with destruction.
    pub(super) unsafe fn deallocate(self) -> Result<()> {
        let context = context::get()?;
        call!(
            context.api,
            PJRT_Buffer_Destroy {
                buffer: self.as_ptr()
            }
        )?;
        tracing::debug!(buffer = ?self, "released PjRt buffer");
        Ok(())
    }

    /// Borrow a live handle and wait for allocation/transfer completion.
    pub(super) unsafe fn wait_ready(self) -> Result<()> {
        let context = context::get()?;
        let args = call!(
            context.api,
            PJRT_Buffer_ReadyEvent {
                buffer: self.as_ptr()
            }
        )?;
        Event::new(args.event)?.wait(context.api)
    }

    /// Borrow a live handle and query the required host storage size.
    pub(super) unsafe fn host_size(self) -> Result<usize> {
        let context = context::get()?;
        let args = call!(context.api, PJRT_Buffer_ToHostBuffer { src: self.as_ptr() })?;
        if !args.event.is_null() {
            Event::new(args.event)?.wait(context.api)?;
        }
        Ok(args.dst_size)
    }

    /// Borrow a live initialized buffer and enqueue a copy to an addressable device.
    pub(super) unsafe fn copy_to_device(self, device: usize) -> Result<Self> {
        let context = context::get()?;
        let args = call!(
            context.api,
            PJRT_Buffer_CopyToDevice {
                buffer: self.as_ptr(),
                dst_device: context.device(device)?.as_ptr(),
            }
        )?;
        let copy = Self(non_null(args.dst_buffer, "copied buffer")?);
        tracing::debug!(source = ?self, destination = ?copy, device, "enqueued PjRt device copy");
        Ok(copy)
    }

    /// Borrow a live initialized buffer and download it. `dst` must be non-null,
    /// aligned and exclusively writable for `bytes` bytes until this returns.
    /// The plugin checks that `bytes` is sufficient for the buffer's layout.
    pub(super) unsafe fn to_host(self, dst: *mut c_void, bytes: usize) -> Result<()> {
        let context = context::get()?;
        let args = call!(
            context.api,
            PJRT_Buffer_ToHostBuffer {
                src: self.as_ptr(),
                dst,
                dst_size: bytes,
            }
        )?;
        Event::new(args.event)?.wait(context.api)?;
        tracing::debug!(buffer = ?self, bytes, "downloaded PjRt buffer");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
    fn allocation_and_transfer_errors() {
        for allocation in [
            Allocation::default(),
            Allocation {
                memory_kind: Some(b"device"),
                layout: Some(super::super::layout::Layout::new(&[0, 1], &[], &[]).unwrap()),
            },
        ] {
            match Buffer::allocate(
                0,
                PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
                &[2, 4],
                allocation,
            ) {
                Ok(buffer) => unsafe {
                    buffer.wait_ready().unwrap();
                    assert_eq!(buffer.host_size().unwrap(), 32);
                    buffer.deallocate().unwrap();
                },
                Err(error) => assert_eq!(
                    error.code,
                    Some(PJRT_Error_Code_PJRT_Error_Code_UNIMPLEMENTED)
                ),
            }
        }
        let missing_memory = Buffer::allocate(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
            &[2, 4],
            Allocation {
                memory_kind: Some(b"missing-memory-kind"),
                ..Default::default()
            },
        )
        .unwrap_err();
        assert!(missing_memory.message.contains("no memory kind"));
        assert!(
            Buffer::allocate(
                usize::MAX,
                PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
                &[1],
                Allocation::default()
            )
            .is_err()
        );
        let value = 3.0f32;
        // An upstream error leaves the same buffer usable for a valid transfer.
        unsafe {
            let buffer = Buffer::from_host(
                0,
                PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
                &[],
                (&value as *const f32).cast(),
            )
            .unwrap();
            let mut output = 0.0f32;
            let error = buffer
                .to_host((&mut output as *mut f32).cast(), 1)
                .unwrap_err();
            assert_eq!(
                error.code,
                Some(PJRT_Error_Code_PJRT_Error_Code_INVALID_ARGUMENT)
            );
            buffer
                .to_host((&mut output as *mut f32).cast(), size_of_val(&output))
                .unwrap();
            assert_eq!(output, value);
            buffer.deallocate().unwrap();
        }
    }
}
