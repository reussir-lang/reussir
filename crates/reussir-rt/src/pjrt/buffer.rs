//! Operations on native PjRt buffer handles. Ownership crosses the FFI directly:
//! allocation/upload/copy return one owned handle, and deallocate consumes it.
//! All handles belong to the runtime's process-lifetime client.

use std::{ffi::c_void, ptr::NonNull};

use super::{
    Result,
    api::{Api, Event, call, non_null},
    context,
    layout::{Allocation, Layout},
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

    /// Copy actual extents into `rank` writable slots while borrowing the buffer.
    pub(super) unsafe fn dimensions(self, api: Api, dims: *mut i64, rank: usize) -> Result<()> {
        let args = call!(
            api,
            PJRT_Buffer_UnpaddedDimensions {
                buffer: self.as_ptr()
            }
        )?;
        if args.num_dims != rank {
            return Err(super::Error::local(
                "PjRt buffer rank does not match array rank",
            ));
        }
        if rank == 0 {
            return Ok(());
        }
        non_null(args.unpadded_dims.cast_mut(), "buffer dimensions")?;
        let actual = unsafe { std::slice::from_raw_parts(args.unpadded_dims, rank) };
        if actual.iter().any(|&dim| dim < 0) {
            return Err(super::Error::local(
                "PjRt buffer extent must be nonnegative",
            ));
        }
        unsafe { std::ptr::copy_nonoverlapping(actual.as_ptr(), dims, rank) };
        Ok(())
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

    /// Upload host data with optional signed byte strides and device placement.
    /// All elements addressed by `data`/`dims`/`byte_strides` must be aligned,
    /// initialized and immutable until this returns.
    pub(super) unsafe fn from_host(
        device: usize,
        element_type: PJRT_Buffer_Type,
        dims: &[i64],
        data: *const c_void,
        byte_strides: Option<&[i64]>,
        allocation: Allocation<'_>,
    ) -> Result<Self> {
        if byte_strides.is_some_and(|strides| strides.len() != dims.len()) {
            return Err(super::Error::local("PjRt host strides must match the rank"));
        }
        let context = context::get()?;
        let device_handle = context.device(device)?;
        let memory = allocation
            .memory_kind
            .map(|kind| device_handle.memory(context.api, kind))
            .transpose()?;
        let mut layout = allocation.layout.as_ref().map(Layout::as_pjrt);
        let host_buffer_semantics =
            PJRT_HostBufferSemantics_PJRT_HostBufferSemantics_kImmutableUntilTransferCompletes;
        let args = call!(
            context.api,
            PJRT_Client_BufferFromHostBuffer {
                client: context.client.as_ptr(),
                device: if memory.is_some() {
                    std::ptr::null_mut()
                } else {
                    device_handle.as_ptr()
                },
                memory: memory.map_or(std::ptr::null_mut(), |memory| memory.as_ptr()),
                device_layout: layout
                    .as_mut()
                    .map_or(std::ptr::null_mut(), std::ptr::from_mut),
                byte_strides: byte_strides.map_or(std::ptr::null(), |strides| strides.as_ptr()),
                num_byte_strides: byte_strides.map_or(0, |strides| strides.len()),
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
    pub(super) unsafe fn host_size(self, layout: Option<&Layout<'_>>) -> Result<usize> {
        let context = context::get()?;
        let mut layout = layout.map(Layout::as_pjrt);
        let args = call!(
            context.api,
            PJRT_Buffer_ToHostBuffer {
                src: self.as_ptr(),
                host_layout: layout
                    .as_mut()
                    .map_or(std::ptr::null_mut(), std::ptr::from_mut),
            }
        )?;
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
    /// aligned and exclusively writable at every address in the host layout
    /// until this returns. The plugin checks `bytes` against that same layout.
    pub(super) unsafe fn to_host(
        self,
        dst: *mut c_void,
        bytes: usize,
        layout: Option<&Layout<'_>>,
    ) -> Result<()> {
        let context = context::get()?;
        let mut layout = layout.map(Layout::as_pjrt);
        let args = call!(
            context.api,
            PJRT_Buffer_ToHostBuffer {
                src: self.as_ptr(),
                dst,
                dst_size: bytes,
                host_layout: layout
                    .as_mut()
                    .map_or(std::ptr::null_mut(), std::ptr::from_mut),
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

    struct Dimensions {
        values: &'static [i64],
        null: bool,
    }

    unsafe extern "C" fn unpadded_dimensions(
        args: *mut PJRT_Buffer_UnpaddedDimensions_Args,
    ) -> *mut PJRT_Error {
        let args = unsafe { &mut *args };
        let dimensions = unsafe { &*args.buffer.cast::<Dimensions>() };
        args.num_dims = dimensions.values.len();
        args.unpadded_dims = if dimensions.null {
            std::ptr::null()
        } else {
            dimensions.values.as_ptr()
        };
        std::ptr::null_mut()
    }

    fn query_dimensions(dimensions: &mut Dimensions, output: &mut [i64]) -> Result<()> {
        // No padded-shape, wait, or destroy callback: the query only borrows.
        let mut table = PJRT_Api {
            PJRT_Buffer_UnpaddedDimensions: Some(unpadded_dimensions),
            ..Default::default()
        };
        let api = Api(NonNull::from(&mut table));
        let buffer = Buffer(NonNull::from(dimensions).cast());
        unsafe { buffer.dimensions(api, output.as_mut_ptr(), output.len()) }
    }

    #[test]
    fn actual_dimensions_include_zero_extents() {
        let mut dimensions = Dimensions {
            values: &[3, 0, 7],
            null: false,
        };
        let mut output = [-1; 5];
        query_dimensions(&mut dimensions, &mut output[1..4]).unwrap();
        assert_eq!(output, [-1, 3, 0, 7, -1]);
        assert_eq!(dimensions.values, &[3, 0, 7]);
    }

    #[test]
    fn backend_dimension_error_preserves_output_and_releases_error() {
        use std::sync::atomic::{AtomicBool, Ordering};

        unsafe extern "C" fn fail(
            args: *mut PJRT_Buffer_UnpaddedDimensions_Args,
        ) -> *mut PJRT_Error {
            // Return the mock buffer itself as an error handle. The output
            // fields remain null: they must not be inspected after failure.
            unsafe { (*args).buffer.cast() }
        }
        unsafe extern "C" fn message(args: *mut PJRT_Error_Message_Args) {
            unsafe {
                (*args).message = c"shape unavailable".as_ptr();
                (*args).message_size = c"shape unavailable".count_bytes();
            }
        }
        unsafe extern "C" fn code(args: *mut PJRT_Error_GetCode_Args) -> *mut PJRT_Error {
            unsafe { (*args).code = PJRT_Error_Code_PJRT_Error_Code_INTERNAL };
            std::ptr::null_mut()
        }
        unsafe extern "C" fn destroy(args: *mut PJRT_Error_Destroy_Args) {
            let destroyed = unsafe { &*(*args).error.cast::<AtomicBool>() };
            assert!(!destroyed.swap(true, Ordering::Relaxed));
        }

        let mut destroyed = AtomicBool::new(false);
        let mut table = PJRT_Api {
            PJRT_Buffer_UnpaddedDimensions: Some(fail),
            PJRT_Error_Message: Some(message),
            PJRT_Error_GetCode: Some(code),
            PJRT_Error_Destroy: Some(destroy),
            ..Default::default()
        };
        let api = Api(NonNull::from(&mut table));
        let buffer = Buffer(NonNull::from(&mut destroyed).cast());
        let mut output = [99; 2];
        let error =
            unsafe { buffer.dimensions(api, output.as_mut_ptr(), output.len()) }.unwrap_err();
        assert_eq!(error.message, "shape unavailable");
        assert_eq!(error.code, Some(PJRT_Error_Code_PJRT_Error_Code_INTERNAL));
        assert_eq!(output, [99; 2]);
        assert!(destroyed.load(Ordering::Relaxed));
    }

    #[test]
    fn scalar_dimensions_allow_null() {
        let mut dimensions = Dimensions {
            values: &[],
            null: true,
        };
        query_dimensions(&mut dimensions, &mut []).unwrap();
    }

    #[test]
    fn invalid_dimensions_leave_output_untouched() {
        for (mut dimensions, message) in [
            (
                Dimensions {
                    values: &[3],
                    null: false,
                },
                "rank does not match",
            ),
            (
                Dimensions {
                    values: &[3, 4],
                    null: true,
                },
                "null buffer dimensions",
            ),
            (
                Dimensions {
                    values: &[3, -1],
                    null: false,
                },
                "extent must be nonnegative",
            ),
        ] {
            let mut output = [99; 2];
            let error = query_dimensions(&mut dimensions, &mut output).unwrap_err();
            assert!(error.message.contains(message), "{}", error.message);
            assert_eq!(output, [99; 2]);
        }
    }

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
                    assert_eq!(buffer.host_size(None).unwrap(), 32);
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
                None,
                Allocation::default(),
            )
            .unwrap();
            let mut output = 0.0f32;
            let layout = Layout::new(&[], &[], &[]).unwrap();
            assert_eq!(
                buffer.host_size(Some(&layout)).unwrap(),
                size_of_val(&output)
            );
            let error = buffer
                .to_host((&mut output as *mut f32).cast(), 1, Some(&layout))
                .unwrap_err();
            assert_eq!(
                error.code,
                Some(PJRT_Error_Code_PJRT_Error_Code_INVALID_ARGUMENT)
            );
            buffer
                .to_host(
                    (&mut output as *mut f32).cast(),
                    size_of_val(&output),
                    Some(&layout),
                )
                .unwrap();
            assert_eq!(output, value);
            // The CPU wrapper rejects strided host layouts. Both entry points
            // must forward the layout and report the backend error.
            let unsupported = Layout::Strides(&[]);
            let size_error = buffer.host_size(Some(&unsupported)).unwrap_err();
            let copy_error = buffer
                .to_host(
                    (&mut output as *mut f32).cast(),
                    size_of_val(&output),
                    Some(&unsupported),
                )
                .unwrap_err();
            assert_eq!(
                size_error.code,
                Some(PJRT_Error_Code_PJRT_Error_Code_INVALID_ARGUMENT)
            );
            assert_eq!(copy_error.code, size_error.code);
            buffer.deallocate().unwrap();
        }
    }
}
