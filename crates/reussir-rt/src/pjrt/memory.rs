use std::{ptr::NonNull, slice};

use super::{
    Error, Result,
    api::{Api, call, non_null},
    client::Device,
    sys::*,
};

/// A memory space borrowed from the process-lifetime client.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub(super) struct Memory(NonNull<PJRT_Memory>);

impl Memory {
    pub(super) fn as_ptr(self) -> *mut PJRT_Memory {
        self.0.as_ptr()
    }

    fn has_kind(self, api: Api, kind: &[u8]) -> Result<bool> {
        let args = call!(
            api,
            PJRT_Memory_Kind {
                memory: self.as_ptr()
            }
        )?;
        let actual: &[u8] = if args.kind_size == 0 {
            &[]
        } else {
            let pointer = non_null(args.kind.cast_mut(), "memory kind")?;
            unsafe { slice::from_raw_parts(pointer.as_ptr().cast(), args.kind_size) }
        };
        Ok(actual == kind)
    }
}

impl Device {
    pub(super) fn memory(self, api: Api, kind: &[u8]) -> Result<Memory> {
        let args = call!(
            api,
            PJRT_Device_AddressableMemories {
                device: self.as_ptr()
            }
        )?;
        if args.num_memories != 0 {
            let pointer = non_null(args.memories.cast_mut(), "memory list")?;
            for &raw in unsafe { slice::from_raw_parts(pointer.as_ptr(), args.num_memories) } {
                let memory = Memory(non_null(raw, "memory")?);
                if memory.has_kind(api, kind)? {
                    return Ok(memory);
                }
            }
        }
        Err(Error::local(format!(
            "PjRt device has no memory kind '{}'",
            String::from_utf8_lossy(kind)
        )))
    }
}
