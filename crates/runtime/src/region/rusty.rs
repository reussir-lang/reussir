use std::{mem::ManuallyDrop, ptr::NonNull};

use crate::region::{Header, VTable};

// For rust side interoperability
#[repr(C)]
pub struct RegionalRcBox<T> {
    header: Header,
    data: ManuallyDrop<T>,
}

pub unsafe trait RegionalObjectTrait: Sized + Clone {
    const SCAN_OFFSETS: &'static [usize];
    unsafe extern "C" fn drop(ptr: *mut u8) {
        if let Some(ptr) = NonNull::new(ptr) {
            let mut obj = ptr.cast::<RegionalRcBox<Self>>();
            unsafe { ManuallyDrop::drop(&mut obj.as_mut().data) };
        }
    }
    const VTABLE: VTable = VTable {
        drop: Some(Self::drop),
        scan_count: Self::SCAN_OFFSETS.len(),
        scan_offsets: Self::SCAN_OFFSETS.as_ptr(),
    };
}

#[repr(transparent)]
pub struct RegionalRc<T: RegionalObjectTrait> (NonNull<RegionalRcBox<T>>);


