use std::{cell::Cell, mem::ManuallyDrop, ptr::NonNull};

use crate::region::{Header, Status, VTable};

// For rust side interoperability
#[repr(C)]
pub struct RegionalRcBox<T> {
    header: Header,
    data: ManuallyDrop<T>,
}

pub unsafe trait RegionalObjectTrait: Sized {
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
        size: std::mem::size_of::<RegionalRcBox<Self>>(),
        alignment: std::mem::align_of::<RegionalRcBox<Self>>(),
    };
}

#[repr(transparent)]
pub struct RegionalRc<T: RegionalObjectTrait>(NonNull<RegionalRcBox<T>>);

impl<T: RegionalObjectTrait> Copy for RegionalRc<T> {}

impl<T: RegionalObjectTrait> Clone for RegionalRc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: RegionalObjectTrait> RegionalRc<T> {
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut self.0.as_mut().data }
    }
    pub unsafe fn deref(&self) -> &T {
        unsafe { &self.0.as_ref().data }
    }
    fn header(&self) -> NonNull<Header> {
        self.0.cast()
    }
    pub unsafe fn freeze(self) -> RigidRc<T> {
        unsafe { Header::freeze(self.header()) };
        RigidRc(self)
    }
    pub unsafe fn read_field(&self) -> RigidRc<T> {
        unsafe { Header::acquire(self.header()) };
        RigidRc(*self)
    }
}

#[repr(transparent)]
pub struct RigidRc<T: RegionalObjectTrait>(RegionalRc<T>);

impl<T: RegionalObjectTrait> RigidRc<T> {
    pub unsafe fn deref(&self) -> &T {
        unsafe { self.0.deref() }
    }
}

impl<T: RegionalObjectTrait> Clone for RigidRc<T> {
    fn clone(&self) -> Self {
        unsafe {
            Header::acquire(self.0.header());
        }
        RigidRc(self.0)
    }
}

impl<T: RegionalObjectTrait> Drop for RigidRc<T> {
    fn drop(&mut self) {
        unsafe {
            Header::release(self.0.header());
        }
    }
}

#[repr(C)]
pub struct Region {
    head: Cell<Option<NonNull<Header>>>,
}

impl Default for Region {
    fn default() -> Self {
        Self::new()
    }
}

impl Region {
    pub fn new() -> Self {
        Self {
            head: Cell::new(None),
        }
    }
    pub fn create<T: RegionalObjectTrait>(&self, value: T) -> RegionalRc<T> {
        let rc_box = Box::new(RegionalRcBox {
            header: Header {
                status: Status::Unmarked.into(),
                next: self
                    .head
                    .get()
                    .map(|h| h.as_ptr())
                    .unwrap_or(std::ptr::null_mut()),
                vtable: &T::VTABLE as *const VTable as *mut VTable,
            },
            data: ManuallyDrop::new(value),
        });
        let ptr = NonNull::from(Box::leak(rc_box));
        self.head.set(Some(ptr.cast()));
        RegionalRc(ptr)
    }
    pub unsafe fn clean(&self) {
        while let Some(head) = self.head.get() {
            self.head.set(NonNull::new(unsafe { head.as_ref().next }));
            if let Status::Unmarked = unsafe { head.as_ref().status.into() } {
                unsafe {
                    Header::drop(head);
                    Header::deallocate(head);
                }
            }
        }
    }
}

macro_rules! impl_regional_object_traits_for_primitive {
    ($($ty:ty),*) => {
        $(
            unsafe impl RegionalObjectTrait for $ty {
                const SCAN_OFFSETS: &'static [usize] = &[];
                unsafe extern "C" fn drop(_ptr: *mut u8) {
                }
            }
        )*
    };
}

impl_regional_object_traits_for_primitive!(
    bool, i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, usize, isize
);

#[cfg(test)]
mod tests {
    use std::mem::offset_of;

    use super::*;

    #[test]
    fn test_region() {
        let region = Region::new();
        unsafe {
            region.clean();
        }
    }

    struct ListNode {
        prev: Option<RegionalRc<Self>>,
        next: Option<RegionalRc<Self>>,
        data: usize,
    }

    unsafe impl RegionalObjectTrait for ListNode {
        const SCAN_OFFSETS: &'static [usize] = &[
            offset_of!(ListNode, prev) + offset_of!(RegionalRcBox<Self>, data),
            offset_of!(ListNode, next) + offset_of!(RegionalRcBox<Self>, data),
        ];
    }

    #[test]
    fn test_list_node() {
        let region = Region::new();
        let _node = region.create(ListNode {
            prev: None,
            next: None,
            data: 0,
        });
        unsafe {
            region.clean();
        }
    }
    #[test]
    fn test_list_0_1_2() {
        let region = Region::new();
        let mut node1 = region.create(ListNode {
            prev: None,
            next: None,
            data: 0,
        });
        let mut node2 = region.create(ListNode {
            prev: None,
            next: None,
            data: 1,
        });
        let mut node3 = region.create(ListNode {
            prev: None,
            next: None,
            data: 2,
        });
        unsafe {
            node3.deref_mut().next = Some(node1);
            node2.deref_mut().next = Some(node3);
            node1.deref_mut().next = Some(node2);
            node1.deref_mut().prev = Some(node3);
            node2.deref_mut().prev = Some(node1);
            node3.deref_mut().prev = Some(node2);
        }

        let rigid_node1 = unsafe { node1.freeze() };
        let rigid_node2 = unsafe { node2.freeze() };
        let rigid_node3 = unsafe { node3.freeze() };
        unsafe {
            region.clean();
            assert_eq!(rigid_node1.deref().data, 0);
            assert_eq!(rigid_node1.deref().prev.unwrap().deref().data, 2);
            assert_eq!(rigid_node1.deref().next.unwrap().deref().data, 1);
            assert_eq!(rigid_node2.deref().data, 1);
            assert_eq!(rigid_node2.deref().prev.unwrap().deref().data, 0);
            assert_eq!(rigid_node2.deref().next.unwrap().deref().data, 2);
            assert_eq!(rigid_node3.deref().data, 2);
            assert_eq!(rigid_node3.deref().prev.unwrap().deref().data, 1);
            assert_eq!(rigid_node3.deref().next.unwrap().deref().data, 0);
            let _node2_clone = rigid_node2.clone();
            let cloned = rigid_node3.deref().next.unwrap().read_field();
            assert_eq!(cloned.deref().data, 0);
        }
    }
}
