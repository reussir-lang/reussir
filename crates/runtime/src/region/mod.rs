use std::{num::NonZeroUsize, ptr::NonNull};

use smallvec::SmallVec;
pub mod rusty;

const STATUS_BITS: usize = 2;
const STATUS_MASK: usize = (1 << STATUS_BITS) - 1;
const HIGHEST_BIT: usize = isize::MIN as usize;
const MAX_VALUE: usize = usize::MAX >> (STATUS_BITS + 1);

#[repr(transparent)]
#[derive(Copy, Clone)]
struct PackedStatus(*mut Header);

impl PackedStatus {
    pub fn increase(&mut self) {
        debug_assert!({
            let status: Status = (*self).into();
            matches!(status, Status::Rc(_) | Status::Rank(_))
        });
        self.0 = self.0.map_addr(|addr| addr + (1 << STATUS_BITS));
    }
}

impl PackedStatus {
    pub fn from_scalar(scalar: usize) -> Self {
       Self(std::ptr::without_provenance_mut(scalar))
    }
}

enum PackedStatusTag {
    Unmarked = 0b00,
    Rank = 0b01,
    Rc = 0b10,
    Disposing = 0b11,
    #[allow(unused)]
    Parent = 0xFF,
}

#[derive(Debug, Copy, Clone)]
enum Status {
    Unmarked,
    Rank(NonZeroUsize),
    Rc(NonZeroUsize),
    Disposing,
    Parent(NonNull<Header>),
}

impl Status {
    pub fn rank1() -> Self {
        Status::Rank(unsafe { NonZeroUsize::new_unchecked(1) })
    }
    pub fn rc1() -> Self {
        Status::Rc(unsafe { NonZeroUsize::new_unchecked(1) })
    }
}

impl From<Status> for PackedStatus {
    fn from(status: Status) -> Self {
        match status {
            Status::Parent(ptr) => Self(ptr.as_ptr()),
            Status::Unmarked => Self::from_scalar(PackedStatusTag::Unmarked as usize | HIGHEST_BIT),
            Status::Rank(non_zero) => {
                debug_assert!(non_zero.get() <= MAX_VALUE, "Rank value too large to pack");
                Self::from_scalar((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rank as usize | HIGHEST_BIT)
            }
            Status::Rc(non_zero) => {
                debug_assert!(non_zero.get() <= MAX_VALUE, "RC value too large to pack");
                Self::from_scalar((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rc as usize | HIGHEST_BIT)
            }
            Status::Disposing => Self::from_scalar(PackedStatusTag::Disposing as usize | HIGHEST_BIT),
        }
    }
}

impl From<PackedStatus> for Status {
    fn from(packed: PackedStatus) -> Self {
        if packed.0.addr() as isize > 0 {
            let ptr = unsafe { NonNull::new_unchecked(packed.0) };
            return Status::Parent(ptr);
        }
        match packed.0.addr() & STATUS_MASK {
            0 => Status::Unmarked,
            1 => {
                let rank = (packed.0.addr() & !HIGHEST_BIT) >> STATUS_BITS;
                Status::Rank(unsafe { NonZeroUsize::new_unchecked(rank) })
            }
            2 => {
                let rc = (packed.0.addr() & !HIGHEST_BIT) >> STATUS_BITS;
                Status::Rc(unsafe { NonZeroUsize::new_unchecked(rc) })
            }
            3 => Status::Disposing,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

#[repr(C)]
pub struct VTable {
    pub drop: Option<unsafe extern "C" fn(*mut u8)>,
    pub scan_count: usize,
    pub scan_offsets: *const usize,
}

#[repr(C)]
struct Header {
    status: PackedStatus,
    next: *mut Self,
    vtable: *mut VTable,
}

impl Header {
    pub unsafe fn children(this: NonNull<Self>) -> impl Iterator<Item = NonNull<Header>> {
        let table = unsafe { &*this.as_ref().vtable };
        static EMPTY: [usize; 0] = [];
        let slice = if table.scan_count > 0 && !table.scan_offsets.is_null() {
            unsafe { std::slice::from_raw_parts(table.scan_offsets, table.scan_count) }
        } else {
            &EMPTY
        };
        slice.iter().copied().filter_map(move |offset| {
            let ptr = unsafe { this.byte_add(offset).cast::<*mut Header>() };
            NonNull::new(unsafe { ptr.read() })
        })
    }

    pub unsafe fn drop(this: NonNull<Self>) {
        let table = unsafe { &*this.as_ref().vtable };
        if let Some(drop_fn) = table.drop {
            unsafe { drop_fn(this.as_ptr() as *mut u8) };
        }
    }

    unsafe fn find(mut cursor: NonNull<Self>) -> NonNull<Self> {
        while let Status::Parent(parent) = unsafe { cursor.as_ref().status }.into() {
            let parent_status = unsafe { parent.as_ref().status };
            if let Status::Parent(..) = parent_status.into() {
                unsafe { cursor.as_mut().status = parent_status };
            }
            cursor = parent;
        }
        cursor
    }

    unsafe fn rank(this: NonNull<Self>) -> NonZeroUsize {
        let status = unsafe { this.as_ref().status };
        if let Status::Rank(rank) = status.into() {
            rank
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

    unsafe fn union(mut r1: NonNull<Self>, mut r2: NonNull<Self>) -> bool {
        r1 = unsafe { Self::find(r1) };
        r2 = unsafe { Self::find(r2) };
        if r1 == r2 {
            return false;
        }
        let rank1 = unsafe { Self::rank(r1) };
        let rank2 = unsafe { Self::rank(r2) };
        if rank1 > rank2 {
            std::mem::swap(&mut r1, &mut r2);
        } else if rank1 == rank2 {
            unsafe {
                r1.as_mut().status.increase();
            };
        }
        unsafe { r2.as_mut().status = Status::Parent(r1).into() };
        true
    }

    pub unsafe fn dispose(this: NonNull<Self>) {}
    pub unsafe fn freeze(this: NonNull<Self>) {
        type Pending = SmallVec<[NonNull<Header>; 16]>;
        const RED_ZONE_SIZE: usize = 4096;
        const BUMP_SIZE: usize = 32768;
        let mut pending = Pending::new();
        fn freeze_recursive(mut this: NonNull<Header>, pending: &mut Pending) {
            stacker::maybe_grow(RED_ZONE_SIZE, BUMP_SIZE, move || {
                let status: Status = unsafe { Header::find(this).as_ref().status }.into();
                match status {
                    Status::Unmarked => {
                        unsafe {
                            this.as_mut().status = Status::rank1().into();
                        }
                        pending.push(this);
                        unsafe {
                            for child in Header::children(this) {
                                freeze_recursive(child, pending);
                            }
                        }
                        if let Some(peek) = pending.last().copied() {
                            if core::ptr::eq(peek.as_ptr(), this.as_ptr()) {
                                pending.pop();
                                unsafe {
                                    Header::find(this).as_mut().status = Status::rc1().into()
                                };
                            }
                        }
                    }
                    Status::Rc(_) => unsafe { Header::find(this).as_mut().status.increase() },
                    Status::Rank(_) => loop {
                        while pending
                            .last()
                            .copied()
                            .map(|p| unsafe { Header::union(this, p) })
                            .unwrap_or(false)
                        {
                            pending.pop();
                        }
                    },
                    _ => (),
                }
            })
        }
        freeze_recursive(this, &mut pending);
    }
}

impl Default for Header {
    fn default() -> Self {
        Self {
            status: Status::Unmarked.into(),
            next: std::ptr::null_mut(),
            vtable: std::ptr::null_mut(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pack_unpack_unmarked() {
        let original = Status::Unmarked;
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();

        match unpacked {
            Status::Unmarked => (),
            _ => panic!("Expected Unmarked, got {:?}", unpacked),
        }
    }

    #[test]
    fn test_pack_unpack_disposing() {
        let original = Status::Disposing;
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();

        match unpacked {
            Status::Disposing => (),
            _ => panic!("Expected Disposing, got {:?}", unpacked),
        }
    }

    #[test]
    fn test_pack_unpack_rank() {
        // Test various rank values
        let test_values = [1, 2, 42, 100, 1000, MAX_VALUE];

        for &value in &test_values {
            let rank = NonZeroUsize::new(value).unwrap();
            let original = Status::Rank(rank);
            let packed: PackedStatus = original.into();
            let unpacked: Status = packed.into();

            match unpacked {
                Status::Rank(unpacked_rank) => {
                    assert_eq!(
                        rank.get(),
                        unpacked_rank.get(),
                        "Rank value mismatch: original {}, unpacked {}",
                        rank.get(),
                        unpacked_rank.get()
                    );
                }
                _ => panic!("Expected Rank({}), got {:?}", value, unpacked),
            }
        }
    }

    #[test]
    fn test_pack_unpack_rc() {
        // Test various reference count values
        let test_values = [1, 2, 42, 100, 1000, MAX_VALUE];

        for &value in &test_values {
            let rc = NonZeroUsize::new(value).unwrap();
            let original = Status::Rc(rc);
            let packed: PackedStatus = original.into();
            let unpacked: Status = packed.into();

            match unpacked {
                Status::Rc(unpacked_rc) => {
                    assert_eq!(
                        rc.get(),
                        unpacked_rc.get(),
                        "RC value mismatch: original {}, unpacked {}",
                        rc.get(),
                        unpacked_rc.get()
                    );
                }
                _ => panic!("Expected Rc({}), got {:?}", value, unpacked),
            }
        }
    }

    #[test]
    fn test_pack_unpack_parent() {
        // Create a dummy header on the stack for testing
        let header = Header::default();
        let ptr = NonNull::from(&header);
        let original = Status::Parent(ptr);
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();

        match unpacked {
            Status::Parent(unpacked_ptr) => {
                assert_eq!(
                    ptr.as_ptr(),
                    unpacked_ptr.as_ptr(),
                    "Parent pointer mismatch"
                );
            }
            _ => panic!("Expected Parent, got {:?}", unpacked),
        }
    }

    #[test]
    fn test_pack_unpack_round_trip_all_variants() {
        let header = Header::default();
        let ptr = NonNull::from(&header);

        // Test each variant individually to avoid ownership issues

        // Test Unmarked
        let original = Status::Unmarked;
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();
        assert!(
            matches!(unpacked, Status::Unmarked),
            "Unmarked variant failed"
        );

        // Test Disposing
        let original = Status::Disposing;
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();
        assert!(
            matches!(unpacked, Status::Disposing),
            "Disposing variant failed"
        );

        // Test Rank
        let original = Status::Rank(NonZeroUsize::new(42).unwrap());
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();
        match unpacked {
            Status::Rank(val) => assert_eq!(val.get(), 42, "Rank value mismatch"),
            _ => panic!("Expected Rank variant"),
        }

        // Test Rc
        let original = Status::Rc(NonZeroUsize::new(100).unwrap());
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();
        match unpacked {
            Status::Rc(val) => assert_eq!(val.get(), 100, "Rc value mismatch"),
            _ => panic!("Expected Rc variant"),
        }

        // Test Parent
        let original = Status::Parent(ptr);
        let packed: PackedStatus = original.into();
        let unpacked: Status = packed.into();
        match unpacked {
            Status::Parent(unp_ptr) => {
                assert_eq!(ptr.as_ptr(), unp_ptr.as_ptr(), "Parent pointer mismatch")
            }
            _ => panic!("Expected Parent variant"),
        }
    }

    #[test]
    fn freeze_singleton() {
        let empty_table = VTable {
            drop: None,
            scan_count: 0,
            scan_offsets: std::ptr::null(),
        };
        let mut header = Header {
            status: Status::Unmarked.into(),
            next: std::ptr::null_mut(),
            vtable: &empty_table as *const VTable as *mut VTable,
        };
        let ptr = NonNull::from(&mut header);
        unsafe { Header::freeze(ptr) };
        let status: Status = header.status.into();
        match status {
            Status::Rc(rc) => assert_eq!(rc.get(), 1, "Expected RC to be 1"),
            _ => panic!("Expected Rc status, got {:?}", status),
        }
    }
}
