use std::{num::NonZeroUsize, os::linux::raw::stat, ptr::NonNull};

use smallvec::SmallVec;
pub mod rusty;

const STATUS_BITS: usize = 2;
const STATUS_MASK: usize = (1 << STATUS_BITS) - 1;
const HIGHEST_BIT: usize = isize::MIN as usize;
const MAX_VALUE: usize = usize::MAX >> (STATUS_BITS + 1);

#[repr(transparent)]
#[derive(Copy, Clone)]
struct PackedStatus(usize);

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

impl From<Status> for PackedStatus {
    fn from(status: Status) -> Self {
        match status {
            Status::Parent(ptr) => Self(ptr.as_ptr().expose_provenance()),
            Status::Unmarked => Self(PackedStatusTag::Unmarked as usize | HIGHEST_BIT),
            Status::Rank(non_zero) => {
                debug_assert!(non_zero.get() <= MAX_VALUE, "Rank value too large to pack");
                Self((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rank as usize | HIGHEST_BIT)
            }
            Status::Rc(non_zero) => {
                debug_assert!(non_zero.get() <= MAX_VALUE, "RC value too large to pack");
                Self((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rc as usize | HIGHEST_BIT)
            }
            Status::Disposing => Self(PackedStatusTag::Disposing as usize | HIGHEST_BIT),
        }
    }
}

impl From<PackedStatus> for Status {
    fn from(packed: PackedStatus) -> Self {
        if packed.0 as isize > 0 {
            let ptr = std::ptr::with_exposed_provenance_mut(packed.0);
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            return Status::Parent(ptr);
        }
        match packed.0 & STATUS_MASK {
            0 => Status::Unmarked,
            1 => {
                let rank = (packed.0 & !HIGHEST_BIT) >> STATUS_BITS;
                Status::Rank(unsafe { NonZeroUsize::new_unchecked(rank) })
            }
            2 => {
                let rc = (packed.0 & !HIGHEST_BIT) >> STATUS_BITS;
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
    pub fn children(this: NonNull<Self>) -> impl Iterator<Item = NonNull<Header>> {
        let table = unsafe { &*this.as_ref().vtable };
        let slice = unsafe { std::slice::from_raw_parts(table.scan_offsets, table.scan_count) };
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
            unsafe { r1.as_mut().status = Status::Rank(rank1.saturating_add(1)).into() };
        }
        unsafe { r2.as_mut().status = Status::Parent(r1).into() };
        true
    }

    pub unsafe fn dispose(this: NonNull<Self>) {}
    pub unsafe fn freeze(this: NonNull<Self>) {
        type Pending = SmallVec<[NonNull<Header>; 16]>;
        let mut pending = Pending::new();
        fn freeze_recursive(this: NonNull<Header>, pending: &mut Pending) {
            stacker::maybe_grow(4096, 32768, move || {})
        }
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
}
