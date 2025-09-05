use std::{num::NonZeroUsize, ptr::NonNull};

const STATUS_BITS: usize = 2;
const STATUS_MASK: usize = (1 << STATUS_BITS) - 1;

#[repr(transparent)]
struct PackedStatus(usize);

enum PackedStatusTag {
    Unmarked = 0b00,
    Rank = 0b01,
    Rc = 0b10,
    Disposing = 0b11,
    Parent = 0xFF,
}

enum Status {
    Unmarked,
    Rank(NonZeroUsize),
    Rc(NonZeroUsize),
    Disposing,
    Parent(NonNull<Header>),
}

impl PackedStatus {
    pub fn unpack(self) -> Status {
        if self.0 as isize > 0 {
            let ptr = std::ptr::with_exposed_provenance_mut(self.0);
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            return Status::Parent(ptr);
        }
        match self.0 & STATUS_MASK {
            0 => Status::Unmarked,
            1 => {
                let rank = self.0 >> STATUS_BITS;
                Status::Rank(unsafe { NonZeroUsize::new_unchecked(rank) })
            }
            2 => {
                let rc = self.0 >> STATUS_BITS;
                Status::Rc(unsafe { NonZeroUsize::new_unchecked(rc) })
            }
            3 => Status::Disposing,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
    pub fn pack(status: Status) -> Self {
        match status {
            Status::Parent(ptr) => Self(ptr.as_ptr().expose_provenance()),
            Status::Unmarked => Self(PackedStatusTag::Unmarked as usize),
            Status::Rank(non_zero) => {
                Self((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rank as usize)
            }
            Status::Rc(non_zero) => {
                Self((non_zero.get() << STATUS_BITS) | PackedStatusTag::Rc as usize)
            }
            Status::Disposing => Self(PackedStatusTag::Disposing as usize),
        }
    }
}

struct Header {}
