use std::{num::NonZeroUsize, ptr::NonNull};

enum Status {
    Unmarked,
    Parent(NonNull<Header>),
    Rank(NonZeroUsize),
    Rc(NonZeroUsize),
    Disposing,
}

struct Header {}
