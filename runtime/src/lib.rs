#![allow(clippy::missing_safety_doc)]

#[cfg(all(feature = "mimalloc", not(miri)))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

pub mod alloc;
pub mod collections;
pub mod nullable;
pub mod option;
pub mod panic;
pub mod rc;
pub mod region;
pub mod sync;
use panic::panic;
