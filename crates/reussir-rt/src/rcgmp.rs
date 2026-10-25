//! An owned handle of a `Nat` or an `Int`, for Rust code that holds the
//! integers as its own values, such as the textures of the polymorphic FFI.
//!
//! An [`Mpz`] owns one reference of an rcgmp integer
//! (`RCGMP/Runtime/Mpz.h`): cloning retains it and dropping releases it, and
//! it has the layout of the handle itself (`#[repr(transparent)]`). Counting
//! is nonatomic: an `Arc` coloring never crosses the FFI boundary.
//! Comparisons read the values (`rcgmp_mpz_cmp`), so ordered containers keyed
//! by integers order them by value.
//!
//! Cloning and dropping call `__reussir_rcgmp_ffi_acquire` and
//! `__reussir_rcgmp_ffi_release`, glue the compiler emits into every module
//! that uses `Nat` or `Int`: `rcgmp.retain` and `rcgmp.release`, `weak_odr`
//! and hidden, so that each linked module or shared library binds its own
//! copy, next to its own view of the rcgmp runtime. LLVM inlines the glue
//! into the linked texture code: an immediate costs a bit test, and the rcgmp
//! lowering decides the test, as the dialect does not make its tag an ABI.
//! Runtime code outside the textures that holds an `Mpz` resolves the glue
//! against the program it is linked into, which uses integers.
//!
//! Every method of [`Mpz`] is `#[inline]`, compiled only into the code that
//! uses it: this crate references neither the glue nor the rcgmp runtime, and
//! a program without integers links without them.

use core::cmp::Ordering;
use core::ffi::c_void;

unsafe extern "C" {
    fn __reussir_rcgmp_ffi_acquire(value: *mut c_void);
    fn __reussir_rcgmp_ffi_release(value: *mut c_void);
    fn rcgmp_mpz_cmp(lhs: *mut c_void, rhs: *mut c_void) -> i32;
}

/// One owned, nonatomically counted reference of a `Nat` or an `Int`.
#[repr(transparent)]
pub struct Mpz(*mut c_void);

impl Mpz {
    /// Takes over the reference `handle` owns.
    ///
    /// # Safety
    ///
    /// `handle` must be a canonical rcgmp handle owning one nonatomic
    /// reference, which the result owns from now on.
    #[inline]
    pub unsafe fn from_raw(handle: *mut c_void) -> Self {
        Self(handle)
    }

    /// Gives up the reference as the raw handle, without releasing it.
    #[inline]
    pub fn into_raw(self) -> *mut c_void {
        let handle = self.0;
        core::mem::forget(self);
        handle
    }

    /// The raw handle, borrowed: the reference stays with `self`.
    #[inline]
    pub fn as_ptr(&self) -> *mut c_void {
        self.0
    }
}

impl Clone for Mpz {
    #[inline]
    fn clone(&self) -> Self {
        unsafe { __reussir_rcgmp_ffi_acquire(self.0) };
        Self(self.0)
    }
}

impl Drop for Mpz {
    #[inline]
    fn drop(&mut self) {
        unsafe { __reussir_rcgmp_ffi_release(self.0) }
    }
}

impl PartialEq for Mpz {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Mpz {}

impl PartialOrd for Mpz {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mpz {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        unsafe { rcgmp_mpz_cmp(self.0, other.0) }.cmp(&0)
    }
}

#[cfg(test)]
mod tests {
    use super::Mpz;
    use core::ffi::c_void;
    use core::mem::ManuallyDrop;

    /// The handle has the layout of a pointer, and `from_raw` and `into_raw`
    /// hand its reference over unchanged. No count is touched: the crate's
    /// tests do not link the rcgmp runtime.
    #[test]
    fn handle_is_one_pointer_and_round_trips() {
        assert_eq!(size_of::<Mpz>(), size_of::<*mut c_void>());
        assert_eq!(align_of::<Mpz>(), align_of::<*mut c_void>());
        // The tagged bits of the immediate 1 (`4 * x + 1`).
        let raw = core::ptr::without_provenance_mut::<c_void>(5);
        let handle = ManuallyDrop::new(unsafe { Mpz::from_raw(raw) });
        assert_eq!(handle.as_ptr(), raw);
        assert_eq!(ManuallyDrop::into_inner(handle).into_raw(), raw);
    }
}
