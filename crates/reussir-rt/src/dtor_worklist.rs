//! Explicit pending work for branching destruction.
//!
//! The owner places a context on its stack, enqueues work, drains it, then
//! destroys it. Callbacks receive that same context and may enqueue children.
//! There is no ambient state, implicit drain, or modification of object headers.
//! Compiler selection of branching drops is deliberately separate from this ABI.

use core::{
    ffi::c_void,
    mem::{MaybeUninit, align_of, size_of},
};
use smallvec::SmallVec;

/// Must return normally; may enqueue work on `context`, but must not drain or
/// destroy it, retain it after returning, or use it concurrently.
pub type DtorCallback = unsafe extern "C" fn(object: *mut c_void, context: *mut DtorWorklist);

#[derive(Clone, Copy)]
struct Entry {
    object: *mut c_void,
    callback: DtorCallback,
}

const INLINE_CAPACITY: usize = 8;

/// Caller-owned context with an opaque FFI layout. Query its size/alignment
/// before allocating suitably aligned storage for placement initialization.
/// Eight pending entries fit inline, and SmallVec owns any overflow storage.
/// Rust callers can use `new` and normal scope cleanup. All work must be drained
/// before cleanup. Never copy a live context or move it while a callback runs.
pub struct DtorWorklist {
    pending: SmallVec<[Entry; INLINE_CAPACITY]>,
}

impl DtorWorklist {
    pub fn new() -> Self {
        Self {
            pending: SmallVec::new(),
        }
    }
}

impl Default for DtorWorklist {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for DtorWorklist {
    fn drop(&mut self) {
        assert!(
            self.pending.is_empty(),
            "destructor worklist must be drained before destruction"
        );
        // SmallVec drops its overflow buffer; Entry owns no object and invokes
        // no callback on drop.
    }
}

/// Bytes of caller-provided storage needed by the linked runtime.
#[unsafe(no_mangle)]
pub extern "C" fn __reussir_dtor_worklist_size() -> usize {
    size_of::<DtorWorklist>()
}

/// Required storage alignment in bytes (a nonzero power of two).
#[unsafe(no_mangle)]
pub extern "C" fn __reussir_dtor_worklist_align() -> usize {
    align_of::<DtorWorklist>()
}

/// Initialize caller-provided storage without allocating, returning an opaque
/// context pointer. The caller obtains its storage layout from the queries above.
///
/// # Safety
/// `storage` must be non-null, aligned to `__reussir_dtor_worklist_align()`, and
/// designate writable, exclusively owned space for at least
/// `__reussir_dtor_worklist_size()` bytes. It must contain no live context and
/// remain alive until destroy returns. These requirements are not checked.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_create_dtor_worklist(storage: *mut c_void) -> *mut DtorWorklist {
    let context = unsafe { &mut *storage.cast::<MaybeUninit<DtorWorklist>>() };
    context.write(DtorWorklist::new())
}

/// Append one callback, without invoking it. Entries execute last-in first-out.
///
/// # Safety
/// `context` must be live and exclusively accessible for this call. The caller
/// transfers responsibility for `object` to `callback`; it must remain valid
/// until that callback runs. The runtime neither dereferences nor frees it.
/// `callback` must obey DtorCallback's contract. Null object pointers are allowed
/// when the callback accepts them; the callback itself must be non-null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_enqueue_dtor(
    context: *mut DtorWorklist,
    object: *mut c_void,
    callback: DtorCallback,
) {
    let context = unsafe { &mut *context.cast::<MaybeUninit<DtorWorklist>>() };
    let context = unsafe { context.assume_init_mut() };
    context.pending.push(Entry { object, callback });
}

/// Execute all pending work, including work enqueued by callbacks.
///
/// # Safety
/// The caller must own the live context. It may not already be draining.
/// Callbacks may mutate the context through enqueue, but must not call drain or
/// destroy on it. An independent context may be used by a callback.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drain_dtor_worklist(context: *mut DtorWorklist) {
    loop {
        // Do not hold a Rust reference to the context across foreign callbacks:
        // a callback may enqueue enough work to reallocate the overflow buffer.
        let entry = {
            let storage = unsafe { &mut *context.cast::<MaybeUninit<DtorWorklist>>() };
            unsafe { storage.assume_init_mut() }.pending.pop()
        };
        let Some(entry) = entry else { return };
        unsafe { (entry.callback)(entry.object, context) };
    }
}

/// Release overflow storage. Does not run destructors; pending work is an error.
///
/// # Safety
/// `context` must be live, empty, and not currently draining. It becomes
/// uninitialized storage; do not use or destroy it again without create.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_destroy_dtor_worklist(context: *mut DtorWorklist) {
    let context = unsafe { &mut *context.cast::<MaybeUninit<DtorWorklist>>() };
    unsafe { context.assume_init_drop() };
}

#[cfg(test)]
mod tests;
