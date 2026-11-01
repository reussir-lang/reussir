//! A per-thread stack of pending drops.
//!
//! Drop glue whose release can recurse without bound runs under [`run`],
//! which marks the thread as draining. Inside, each box whose count reaches
//! zero is deferred onto this stack with the function that releases it
//! ([`defer`]), and the outermost [`run`] pops the entries, last deferred
//! first, until none is left, as Lean's `lean_del` does with its to-do list.
//! A popped release may defer more entries; a [`run`] started while the
//! thread drains does not drain, its deferrals join the running drain. So the
//! native stack stays bounded however deep the value is, for the box edges
//! the glue defers. Outside a drain the stack is empty. The stack never reads
//! or writes a deferred pointer; its buffer shrinks back to `KEEP` entries
//! after a drain and is freed at thread exit.

use core::cell::Cell;
use core::mem::ManuallyDrop;

/// Releases one deferred pointer.
pub type Release = unsafe extern "C" fn(*mut u8);

#[derive(Clone, Copy)]
struct Entry {
    cell: *mut u8,
    release: Release,
}

/// Capacity the buffer keeps after a drain.
const KEEP: usize = 64;

/// Whether the thread drains, whether its exit guard ran (a later drain then
/// frees its buffer), and its pending drops, a `Vec<Entry>` by raw parts.
struct State {
    releasing: Cell<bool>,
    exited: Cell<bool>,
    len: Cell<usize>,
    cap: Cell<usize>,
    ptr: Cell<*mut Entry>,
}

thread_local! {
    static STATE: State = const {
        State {
            releasing: Cell::new(false),
            exited: Cell::new(false),
            len: Cell::new(0),
            cap: Cell::new(0),
            ptr: Cell::new(core::ptr::dangling_mut()),
        }
    };
    static GUARD: Guard = const { Guard };
}

/// Frees the buffer at thread exit (registered with the first buffer).
struct Guard;

impl Drop for Guard {
    fn drop(&mut self) {
        let _ = STATE.try_with(|state| {
            state.exited.set(true);
            state.with_vec(|v| *v = Vec::new());
        });
    }
}

/// The current thread's state (no destructor: it outlives the others).
#[inline(always)]
fn state() -> &'static State {
    STATE.with(|state| unsafe { &*(state as *const State) })
}

impl State {
    fn with_vec(&self, f: impl FnOnce(&mut Vec<Entry>)) {
        let (ptr, len, cap) = (self.ptr.get(), self.len.get(), self.cap.get());
        let mut v = ManuallyDrop::new(unsafe { Vec::from_raw_parts(ptr, len, cap) });
        f(&mut v);
        self.ptr.set(v.as_mut_ptr());
        self.len.set(v.len());
        self.cap.set(v.capacity());
    }
    /// Pops and runs entries (the thread draining) until none is left.
    unsafe fn pop_all(&self) {
        while self.len.get() != 0 {
            let len = self.len.get() - 1;
            self.len.set(len);
            let entry = unsafe { self.ptr.get().add(len).read() };
            unsafe { (entry.release)(entry.cell) };
        }
        if self.exited.get() {
            self.with_vec(|v| *v = Vec::new());
        } else if self.cap.get() > KEEP {
            self.with_vec(|v| v.shrink_to(KEEP));
        }
    }
}

/// Whether the current thread drains: a release started now joins the drain.
#[inline(always)]
pub fn releasing() -> bool {
    state().releasing.get()
}

/// Defers `release(cell)` until the current thread's drain pops it. Outside
/// a drain, runs it at once, as a [`run`] of it. Safety: `release(cell)` must
/// be sound whenever it runs.
#[inline(always)]
pub unsafe fn defer(cell: *mut u8, release: Release) {
    let state = state();
    if !state.releasing.get() {
        return unsafe { run_outermost(cell, release) };
    }
    let len = state.len.get();
    if len == state.cap.get() {
        return unsafe { defer_slow(cell, release) };
    }
    unsafe { state.ptr.get().add(len).write(Entry { cell, release }) };
    state.len.set(len + 1);
}

/// Runs `walk(object)` with the thread draining and then, unless a drain was
/// already running, every entry deferred meanwhile. Inside a drain (a drop
/// made by a release, or by a foreign hook, that the drain runs) it returns
/// after the walk, before the dead members it deferred are released: the
/// running drain releases them. Safety: `walk(object)` must be sound now.
#[inline(always)]
pub unsafe fn run(object: *mut u8, walk: Release) {
    if releasing() {
        return unsafe { walk(object) };
    }
    unsafe { run_outermost(object, walk) }
}

#[inline(never)]
unsafe extern "C" fn run_outermost(object: *mut u8, walk: Release) {
    let state = state();
    state.releasing.set(true);
    unsafe { walk(object) };
    unsafe { state.pop_all() };
    state.releasing.set(false);
}

#[cold]
#[inline(never)]
unsafe extern "C" fn defer_slow(cell: *mut u8, release: Release) {
    let state = state();
    if state.cap.get() == 0 {
        // Past the guard's destruction the drain frees the buffer (`exited`).
        let _ = GUARD.try_with(|_| ());
    }
    state.with_vec(|v| v.push(Entry { cell, release }));
}

/// C entry point of [`defer`], called by compiled drop glue.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer(cell: *mut u8, release: Release) {
    unsafe { defer(cell, release) }
}

/// C entry point of [`run`], called by compiled drop glue.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_run(object: *mut u8, walk: Release) {
    unsafe { run(object, walk) }
}

#[cfg(test)]
mod tests;
