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
//! or writes a deferred pointer (its layout is the deferring code's). Drops
//! deferred one after the other with one release function form one run,
//! which takes one entry, and one pointer per cell but its head. Its buffers
//! shrink back to `KEEP` elements after a drain and are freed at thread exit.
//!
//! A host runtime whose releases may need work that must not run inside a
//! drain (code that may block or switch contexts, which would leave the
//! thread's drain suspended) registers one end function ([`set_end_work`]);
//! a release asks for it ([`want_end_work`]), and the outermost drain calls
//! it once it has ended, with the thread no longer draining and nothing
//! pending. A drain that nobody asked costs one test at its end.

use core::cell::Cell;
use core::mem::ManuallyDrop;
use core::ptr::NonNull;
use core::sync::atomic::{AtomicPtr, Ordering};

/// Releases one deferred pointer.
pub type Release = unsafe extern "C" fn(*mut u8);

/// A run below the top of the stack: `n` deferred cells, the last deferred
/// at `head`, to be released with `release`; its other `n - 1` cells are in
/// the cells vector, below the cells of the runs above it (see [`State`]).
#[derive(Clone, Copy)]
struct Run {
    release: Release,
    head: *mut u8,
    n: u32,
}

/// Capacity each vector keeps after a drain.
const KEEP: usize = 64;

/// A `Vec<T>` by its raw parts, in cells (the state has no destructor; the
/// guard frees the buffers).
struct Parts<T> {
    len: Cell<usize>,
    ptr: Cell<NonNull<T>>,
    cap: Cell<usize>,
}

impl<T> Parts<T> {
    const fn new() -> Self {
        Parts { len: Cell::new(0), ptr: Cell::new(NonNull::dangling()), cap: Cell::new(0) }
    }

    fn with_vec(&self, f: impl FnOnce(&mut Vec<T>)) {
        let (ptr, len, cap) = (self.ptr.get().as_ptr(), self.len.get(), self.cap.get());
        let mut v = ManuallyDrop::new(unsafe { Vec::from_raw_parts(ptr, len, cap) });
        f(&mut v);
        self.ptr.set(unsafe { NonNull::new_unchecked(v.as_mut_ptr()) });
        self.len.set(v.len());
        self.cap.set(v.capacity());
    }

    /// Makes room for one more element.
    #[cold]
    #[inline(never)]
    fn grow(&self) {
        if self.cap.get() == 0 {
            // Past the guard's destruction the drain frees the buffer (`exited`).
            let _ = GUARD.try_with(|_| ());
        }
        self.with_vec(|v| v.reserve(1));
    }

    /// Pushes `x`; answers false, changing nothing, when the vector is full.
    #[inline(always)]
    fn try_push(&self, x: T) -> bool {
        let len = self.len.get();
        if len == self.cap.get() {
            return false;
        }
        unsafe { self.ptr.get().as_ptr().add(len).write(x) };
        self.len.set(len + 1);
        true
    }

    /// The last element, removed. The vector is not empty.
    #[inline(always)]
    unsafe fn pop_unchecked(&self) -> T {
        let len = self.len.get() - 1;
        self.len.set(len);
        unsafe { self.ptr.get().as_ptr().add(len).read() }
    }

    /// After a drain: the buffer back to `KEEP` elements, or freed once the
    /// thread's exit guard has run.
    fn trim(&self, exited: bool) {
        if exited {
            self.with_vec(|v| *v = Vec::new());
        } else if self.cap.get() > KEEP {
            self.with_vec(|v| v.shrink_to(KEEP));
        }
    }
}

/// The thread's pending drops: the runs of the stack vector, then the run
/// `(n, head, release)` on top if `n` is not 0. Drops deferred one after the
/// other with one release function form one run: the run is one entry, and
/// each cell but its head one pointer in the cells vector (8 bytes while it
/// waits, as the boxed heads of a list that the glue frees whole). The top
/// run is kept in the state itself, so a deferral or a pop that stays in it
/// touches no vector but the cells'. Also whether the thread drains, whether
/// a release asked for the end work, and whether its exit guard ran (a later
/// drain then frees its buffers).
struct State {
    n: Cell<u32>,
    releasing: Cell<bool>,
    end_work: Cell<bool>,
    exited: Cell<bool>,
    head: Cell<*mut u8>,
    release: Cell<Release>,
    stack: Parts<Run>,
    cells: Parts<*mut u8>,
}

/// No release function (the top run's while no run is on top).
unsafe extern "C" fn unnamed(_: *mut u8) {
    unreachable!("reussir-rt: a pending drop without a release function")
}

thread_local! {
    static STATE: State = const {
        State {
            n: Cell::new(0),
            releasing: Cell::new(false),
            end_work: Cell::new(false),
            exited: Cell::new(false),
            head: Cell::new(core::ptr::null_mut()),
            release: Cell::new(unnamed as Release),
            stack: Parts::new(),
            cells: Parts::new(),
        }
    };
    static GUARD: Guard = const { Guard };
}

/// Frees the buffers at thread exit (registered with the first buffer).
struct Guard;

impl Drop for Guard {
    fn drop(&mut self) {
        let _ = STATE.try_with(|state| {
            state.exited.set(true);
            state.stack.with_vec(|v| *v = Vec::new());
            state.cells.with_vec(|v| *v = Vec::new());
        });
    }
}

/// The current thread's state (no destructor: it outlives the others).
#[inline(always)]
fn state() -> &'static State {
    STATE.with(|state| unsafe { &*(state as *const State) })
}

impl State {
    /// Moves the run on top, if any, to the stack vector (another run goes
    /// on top). Answers false, changing nothing, when the vector is full.
    #[inline(always)]
    fn try_spill(&self) -> bool {
        let n = self.n.get();
        if n == 0 {
            return true;
        }
        if !self.stack.try_push(Run { release: self.release.get(), head: self.head.get(), n }) {
            return false;
        }
        self.n.set(0);
        true
    }

    /// After the top run's last cell left: the stack vector's last run, if
    /// any, goes on top.
    #[inline(always)]
    fn unspill(&self) {
        if self.stack.len.get() == 0 {
            self.n.set(0);
            return;
        }
        let Run { release, head, n } = unsafe { self.stack.pop_unchecked() };
        self.release.set(release);
        self.head.set(head);
        self.n.set(n);
    }

    /// Whether a drop deferred with `release` joins the run on top (one with
    /// the same function and room for one more cell).
    #[inline(always)]
    fn joins(&self, release: Release) -> bool {
        let n = self.n.get();
        n != 0 && n < u32::MAX && self.release.get() as usize == release as usize
    }

    /// Defers `release(cell)`: adds it to the run on top, or starts a run.
    /// Answers false, changing nothing, when that needs a vector to grow.
    #[inline(always)]
    unsafe fn try_defer(&self, cell: *mut u8, release: Release) -> bool {
        if self.joins(release) {
            if !self.cells.try_push(self.head.get()) {
                return false;
            }
            self.head.set(cell);
            self.n.set(self.n.get() + 1);
            return true;
        }
        if !self.try_spill() {
            return false;
        }
        self.head.set(cell);
        self.release.set(release);
        self.n.set(1);
        true
    }

    /// Pops and runs the pending drops, last deferred first, until none is
    /// left (the thread draining).
    unsafe fn pop_all(&self) {
        loop {
            let n = self.n.get();
            if n == 0 {
                break;
            }
            let (head, release) = (self.head.get(), self.release.get());
            if n == 1 {
                self.unspill();
            } else {
                self.head.set(unsafe { self.cells.pop_unchecked() });
                self.n.set(n - 1);
            }
            unsafe { release(head) };
        }
        debug_assert_eq!(self.cells.len.get(), 0, "reussir-rt: pending cells left after a drain");
        let exited = self.exited.get();
        self.stack.trim(exited);
        self.cells.trim(exited);
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
    if !unsafe { state.try_defer(cell, release) } {
        unsafe { defer_slow(cell, release) }
    }
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
    if state.end_work.get() {
        end_work(state);
    }
}

/// The end function a host registered ([`set_end_work`]), or null.
static END_WORK: AtomicPtr<()> = AtomicPtr::new(core::ptr::null_mut());

/// The function the end of a drain calls when a release asked for it
/// ([`want_end_work`]). One per process: a later call replaces it.
pub fn set_end_work(f: unsafe extern "C" fn()) {
    END_WORK.store(f as *mut (), Ordering::Release);
}

/// Asks for the end function once the current thread's outermost drain is
/// over. Outside a drain, calls it at once, as [`defer`] runs a release.
#[inline(always)]
pub fn want_end_work() {
    let state = state();
    if state.releasing.get() {
        state.end_work.set(true);
    } else {
        end_work(state);
    }
}

/// Uses up the request and calls the end function: the drain is over (the
/// thread no longer drains, nothing is pending), so it may drop values,
/// whose drains then end on their own. A drain inside the end function
/// whose releases ask for the end work again runs it at its own end, inside
/// this call: one level of nesting per generation, as native Lean's frees
/// nest inside the dependents they run (deliberate; a host whose end work
/// can chain without bound bounds it itself).
#[cold]
#[inline(never)]
fn end_work(state: &State) {
    state.end_work.set(false);
    let f = END_WORK.load(Ordering::Acquire);
    if !f.is_null() {
        let f = unsafe { core::mem::transmute::<*mut (), unsafe extern "C" fn()>(f) };
        unsafe { f() };
    }
}

/// [`State::try_defer`] when a vector must grow first: the cells vector if
/// the drop joins the run on top, else the stack vector (for the run it
/// spills).
#[cold]
#[inline(never)]
unsafe extern "C" fn defer_slow(cell: *mut u8, release: Release) {
    let state = state();
    if state.joins(release) {
        state.cells.grow();
    } else {
        state.stack.grow();
    }
    let deferred = unsafe { state.try_defer(cell, release) };
    debug_assert!(deferred);
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
