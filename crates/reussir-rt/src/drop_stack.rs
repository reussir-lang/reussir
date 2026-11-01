//! Scoped worklists shared by generated drop glue and foreign destructors.
use core::{ffi::c_void, num::NonZeroUsize, ptr::NonNull};
use std::cell::{Cell, RefCell};

const RETAINED_BUFFER_CAPACITY: usize = 64;

/// Releases one deferred object.
pub type Release = unsafe extern "C" fn(*mut u8);

/// An object whose lifetime and release callback are supplied by the caller.
#[derive(Clone, Copy)]
struct Object(NonNull<c_void>);

impl Object {
    unsafe fn new(pointer: *mut u8) -> Self {
        Self(unsafe { NonNull::new_unchecked(pointer.cast()) })
    }
    fn as_ptr(self) -> *mut u8 {
        self.0.as_ptr().cast()
    }
}

/// A LIFO group sharing one release callback. The newest object is `head`;
/// its other `len - 1` objects occupy the top of `Pending::cells`.
#[derive(Clone, Copy)]
struct PendingRun {
    head: Object,
    release: Release,
    len: NonZeroUsize,
}

/// Suspended work resumed after the active run finishes.
#[derive(Clone, Copy)]
enum Work {
    Run(PendingRun),
    Indexed {
        object: Object,
        cursor: usize,
        callback: Indexed,
    },
    Step {
        object: Object,
        callback: Step,
    },
}

/// One indexed callback, with a real cursor into its object.
pub type Indexed = unsafe extern "C" fn(*mut u8, usize);

/// The result of one incremental destruction step.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StepStatus {
    Pending,
    Complete,
}

/// Performs one step using an object pointer as its state.
pub type Step = unsafe extern "C" fn(*mut c_void) -> StepStatus;

/// Owns pending destruction across nested scopes. No borrow of `pending`
/// remains held while a release callback runs, so callbacks can enqueue work.
#[derive(Default)]
pub struct DropContext {
    pending: RefCell<Pending>,
    active_run: ActiveRun,
    /// First pending-work slot belonging to the current nested scope.
    boundary: Cell<usize>,
}

/// The current group stays outside the work vector so appending an object
/// does not push a whole run. Separate cells allow updating only changed fields.
/// A missing head means the other fields are inactive; otherwise `release`
/// is present and `len` is nonzero.
#[derive(Default)]
struct ActiveRun {
    head: Cell<Option<Object>>,
    release: Cell<Option<Release>>,
    len: Cell<usize>,
}
impl ActiveRun {
    /// Suspend the current group, marking the active slot empty.
    fn take(&self) -> Option<PendingRun> {
        self.head.take().map(|head| PendingRun {
            head,
            release: self.release.get().unwrap(),
            len: NonZeroUsize::new(self.len.get()).unwrap(),
        })
    }
    /// Resume a suspended group as the current work.
    fn set(&self, run: PendingRun) {
        self.head.set(Some(run.head));
        self.release.set(Some(run.release));
        self.len.set(run.len.get());
    }
}

/// Suspended groups and their object pointers are separate LIFO stacks,
/// allowing each group to store its callback only once.
#[derive(Default)]
struct Pending {
    work: Vec<Work>,
    cells: Vec<Object>,
}

impl DropContext {
    /// Preserve the active group before newer work takes its place.
    fn spill(&self) {
        if let Some(run) = self.active_run.take() {
            self.pending.borrow_mut().work.push(Work::Run(run));
        }
    }

    /// Resume only work owned by this scope, leaving outer scopes suspended.
    fn restore(&self) {
        let mut pending = self.pending.borrow_mut();
        if pending.work.len() > self.boundary.get()
            && let Some(Work::Run(run)) = pending.work.last().copied()
        {
            pending.work.pop();
            self.active_run.set(run);
        }
    }

    /// Suspend the previous group and start one for this callback.
    fn start_run(&self, object: Object, release: Release) {
        self.spill();
        self.active_run.set(PendingRun {
            head: object,
            release,
            len: NonZeroUsize::new(1).unwrap(),
        });
    }

    /// Consecutive objects with the same callback share a run; only their
    /// pointers are pushed, with the newest object kept in the active slot.
    fn defer(&self, object: Object, release: Release) {
        let active = &self.active_run;
        if let Some(head) = active.head.get()
            && core::ptr::fn_addr_eq(active.release.get().unwrap(), release)
        {
            self.pending.borrow_mut().cells.push(head);
            active.head.set(Some(object));
            active.len.set(active.len.get() + 1);
            return;
        }
        self.start_run(object, release);
    }

    /// Advance the pending state before invoking each callback so work it
    /// enqueues runs before older objects, without retaining a RefCell borrow.
    fn drain(&self) {
        loop {
            let active = &self.active_run;
            if let Some(head) = active.head.get() {
                let release = active.release.get().unwrap();
                let remaining = active.len.get() - 1;
                if remaining == 0 {
                    active.head.set(None);
                    self.restore();
                } else {
                    let mut pending = self.pending.borrow_mut();
                    let next = pending.cells.pop().unwrap();
                    active.head.set(Some(next));
                    active.len.set(remaining);
                }
                unsafe { release(head.as_ptr()) };
                continue;
            }
            let (work, position) = {
                let pending = self.pending.borrow();
                if pending.work.len() == self.boundary.get() {
                    return;
                }
                (*pending.work.last().unwrap(), pending.work.len() - 1)
            };
            match work {
                Work::Run(_) => unreachable!(),
                Work::Indexed {
                    object,
                    cursor,
                    callback,
                } => {
                    self.pending.borrow_mut().work.pop();
                    self.restore();
                    unsafe { callback(object.as_ptr(), cursor) };
                }
                Work::Step { object, callback } => {
                    if unsafe { callback(object.0.as_ptr()) } == StepStatus::Complete {
                        self.pending.borrow_mut().work.remove(position);
                        if self.active_run.head.get().is_none() {
                            self.restore();
                        }
                    }
                }
            }
        }
    }
}

// Borrows a fresh stack context when a later TLS destructor runs after CACHED.
scoped_tls::scoped_thread_local!(static LATE_CONTEXT: DropContext);
thread_local! {
    static ACTIVE: Cell<bool> = const { Cell::new(false) };
    static CACHED: DropContext = DropContext::default();
}

/// Select the cached context or the temporary context used during TLS teardown.
fn with_context<R>(callback: impl FnOnce(&DropContext) -> R) -> R {
    let mut callback = Some(callback);
    CACHED
        .try_with(|context| callback.take().unwrap()(context))
        .unwrap_or_else(|_| LATE_CONTEXT.with(callback.unwrap()))
}

/// Nested public drops finish their own suffix before resuming the outer run.
fn scoped(walk: impl FnOnce()) {
    if ACTIVE.get() {
        with_context(|context| {
            context.spill();
            let previous = context
                .boundary
                .replace(context.pending.borrow().work.len());
            walk();
            context.drain();
            context.boundary.set(previous);
            context.restore();
        });
        return;
    }
    let mut walk = Some(walk);
    match CACHED.try_with(|context| execute(context, walk.take().unwrap())) {
        Ok(()) => (),
        // A late TLS destructor owns a fresh context for this scope.
        Err(_) => execute_fresh(walk.take().unwrap()),
    };
}

/// Own a context for one drop after the thread cache has been destroyed.
#[cold]
#[inline(never)]
fn execute_fresh(walk: impl FnOnce()) {
    let context = DropContext::default();
    LATE_CONTEXT.set(&context, || execute(&context, walk))
}

/// Drain while active, then retain bounded buffer capacity for the next scope.
#[inline]
fn execute(context: &DropContext, walk: impl FnOnce()) {
    {
        let _active = scopeguard::guard(ACTIVE.replace(true), |previous| ACTIVE.set(previous));
        walk();
        context.drain();
    }
    let mut context = context.pending.borrow_mut();
    if context.work.capacity() > RETAINED_BUFFER_CAPACITY {
        context.work.shrink_to(RETAINED_BUFFER_CAPACITY);
    }
    if context.cells.capacity() > RETAINED_BUFFER_CAPACITY {
        context.cells.shrink_to(RETAINED_BUFFER_CAPACITY);
    }
    debug_assert!(context.work.is_empty() && context.cells.is_empty());
}

/// Completes a release and its pending work. Nested calls complete their own
/// suffix while leaving earlier work pending.
///
/// Safety: the non-null object must be valid for `release`.
pub unsafe fn run(object: *mut u8, release: Release) {
    scoped(|| unsafe { release(object) });
}

/// Defers a release without changing its object's storage.
///
/// Safety: the non-null object must remain valid for `release` until it runs.
pub unsafe fn defer(object: *mut u8, release: Release) {
    if ACTIVE.get() {
        with_context(|context| context.defer(unsafe { Object::new(object) }, release));
    } else {
        unsafe { run(object, release) };
    }
}

/// Whether this thread has an active destruction scope.
pub fn releasing() -> bool {
    ACTIVE.get()
}

/// Number of pending entries, including the currently executing step.
pub fn depth() -> usize {
    if ACTIVE.get() {
        with_context(|context| {
            context.pending.borrow().work.len()
                + usize::from(context.active_run.head.get().is_some())
        })
    } else {
        0
    }
}

/// Defers an indexed callback without allocating a cursor object.
/// Safety: the non-null object must remain valid for its callback.
pub unsafe fn defer_indexed(object: *mut u8, cursor: usize, callback: Indexed) {
    let enqueue = || {
        with_context(|context| {
            context.spill();
            context.pending.borrow_mut().work.push(Work::Indexed {
                object: unsafe { Object::new(object) },
                cursor,
                callback,
            })
        })
    };
    if ACTIVE.get() {
        enqueue();
    } else {
        scoped(enqueue);
    }
}

/// Runs an incremental operation and its pending work before returning.
/// Safety: the non-null object must remain valid until its step completes.
pub unsafe fn run_step(object: *mut c_void, callback: Step) {
    scoped(|| unsafe { defer_step(object, callback) });
}

/// Defers a step; outside a drain, completes it before returning.
/// Safety: as [`run_step`].
pub unsafe fn defer_step(object: *mut c_void, callback: Step) {
    let enqueue = || {
        with_context(|context| {
            context.spill();
            context.pending.borrow_mut().work.push(Work::Step {
                object: unsafe { Object::new(object.cast()) },
                callback,
            })
        })
    };
    if ACTIVE.get() {
        enqueue();
    } else {
        scoped(enqueue);
    }
}

/// C entry point of [`defer_indexed`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer_indexed(
    object: *mut u8,
    cursor: usize,
    callback: Indexed,
) {
    unsafe { defer_indexed(object, cursor, callback) }
}

/// C entry point of [`run`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_run(object: *mut u8, release: Release) {
    unsafe { run(object, release) }
}

/// C entry point of [`defer`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer(object: *mut u8, release: Release) {
    unsafe { defer(object, release) }
}

#[cfg(test)]
mod tests;
