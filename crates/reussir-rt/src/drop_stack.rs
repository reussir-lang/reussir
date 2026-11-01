//! Scoped worklists shared by generated drop glue and foreign destructors.
use core::{ffi::c_void, num::NonZeroUsize, ptr::NonNull};
use std::cell::RefCell;
use smallvec::SmallVec;

/// Releases one deferred object.
pub type Release = unsafe extern "C" fn(*mut u8);

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

struct PendingRun {
    head: Object,
    release: Release,
    len: NonZeroUsize,
}

enum Work {
    Run(PendingRun),
}

struct ReleaseTask {
    object: Object,
    release: Release,
}

/// Owns pending work; callbacks access it only through the scoped adapter.
#[derive(Default)]
pub struct DropContext {
    work: SmallVec<[Work; 4]>,
    cells: SmallVec<[Object; 8]>,
    boundary: usize,
}

impl DropContext {
    fn defer(&mut self, object: Object, release: Release) {
        if self.work.len() > self.boundary
            && let Some(Work::Run(run)) = self.work.last_mut()
            && core::ptr::fn_addr_eq(run.release, release)
        {
            self.cells.push(run.head);
            run.head = object;
            run.len = NonZeroUsize::new(run.len.get() + 1).unwrap();
            return;
        }
        self.work.push(Work::Run(PendingRun {
            head: object,
            release,
            len: NonZeroUsize::new(1).unwrap(),
        }));
    }

    fn next(&mut self) -> Option<ReleaseTask> {
        if self.work.len() == self.boundary {
            return None;
        }
        let Work::Run(run) = self.work.last_mut()?;
        let task = ReleaseTask { object: run.head, release: run.release };
        if run.len.get() == 1 {
            self.work.pop();
        } else {
            run.head = self.cells.pop().unwrap();
            run.len = NonZeroUsize::new(run.len.get() - 1).unwrap();
        }
        Some(task)
    }
}

scoped_tls::scoped_thread_local!(static ACTIVE: RefCell<DropContext>);
thread_local! {
    static SPARE: RefCell<Option<DropContext>> = const { RefCell::new(None) };
}

fn drain_pending() {
    while let Some(task) = ACTIVE.with(|context| context.borrow_mut().next()) {
        unsafe { (task.release)(task.object.as_ptr()) };
    }
}

fn scoped(walk: impl FnOnce()) {
    if ACTIVE.is_set() {
        let previous = ACTIVE.with(|context| {
            let mut context = context.borrow_mut();
            let boundary = context.work.len();
            core::mem::replace(&mut context.boundary, boundary)
        });
        walk();
        drain_pending();
        ACTIVE.with(|context| context.borrow_mut().boundary = previous);
        return;
    }
    let context = SPARE.try_with(|slot| slot.borrow_mut().take())
        .ok().flatten().unwrap_or_default();
    let context = RefCell::new(context);
    ACTIVE.set(&context, || {
        walk();
        drain_pending();
    });
    let context = context.into_inner();
    debug_assert!(context.cells.is_empty());
    // A late TLS destructor owns and frees its context when the spare is gone.
    let _ = SPARE.try_with(|slot| *slot.borrow_mut() = Some(context));
}

/// Completes a release and its pending work. Nested calls complete their own
/// suffix while leaving earlier work pending.
///
/// Safety: the non-null object must be valid for `release`.
pub unsafe fn run(object: *mut u8, release: Release) {
    scoped(|| unsafe { release(object) });
}

/// Joins the running drain, or starts one. Intended for internal foreign bridges.
///
/// Safety: as [`run`].
pub unsafe fn join_or_run(object: *mut u8, release: Release) {
    if ACTIVE.is_set() {
        unsafe { release(object) };
    } else {
        unsafe { run(object, release) };
    }
}

/// Defers a release without changing its object's storage.
///
/// Safety: the non-null object must remain valid for `release` until it runs.
pub unsafe fn defer(object: *mut u8, release: Release) {
    if ACTIVE.is_set() {
        ACTIVE.with(|context| context.borrow_mut().defer(unsafe { Object::new(object) }, release));
    } else {
        unsafe { run(object, release) };
    }
}

/// C entry point of [`run`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_run(object: *mut u8, release: Release) {
    unsafe { run(object, release) }
}

/// C entry point of [`join_or_run`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_join_or_run(object: *mut u8, release: Release) {
    unsafe { join_or_run(object, release) }
}

/// C entry point of [`defer`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer(object: *mut u8, release: Release) {
    unsafe { defer(object, release) }
}

#[cfg(test)]
mod tests;
