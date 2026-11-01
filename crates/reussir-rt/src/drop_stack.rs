//! Scoped worklists shared by generated drop glue and foreign destructors.
use core::{ffi::c_void, num::NonZeroUsize, ptr::NonNull};
use smallvec::SmallVec;
use std::cell::RefCell;

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
        let task = ReleaseTask {
            object: run.head,
            release: run.release,
        };
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
    static CACHED: RefCell<DropContext> = RefCell::new(DropContext::default());
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
    let mut walk = Some(walk);
    if CACHED
        .try_with(|context| execute(context, walk.take().unwrap()))
        .is_err()
    {
        execute(&RefCell::new(DropContext::default()), walk.take().unwrap());
    }
}

fn execute(context: &RefCell<DropContext>, walk: impl FnOnce()) {
    ACTIVE.set(context, || {
        walk();
        drain_pending();
    });
    let mut context = context.borrow_mut();
    if context.work.capacity() > 64 {
        context.work.shrink_to_fit();
    }
    if context.cells.capacity() > 64 {
        context.cells.shrink_to_fit();
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
    if ACTIVE.is_set() {
        ACTIVE.with(|context| {
            context
                .borrow_mut()
                .defer(unsafe { Object::new(object) }, release)
        });
    } else {
        unsafe { run(object, release) };
    }
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
