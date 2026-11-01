//! Scoped worklists shared by generated drop glue and foreign destructors.
use core::{ffi::c_void, num::NonZeroUsize, ptr::NonNull};
use smallvec::SmallVec;
use std::cell::{Cell, RefCell};

mod wide;
use wide::{ReleaseTable, WideHeader};

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

#[derive(Clone, Copy)]
struct PendingRun {
    head: Object,
    release: Release,
    len: NonZeroUsize,
    storage: RunStorage,
}

#[derive(Clone, Copy)]
enum RunStorage {
    Cells,
    Wide,
}

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

enum Action {
    Release(ReleaseTask),
    Indexed {
        object: Object,
        cursor: usize,
        callback: Indexed,
    },
    Step {
        object: Object,
        callback: Step,
        position: usize,
    },
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
    end_work: Option<EndWork>,
    releases: ReleaseTable,
}

impl DropContext {
    fn defer(&mut self, object: Object, release: Release) {
        if self.work.len() > self.boundary
            && let Some(Work::Run(run)) = self.work.last_mut()
            && matches!(run.storage, RunStorage::Cells)
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
            storage: RunStorage::Cells,
        }));
    }

    unsafe fn defer_wide(&mut self, object: Object, release: Release) {
        if self.work.len() > self.boundary
            && let Some(Work::Run(run)) = self.work.last_mut()
            && matches!(run.storage, RunStorage::Wide)
        {
            let header = unsafe { WideHeader::new(object) };
            let joined = if core::ptr::fn_addr_eq(run.release, release) {
                self.cells.push(run.head);
                unsafe { header.mark_cell() };
                true
            } else {
                self.releases
                    .index(run.release)
                    .is_some_and(|index| unsafe { header.link(run.head, index) })
            };
            if joined {
                run.head = object;
                run.release = release;
                run.len = NonZeroUsize::new(run.len.get() + 1).unwrap();
                return;
            }
        }
        self.work.push(Work::Run(PendingRun {
            head: object,
            release,
            len: NonZeroUsize::new(1).unwrap(),
            storage: RunStorage::Wide,
        }));
    }

    fn next(&mut self) -> Option<Action> {
        if self.work.len() == self.boundary {
            return None;
        }
        match *self.work.last()? {
            Work::Run(run) => {
                let task = ReleaseTask {
                    object: run.head,
                    release: run.release,
                };
                if run.len.get() == 1 {
                    self.work.pop();
                } else {
                    let Work::Run(top) = self.work.last_mut().unwrap() else {
                        unreachable!()
                    };
                    top.head = match run.storage {
                        RunStorage::Cells => self.cells.pop().unwrap(),
                        RunStorage::Wide => {
                            match unsafe { WideHeader::new(run.head).take_link() } {
                                Some(link) => {
                                    top.release = self.releases.get(link.release);
                                    link.previous
                                }
                                None => self.cells.pop().unwrap(),
                            }
                        }
                    };
                    top.len = NonZeroUsize::new(run.len.get() - 1).unwrap();
                }
                Some(Action::Release(task))
            }
            Work::Indexed {
                object,
                cursor,
                callback,
            } => {
                self.work.pop();
                Some(Action::Indexed {
                    object,
                    cursor,
                    callback,
                })
            }
            Work::Step { object, callback } => Some(Action::Step {
                object,
                callback,
                position: self.work.len() - 1,
            }),
        }
    }
}

scoped_tls::scoped_thread_local!(static ACTIVE: RefCell<DropContext>);
thread_local! {
    static CACHED: RefCell<DropContext> = RefCell::new(DropContext::default());
    static END_WORK: Cell<Option<EndWork>> = const { Cell::new(None) };
}

fn drain_pending() {
    while let Some(action) = ACTIVE.with(|context| context.borrow_mut().next()) {
        match action {
            Action::Release(task) => unsafe { (task.release)(task.object.as_ptr()) },
            Action::Indexed {
                object,
                cursor,
                callback,
            } => unsafe { callback(object.as_ptr(), cursor) },
            Action::Step {
                object,
                callback,
                position,
            } => {
                if unsafe { callback(object.0.as_ptr()) } == StepStatus::Complete {
                    ACTIVE.with(|context| {
                        context.borrow_mut().work.remove(position);
                    });
                }
            }
        }
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
    let end_work = match CACHED.try_with(|context| execute(context, walk.take().unwrap())) {
        Ok(end_work) => end_work,
        // A late TLS destructor owns a fresh context for this scope.
        Err(_) => execute(&RefCell::new(DropContext::default()), walk.take().unwrap()),
    };
    if let Some(callback) = end_work {
        unsafe { callback() };
    }
}

fn execute(context: &RefCell<DropContext>, walk: impl FnOnce()) -> Option<EndWork> {
    ACTIVE.set(context, || {
        walk();
        drain_pending();
    });
    let mut context = context.borrow_mut();
    let end_work = context.end_work.take();
    context.releases.clear();
    if context.work.capacity() > 64 {
        context.work.shrink_to_fit();
    }
    if context.cells.capacity() > 64 {
        context.cells.shrink_to_fit();
    }
    debug_assert!(context.work.is_empty() && context.cells.is_empty());
    end_work
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
        ACTIVE.with(|context| {
            context
                .borrow_mut()
                .defer(unsafe { Object::new(object) }, release)
        });
    } else {
        unsafe { run(object, release) };
    }
}

/// Work that runs after the outermost scope releases its context.
pub type EndWork = unsafe extern "C" fn();

/// Registers this thread's host end callback.
pub fn set_end_work(callback: EndWork) {
    END_WORK.set(Some(callback));
}

/// Requests the registered callback after the outermost drain, or now when idle.
pub fn want_end_work() {
    let callback = END_WORK.get();
    if ACTIVE.is_set() {
        ACTIVE.with(|context| context.borrow_mut().end_work = callback);
    } else if let Some(callback) = callback {
        unsafe { callback() };
    }
}

/// Whether this thread has an active destruction scope.
pub fn releasing() -> bool {
    ACTIVE.is_set()
}

/// Number of pending entries, including the currently executing step.
pub fn depth() -> usize {
    if ACTIVE.is_set() {
        ACTIVE.with(|context| context.borrow().work.len())
    } else {
        0
    }
}

/// Defers an indexed callback without allocating a cursor object.
/// Safety: the non-null object must remain valid for its callback.
pub unsafe fn defer_indexed(object: *mut u8, cursor: usize, callback: Indexed) {
    let enqueue = || {
        ACTIVE.with(|context| {
            context.borrow_mut().work.push(Work::Indexed {
                object: unsafe { Object::new(object) },
                cursor,
                callback,
            })
        })
    };
    if ACTIVE.is_set() {
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
        ACTIVE.with(|context| {
            context.borrow_mut().work.push(Work::Step {
                object: unsafe { Object::new(object.cast()) },
                callback,
            })
        })
    };
    if ACTIVE.is_set() {
        enqueue();
    } else {
        scoped(enqueue);
    }
}

/// Runs incremental host work, joining an existing drain when present.
/// Safety: as [`run_step`].
pub unsafe fn join_or_run_step(object: *mut c_void, callback: Step) {
    unsafe { defer_step(object, callback) };
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

/// Defers an exclusively owned dead box with a count of zero or one and
/// padding or a tag below 2^16 in its first eight bytes. Encoded links are
/// restored before release; the callback must not depend on the count.
/// Safety: as [`defer`]; the box is four-byte aligned and nobody accesses it
/// before its release. Header bytes outside the tag may be used while pending.
pub unsafe fn defer_wide(object: *mut u8, release: Release) {
    if ACTIVE.is_set() {
        ACTIVE.with(|context| unsafe {
            context
                .borrow_mut()
                .defer_wide(Object::new(object), release)
        });
    } else {
        unsafe { run(object, release) };
    }
}

/// C entry point of [`defer_wide`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer_wide(object: *mut u8, release: Release) {
    unsafe { defer_wide(object, release) }
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
