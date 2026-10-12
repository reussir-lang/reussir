use core::{alloc::Layout, ffi::c_void, mem::MaybeUninit};
use smallvec::SmallVec;
use stumpalo::Arena;

pub type DtorStart = unsafe extern "C" fn(*mut c_void, *mut c_void, *mut DtorStep);
pub type DtorResume = unsafe extern "C" fn(*mut c_void, bool, *mut DtorStep);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DtorRoutine {
    pub start: DtorStart,
    pub size: usize,
    pub align: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DtorTask {
    pub routine: *const DtorRoutine,
    pub object: *mut c_void,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DtorStep {
    pub resume: Option<DtorResume>,
    pub child: *const DtorTask,
}

struct Continuation {
    buffer: *mut c_void,
    resume: DtorResume,
}

pub struct DtorExecutor {
    arena: Arena,
    allocations: usize,
    continuations: SmallVec<[Continuation; 8]>,
    running: bool,
}

impl DtorExecutor {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            allocations: 0,
            continuations: SmallVec::new(),
            running: false,
        }
    }

    fn allocate_buffer(&self, size: usize, align: usize) -> *mut c_void {
        let layout =
            Layout::from_size_align(size.max(1), align).expect("invalid continuation layout");
        self.arena.alloc_layout(layout).as_ptr().cast()
    }

    fn allocate(&mut self, size: usize, align: usize) -> *mut c_void {
        let buffer = self.allocate_buffer(size, align);
        self.allocations += 1;
        buffer
    }

    fn deallocate(&mut self) {
        self.allocations = self
            .allocations
            .checked_sub(1)
            .expect("empty destructor allocation stack");
    }
}

impl Default for DtorExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for DtorExecutor {
    fn drop(&mut self) {
        assert!(
            !self.running && self.allocations == 0 && self.continuations.is_empty(),
            "destructor executor is active"
        );
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __reussir_dtor_executor_size() -> usize {
    size_of::<DtorExecutor>()
}
#[unsafe(no_mangle)]
pub extern "C" fn __reussir_dtor_executor_align() -> usize {
    align_of::<DtorExecutor>()
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_create_dtor_executor(storage: *mut c_void) -> *mut DtorExecutor {
    unsafe { (*storage.cast::<MaybeUninit<DtorExecutor>>()).write(DtorExecutor::new()) }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_destroy_dtor_executor(context: *mut DtorExecutor) {
    unsafe { (*context.cast::<MaybeUninit<DtorExecutor>>()).assume_init_drop() };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_dtor_conti_alloc(
    context: *mut DtorExecutor,
    size: usize,
    align: usize,
) -> *mut c_void {
    unsafe {
        (*context.cast::<MaybeUninit<DtorExecutor>>())
            .assume_init_mut()
            .allocate(size, align)
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_dtor_conti_free(context: *mut DtorExecutor) {
    unsafe {
        (*context.cast::<MaybeUninit<DtorExecutor>>())
            .assume_init_mut()
            .deallocate()
    };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_run_dtor(
    context: *mut DtorExecutor,
    object: *mut c_void,
    routine: *const DtorRoutine,
) {
    {
        let executor = unsafe { (*context.cast::<MaybeUninit<DtorExecutor>>()).assume_init_mut() };
        assert!(
            !executor.running && executor.allocations == 0 && executor.continuations.is_empty(),
            "destructor executor is active"
        );
        executor.running = true;
    }
    let mut continuations = unsafe { core::mem::take(&mut (*context).continuations) };
    let mut task = DtorTask { routine, object };
    loop {
        let routine = unsafe { *task.routine };
        let buffer = unsafe { (*context).allocate_buffer(routine.size, routine.align) };
        let mut result = MaybeUninit::<DtorStep>::uninit();
        unsafe { (routine.start)(buffer, task.object, result.as_mut_ptr()) };
        let mut step = unsafe { result.assume_init() };
        let mut buffer = buffer;
        loop {
            if let Some(resume) = step.resume {
                assert!(
                    !step.child.is_null(),
                    "suspended destructor has no child task"
                );
                unsafe {
                    task = *step.child;
                    continuations.push(Continuation { buffer, resume });
                }
                break;
            }
            let Some(continuation) = continuations.pop() else {
                unsafe {
                    let executor = &mut *context;
                    assert_eq!(executor.allocations, 0, "destructor allocations are active");
                    executor.continuations = continuations;
                    executor.arena = Arena::new();
                    executor.running = false;
                }
                return;
            };
            buffer = continuation.buffer;
            unsafe { (continuation.resume)(buffer, false, result.as_mut_ptr()) };
            step = unsafe { result.assume_init() };
        }
    }
}

#[cfg(test)]
mod tests;
