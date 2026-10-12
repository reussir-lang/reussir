use crate::alloc::{__reussir_allocate, __reussir_dealloc_unsized};
use core::{alloc::Layout, ffi::c_void, mem::MaybeUninit};
use smallvec::SmallVec;

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
    allocations: SmallVec<[*mut u8; 8]>,
    continuations: SmallVec<[Continuation; 8]>,
    running: bool,
}

impl DtorExecutor {
    pub fn new() -> Self {
        Self {
            allocations: SmallVec::new(),
            continuations: SmallVec::new(),
            running: false,
        }
    }

    fn allocate(&mut self, size: usize, align: usize) -> *mut c_void {
        let layout = Layout::from_size_align(size, align).expect("invalid continuation layout");
        let buffer = unsafe { __reussir_allocate(layout.align(), layout.size().max(1)) };
        self.allocations.push(buffer);
        buffer.cast()
    }

    fn deallocate(&mut self) {
        let buffer = self
            .allocations
            .pop()
            .expect("empty destructor allocation stack");
        unsafe { __reussir_dealloc_unsized(buffer) };
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
            !self.running && self.allocations.is_empty() && self.continuations.is_empty(),
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
            !executor.running
                && executor.allocations.is_empty()
                && executor.continuations.is_empty(),
            "destructor executor is active"
        );
        executor.running = true;
    }
    let mut task = DtorTask { routine, object };
    loop {
        let routine = unsafe { *task.routine };
        let buffer = unsafe { (*context).allocate(routine.size, routine.align) };
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
                    (*context)
                        .continuations
                        .push(Continuation { buffer, resume });
                }
                break;
            }
            unsafe { (*context).deallocate() };
            let continuation = unsafe { (*context).continuations.pop() };
            let Some(continuation) = continuation else {
                unsafe { (*context).running = false };
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
