use core::{ffi::c_void, mem::MaybeUninit, ptr};
use stumpalo::Arena;

pub const DTOR_CONTI_SIZE: usize = 32;
pub const DTOR_CONTI_ALIGN: usize = 8;
pub type DtorStart = unsafe extern "C" fn(*mut c_void, *mut c_void) -> DtorStep;
pub type DtorResume = unsafe extern "C" fn(*mut c_void, bool) -> DtorStep;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DtorTask {
    pub start: DtorStart,
    pub object: *mut c_void,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DtorStep {
    pub resume: Option<DtorResume>,
    pub child: *const DtorTask,
}

#[repr(C, align(8))]
struct Frame {
    buffer: [MaybeUninit<u8>; DTOR_CONTI_SIZE],
    resume: Option<DtorResume>,
    link: *mut Frame,
}

pub struct DtorExecutor {
    arena: Arena,
    top: *mut Frame,
    free: *mut Frame,
    running: bool,
}

impl DtorExecutor {
    pub fn new() -> Self {
        Self {
            arena: Arena::with_chunk(1024),
            top: ptr::null_mut(),
            free: ptr::null_mut(),
            running: false,
        }
    }

    unsafe fn push(&mut self) -> *mut c_void {
        let frame = if self.free.is_null() {
            self.arena.alloc(Frame {
                buffer: [MaybeUninit::uninit(); DTOR_CONTI_SIZE],
                resume: None,
                link: ptr::null_mut(),
            }) as *mut Frame
        } else {
            let frame = self.free;
            self.free = unsafe { (*frame).link };
            frame
        };
        unsafe { (*frame).link = self.top };
        self.top = frame;
        unsafe { ptr::addr_of_mut!((*frame).buffer).cast() }
    }

    unsafe fn pop(&mut self) {
        let frame = self.top;
        assert!(!frame.is_null(), "empty destructor continuation stack");
        unsafe {
            self.top = (*frame).link;
            (*frame).link = self.free;
        }
        self.free = frame;
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
            !self.running && self.top.is_null(),
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
pub unsafe extern "C" fn __reussir_dtor_conti_alloc(context: *mut DtorExecutor) -> *mut c_void {
    unsafe {
        (*context.cast::<MaybeUninit<DtorExecutor>>())
            .assume_init_mut()
            .push()
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_dtor_conti_free(context: *mut DtorExecutor) {
    unsafe {
        (*context.cast::<MaybeUninit<DtorExecutor>>())
            .assume_init_mut()
            .pop()
    };
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_run_dtor(
    context: *mut DtorExecutor,
    object: *mut c_void,
    start: DtorStart,
) {
    {
        let executor = unsafe { (*context.cast::<MaybeUninit<DtorExecutor>>()).assume_init_mut() };
        assert!(
            !executor.running && executor.top.is_null(),
            "destructor executor is active"
        );
        executor.running = true;
    }
    let mut task = DtorTask { start, object };
    loop {
        let buffer = unsafe { __reussir_dtor_conti_alloc(context) };
        let mut step = unsafe { (task.start)(buffer, task.object) };
        loop {
            if let Some(resume) = step.resume {
                assert!(
                    !step.child.is_null(),
                    "suspended destructor has no child task"
                );
                unsafe {
                    (*(*context).top).resume = Some(resume);
                    task = *step.child;
                }
                break;
            }
            unsafe { __reussir_dtor_conti_free(context) };
            let top = unsafe { (*context).top };
            if top.is_null() {
                unsafe { (*context).running = false };
                return;
            }
            let resume = unsafe { (*top).resume.unwrap() };
            step = unsafe { resume(ptr::addr_of_mut!((*top).buffer).cast(), false) };
        }
    }
}

#[cfg(test)]
mod tests;
