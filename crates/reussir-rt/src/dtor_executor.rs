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
    pub resume: *const DtorResumeAction,
    pub child: *const DtorTask,
}

/// A continuation either invokes a callback or frees a pointer loaded from its
/// frame. The offset is used only when `callback` is `None`; the load and free
/// occur after the child finishes, preserving destructor order and mutations.
#[repr(C)]
pub struct DtorResumeAction {
    pub callback: Option<DtorResume>,
    pub offset: usize,
}

struct Continuation {
    buffer: *mut c_void,
    resume: *const DtorResumeAction,
}

struct FrameBuffer {
    pointer: *mut c_void,
    size: usize,
    align: usize,
}

#[derive(Default)]
struct FrameBuffers(SmallVec<[FrameBuffer; 8]>);

impl FrameBuffers {
    fn get(&mut self, arena: &Arena, depth: usize, size: usize, align: usize) -> *mut c_void {
        // Completed siblings share a slot. Suspended ancestors keep their stable
        // arena addresses even when the slot vector moves or grows.
        if depth == self.0.len() {
            self.0.push(FrameBuffer {
                pointer: core::ptr::null_mut(),
                size: 0,
                align: 1,
            });
        }
        let slot = &mut self.0[depth];
        if slot.pointer.is_null() || slot.size < size || slot.align < align {
            // Keep both dimensions monotone: alternating large and over-aligned
            // siblings must not repeatedly replace the same slot.
            slot.size = slot.size.max(size);
            slot.align = slot.align.max(align);
            let layout = Layout::from_size_align(slot.size.max(1), slot.align)
                .expect("invalid continuation layout");
            slot.pointer = arena.alloc_layout(layout).as_ptr().cast();
        }
        slot.pointer
    }
}

pub struct DtorExecutor {
    arena: Arena,
    allocations: usize,
    running: bool,
}

impl DtorExecutor {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            allocations: 0,
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
            !self.running && self.allocations == 0,
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
            !executor.running && executor.allocations == 0,
            "destructor executor is active"
        );
        executor.running = true;
    }
    unsafe { drain(context, DtorTask { routine, object }, None) };
}

/// Finish a root whose first step already suspended, without restarting it.
///
/// # Safety
/// `resume` must point to a valid action for the duration of this call.
/// `buffer` must remain live and suitably aligned for that action until this call
/// returns. `child` must describe the yielded task and remain valid until copied.
/// The root must obey the same child/continuation lifetime rules as `run_dtor`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_finish_dtor(
    buffer: *mut c_void,
    resume: *const DtorResumeAction,
    child: *const DtorTask,
) {
    assert!(!child.is_null(), "suspended destructor has no child task");
    let mut executor = DtorExecutor::new();
    executor.running = true;
    unsafe { drain(&mut executor, *child, Some(Continuation { buffer, resume })) };
}

unsafe fn drain(context: *mut DtorExecutor, mut task: DtorTask, root: Option<Continuation>) {
    // Keep inline storage in this drain; moving an empty SmallVec still copies it.
    let mut continuations: SmallVec<[Continuation; 8]> = SmallVec::new();
    let mut buffers = FrameBuffers::default();
    if let Some(root) = root {
        // Slot zero belongs to the caller. It is only resumed, never allocated
        // or recycled; children start at depth one.
        buffers.0.push(FrameBuffer {
            pointer: root.buffer,
            size: 0,
            align: 1,
        });
        continuations.push(root);
    }
    loop {
        let routine = unsafe { *task.routine };
        let buffer = buffers.get(
            unsafe { &(*context).arena },
            continuations.len(),
            routine.size,
            routine.align,
        );
        let mut result = MaybeUninit::<DtorStep>::uninit();
        unsafe { (routine.start)(buffer, task.object, result.as_mut_ptr()) };
        let mut step = unsafe { result.assume_init() };
        let mut buffer = buffer;
        loop {
            if !step.resume.is_null() {
                let resume = step.resume;
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
                    executor.arena = Arena::new();
                    executor.running = false;
                }
                return;
            };
            buffer = continuation.buffer;
            let action = unsafe { &*continuation.resume };
            if let Some(resume) = action.callback {
                unsafe { resume(buffer, false, result.as_mut_ptr()) };
                step = unsafe { result.assume_init() };
            } else {
                // Read the payload only after the child completes, as the
                // replaced callback would; do not move observable frees.
                let object = unsafe {
                    buffer
                        .byte_add(action.offset)
                        .cast::<*mut c_void>()
                        .read_unaligned()
                };
                unsafe { crate::alloc::__reussir_dealloc_unsized(object.cast()) };
                step = DtorStep {
                    resume: core::ptr::null(),
                    child: core::ptr::null(),
                };
            }
        }
    }
}

#[cfg(test)]
mod tests;
