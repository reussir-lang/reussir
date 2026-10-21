use super::*;
use core::ptr;

// Keep native stack-stress coverage while making provenance checks practical
// under interpretation. Miri still exercises continuation table growth.
const DEEP_DEPTH: usize = if cfg!(miri) { 64 } else { 100_000 };
const BRANCH_DEPTH: usize = if cfg!(miri) { 5 } else { 12 };
const LAZY_BRANCH_DEPTH: usize = if cfg!(miri) { 5 } else { 16 };

#[repr(C)]
struct State {
    args: (usize, *mut Vec<usize>),
    value: usize,
    task: DtorTask,
}

const SMALL: DtorRoutine = DtorRoutine {
    start,
    size: size_of::<State>(),
    align: align_of::<State>(),
};
const LARGE: DtorRoutine = DtorRoutine {
    start: large_start,
    size: size_of::<State>() + 513,
    align: 256,
};

unsafe extern "C" fn resume(
    buffer: *mut c_void,
    cancel: bool,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    assert!(!cancel);
    let state = unsafe { &*buffer.cast::<State>() };
    unsafe { (*state.args.1).push(state.value) };
    unsafe { complete_or_yield(output_step, ptr::null(), ptr::null()) }
}
unsafe extern "C" fn start(
    buffer: *mut c_void,
    object: *mut c_void,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    let (n, output) = unsafe { *object.cast::<(usize, *mut Vec<usize>)>() };
    if n == 0 {
        unsafe { (*output).push(0) };
        return unsafe { complete_or_yield(output_step, ptr::null(), ptr::null()) };
    }
    let state = buffer.cast::<State>();
    unsafe {
        state.write(State {
            args: (n - 1, output),
            value: n,
            task: DtorTask {
                routine: if n % 2 == 0 { &LARGE } else { &SMALL },
                object: ptr::addr_of_mut!((*state).args).cast(),
            },
        });
    }
    unsafe { complete_or_yield(output_step, &RESUME_ACTION, ptr::addr_of!((*state).task)) }
}
unsafe extern "C" fn large_start(
    buffer: *mut c_void,
    object: *mut c_void,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    assert_eq!(buffer as usize % LARGE.align, 0);
    unsafe {
        buffer
            .byte_add(size_of::<State>())
            .cast::<[u8; 513]>()
            .write([0xa5; 513])
    };
    let resume = unsafe { start(buffer, object, output_step) };
    if !resume.is_null() {
        &LARGE_RESUME_ACTION
    } else {
        resume
    }
}
unsafe extern "C" fn large_resume(
    buffer: *mut c_void,
    cancel: bool,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    assert_eq!(
        unsafe { *buffer.byte_add(size_of::<State>()).cast::<[u8; 513]>() },
        [0xa5; 513]
    );
    unsafe { resume(buffer, cancel, output_step) }
}

#[test]
fn deep_mixed_entries_preserve_order_across_drains() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| unsafe {
            let mut storage = MaybeUninit::<DtorExecutor>::uninit();
            let context = __reussir_create_dtor_executor(storage.as_mut_ptr().cast());
            let mut output: Vec<usize> = Vec::new();
            let mut args = (DEEP_DEPTH, ptr::addr_of_mut!(output));
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), &SMALL);
            assert_eq!(output, (0..=DEEP_DEPTH).collect::<Vec<_>>());
            output.clear();
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), &SMALL);
            assert_eq!(output, (0..=DEEP_DEPTH).collect::<Vec<_>>());
            assert_eq!((*context).allocations, 0);
            assert!(!(*context).running);
            assert_eq!((*context).arena.allocated_bytes(), 0);
            __reussir_destroy_dtor_executor(context);
            __reussir_create_dtor_executor(storage.as_mut_ptr().cast());
            __reussir_destroy_dtor_executor(context);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn continuation_storage_is_aligned_stable_and_lifo() {
    let mut context = DtorExecutor::new();
    let layouts = [(0, 1), (1, 1), (33, 8), (2049, 256), (65537, 4096)];
    unsafe {
        let mut buffers = Vec::new();
        for &(size, align) in &layouts {
            let buffer = __reussir_dtor_conti_alloc(&mut context, size, align);
            assert_eq!(buffer as usize % align, 0);
            buffer.cast::<u8>().write_bytes(0xa5, size);
            buffers.push(buffer);
        }
        for (&buffer, &(size, _)) in buffers.iter().zip(&layouts).rev() {
            assert!(
                std::slice::from_raw_parts(buffer.cast::<u8>(), size)
                    .iter()
                    .all(|&v| v == 0xa5)
            );
            __reussir_dtor_conti_free(&mut context);
        }
        for _ in 0..if cfg!(miri) { 8 } else { 1000 } {
            for &(size, align) in layouts.iter().rev() {
                __reussir_dtor_conti_alloc(&mut context, size, align);
            }
            for _ in &layouts {
                __reussir_dtor_conti_free(&mut context);
            }
        }
        assert_eq!(context.allocations, 0);
    }
}

#[test]
fn invalid_layouts_and_empty_free_are_rejected() {
    for (size, align) in [(1, 0), (1, 3), (usize::MAX, 8), (isize::MAX as usize, 8)] {
        let mut context = DtorExecutor::new();
        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(
                || context.allocate(size, align)
            ))
            .is_err()
        );
        assert_eq!(context.allocations, 0);
    }
    let mut context = DtorExecutor::new();
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| context.deallocate())).is_err()
    );
}

unsafe extern "C" fn nested(
    _: *mut c_void,
    object: *mut c_void,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    let mut inner = DtorExecutor::new();
    unsafe { __reussir_run_dtor(&mut inner, object, &SMALL) };
    unsafe { complete_or_yield(output_step, ptr::null(), ptr::null()) }
}
#[test]
fn independent_nested_and_parallel_executors() {
    let threads: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(|| {
                let mut context = DtorExecutor::new();
                let mut output: Vec<usize> = Vec::new();
                let mut args = (10usize, ptr::addr_of_mut!(output));
                unsafe {
                    __reussir_run_dtor(
                        &mut context,
                        ptr::addr_of_mut!(args).cast(),
                        &DtorRoutine {
                            start: nested,
                            size: 0,
                            align: 1,
                        },
                    )
                };
                assert_eq!(output, (0..=10).collect::<Vec<_>>());
            })
        })
        .collect();
    for t in threads {
        t.join().unwrap();
    }
}

#[test]
fn ffi_layout_and_symbol_exports() {
    assert_eq!(__reussir_dtor_executor_size(), size_of::<DtorExecutor>());
    assert_eq!(__reussir_dtor_executor_align(), align_of::<DtorExecutor>());
    assert_eq!(size_of::<DtorResumeAction>(), 2 * size_of::<usize>());
    assert_eq!(
        core::mem::offset_of!(DtorResumeAction, offset),
        size_of::<usize>()
    );
    for name in [
        "__reussir_dtor_executor_size",
        "__reussir_dtor_executor_align",
        "__reussir_create_dtor_executor",
        "__reussir_destroy_dtor_executor",
        "__reussir_dtor_conti_alloc",
        "__reussir_dtor_conti_free",
        "__reussir_run_dtor",
        "__reussir_finish_dtor",
    ] {
        assert!(
            crate::symbols::exported_symbols()
                .iter()
                .any(|s| s.name == name && !s.address.is_null())
        );
    }
}

#[repr(C)]
struct Branch {
    args: (usize, *mut Vec<usize>),
    second: bool,
    task: DtorTask,
}
const BRANCH: DtorRoutine = DtorRoutine {
    start: branch_start,
    size: size_of::<Branch>(),
    align: align_of::<Branch>(),
};
unsafe extern "C" fn branch_start(
    buffer: *mut c_void,
    object: *mut c_void,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    let (depth, output) = unsafe { *object.cast::<(usize, *mut Vec<usize>)>() };
    if depth == 0 {
        unsafe {
            (*output).push(0);
        }
        return unsafe { complete_or_yield(output_step, ptr::null(), ptr::null()) };
    }
    let state = buffer.cast::<Branch>();
    unsafe {
        state.write(Branch {
            args: (depth - 1, output),
            second: false,
            task: DtorTask {
                routine: &BRANCH,
                object: ptr::addr_of_mut!((*state).args).cast(),
            },
        });
    }
    unsafe {
        complete_or_yield(
            output_step,
            &BRANCH_RESUME_ACTION,
            ptr::addr_of!((*state).task),
        )
    }
}
unsafe extern "C" fn branch_resume(
    buffer: *mut c_void,
    cancel: bool,
    output_step: *mut *const DtorTask,
) -> *const DtorResumeAction {
    assert!(!cancel);
    let state = unsafe { &mut *buffer.cast::<Branch>() };
    if !state.second {
        state.second = true;
        unsafe { complete_or_yield(output_step, &BRANCH_RESUME_ACTION, &state.task) }
    } else {
        unsafe {
            (*state.args.1).push(state.args.0 + 1);
        }
        unsafe { complete_or_yield(output_step, ptr::null(), ptr::null()) }
    }
}
#[test]
fn repeated_suspensions_preserve_postorder() {
    fn expected(depth: usize, output: &mut Vec<usize>) {
        if depth != 0 {
            expected(depth - 1, output);
            expected(depth - 1, output);
        }
        output.push(depth);
    }
    let mut context = DtorExecutor::new();
    let mut output = Vec::<usize>::new();
    let mut args = (BRANCH_DEPTH, ptr::addr_of_mut!(output));
    let mut reference = Vec::new();
    expected(BRANCH_DEPTH, &mut reference);
    for _ in 0..2 {
        unsafe {
            __reussir_run_dtor(&mut context, ptr::addr_of_mut!(args).cast(), &BRANCH);
        }
        assert_eq!(output, reference);
        assert_eq!(context.allocations, 0);
        assert_eq!(context.arena.allocated_bytes(), 0);
        output.clear();
    }
}

#[repr(C)]
struct AllocatingParent {
    context: *mut DtorExecutor,
    allocation: *mut u8,
    args: (usize, *mut Vec<usize>),
    task: DtorTask,
}

unsafe extern "C" fn allocating_start(
    buffer: *mut c_void,
    object: *mut c_void,
    output: *mut *const DtorTask,
) -> *const DtorResumeAction {
    let (context, values) = unsafe { *object.cast::<(*mut DtorExecutor, *mut Vec<usize>)>() };
    let frame = buffer.cast::<AllocatingParent>();
    unsafe {
        assert_eq!((*context).allocations, 0);
        let allocation = __reussir_dtor_conti_alloc(context, 513, 256).cast::<u8>();
        allocation.write_bytes(0xa5, 513);
        frame.write(AllocatingParent {
            context,
            allocation,
            args: (DEEP_DEPTH, values),
            task: DtorTask {
                routine: &SMALL,
                object: ptr::addr_of_mut!((*frame).args).cast(),
            },
        });
        complete_or_yield(
            output,
            &ALLOCATING_RESUME_ACTION,
            ptr::addr_of!((*frame).task),
        )
    }
}

unsafe extern "C" fn allocating_resume(
    buffer: *mut c_void,
    cancel: bool,
    output: *mut *const DtorTask,
) -> *const DtorResumeAction {
    assert!(!cancel);
    unsafe {
        let frame = &*buffer.cast::<AllocatingParent>();
        assert_eq!((*frame.context).allocations, 1);
        assert_eq!(
            std::slice::from_raw_parts(frame.allocation, 513),
            &[0xa5; 513]
        );
        __reussir_dtor_conti_free(frame.context);
        complete_or_yield(output, ptr::null(), ptr::null())
    }
}

#[test]
fn explicit_allocation_survives_child_drain() {
    let mut context = DtorExecutor::new();
    let mut values = Vec::<usize>::new();
    let routine = DtorRoutine {
        start: allocating_start,
        size: size_of::<AllocatingParent>(),
        align: align_of::<AllocatingParent>(),
    };
    for _ in 0..2 {
        // Pass the same raw pointer that the callback receives. A fresh &mut
        // context here invalidates the callback's saved pointer under Miri.
        let context_ptr = ptr::addr_of_mut!(context);
        let mut args = (context_ptr, ptr::addr_of_mut!(values));
        unsafe { __reussir_run_dtor(context_ptr, ptr::addr_of_mut!(args).cast(), &routine) };
        assert_eq!(values, (0..=DEEP_DEPTH).collect::<Vec<_>>());
        assert_eq!(context.allocations, 0);
        assert_eq!(context.arena.allocated_bytes(), 0);
        assert!(!context.running);
        values.clear();
    }
}

#[test]
fn completed_sibling_frames_reuse_storage_without_touching_ancestors() {
    let arena = Arena::new();
    let mut buffers = FrameBuffers::default();
    let parent = buffers.get(&arena, 0, 64, 8);
    unsafe { parent.cast::<u8>().write_bytes(0xa5, 64) };
    buffers.get(&arena, 1, 4096, 8);
    let sibling = buffers.get(&arena, 1, 32, 256);
    assert_eq!(sibling as usize % 256, 0);
    let allocated = arena.allocated_bytes();
    for _ in 0..if cfg!(miri) { 32 } else { 10_000 } {
        assert_eq!(buffers.get(&arena, 1, 4096, 8), sibling);
        assert_eq!(buffers.get(&arena, 1, 32, 256), sibling);
    }
    assert_eq!(arena.allocated_bytes(), allocated);
    // Grow the metadata vector repeatedly while the parent frame stays live.
    for depth in 2..32 {
        buffers.get(&arena, depth, 64, 8);
    }
    assert_eq!(buffers.get(&arena, 0, 64, 8), parent);
    assert_eq!(
        unsafe { core::slice::from_raw_parts(parent.cast::<u8>(), 64) },
        &[0xa5; 64]
    );
}

#[test]
fn lazy_entry_finishes_deep_mixed_frames_without_restarting_root() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| unsafe {
            let mut output = Vec::<usize>::new();
            let mut args = (DEEP_DEPTH, ptr::addr_of_mut!(output));
            let mut root = MaybeUninit::<State>::uninit();
            let buffer = root.as_mut_ptr();
            let mut child = MaybeUninit::<*const DtorTask>::uninit();
            let resume = start(
                buffer.cast(),
                ptr::addr_of_mut!(args).cast(),
                child.as_mut_ptr(),
            );
            let child = child.assume_init();
            // A restart would lose this already-established root state.
            (*buffer).value = DEEP_DEPTH + 1;
            __reussir_finish_dtor(buffer.cast(), resume, child);
            assert_eq!(&output[..DEEP_DEPTH], &(0..DEEP_DEPTH).collect::<Vec<_>>());
            assert_eq!(output[DEEP_DEPTH], DEEP_DEPTH + 1);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn lazy_entry_preserves_repeated_suspensions_and_postorder() {
    let mut expected = Vec::new();
    let mut args = (LAZY_BRANCH_DEPTH, ptr::addr_of_mut!(expected));
    unsafe {
        __reussir_run_dtor(
            &mut DtorExecutor::new(),
            ptr::addr_of_mut!(args).cast(),
            &BRANCH,
        )
    };
    let mut output = Vec::<usize>::new();
    args.1 = ptr::addr_of_mut!(output);
    let mut root = MaybeUninit::<Branch>::uninit();
    let buffer = root.as_mut_ptr();
    let mut child = MaybeUninit::<*const DtorTask>::uninit();
    unsafe {
        let resume = branch_start(
            buffer.cast(),
            ptr::addr_of_mut!(args).cast(),
            child.as_mut_ptr(),
        );
        let child = child.assume_init();
        __reussir_finish_dtor(buffer.cast(), resume, child);
    }
    assert_eq!(output, expected);
    assert_eq!(output.len(), (1 << (LAZY_BRANCH_DEPTH + 1)) - 1);
}

const RESUME_ACTION: DtorResumeAction = DtorResumeAction {
    callback: Some(resume),
    offset: 0,
};

const LARGE_RESUME_ACTION: DtorResumeAction = DtorResumeAction {
    callback: Some(large_resume),
    offset: 0,
};

const BRANCH_RESUME_ACTION: DtorResumeAction = DtorResumeAction {
    callback: Some(branch_resume),
    offset: 0,
};

const ALLOCATING_RESUME_ACTION: DtorResumeAction = DtorResumeAction {
    callback: Some(allocating_resume),
    offset: 0,
};

// The child can replace the pointer in a suspended parent's frame. Loading it
// when the action is pushed would free the old allocation twice and leak the new
// one. Miri checks both allocation lifetime and provenance here.
unsafe extern "C" fn replace_free_payload(
    _: *mut c_void,
    object: *mut c_void,
    out: *mut *const DtorTask,
) -> *const DtorResumeAction {
    unsafe {
        let old = object.cast::<*mut u8>().read_unaligned();
        let new = crate::alloc::__reussir_allocate_small(32);
        crate::alloc::__reussir_dealloc_unsized(old);
        object.cast::<*mut u8>().write_unaligned(new);
        complete_or_yield(out, ptr::null(), ptr::null())
    }
}
#[test]
fn deferred_free_loads_payload_after_child_finishes() {
    unsafe {
        // Also exercise an unaligned, nonzero offset in the saved frame.
        let mut frame = [0u8; 1 + size_of::<*mut u8>()];
        let buffer = frame.as_mut_ptr().cast::<c_void>();
        let payload = buffer.byte_add(1);
        payload
            .cast::<*mut u8>()
            .write_unaligned(crate::alloc::__reussir_allocate_small(32));
        let action = DtorResumeAction {
            callback: None,
            offset: 1,
        };
        let routine = DtorRoutine {
            start: replace_free_payload,
            size: 0,
            align: 1,
        };
        let task = DtorTask {
            routine: &routine,
            object: payload,
        };
        __reussir_finish_dtor(buffer, &action, &task);
    }
}

unsafe fn complete_or_yield(
    out: *mut *const DtorTask,
    resume: *const DtorResumeAction,
    child: *const DtorTask,
) -> *const DtorResumeAction {
    if !resume.is_null() {
        unsafe { out.write(child) };
    }
    resume
}

#[test]
fn completion_does_not_require_a_child_output() {
    unsafe extern "C" fn complete(
        _: *mut c_void,
        _: *mut c_void,
        _: *mut *const DtorTask,
    ) -> *const DtorResumeAction {
        core::ptr::null()
    }
    // The callback deliberately leaves the output uninitialized. Miri catches
    // an executor that reads it on the completion path.
    let routine = DtorRoutine {
        start: complete,
        size: 0,
        align: 1,
    };
    unsafe { __reussir_run_dtor(&mut DtorExecutor::new(), ptr::null_mut(), &routine) };
}

// Drop a chain of `depth` levels through the lazy entry point.
unsafe fn finish_chain(depth: usize) -> Vec<usize> {
    let mut output = Vec::<usize>::new();
    let mut args = (depth, ptr::addr_of_mut!(output));
    let mut root = MaybeUninit::<State>::uninit();
    let buffer = root.as_mut_ptr();
    let mut child = MaybeUninit::<*const DtorTask>::uninit();
    unsafe {
        let resume = start(
            buffer.cast(),
            ptr::addr_of_mut!(args).cast(),
            child.as_mut_ptr(),
        );
        __reussir_finish_dtor(buffer.cast(), resume, child.assume_init());
    }
    output
}

#[test]
fn root_drops_reuse_the_thread_scratch_within_bounds() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| unsafe {
            let cached_table = || {
                let scratch = Scratch::take().expect("a small drop keeps its scratch");
                let table = scratch.buffers.as_ptr();
                SCRATCH.with(|cache| cache.set(Some(scratch)));
                table
            };
            assert_eq!(finish_chain(4), [0, 1, 2, 3, 4]);
            let table = cached_table();
            assert_eq!(finish_chain(4), [0, 1, 2, 3, 4]);
            assert_eq!(cached_table(), table);
            // A deep drop takes the cache and does not give its storage back.
            assert_eq!(finish_chain(DEEP_DEPTH).len(), DEEP_DEPTH + 1);
            assert!(Scratch::take().is_none());
        })
        .unwrap()
        .join()
        .unwrap();
}
