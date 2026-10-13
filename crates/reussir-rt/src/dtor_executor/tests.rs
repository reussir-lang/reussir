use super::*;
use core::ptr;

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

unsafe extern "C" fn resume(buffer: *mut c_void, cancel: bool, output_step: *mut DtorStep) {
    assert!(!cancel);
    let state = unsafe { &*buffer.cast::<State>() };
    unsafe { (*state.args.1).push(state.value) };
    unsafe {
        output_step.write(DtorStep {
            resume: None,
            child: ptr::null(),
        })
    }
}
unsafe extern "C" fn start(buffer: *mut c_void, object: *mut c_void, output_step: *mut DtorStep) {
    let (n, output) = unsafe { *object.cast::<(usize, *mut Vec<usize>)>() };
    if n == 0 {
        unsafe { (*output).push(0) };
        return unsafe {
            output_step.write(DtorStep {
                resume: None,
                child: ptr::null(),
            })
        };
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
    unsafe {
        output_step.write(DtorStep {
            resume: Some(resume),
            child: ptr::addr_of!((*state).task),
        })
    }
}
unsafe extern "C" fn large_start(
    buffer: *mut c_void,
    object: *mut c_void,
    output_step: *mut DtorStep,
) {
    assert_eq!(buffer as usize % LARGE.align, 0);
    unsafe {
        buffer
            .byte_add(size_of::<State>())
            .cast::<[u8; 513]>()
            .write([0xa5; 513])
    };
    unsafe { start(buffer, object, output_step) };
    let step = unsafe { &mut *output_step };
    if step.resume.is_some() {
        step.resume = Some(large_resume);
    }
}
unsafe extern "C" fn large_resume(buffer: *mut c_void, cancel: bool, output_step: *mut DtorStep) {
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
            let mut args = (100_000usize, ptr::addr_of_mut!(output));
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), &SMALL);
            assert_eq!(output, (0..=100_000).collect::<Vec<_>>());
            output.clear();
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), &SMALL);
            assert_eq!(output, (0..=100_000).collect::<Vec<_>>());
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
        for _ in 0..1000 {
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

unsafe extern "C" fn nested(_: *mut c_void, object: *mut c_void, output_step: *mut DtorStep) {
    let mut inner = DtorExecutor::new();
    unsafe { __reussir_run_dtor(&mut inner, object, &SMALL) };
    unsafe {
        output_step.write(DtorStep {
            resume: None,
            child: ptr::null(),
        })
    }
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
    assert_eq!(size_of::<DtorStep>(), 2 * size_of::<usize>());
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
    output_step: *mut DtorStep,
) {
    let (depth, output) = unsafe { *object.cast::<(usize, *mut Vec<usize>)>() };
    if depth == 0 {
        unsafe {
            (*output).push(0);
        }
        return unsafe {
            output_step.write(DtorStep {
                resume: None,
                child: ptr::null(),
            })
        };
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
        output_step.write(DtorStep {
            resume: Some(branch_resume),
            child: ptr::addr_of!((*state).task),
        })
    }
}
unsafe extern "C" fn branch_resume(buffer: *mut c_void, cancel: bool, output_step: *mut DtorStep) {
    assert!(!cancel);
    let state = unsafe { &mut *buffer.cast::<Branch>() };
    if !state.second {
        state.second = true;
        unsafe {
            output_step.write(DtorStep {
                resume: Some(branch_resume),
                child: &state.task,
            })
        }
    } else {
        unsafe {
            (*state.args.1).push(state.args.0 + 1);
        }
        unsafe {
            output_step.write(DtorStep {
                resume: None,
                child: ptr::null(),
            })
        }
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
    let mut args = (12usize, ptr::addr_of_mut!(output));
    let mut reference = Vec::new();
    expected(12, &mut reference);
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
    output: *mut DtorStep,
) {
    let (context, values) = unsafe { *object.cast::<(*mut DtorExecutor, *mut Vec<usize>)>() };
    let frame = buffer.cast::<AllocatingParent>();
    unsafe {
        assert_eq!((*context).allocations, 0);
        let allocation = __reussir_dtor_conti_alloc(context, 513, 256).cast::<u8>();
        allocation.write_bytes(0xa5, 513);
        frame.write(AllocatingParent {
            context,
            allocation,
            args: (100_000, values),
            task: DtorTask {
                routine: &SMALL,
                object: ptr::addr_of_mut!((*frame).args).cast(),
            },
        });
        output.write(DtorStep {
            resume: Some(allocating_resume),
            child: ptr::addr_of!((*frame).task),
        });
    }
}

unsafe extern "C" fn allocating_resume(buffer: *mut c_void, cancel: bool, output: *mut DtorStep) {
    assert!(!cancel);
    unsafe {
        let frame = &*buffer.cast::<AllocatingParent>();
        assert_eq!((*frame.context).allocations, 1);
        assert_eq!(
            std::slice::from_raw_parts(frame.allocation, 513),
            &[0xa5; 513]
        );
        __reussir_dtor_conti_free(frame.context);
        output.write(DtorStep {
            resume: None,
            child: ptr::null(),
        });
    }
}

#[test]
fn explicit_allocation_survives_child_drain() {
    let mut context = DtorExecutor::new();
    let mut values = Vec::<usize>::new();
    let mut args = (ptr::addr_of_mut!(context), ptr::addr_of_mut!(values));
    let routine = DtorRoutine {
        start: allocating_start,
        size: size_of::<AllocatingParent>(),
        align: align_of::<AllocatingParent>(),
    };
    for _ in 0..2 {
        unsafe { __reussir_run_dtor(&mut context, ptr::addr_of_mut!(args).cast(), &routine) };
        assert_eq!(values, (0..=100_000).collect::<Vec<_>>());
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
    for _ in 0..10_000 {
        assert_eq!(buffers.get(&arena, 1, 4096, 8), sibling);
        assert_eq!(buffers.get(&arena, 1, 32, 256), sibling);
    }
    assert_eq!(arena.allocated_bytes(), allocated);
    // Force the metadata vector out of its inline storage while the parent lives.
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
            let mut args = (100_000usize, ptr::addr_of_mut!(output));
            let mut root = MaybeUninit::<State>::uninit();
            let mut step = MaybeUninit::<DtorStep>::uninit();
            start(
                root.as_mut_ptr().cast(),
                ptr::addr_of_mut!(args).cast(),
                step.as_mut_ptr(),
            );
            let step = step.assume_init();
            // A restart would lose this already-established root state.
            (*root.as_mut_ptr()).value = 100_001;
            __reussir_finish_dtor(root.as_mut_ptr().cast(), step.resume.unwrap(), step.child);
            assert_eq!(&output[..100_000], &(0..100_000).collect::<Vec<_>>());
            assert_eq!(output[100_000], 100_001);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn lazy_entry_preserves_repeated_suspensions_and_postorder() {
    let mut expected = Vec::new();
    let mut args = (16usize, ptr::addr_of_mut!(expected));
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
    let mut step = MaybeUninit::<DtorStep>::uninit();
    unsafe {
        branch_start(
            root.as_mut_ptr().cast(),
            ptr::addr_of_mut!(args).cast(),
            step.as_mut_ptr(),
        );
        let step = step.assume_init();
        __reussir_finish_dtor(root.as_mut_ptr().cast(), step.resume.unwrap(), step.child);
    }
    assert_eq!(output, expected);
    assert_eq!(output.len(), (1 << 17) - 1);
}
