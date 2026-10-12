use super::*;

#[repr(C)]
struct State {
    n: usize,
    output: *mut Vec<usize>,
    task: DtorTask,
}

unsafe extern "C" fn resume(buffer: *mut c_void, cancel: bool) -> DtorStep {
    assert!(!cancel);
    let state = unsafe { &*buffer.cast::<State>() };
    unsafe { (*state.output).push(state.n) };
    DtorStep {
        resume: None,
        child: ptr::null(),
    }
}
unsafe extern "C" fn start(buffer: *mut c_void, object: *mut c_void) -> DtorStep {
    let (n, output) = unsafe { *object.cast::<(usize, *mut Vec<usize>)>() };
    if n == 0 {
        unsafe { (*output).push(0) };
        return DtorStep {
            resume: None,
            child: ptr::null(),
        };
    }
    let state = buffer.cast::<State>();
    unsafe {
        state.write(State {
            n: n - 1,
            output,
            task: DtorTask {
                start: child_start,
                object: state.cast(),
            },
        });
    }
    DtorStep {
        resume: Some(resume),
        child: unsafe { ptr::addr_of!((*state).task) },
    }
}
unsafe extern "C" fn child_start(buffer: *mut c_void, object: *mut c_void) -> DtorStep {
    let parent = unsafe { &mut *object.cast::<State>() };
    let mut args = (parent.n, parent.output);
    parent.n += 1;
    unsafe { start(buffer, ptr::addr_of_mut!(args).cast()) }
}

#[test]
fn deep_mixed_entries_preserve_order_and_reuse_slots() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| unsafe {
            assert!(size_of::<State>() <= DTOR_CONTI_SIZE);
            let mut storage = MaybeUninit::<DtorExecutor>::uninit();
            let context = __reussir_create_dtor_executor(storage.as_mut_ptr().cast());
            let mut output: Vec<usize> = Vec::new();
            let mut args = (100_000usize, ptr::addr_of_mut!(output));
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), start);
            assert_eq!(output, (0..=100_000).collect::<Vec<_>>());
            let bytes = (*context).arena.allocated_bytes_including_metadata();
            output.clear();
            __reussir_run_dtor(context, ptr::addr_of_mut!(args).cast(), start);
            assert_eq!(output, (0..=100_000).collect::<Vec<_>>());
            assert_eq!((*context).arena.allocated_bytes_including_metadata(), bytes);
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
    unsafe {
        let first = __reussir_dtor_conti_alloc(&mut context);
        assert_eq!(first as usize % DTOR_CONTI_ALIGN, 0);
        first
            .cast::<[u8; DTOR_CONTI_SIZE]>()
            .write([0xa5; DTOR_CONTI_SIZE]);
        let mut last = first;
        for _ in 0..1000 {
            last = __reussir_dtor_conti_alloc(&mut context);
        }
        assert_eq!(
            *first.cast::<[u8; DTOR_CONTI_SIZE]>(),
            [0xa5; DTOR_CONTI_SIZE]
        );
        __reussir_dtor_conti_free(&mut context);
        assert_eq!(__reussir_dtor_conti_alloc(&mut context), last);
        for _ in 0..1001 {
            __reussir_dtor_conti_free(&mut context);
        }
    }
}

unsafe extern "C" fn nested(_: *mut c_void, object: *mut c_void) -> DtorStep {
    let mut inner = DtorExecutor::new();
    unsafe { __reussir_run_dtor(&mut inner, object, start) };
    DtorStep {
        resume: None,
        child: ptr::null(),
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
                unsafe { __reussir_run_dtor(&mut context, ptr::addr_of_mut!(args).cast(), nested) };
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
    ] {
        assert!(
            crate::symbols::exported_symbols()
                .iter()
                .any(|s| s.name == name && !s.address.is_null())
        );
    }
}
