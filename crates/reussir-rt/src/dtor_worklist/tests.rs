use super::*;
use core::{mem::MaybeUninit, ptr};

unsafe extern "C" fn increment(object: *mut c_void, _: *mut DtorWorklist) {
    unsafe { *object.cast::<usize>() += 1 };
}

#[test]
fn inline_overflow_and_reuse() {
    let mut storage = MaybeUninit::<DtorWorklist>::uninit();
    let context = storage.as_mut_ptr();
    let mut count = 0usize;
    unsafe {
        __reussir_create_dtor_worklist(context.cast());
        __reussir_drain_dtor_worklist(context); // Empty drain is valid.
        for _ in 0..INLINE_CAPACITY {
            __reussir_enqueue_dtor(context, ptr::addr_of_mut!(count).cast(), increment);
        }
        assert!(!(*context).pending.spilled());
        __reussir_drain_dtor_worklist(context);
        assert_eq!(count, INLINE_CAPACITY);
        for _ in 0..1024 {
            __reussir_enqueue_dtor(context, ptr::addr_of_mut!(count).cast(), increment);
        }
        assert!((*context).pending.spilled());
        __reussir_drain_dtor_worklist(context);
        assert_eq!(count, INLINE_CAPACITY + 1024);
        let capacity = (*context).pending.capacity();
        __reussir_enqueue_dtor(context, ptr::addr_of_mut!(count).cast(), increment);
        __reussir_drain_dtor_worklist(context);
        assert_eq!((*context).pending.capacity(), capacity);
        __reussir_destroy_dtor_worklist(context);
        __reussir_create_dtor_worklist(context.cast());
        assert!(!(*context).pending.spilled());
        __reussir_destroy_dtor_worklist(context);
    }
}

struct Event {
    id: usize,
    output: *mut Vec<usize>,
}

unsafe extern "C" fn record(object: *mut c_void, _: *mut DtorWorklist) {
    let event = unsafe { Box::from_raw(object.cast::<Event>()) };
    unsafe { (*event.output).push(event.id) };
}

unsafe fn enqueue_event(context: *mut DtorWorklist, output: *mut Vec<usize>, id: usize) {
    let event = Box::into_raw(Box::new(Event { id, output }));
    unsafe { __reussir_enqueue_dtor(context, event.cast(), record) };
}

unsafe extern "C" fn expand(object: *mut c_void, context: *mut DtorWorklist) {
    let output = object.cast::<Vec<usize>>();
    // A continuation is ordinary pending work. Children enqueued afterwards
    // complete first, including across buffer growth during this callback.
    unsafe { enqueue_event(context, output, 1000) };
    for id in 0..128 {
        unsafe { enqueue_event(context, output, id) };
    }
}

#[test]
fn callbacks_enqueue_children_before_continuation_and_older_work() {
    let mut context = DtorWorklist::new();
    let mut output = Vec::new();
    unsafe {
        enqueue_event(&mut context, ptr::addr_of_mut!(output), 2000);
        __reussir_enqueue_dtor(&mut context, ptr::addr_of_mut!(output).cast(), expand);
        __reussir_drain_dtor_worklist(&mut context);
    }
    let expected: Vec<_> = (0..128).rev().chain([1000, 2000]).collect();
    assert_eq!(output, expected);
}

unsafe extern "C" fn independent(object: *mut c_void, _: *mut DtorWorklist) {
    let mut nested = DtorWorklist::new();
    unsafe {
        enqueue_event(&mut nested, object.cast(), 2);
        __reussir_drain_dtor_worklist(&mut nested);
    }
}

#[test]
fn independent_nested_context_does_not_drain_outer_work() {
    let mut outer = DtorWorklist::new();
    let mut other = DtorWorklist::new();
    let mut output = Vec::new();
    unsafe {
        enqueue_event(&mut outer, ptr::addr_of_mut!(output), 1);
        enqueue_event(&mut other, ptr::addr_of_mut!(output), 3);
        __reussir_enqueue_dtor(&mut outer, ptr::addr_of_mut!(output).cast(), independent);
        __reussir_drain_dtor_worklist(&mut outer);
        assert_eq!(output, [2, 1]);
        __reussir_drain_dtor_worklist(&mut other);
    }
    assert_eq!(output, [2, 1, 3]);
}

struct Node {
    left: *mut Node,
    right: *mut Node,
    freed: *mut usize,
}

unsafe extern "C" fn release_node(object: *mut c_void, context: *mut DtorWorklist) {
    let node = unsafe { Box::from_raw(object.cast::<Node>()) };
    for child in [node.left, node.right] {
        if !child.is_null() {
            unsafe { __reussir_enqueue_dtor(context, child.cast(), release_node) };
        }
    }
    unsafe { *node.freed += 1 };
}

#[test]
fn deep_branching_drop_on_small_stack() {
    std::thread::Builder::new()
        .stack_size(64 * 1024)
        .spawn(|| {
            let depth = if cfg!(miri) { 128 } else { 100_000 };
            let mut freed = 0;
            let mut root = ptr::null_mut();
            for _ in 0..depth {
                let right = Box::into_raw(Box::new(Node {
                    left: ptr::null_mut(),
                    right: ptr::null_mut(),
                    freed: ptr::addr_of_mut!(freed),
                }));
                root = Box::into_raw(Box::new(Node {
                    left: root,
                    right,
                    freed: ptr::addr_of_mut!(freed),
                }));
            }
            let mut context = DtorWorklist::new();
            unsafe {
                __reussir_enqueue_dtor(&mut context, root.cast(), release_node);
                __reussir_drain_dtor_worklist(&mut context);
            }
            assert_eq!(freed, depth * 2);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn contexts_are_independent_across_threads() {
    let threads: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(|| {
                let mut context = DtorWorklist::new();
                let mut count = 0usize;
                for _ in 0..32 {
                    unsafe {
                        __reussir_enqueue_dtor(
                            &mut context,
                            ptr::addr_of_mut!(count).cast(),
                            increment,
                        )
                    };
                }
                unsafe { __reussir_drain_dtor_worklist(&mut context) };
                assert_eq!(count, 32);
            })
        })
        .collect();
    for thread in threads {
        thread.join().unwrap();
    }
}

#[test]
fn jit_can_resolve_worklist_abi() {
    for name in [
        "__reussir_create_dtor_worklist",
        "__reussir_dtor_worklist_size",
        "__reussir_dtor_worklist_align",
        "__reussir_enqueue_dtor",
        "__reussir_drain_dtor_worklist",
        "__reussir_destroy_dtor_worklist",
    ] {
        assert!(
            crate::symbols::exported_symbols()
                .iter()
                .any(|symbol| symbol.name == name && !symbol.address.is_null())
        );
    }
}

#[test]
fn queried_layout_initializes_opaque_storage() {
    let size = __reussir_dtor_worklist_size();
    let align = __reussir_dtor_worklist_align();
    // Model dynamically sized storage without depending on the Rust type's
    // layout. Stronger alignment and extra space are also accepted.
    let layout = std::alloc::Layout::from_size_align(size + 32, align * 2).unwrap();
    unsafe {
        let storage = std::alloc::alloc(layout);
        if storage.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        storage.write_bytes(0xa5, layout.size());
        let context = __reussir_create_dtor_worklist(storage.cast());
        assert_eq!(context.cast::<u8>(), storage);
        __reussir_drain_dtor_worklist(context);
        __reussir_destroy_dtor_worklist(context);
        for i in size..layout.size() {
            assert_eq!(*storage.add(i), 0xa5);
        }
        std::alloc::dealloc(storage, layout);
    }
}
