use super::*;

const TEST_STACK_SIZE: usize = 64 * 1024;
const DEEP_CHAIN_LENGTH: usize = 1_000_000;

struct Chain {
    remaining: usize,
}
unsafe extern "C" fn chain(object: *mut u8) {
    let chain = unsafe { &mut *object.cast::<Chain>() };
    if chain.remaining != 0 {
        chain.remaining -= 1;
        unsafe { defer(object, self::chain) };
    }
}

#[test]
fn deep_work_uses_a_bounded_native_stack() {
    std::thread::Builder::new()
        .stack_size(TEST_STACK_SIZE)
        .spawn(|| {
            let mut object = Chain {
                remaining: DEEP_CHAIN_LENGTH,
            };
            unsafe { run((&mut object as *mut Chain).cast(), chain) };
            assert_eq!(object.remaining, 0);
        })
        .unwrap()
        .join()
        .unwrap();
}

struct Ordered {
    log: Vec<usize>,
}
unsafe extern "C" fn first(object: *mut u8) {
    unsafe { (*object.cast::<Ordered>()).log.push(1) };
}
unsafe extern "C" fn defer_first(object: *mut u8) {
    unsafe { defer(object, first) };
}
unsafe extern "C" fn nested(object: *mut u8) {
    unsafe { defer(object, first) };
    unsafe { run(object, defer_first) };
    unsafe { (*object.cast::<Ordered>()).log.push(2) };
}

#[test]
fn public_nested_calls_complete_only_their_suffix() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), nested) };
    assert_eq!(object.log, [1, 2, 1]);
}

unsafe extern "C" fn merged_nested(object: *mut u8) {
    let release: Release = first;
    unsafe { defer(object, release) };
    unsafe { defer(object, release) };
    unsafe { run(object, nested) };
    unsafe { (*object.cast::<Ordered>()).log.push(3) };
    unsafe { defer(object, release) };
    assert_eq!(depth(), 1);
}

#[test]
fn nested_boundaries_preserve_and_restore_merged_runs() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), merged_nested) };
    assert_eq!(object.log, [1, 2, 1, 3, 1, 1, 1]);
}

struct Cursor {
    next: usize,
    visited: Vec<usize>,
}
unsafe extern "C" fn visit(object: *mut u8) {
    let cursor = unsafe { &mut *object.cast::<Cursor>() };
    cursor.visited.push(cursor.next);
    cursor.next += 1;
}
unsafe extern "C" fn advance(object: *mut c_void) -> StepStatus {
    let cursor = unsafe { &*object.cast::<Cursor>() };
    if cursor.next == 4 {
        StepStatus::Complete
    } else {
        unsafe { defer(object.cast(), visit) };
        StepStatus::Pending
    }
}
#[test]
fn steps_yield_to_the_work_they_enqueue() {
    let mut cursor = Cursor {
        next: 0,
        visited: vec![],
    };
    unsafe { run_step((&mut cursor as *mut Cursor).cast(), advance) };
    assert_eq!(cursor.visited, [0, 1, 2, 3]);
}
unsafe extern "C" fn indexed(object: *mut u8, index: usize) {
    if index != 4 {
        unsafe { defer_indexed(object, index + 1, self::indexed) };
        unsafe { defer(object, visit) };
    }
}
#[test]
fn indexed_callbacks_visit_one_child_at_a_time() {
    let mut cursor = Cursor {
        next: 0,
        visited: vec![],
    };
    unsafe { defer_indexed((&mut cursor as *mut Cursor).cast(), 0, indexed) };
    assert_eq!(cursor.visited, [0, 1, 2, 3]);
}

thread_local! {
    static EXITS: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}
fn exit_first() {
    assert!(!releasing());
    assert_eq!(depth(), 0);
    EXITS.with_borrow_mut(|events| events.push(1));
}
fn exit_second() {
    assert!(!releasing());
    assert_eq!(depth(), 0);
    EXITS.with_borrow_mut(|events| events.push(2));
}
unsafe extern "C" fn requesting_inner(_: *mut u8) {
    defer_until_scope_exit(exit_second);
}
unsafe extern "C" fn requesting(object: *mut u8) {
    defer_until_scope_exit(exit_first);
    unsafe { run(object, requesting_inner) };
    EXITS.with_borrow(|events| assert!(events.is_empty()));
    defer_until_scope_exit(exit_first);
    unsafe { defer(object, first) };
}
#[test]
fn scope_exit_callbacks_wait_and_keep_request_order() {
    EXITS.with_borrow_mut(Vec::clear);
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), requesting) };
    assert_eq!(object.log, [1]);
    EXITS.with_borrow(|events| assert_eq!(events, &[1, 2, 1]));
}
fn exit_reenters() {
    assert!(!releasing());
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), requesting_inner) };
    EXITS.with_borrow_mut(|events| events.push(3));
}
#[test]
fn scope_exit_callbacks_can_start_another_drop() {
    EXITS.with_borrow_mut(Vec::clear);
    scoped(|| {
        defer_until_scope_exit(exit_reenters);
        defer_until_scope_exit(exit_first);
    });
    EXITS.with_borrow(|events| assert_eq!(events, &[2, 3, 1]));
}
#[test]
fn scope_exit_callbacks_run_immediately_when_idle() {
    EXITS.with_borrow_mut(Vec::clear);
    defer_until_scope_exit(exit_first);
    EXITS.with_borrow(|events| assert_eq!(events, &[1]));
}

struct Late(std::sync::Arc<std::sync::atomic::AtomicUsize>);
unsafe extern "C" fn late_release(object: *mut u8) {
    let object = unsafe { &*object.cast::<Late>() };
    object.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}
unsafe extern "C" fn late_enqueue(object: *mut u8) {
    for _ in 0..128 {
        unsafe { defer(object, late_release) };
    }
}
impl Drop for Late {
    fn drop(&mut self) {
        unsafe { run((self as *mut Late).cast(), late_enqueue) };
    }
}
thread_local! {
    static LATE: RefCell<Option<Late>> = const { RefCell::new(None) };
}
#[test]
fn drops_after_the_buffer_cache_destructor_own_their_context() {
    let count = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let observed = count.clone();
    std::thread::spawn(move || {
        LATE.with(|slot| *slot.borrow_mut() = Some(Late(observed)));
        let mut object = Chain { remaining: 100 };
        unsafe { run((&mut object as *mut Chain).cast(), chain) };
    })
    .join()
    .unwrap();
    assert_eq!(count.load(std::sync::atomic::Ordering::Relaxed), 128);
}
