use super::*;

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
    std::thread::Builder::new().stack_size(64 * 1024).spawn(|| {
        let mut object = Chain { remaining: 1_000_000 };
        unsafe { run((&mut object as *mut Chain).cast(), chain) };
        assert_eq!(object.remaining, 0);
    }).unwrap().join().unwrap();
}

struct Ordered {
    log: Vec<usize>,
}
unsafe extern "C" fn first(object: *mut u8) {
    unsafe { (*object.cast::<Ordered>()).log.push(1) };
}
unsafe extern "C" fn nested(object: *mut u8) {
    unsafe { defer(object, first) };
    unsafe { run(object, first) };
    unsafe { (*object.cast::<Ordered>()).log.push(2) };
}

#[test]
fn public_nested_calls_complete_only_their_suffix() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), nested) };
    assert_eq!(object.log, [1, 2, 1]);
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
    let mut cursor = Cursor { next: 0, visited: vec![] };
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
    let mut cursor = Cursor { next: 0, visited: vec![] };
    unsafe { defer_indexed((&mut cursor as *mut Cursor).cast(), 0, indexed) };
    assert_eq!(cursor.visited, [0, 1, 2, 3]);
}

thread_local! {
    static ENDS: Cell<usize> = const { Cell::new(0) };
}
unsafe extern "C" fn end_work() {
    assert!(!releasing());
    assert_eq!(depth(), 0);
    ENDS.set(ENDS.get() + 1);
}
unsafe extern "C" fn requesting(object: *mut u8) {
    want_end_work();
    unsafe { run(object, first) };
    assert_eq!(ENDS.get(), 0);
}
#[test]
fn end_work_waits_for_the_outermost_scope() {
    set_end_work(end_work);
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), requesting) };
    assert_eq!(ENDS.get(), 1);
}

struct Late(std::sync::Arc<std::sync::atomic::AtomicUsize>);
unsafe extern "C" fn late_release(object: *mut u8) {
    let object = unsafe { &*object.cast::<Late>() };
    object.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}
impl Drop for Late {
    fn drop(&mut self) {
        unsafe { run((self as *mut Late).cast(), late_release) };
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
    }).join().unwrap();
    assert_eq!(count.load(std::sync::atomic::Ordering::Relaxed), 1);
}
