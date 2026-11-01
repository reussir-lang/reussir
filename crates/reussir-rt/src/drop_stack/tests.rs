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
    std::thread::Builder::new()
        .stack_size(64 * 1024)
        .spawn(|| {
            let mut object = Chain {
                remaining: 1_000_000,
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

#[derive(Debug, PartialEq, Eq)]
enum ReleaseEvent {
    Primary(usize),
    Secondary(usize),
}
#[repr(C, align(8))]
struct Wide {
    count: u32,
    tag: u32,
    id: usize,
    events: *mut Vec<ReleaseEvent>,
}
unsafe fn take_wide(object: *mut u8) -> Box<Wide> {
    let object = unsafe { Box::from_raw(object.cast::<Wide>()) };
    assert_eq!(object.count, 1);
    assert_eq!(object.tag & 0xffff, object.id as u32);
    object
}
unsafe extern "C" fn primary(object: *mut u8) {
    let object = unsafe { take_wide(object) };
    unsafe { (*object.events).push(ReleaseEvent::Primary(object.id)) };
}
unsafe extern "C" fn secondary(object: *mut u8) {
    let object = unsafe { take_wide(object) };
    unsafe { (*object.events).push(ReleaseEvent::Secondary(object.id)) };
}
unsafe extern "C" fn enqueue_wide(object: *mut u8) {
    let events = object.cast::<Vec<ReleaseEvent>>();
    let first: Release = primary;
    let second: Release = secondary;
    for (id, callback) in [(1, first), (2, first), (3, second), (4, first)] {
        let object = Box::into_raw(Box::new(Wide {
            count: 1,
            tag: id as u32,
            id,
            events,
        }));
        unsafe { defer_wide(object.cast(), callback) };
    }
    ACTIVE.with(|context| {
        let context = context.borrow();
        assert_eq!(context.work.len(), 1);
        assert_eq!(context.cells.len(), 1);
    });
}
#[test]
fn mixed_wide_runs_restore_headers_and_keep_lifo_order() {
    let mut events = vec![];
    unsafe { run((&mut events as *mut Vec<ReleaseEvent>).cast(), enqueue_wide) };
    assert_eq!(
        events,
        [
            ReleaseEvent::Primary(4),
            ReleaseEvent::Secondary(3),
            ReleaseEvent::Primary(2),
            ReleaseEvent::Primary(1)
        ]
    );
}

unsafe extern "C" fn enqueue_join(object: *mut u8) {
    unsafe { defer(object, first) };
    unsafe { join_or_run(object, defer_first) };
    unsafe { (*object.cast::<Ordered>()).log.push(2) };
}
#[test]
fn internal_bridges_join_the_current_drain() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), enqueue_join) };
    assert_eq!(object.log, [2, 1, 1]);
}

#[test]
fn callback_symbols_are_exported() {
    let symbols = crate::symbols::exported_symbols();
    for name in [
        "__reussir_drop_run",
        "__reussir_drop_join_or_run",
        "__reussir_drop_defer",
        "__reussir_drop_defer_wide",
        "__reussir_drop_defer_indexed",
    ] {
        assert!(
            symbols
                .iter()
                .any(|symbol| symbol.name == name && !symbol.address.is_null())
        );
    }
}

unsafe extern "C" fn record_drop(object: *mut u8) {
    unsafe { join_or_run(object, record_walk) };
}
unsafe extern "C" fn record_walk(object: *mut u8) {
    unsafe { defer(object, host_drop) };
}
unsafe extern "C" fn host_drop(object: *mut u8) {
    unsafe { join_or_run_step(object.cast(), host_step) };
}
unsafe extern "C" fn host_step(object: *mut c_void) -> StepStatus {
    let chain = unsafe { &mut *object.cast::<Chain>() };
    if chain.remaining != 0 {
        chain.remaining -= 1;
        unsafe { defer(object.cast(), record_drop) };
    }
    StepStatus::Complete
}
#[test]
fn alternating_foreign_and_record_drops_remain_iterative() {
    std::thread::Builder::new()
        .stack_size(64 * 1024)
        .spawn(|| {
            let mut object = Chain {
                remaining: 1_000_000,
            };
            unsafe { run((&mut object as *mut Chain).cast(), record_drop) };
            assert_eq!(object.remaining, 0);
        })
        .unwrap()
        .join()
        .unwrap();
}

unsafe extern "C" fn bulk(object: *mut u8) {
    for _ in 0..128 {
        unsafe { defer(object, first) };
    }
    ACTIVE.with(|context| {
        let context = context.borrow();
        assert_eq!(context.work.len(), 1);
        assert_eq!(context.cells.len(), 127);
    });
}
#[test]
fn large_runs_keep_only_bounded_spare_buffers() {
    let mut object = Ordered { log: vec![] };
    unsafe { run((&mut object as *mut Ordered).cast(), bulk) };
    assert_eq!(object.log.len(), 128);
    SPARE.with(|slot| {
        let slot = slot.borrow();
        let context = slot.as_ref().unwrap();
        assert!(context.work.capacity() <= 64 && context.cells.capacity() <= 64);
    });
}
