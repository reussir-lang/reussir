use super::*;
use std::cell::RefCell;

thread_local! {
    static LOG: RefCell<Vec<(char, usize)>> = const { RefCell::new(Vec::new()) };
}
fn log(kind: char, value: usize) {
    // A drop in a thread-local destructor may outlive LOG.
    let _ = LOG.try_with(|l| l.borrow_mut().push((kind, value)));
}
fn take_log() -> Vec<(char, usize)> {
    LOG.with(|l| core::mem::take(&mut *l.borrow_mut()))
}
fn p(value: usize) -> *mut u8 {
    core::ptr::without_provenance_mut(value)
}
const CHAIN: usize = if cfg!(miri) { 1_000 } else { 1_000_000 };
const MANY: usize = if cfg!(miri) { 200 } else { 10_000 }; // Miri: smaller

// `a(v)` defers `b(10 v)` and `b(10 v + 1)` for v < 10; `b(v)` defers nothing.
unsafe extern "C" fn a(cell: *mut u8) {
    let v = cell as usize;
    log('a', v);
    if v < 10 {
        unsafe { defer(p(10 * v), b) };
        unsafe { defer(p(10 * v + 1), b) };
    }
}
unsafe extern "C" fn b(cell: *mut u8) {
    log('b', cell as usize);
}
// A walk deferring `a(1)`, `b(2)`, `a(3)` in that order.
unsafe extern "C" fn walk(_: *mut u8) {
    log('w', 0);
    unsafe { defer(p(1), a) };
    unsafe { defer(p(2), b) };
    unsafe { defer(p(3), a) };
}
const WALK: [(char, usize); 7] =
    [('a', 3), ('b', 31), ('b', 30), ('b', 2), ('a', 1), ('b', 11), ('b', 10)];

#[test]
fn last_deferred_first() {
    unsafe { run(p(0), walk) };
    assert_eq!(take_log(), [[('w', 0)].as_slice(), &WALK].concat());
}

// A release that starts another drop: its walk runs at once and its
// deferrals join the running drain.
unsafe extern "C" fn nested(cell: *mut u8) {
    log('n', cell as usize);
    unsafe { run(p(0), walk) };
    log('m', cell as usize);
}
unsafe extern "C" fn outer(_: *mut u8) {
    unsafe { defer(p(7), b) };
    unsafe { defer(p(8), nested) };
}

#[test]
fn a_nested_run_joins_the_drain() {
    unsafe { run(p(0), outer) };
    let head = [('n', 8), ('w', 0), ('m', 8)];
    assert_eq!(take_log(), [head.as_slice(), &WALK, &[('b', 7)]].concat());
}

// A chain of CHAIN cells, each release deferring the next (and a `b` for an
// even cell), on a small native stack; then MANY pending entries.
unsafe extern "C" fn chain(cell: *mut u8) {
    let v = cell as usize;
    if v > 1 {
        unsafe { defer(p(v - 1), chain) };
    }
    if v % 2 == 0 {
        unsafe { defer(p(v), b) };
    }
}
unsafe extern "C" fn many(_: *mut u8) {
    (0..MANY).for_each(|i| unsafe { defer(p(i), b) });
}

#[test]
fn deep_chains_and_many_pending_entries() {
    let thread = std::thread::Builder::new().stack_size(64 * 1024);
    let check = || {
        unsafe { run(p(CHAIN), chain) };
        let log = take_log();
        assert_eq!(log.len(), CHAIN / 2);
        assert!(log.windows(2).all(|w| w[0].1 > w[1].1));
        assert!(state().len.get() == 0 && state().cap.get() <= KEEP);
        unsafe { run(p(0), many) };
        assert_eq!(take_log(), (0..MANY).rev().map(|i| ('b', i)).collect::<Vec<_>>());
        assert!(state().len.get() == 0 && state().cap.get() <= KEEP);
    };
    thread.spawn(check).unwrap().join().unwrap();
}

#[test]
fn defer_outside_a_drain_runs_at_once() {
    unsafe { defer(p(4), a) };
    assert_eq!(take_log(), [('a', 4), ('b', 41), ('b', 40)]);
    assert!(!releasing() && state().len.get() == 0);
}

// A drop in a thread-local destructor after the guard's: the drain frees the
// buffer it needed (Miri checks for the leak).
struct Late;
impl Drop for Late {
    fn drop(&mut self) {
        unsafe { run(p(0), walk) };
    }
}
thread_local! {
    static LATE: Late = const { Late };
}
#[test]
fn a_drop_after_the_thread_exit_guard() {
    let thread = std::thread::spawn(|| {
        LATE.with(|_| ());
        unsafe { run(p(0), walk) };
    });
    thread.join().unwrap();
}

// The end work: asked for by releases inside a drain, called once after the
// outermost drain, with the thread no longer draining and nothing pending.
thread_local! {
    static ENDS: RefCell<Vec<(bool, usize)>> = const { RefCell::new(Vec::new()) };
}
unsafe extern "C" fn end() {
    ENDS.with(|e| e.borrow_mut().push((releasing(), state().len.get())));
    log('e', 0);
}
unsafe extern "C" fn asks(cell: *mut u8) {
    log('k', cell as usize);
    want_end_work();
}
unsafe extern "C" fn asking_walk(_: *mut u8) {
    unsafe { defer(p(1), asks) };
    unsafe { defer(p(2), b) };
    unsafe { defer(p(3), asks) };
    unsafe { run(p(0), walk) };
}

#[test]
fn end_work_runs_once_after_the_outermost_drain() {
    set_end_work(end);
    unsafe { run(p(0), asking_walk) };
    let log = take_log();
    assert_eq!(log.last(), Some(&('e', 0)));
    assert_eq!(log.iter().filter(|e| e.0 == 'e').count(), 1);
    assert_eq!(ENDS.with(|e| core::mem::take(&mut *e.borrow_mut())), [(false, 0)]);
    // A drain that nobody asked: no end work.
    unsafe { run(p(0), walk) };
    assert!(!take_log().contains(&('e', 0)));
    // Outside a drain the end work runs at once.
    want_end_work();
    assert_eq!(take_log(), [('e', 0)]);
    assert_eq!(ENDS.with(|e| core::mem::take(&mut *e.borrow_mut())), [(false, 0)]);
}

#[test]
fn symbols_are_exported() {
    let symbols = crate::symbols::exported_symbols();
    for name in ["__reussir_drop_defer", "__reussir_drop_run"] {
        assert!(symbols.iter().any(|s| s.name == name && !s.address.is_null()));
    }
}
