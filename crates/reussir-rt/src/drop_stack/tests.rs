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
// The pending drops (top run, runs below, their other cells), and whether
// both buffers are back to `KEEP` after a drain.
fn pending() -> usize {
    let s = state();
    s.n.get() as usize + s.stack.len.get() + s.cells.len.get()
}
fn trimmed() -> bool {
    state().stack.cap.get() <= KEEP && state().cells.cap.get() <= KEEP
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
const WALK: [(char, usize); 7] = [
    ('a', 3),
    ('b', 31),
    ('b', 30),
    ('b', 2),
    ('a', 1),
    ('b', 11),
    ('b', 10),
];

#[test]
fn last_deferred_first() {
    unsafe { run(p(0), walk) };
    assert_eq!(take_log(), [[('w', 0)].as_slice(), &WALK].concat());
}

// A nested public call completes its suffix before the callback resumes.
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
fn a_nested_run_completes_its_suffix() {
    unsafe { run(p(0), outer) };
    let head = [('n', 8), ('w', 0)];
    assert_eq!(
        take_log(),
        [head.as_slice(), &WALK, &[('m', 8), ('b', 7)]].concat()
    );
}

// A chain of CHAIN cells, each release deferring the next (and a `b` for an
// even cell), on a small native stack; then MANY pending entries.
unsafe extern "C" fn chain(cell: *mut u8) {
    let v = cell as usize;
    if v > 1 {
        unsafe { defer(p(v - 1), chain) };
    }
    if v.is_multiple_of(2) {
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
        assert!(pending() == 0 && trimmed());
        unsafe { run(p(0), many) };
        assert_eq!(
            take_log(),
            (0..MANY).rev().map(|i| ('b', i)).collect::<Vec<_>>()
        );
        assert!(pending() == 0 && trimmed());
    };
    thread.spawn(check).unwrap().join().unwrap();
}

// Runs: drops deferred one after the other with one release function share
// one entry, each cell but the run's head one pointer in the cells vector.
thread_local! {
    static RUNS: Cell<(usize, u32, usize)> = const { Cell::new((0, 0, 0)) };
}
unsafe extern "C" fn runs_walk(_: *mut u8) {
    // One pointer, reified once: Rust (and Miri) may give a function item a
    // different address at each reification.
    let rb: Release = b;
    (0..5).for_each(|i| unsafe { defer(p(i), rb) });
    unsafe { defer(p(5), a) };
    (6..9).for_each(|i| unsafe { defer(p(i), rb) });
    let s = state();
    RUNS.with(|r| r.set((s.stack.len.get(), s.n.get(), s.cells.len.get())));
}

#[test]
fn runs_of_one_release_share_an_entry() {
    unsafe { run(p(0), runs_walk) };
    // Below: the run of b(0..5) and the run of a(5); on top: b(6..9).
    assert_eq!(RUNS.with(|r| r.get()), (2, 3, 4 + 2));
    let order = [8, 7, 6]
        .map(|i| ('b', i))
        .into_iter()
        .chain([('a', 5), ('b', 51), ('b', 50)]);
    let order: Vec<_> = order.chain((0..5).rev().map(|i| ('b', i))).collect();
    assert_eq!(take_log(), order);
    assert!(pending() == 0 && trimmed());
}

#[test]
fn defer_outside_a_drain_runs_at_once() {
    unsafe { defer(p(4), a) };
    assert_eq!(take_log(), [('a', 4), ('b', 41), ('b', 40)]);
    assert!(!releasing() && pending() == 0);
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
    ENDS.with(|e| e.borrow_mut().push((releasing(), pending())));
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
    assert_eq!(
        ENDS.with(|e| core::mem::take(&mut *e.borrow_mut())),
        [(false, 0)]
    );
    // A drain that nobody asked: no end work.
    unsafe { run(p(0), walk) };
    assert!(!take_log().contains(&('e', 0)));
    // Outside a drain the end work runs at once.
    want_end_work();
    assert_eq!(take_log(), [('e', 0)]);
    assert_eq!(
        ENDS.with(|e| core::mem::take(&mut *e.borrow_mut())),
        [(false, 0)]
    );
}

// Steps. `counting(p)`: p = 100 k + left; defers `b(k + left)` and yields
// while elements are left (each deferral is popped before the next step),
// as a host's array free does.
unsafe fn counting(v: usize) -> bool {
    let (k, left) = (v / 100, v % 100);
    log('s', depth());
    if left == 0 {
        return true;
    }
    unsafe { defer(p(k + left), b) };
    STEP_ARG.with(|c| c.set(v - 1));
    false
}
thread_local! {
    static STEP_ARG: Cell<usize> = const { Cell::new(0) };
}
// The counter's state lives in STEP_ARG: the entry's own argument is fixed.
unsafe extern "C" fn counting_entry(_: usize) -> bool {
    unsafe { counting(STEP_ARG.with(|c| c.get())) }
}
// A step that defers two drops and is done in the same call: its entry
// leaves from under them.
unsafe extern "C" fn done_with_work(_: usize) -> bool {
    unsafe { defer(p(1), b) };
    unsafe { defer(p(2), a) };
    true
}

#[test]
fn a_step_yields_to_what_it_defers() {
    STEP_ARG.with(|c| c.set(1000 + 3));
    unsafe { run_step(0, counting_entry) };
    // Each step sees itself as the only entry; its deferral comes next.
    let s = |n| ('s', n);
    assert_eq!(
        take_log(),
        [s(1), ('b', 13), s(1), ('b', 12), s(1), ('b', 11), s(1)]
    );
    assert!(!releasing() && pending() == 0 && depth() == 0);
}

#[test]
fn a_step_done_with_work_left_above_it() {
    unsafe { run_step(0, done_with_work) };
    assert_eq!(take_log(), [('a', 2), ('b', 21), ('b', 20), ('b', 1)]);
    assert!(pending() == 0 && trimmed());
}

// A step deferred by a release inside a drain joins it, below what the
// release defers after it.
unsafe extern "C" fn defers_a_step(_: *mut u8) {
    STEP_ARG.with(|c| c.set(2000 + 1));
    unsafe { defer_step(0, counting_entry) };
    unsafe { defer(p(9), b) };
}

#[test]
fn steps_inside_a_drain_and_an_empty_drain() {
    drain();
    assert!(take_log().is_empty());
    unsafe { run(p(0), defers_a_step) };
    assert_eq!(take_log(), [('b', 9), ('s', 1), ('b', 21), ('s', 1)]);
    assert!(!releasing() && depth() == 0);
}

// Linked runs: boxes with a wide header (count, then a tag below 2^16 or
// padding: an odd box's upper 16 bits are garbage), deferred with
// `defer_wide`. A release checks that its count and tag are back.
#[repr(C, align(8))]
struct Wide {
    count: u32,
    tag: u32,
    id: usize,
}
fn wide(id: usize) -> *mut u8 {
    let garbage = if id % 2 == 1 { 0xabcd << 16 } else { 0 };
    Box::into_raw(Box::new(Wide {
        count: 1,
        tag: (id as u32 % 7) | garbage,
        id,
    }))
    .cast()
}
unsafe fn take(cell: *mut u8, kind: char) -> usize {
    let b = unsafe { Box::from_raw(cell.cast::<Wide>()) };
    assert_eq!((b.count, b.tag & 0xffff), (1, b.id as u32 % 7));
    log(kind, b.id);
    b.id
}
// `wa(id)` and `wb(id)` run the plan of `id`: `kids(id)` children, each
// deferred wide or as a host cell (`DIFF` deferring all as host cells), or
// a step, or a nested run.
thread_local! {
    static WIDE: Cell<bool> = const { Cell::new(true) };
    // The most cells pending while a release ran.
    static CELLS: Cell<usize> = const { Cell::new(0) };
}
fn kids(id: usize) -> usize {
    if id > 2000 {
        0
    } else {
        (id.wrapping_mul(2654435761) >> 7) % 4
    }
}
unsafe fn plan(id: usize) {
    for k in 0..kids(id) {
        let c = id * 4 + k;
        let f = if c.is_multiple_of(3) { WB } else { WA };
        match (c >> 3) % 5 {
            0 => unsafe { defer_step(c, wide_step) },
            1 => unsafe { run(wide(c), f) },
            2 => unsafe { defer(wide(c), f) },
            _ if WIDE.with(|w| w.get()) => unsafe { defer_wide(wide(c), f) },
            _ => unsafe { defer(wide(c), f) },
        }
    }
}
unsafe extern "C" fn wa(cell: *mut u8) {
    let id = unsafe { take(cell, 'A') };
    CELLS.with(|c| c.set(c.get().max(state().cells.len.get())));
    unsafe { plan(id) };
}
unsafe extern "C" fn wb(cell: *mut u8) {
    let id = unsafe { take(cell, 'B') };
    unsafe { plan(id) };
}
// Reified once each (Miri gives every reification its own address).
static WA: Release = wa;
static WB: Release = wb;
unsafe extern "C" fn wide_step(c: usize) -> bool {
    log('s', c);
    unsafe { plan(c + 1) };
    true
}

#[test]
fn linked_runs_keep_the_order() {
    // The same drops, wide or not: the same events. Wide boxes deferred one
    // after the other with one release join a run of cells; a change of
    // release links through the header.
    let mut logs = vec![];
    for linked in [true, false] {
        WIDE.with(|w| w.set(linked));
        for root in 1..if cfg!(miri) { 10 } else { 60 } {
            unsafe { run(wide(root), if root % 2 == 0 { WB } else { WA }) };
        }
        logs.push(take_log());
        assert!(!releasing() && pending() == 0 && trimmed());
        assert!(CELLS.with(|c| c.replace(0)) > 0);
    }
    assert!(logs[0].len() > if cfg!(miri) { 20 } else { 300 });
    assert_eq!(logs[0], logs[1]);
}

unsafe extern "C" fn la(cell: *mut u8) {
    unsafe { take(cell, 'A') };
}
unsafe extern "C" fn lb(cell: *mut u8) {
    unsafe { take(cell, 'B') };
}
// A walk deferring wide boxes 1 to 6, alternating two releases, then a
// host cell 7, a wide box 8, and wide boxes that cannot link: 9 a few bytes
// off (a new run), 10 after a full table.
unsafe extern "C" fn alternating(_: *mut u8) {
    for id in 1..=6 {
        unsafe { defer_wide(wide(id), if id % 2 == 0 { lb } else { la }) };
    }
    let s = state();
    log('d', s.depth() * 100 + s.cells.len.get());
    unsafe { defer(p(7), b) };
    unsafe { defer_wide(wide(8), la) };
    log('d', s.depth());
    let off = unsafe { wide(9).add(4) };
    unsafe { defer_wide(off, off_release) };
    log('d', s.depth());
    // Fill the table (its entries in use stay).
    TABLE_STATE.with(|t| {
        t.releases[s.known.get() as usize..]
            .iter()
            .for_each(|r| r.set(Some(unnamed)))
    });
    s.known.set(TABLE as u8);
    // One reified function, so that 11 links to 10 (Miri gives each
    // reification its own address).
    let f: Release = lb;
    unsafe { defer_wide(wide(10), f) };
    unsafe { defer_wide(wide(11), f) };
    log('d', s.depth());
}
unsafe extern "C" fn off_release(cell: *mut u8) {
    // Never linked: its "header" (box 9's tag and number) is untouched.
    let words = cell.cast::<u32>();
    assert_eq!(
        unsafe { (words.read(), words.add(1).read()) },
        (2 | 0xabcd << 16, 9)
    );
    drop(unsafe { Box::from_raw(cell.sub(4).cast::<Wide>()) });
    log('o', 9);
}

#[test]
fn boxes_link_through_their_headers() {
    let _ = take_log();
    unsafe { run(p(0), alternating) };
    let log = take_log();
    let (a, b2, d) = (|n| ('A', n), |n| ('B', n), |n| ('d', n));
    let tail = [b2(6), a(5), b2(4), a(3), b2(2), a(1)];
    assert_eq!(
        log,
        [
            &[
                d(100),
                d(3),
                d(4),
                d(5),
                b2(11),
                b2(10),
                ('o', 9),
                a(8),
                ('b', 7)
            ][..],
            &tail
        ]
        .concat()
    );
    assert_eq!(state().known.get(), 0);
}

#[test]
fn symbols_are_exported() {
    let symbols = crate::symbols::exported_symbols();
    for name in [
        "__reussir_drop_defer",
        "__reussir_drop_defer_wide",
        "__reussir_drop_run",
        "__reussir_drop_defer_step",
        "__reussir_drop_run_step",
        "__reussir_drop_defer_indexed",
    ] {
        assert!(
            symbols
                .iter()
                .any(|s| s.name == name && !s.address.is_null())
        );
    }
}

unsafe extern "C" fn nested_same_release(_: *mut u8) {
    unsafe { defer(p(1), b) };
    unsafe { run(p(2), b) };
    log('m', 0);
    unsafe { defer(p(3), b) };
}

#[test]
fn nested_calls_do_not_merge_with_an_earlier_run() {
    unsafe { run(p(0), nested_same_release) };
    assert_eq!(take_log(), [('b', 2), ('m', 0), ('b', 3), ('b', 1)]);
}

unsafe extern "C" fn nested_step(_: usize) -> bool {
    unsafe { run(p(2), b) };
    log('m', 0);
    true
}
unsafe extern "C" fn nested_step_walk(_: *mut u8) {
    unsafe { defer(p(1), b) };
    unsafe { run_step(0, nested_step) };
    log('w', 0);
}

#[test]
fn nested_steps_preserve_the_outer_pending_work() {
    unsafe { run(p(0), nested_step_walk) };
    assert_eq!(take_log(), [('b', 2), ('m', 0), ('w', 0), ('b', 1)]);
}

unsafe extern "C" fn indexed(object: *mut u8, cursor: usize) {
    assert_eq!(object, p(42));
    if cursor < 3 {
        unsafe { defer_indexed(object, cursor + 1, indexed) };
    }
    unsafe { defer(p(cursor), b) };
}

#[test]
fn indexed_work_advances_after_each_child() {
    unsafe { defer_indexed(p(42), 0, indexed) };
    assert_eq!(take_log(), (0..4).map(|i| ('b', i)).collect::<Vec<_>>());
    assert!(!releasing() && pending() == 0);
}

unsafe extern "C" fn indexed_chain(_: *mut u8, cursor: usize) {
    if cursor != 0 {
        unsafe { defer_indexed(p(0), cursor - 1, indexed_chain) };
    }
}

#[test]
fn indexed_work_uses_a_bounded_native_stack() {
    std::thread::Builder::new()
        .stack_size(64 * 1024)
        .spawn(|| {
            unsafe { defer_indexed(p(0), CHAIN, indexed_chain) };
            assert!(pending() == 0 && trimmed());
        })
        .unwrap()
        .join()
        .unwrap();
}
