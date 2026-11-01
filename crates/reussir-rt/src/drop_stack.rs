//! A per-thread stack of pending drops.
//!
//! Drop glue whose release can recurse without bound runs under [`run`],
//! which marks the thread as draining. Inside, each box whose count reaches
//! zero is deferred onto this stack with the function that releases it
//! ([`defer`]), and the outermost [`run`] pops the entries, last deferred
//! first, until none is left, as Lean's `lean_del` does with its to-do list.
//! A popped release may defer more entries. A nested [`run`] completes its
//! own pending work before returning, leaving earlier work pending. Thus the
//! native stack stays bounded however deep the value is, for the box edges
//! the glue defers. Outside a drain the stack is empty. The stack never reads
//! or writes a deferred pointer (its layout is the deferring code's). Drops
//! deferred one after the other with one release function form one run,
//! which takes one entry, and one pointer per cell but its head. Its buffers
//! shrink back to `KEEP` elements after a drain and are freed at thread exit.
//!
//! A host runtime with releases of its own (containers whose elements it
//! frees in a given order) shares the stack, so that one drain pops its work
//! and the glue's in one order, last deferred first: it defers cells with
//! [`defer`], incremental work with [`defer_step`] (a step stays on the
//! stack until it is done, and what it defers is popped before its next
//! step; [`depth`] tells it whether a release deferred work), and drains
//! with [`drain`] or [`run_step`].
//!
//! Drop glue defers a box whose first 8 bytes are header (a 32-bit count,
//! then a 32-bit word that is padding or a tag below 2^16) with
//! [`defer_wide`]: such boxes form one linked run whatever their release
//! functions. A box released by the same function as the box deferred
//! before it joins the run's cells, as a host's cell does (8 bytes while it
//! waits; for a value of one type, usually as many as its height), and the
//! upper 16 bits of its header's second word are set to 0. A box released
//! by another function links to that box through its header, as Lean's
//! to-do list is threaded through its objects, so it costs no memory: its
//! count and those 16 bits keep a 40-bit signed offset in 8-byte units and,
//! in the high 8 bits, 1 plus the index of the previous box's function in a
//! per-thread table emptied with the stack. So a pop tells a link (nonzero
//! bits) from a cell. A link goes back (count 1, upper bits 0) before the
//! box's release runs. A box that cannot link starts a new run: one with a nonzero top byte (under top-byte tagging, an
//! immediate's dummy box, which the glue never defers), one too far from
//! the previous box (or not a multiple of 8 bytes away), or a new function
//! with the table full. A pop rebuilds the linked box's pointer from its
//! address, whose provenance was exposed (`-Zmiri-strict-provenance`
//! rejects that, as expected; Miri's default mode accepts it). The stack never
//! writes a cell deferred with [`defer`] (a host's).
//!
//! A host runtime whose releases may need work that must not run inside a
//! drain (code that may block or switch contexts, which would leave the
//! thread's drain suspended) registers one end function ([`set_end_work`]);
//! a release asks for it ([`want_end_work`]), and the outermost drain calls
//! it once it has ended, with the thread no longer draining and nothing
//! pending. A drain that nobody asked costs one test at its end.

use core::cell::Cell;
use core::mem::ManuallyDrop;
use core::ptr::NonNull;
use core::sync::atomic::{AtomicPtr, Ordering};

/// Releases one deferred pointer.
pub type Release = unsafe extern "C" fn(*mut u8);

/// One step of incremental work on `p` ([`defer_step`]); answers whether the
/// work is done. `extern "C"`, as a release: a step that panics aborts
/// instead of unwinding out of the drain with the thread still draining and
/// its entry left.
pub type Step = unsafe extern "C" fn(usize) -> bool;

/// Releases one indexed part without modifying the object to store its cursor.
pub type Indexed = unsafe extern "C" fn(*mut u8, usize);

/// An entry below the top of the stack.
#[derive(Clone, Copy)]
enum Work {
    /// `n` deferred cells, the last deferred at `head`, to be released with
    /// `release`; the run's other `n - 1` cells are in the cells vector,
    /// below the cells of the runs above it (see [`State`]); in a linked
    /// run (`n` with [`LINKED`]) a box released by another function than
    /// the one before it links to it through its header ([`defer_wide`]).
    Run {
        release: Release,
        head: *mut u8,
        n: u32,
    },
    /// Incremental work: `step(p)` runs until it answers that it is done.
    Step(usize, Step),
    Indexed(*mut u8, usize, Indexed),
}

/// Capacity each vector keeps after a drain.
const KEEP: usize = 64;

/// The release functions a table holds, and the slots of its cache.
const TABLE: usize = 255;
const CACHE: usize = 16;
/// A run's `n` in the stack vector: whether the run is linked.
const LINKED: u32 = 1 << 31;
/// The most cells a run holds.
const MAX_RUN: u32 = LINKED - 1;
/// Link offsets, in 8-byte units: 40 bits, signed.
const OFFSET_BITS: u32 = 40;

/// A `Vec<T>` by its raw parts, in cells (the state has no destructor; the
/// guard frees the buffers).
struct Parts<T> {
    len: Cell<usize>,
    ptr: Cell<NonNull<T>>,
    cap: Cell<usize>,
}

impl<T> Parts<T> {
    const fn new() -> Self {
        Parts {
            len: Cell::new(0),
            ptr: Cell::new(NonNull::dangling()),
            cap: Cell::new(0),
        }
    }

    fn with_vec(&self, f: impl FnOnce(&mut Vec<T>)) {
        let (ptr, len, cap) = (self.ptr.get().as_ptr(), self.len.get(), self.cap.get());
        let mut v = ManuallyDrop::new(unsafe { Vec::from_raw_parts(ptr, len, cap) });
        f(&mut v);
        self.ptr
            .set(unsafe { NonNull::new_unchecked(v.as_mut_ptr()) });
        self.len.set(v.len());
        self.cap.set(v.capacity());
    }

    /// Makes room for one more element.
    #[cold]
    #[inline(never)]
    fn grow(&self) {
        if self.cap.get() == 0 {
            // Past the guard's destruction the drain frees the buffer (`exited`).
            let _ = GUARD.try_with(|_| ());
        }
        self.with_vec(|v| v.reserve(1));
    }

    /// Pushes `x`; answers false, changing nothing, when the vector is full.
    #[inline(always)]
    fn try_push(&self, x: T) -> bool {
        let len = self.len.get();
        if len == self.cap.get() {
            return false;
        }
        unsafe { self.ptr.get().as_ptr().add(len).write(x) };
        self.len.set(len + 1);
        true
    }

    /// The last element, removed. The vector is not empty.
    #[inline(always)]
    unsafe fn pop_unchecked(&self) -> T {
        let len = self.len.get() - 1;
        self.len.set(len);
        unsafe { self.ptr.get().as_ptr().add(len).read() }
    }

    /// After a drain: the buffer back to `KEEP` elements, or freed once the
    /// thread's exit guard has run.
    fn trim(&self, exited: bool) {
        if exited {
            self.with_vec(|v| *v = Vec::new());
        } else if self.cap.get() > KEEP {
            self.with_vec(|v| v.shrink_to(KEEP));
        }
    }
}

/// The thread's pending drops: the entries of the stack vector, then the run
/// `(n, head, release)` on top if `n` is not 0 (a run on top is always kept
/// there; when `n` is 0 the last entry is a step or indexed callback). Drops deferred one after the
/// other with one release function form one run: the run is one entry, and
/// each cell but its head one pointer in the cells vector (8 bytes while it
/// waits, as the boxed heads of a list that the glue frees whole); boxes
/// deferred with [`defer_wide`] one after the other form one linked run,
/// whatever their functions: a change of function links through the box's
/// header. The top
/// run is kept in the state itself, so a deferral or a pop that stays in it
/// touches no vector but the cells'. Also whether the thread drains, whether
/// a release asked for the end work, and whether its exit guard ran (a later
/// drain then frees its buffers).
struct State {
    n: Cell<u32>,
    releasing: Cell<bool>,
    floor: Cell<usize>,
    end_work: Cell<bool>,
    exited: Cell<bool>,
    /// Whether the run on top is linked.
    linked: Cell<bool>,
    head: Cell<*mut u8>,
    release: Cell<Release>,
    stack: Parts<Work>,
    cells: Parts<*mut u8>,
    /// The entries of [`TABLE_STATE`]'s table in use.
    known: Cell<u8>,
}

/// No release function (the top run's while no run is on top).
unsafe extern "C" fn unnamed(_: *mut u8) {
    unreachable!("reussir-rt: a pending drop without a release function")
}

thread_local! {
    static STATE: State = const {
        State {
            n: Cell::new(0),
            releasing: Cell::new(false),
            floor: Cell::new(0),
            end_work: Cell::new(false),
            exited: Cell::new(false),
            linked: Cell::new(false),
            head: Cell::new(core::ptr::null_mut()),
            release: Cell::new(unnamed as Release),
            stack: Parts::new(),
            cells: Parts::new(),
            known: Cell::new(0),
        }
    };
    /// The release functions that links name by index, and a cache of
    /// (function, index) pairs, direct-mapped by address (zero-initialized,
    /// so it adds nothing to the image a new thread copies).
    static TABLE_STATE: Table = const {
        Table { releases: [const { Cell::new(None) }; TABLE], cache: [const { Cell::new((0, 0)) }; CACHE] }
    };
    static GUARD: Guard = const { Guard };
}

struct Table {
    releases: [Cell<Option<Release>>; TABLE],
    cache: [Cell<(usize, u8)>; CACHE],
}

/// The upper half of a box header's second word (its bits 16 to 31,
/// whatever the byte order).
#[inline(always)]
fn upper(cell: *mut u8) -> *mut u16 {
    cell.wrapping_add(if cfg!(target_endian = "little") { 6 } else { 4 })
        .cast()
}

/// Frees the buffers at thread exit (registered with the first buffer).
struct Guard;

impl Drop for Guard {
    fn drop(&mut self) {
        let _ = STATE.try_with(|state| {
            state.exited.set(true);
            state.stack.with_vec(|v| *v = Vec::new());
            state.cells.with_vec(|v| *v = Vec::new());
        });
    }
}

/// The current thread's state (no destructor: it outlives the others).
#[inline(always)]
fn state() -> &'static State {
    STATE.with(|state| unsafe { &*(state as *const State) })
}

impl State {
    /// Moves the run on top, if any, to the stack vector (another run goes
    /// on top). Answers false, changing nothing, when the vector is full.
    #[inline(always)]
    fn try_spill(&self) -> bool {
        let n = self.n.get();
        if n == 0 {
            return true;
        }
        let (release, head) = (self.release.get(), self.head.get());
        let n = if self.linked.get() { n | LINKED } else { n };
        if !self.stack.try_push(Work::Run { release, head, n }) {
            return false;
        }
        self.n.set(0);
        true
    }

    /// After the top entry left: the stack vector's last entry is the top
    /// now, kept in the state if it is a run.
    #[inline(always)]
    fn unspill(&self) {
        let len = self.stack.len.get();
        if len > self.floor.get() {
            let last = unsafe { self.stack.ptr.get().as_ptr().add(len - 1).read() };
            if let Work::Run { release, head, n } = last {
                self.release.set(release);
                self.head.set(head);
                self.n.set(n & MAX_RUN);
                self.linked.set(n & LINKED != 0);
                self.stack.len.set(len - 1);
                return;
            }
        }
        self.n.set(0);
    }

    /// The number of entries.
    #[inline(always)]
    fn depth(&self) -> usize {
        self.stack.len.get() + (self.n.get() != 0) as usize
    }

    /// Whether a drop deferred with `release` joins the run on top (one with
    /// the same function and room for one more cell).
    #[inline(always)]
    fn joins(&self, release: Release) -> bool {
        let n = self.n.get();
        n != 0
            && n < MAX_RUN
            && !self.linked.get()
            && self.release.get() as usize == release as usize
    }

    /// Defers `release(cell)`: adds it to the run on top, or starts a run.
    /// Answers false, changing nothing, when that needs a vector to grow.
    #[inline(always)]
    unsafe fn try_defer(&self, cell: *mut u8, release: Release) -> bool {
        if self.joins(release) {
            if !self.cells.try_push(self.head.get()) {
                return false;
            }
            self.head.set(cell);
            self.n.set(self.n.get() + 1);
            return true;
        }
        unsafe { self.try_start(cell, release, false) }
    }

    /// Starts a run (linked or not) with `cell` on top. Answers false,
    /// changing nothing, when the stack vector must grow first.
    #[inline(always)]
    unsafe fn try_start(&self, cell: *mut u8, release: Release, linked: bool) -> bool {
        if !self.try_spill() {
            return false;
        }
        self.head.set(cell);
        self.release.set(release);
        self.n.set(1);
        self.linked.set(linked);
        true
    }

    /// The index of `prev` in the table, through the cache; None with the
    /// table full.
    #[inline(always)]
    fn index(&self, prev: Release) -> Option<u32> {
        let key = prev as usize;
        TABLE_STATE.with(|t| {
            let slot = &t.cache[(key >> 4) % CACHE];
            if slot.get().0 == key {
                return Some(slot.get().1 as u32);
            }
            let known = self.known.get() as usize;
            let same = |r: &Cell<Option<Release>>| r.get().is_some_and(|r| r as usize == key);
            let i = match t.releases[..known].iter().rposition(same) {
                Some(i) => i,
                None if known < TABLE => {
                    t.releases[known].set(Some(prev));
                    self.known.set(known as u8 + 1);
                    known
                }
                None => return None,
            };
            slot.set((key, i as u8));
            Some(i as u32)
        })
    }

    /// Defers a box with a wide header ([`defer_wide`]): adds it to the
    /// linked run on top (a cell after a box of its function, else a link),
    /// or starts a linked run. Answers false, changing nothing, when a
    /// vector must grow first.
    #[inline(always)]
    unsafe fn try_defer_wide(&self, cell: *mut u8, release: Release) -> bool {
        let (n, head, prev) = (self.n.get(), self.head.get(), self.release.get());
        // The offset to the head, in 8-byte units (modulo the address width,
        // as the decoding adds it back).
        let diff = (head as usize).wrapping_sub(cell as usize);
        let units = (diff as isize >> 3) as i64;
        if n != 0 && n < MAX_RUN && self.linked.get() && prev as usize == release as usize {
            if !self.cells.try_push(head) {
                return false;
            }
            unsafe { upper(cell).write(0) };
            self.head.set(cell);
            self.n.set(n + 1);
            return true;
        }
        if n != 0 && n < MAX_RUN && self.linked.get() && cell as u64 >> 56 == 0 && diff & 7 == 0 {
            // The box is the caller's alone until its release: its count and
            // the upper half of the word after it are free.
            let link = if units << (64 - OFFSET_BITS) >> (64 - OFFSET_BITS) == units {
                self.index(prev)
                    .map(|i| ((units >> 32) as u16 & 0xff) | ((i as u16 + 1) << 8))
            } else {
                None
            };
            if let Some(high) = link {
                unsafe {
                    (cell as *mut u32).write(units as u32);
                    upper(cell).write(high);
                }
                head.expose_provenance();
                self.head.set(cell);
                self.release.set(release);
                self.n.set(n + 1);
                return true;
            }
        }
        unsafe { self.try_start(cell, release, true) }
    }

    /// Takes the head off the linked run on top (it has another box): with
    /// its upper bits 0 the cells' last one is the head now, else the box it
    /// links to (its header restored).
    #[inline(always)]
    unsafe fn unlink(&self, head: *mut u8) {
        let high = unsafe { upper(head).read() };
        self.n.set(self.n.get() - 1);
        if high == 0 {
            self.head.set(unsafe { self.cells.pop_unchecked() });
            return;
        }
        let low = unsafe { (head as *mut u32).read() };
        unsafe {
            (head as *mut u32).write(1);
            upper(head).write(0);
        }
        let units = ((high as u64 & 0xff) << 32 | low as u64) as i64;
        let units = units << (64 - OFFSET_BITS) >> (64 - OFFSET_BITS);
        let prev = (head as usize).wrapping_add((units as usize).wrapping_mul(8));
        self.head.set(core::ptr::with_exposed_provenance_mut(prev));
        let index = (high >> 8) as usize - 1;
        self.release
            .set(TABLE_STATE.with(|t| t.releases[index].get().unwrap_or(unnamed)));
    }

    /// Pops and runs the pending drops, last deferred first, until none is
    /// left (the thread draining).
    unsafe fn pop_all(&self) {
        loop {
            let n = self.n.get();
            if n != 0 {
                let (head, release) = (self.head.get(), self.release.get());
                if n == 1 {
                    self.unspill();
                } else if self.linked.get() {
                    unsafe { self.unlink(head) };
                } else {
                    self.head.set(unsafe { self.cells.pop_unchecked() });
                    self.n.set(n - 1);
                }
                unsafe { release(head) };
                continue;
            }
            let len = self.stack.len.get();
            if len == self.floor.get() {
                break;
            }
            if let Work::Indexed(object, cursor, callback) =
                unsafe { self.stack.ptr.get().as_ptr().add(len - 1).read() }
            {
                self.stack.len.set(len - 1);
                self.unspill();
                unsafe { callback(object, cursor) };
                continue;
            }
            // The top entry is a step: it stays while it runs, so whatever it
            // pushes is done before its next step.
            let index = len - 1;
            let Work::Step(p, step) = (unsafe { self.stack.ptr.get().as_ptr().add(index).read() })
            else {
                unreachable!("reussir-rt: the stack vector's last entry is a run")
            };
            if unsafe { step(p) } {
                // Whatever the step pushed stays above it (its runs' cells are
                // above every cell of the runs below the step).
                let now = self.stack.len.get();
                unsafe {
                    let at = self.stack.ptr.get().as_ptr().add(index);
                    core::ptr::copy(at.add(1), at, now - index - 1);
                }
                self.stack.len.set(now - 1);
                if now == len && self.n.get() == 0 {
                    self.unspill();
                }
            }
        }
    }

    fn reset(&self) {
        debug_assert_eq!(
            self.cells.len.get(),
            0,
            "reussir-rt: pending cells left after a drain"
        );
        if self.known.get() != 0 {
            self.known.set(0);
            TABLE_STATE.with(|t| t.cache.iter().for_each(|slot| slot.set((0, 0))));
        }
        let exited = self.exited.get();
        self.stack.trim(exited);
        self.cells.trim(exited);
    }
}

/// Whether the current thread drains: a release started now joins the drain.
#[inline(always)]
pub fn releasing() -> bool {
    state().releasing.get()
}

/// The number of entries on the stack. Work deferred while a step runs (the
/// step is then the top entry) makes a new entry, so a step can tell whether
/// a release it made deferred work. 0 whenever [`releasing`] is false.
#[inline(always)]
pub fn depth() -> usize {
    state().depth()
}

/// Pushes incremental work: `step(p)` runs, while the entry stays on the
/// stack, until it answers that it is done; what it defers meanwhile is
/// popped before its next step. Outside a drain, call [`drain`] right after
/// ([`run_step`]). Safety: `step(p)` must be sound whenever it runs.
pub unsafe fn defer_step(p: usize, step: Step) {
    let state = state();
    debug_assert!(
        state.releasing.get() || (state.depth() == 0 && state.cells.len.get() == 0),
        "reussir-rt: work pushed outside a drain while other work is pending"
    );
    if !state.try_spill() {
        state.stack.grow();
        let spilled = state.try_spill();
        debug_assert!(spilled);
    }
    if !state.stack.try_push(Work::Step(p, step)) {
        state.stack.grow();
        let pushed = state.stack.try_push(Work::Step(p, step));
        debug_assert!(pushed);
    }
}

/// Defers one indexed callback. Enqueue its successor before releasing the
/// current child to visit children in forward order. Outside a drain, runs
/// the callback and its pending work before returning.
/// Safety: `callback(object, cursor)` must be sound whenever it runs.
pub unsafe fn defer_indexed(object: *mut u8, cursor: usize, callback: Indexed) {
    let state = state();
    let outermost = !state.releasing.get();
    if !state.try_spill() {
        state.stack.grow();
        let spilled = state.try_spill();
        debug_assert!(spilled);
    }
    if !state
        .stack
        .try_push(Work::Indexed(object, cursor, callback))
    {
        state.stack.grow();
        let pushed = state
            .stack
            .try_push(Work::Indexed(object, cursor, callback));
        debug_assert!(pushed);
    }
    if outermost {
        drain();
    }
}

/// Runs incremental work and everything it defers before returning.
/// Safety: as [`defer_step`].
pub unsafe fn run_step(p: usize, step: Step) {
    if releasing() {
        nested(|| unsafe { defer_step(p, step) });
    } else {
        unsafe { defer_step(p, step) };
        drain();
    }
}

/// Runs the pending drops, last deferred first, then the end work if a
/// release asked for it. Inside a drain, returns at once: the outermost one
/// runs whatever is pushed meanwhile.
#[inline(always)]
pub fn drain() {
    let state = state();
    if state.releasing.get() || state.depth() == 0 {
        return;
    }
    drain_outermost();
}

#[inline(never)]
fn drain_outermost() {
    let state = state();
    state.releasing.set(true);
    unsafe { state.pop_all() };
    finish(state);
}

/// Defers `release(cell)` until the current thread's drain pops it. Outside
/// a drain, runs it at once, as a [`run`] of it. Safety: `release(cell)` must
/// be sound whenever it runs.
#[inline(always)]
pub unsafe fn defer(cell: *mut u8, release: Release) {
    let state = state();
    if !state.releasing.get() {
        return unsafe { run_outermost(cell, release) };
    }
    if !unsafe { state.try_defer(cell, release) } {
        unsafe { defer_slow(cell, release) }
    }
}

/// Defers `release(cell)` as [`defer`] does, for a box whose first 8 bytes
/// are header: a 32-bit count, then a 32-bit word whose upper 16 bits are 0
/// (a tag below 2^16) or padding. The stack may link the box through those
/// 48 bits while it waits (see the module comment) and writes count 1 and
/// upper bits 0 back before `release(cell)` runs. Safety: as [`defer`]; the
/// caller owns the box alone, and nothing reads or writes it until its
/// release runs; the box is 4-byte aligned.
#[inline(always)]
pub unsafe fn defer_wide(cell: *mut u8, release: Release) {
    let state = state();
    if !state.releasing.get() {
        return unsafe { run_outermost(cell, release) };
    }
    if !unsafe { state.try_defer_wide(cell, release) } {
        unsafe { defer_wide_slow(cell, release) }
    }
}

#[cold]
#[inline(never)]
unsafe extern "C" fn defer_wide_slow(cell: *mut u8, release: Release) {
    let state = state();
    let n = state.n.get();
    if n != 0
        && n < MAX_RUN
        && state.linked.get()
        && state.release.get() as usize == release as usize
    {
        state.cells.grow();
    } else {
        state.stack.grow();
    }
    let deferred = unsafe { state.try_defer_wide(cell, release) };
    debug_assert!(deferred);
}

/// Runs `walk(object)` and its pending drops before returning. A nested
/// call completes only its own work. Safety: `walk(object)` must be sound now.
#[inline(always)]
pub unsafe fn run(object: *mut u8, walk: Release) {
    if releasing() {
        nested(|| unsafe { walk(object) });
    } else {
        unsafe { run_outermost(object, walk) };
    }
}

fn nested(walk: impl FnOnce()) {
    let state = state();
    if !state.try_spill() {
        state.stack.grow();
        let spilled = state.try_spill();
        debug_assert!(spilled);
    }
    let floor = state.floor.replace(state.stack.len.get());
    walk();
    unsafe { state.pop_all() };
    state.floor.set(floor);
    state.unspill();
}

#[inline(never)]
unsafe extern "C" fn run_outermost(object: *mut u8, walk: Release) {
    let state = state();
    state.releasing.set(true);
    unsafe { walk(object) };
    unsafe { state.pop_all() };
    finish(state);
}

/// The end of an outermost drain: the thread no longer drains; then the end
/// work, if a release asked for it.
#[inline(always)]
fn finish(state: &State) {
    state.reset();
    state.releasing.set(false);
    if state.end_work.get() {
        end_work(state);
    }
}

/// The end function a host registered ([`set_end_work`]), or null.
static END_WORK: AtomicPtr<()> = AtomicPtr::new(core::ptr::null_mut());

/// The function the end of a drain calls when a release asked for it
/// ([`want_end_work`]). One per process: a later call replaces it.
pub fn set_end_work(f: unsafe extern "C" fn()) {
    END_WORK.store(f as *mut (), Ordering::Release);
}

/// Asks for the end function once the current thread's outermost drain is
/// over. Outside a drain, calls it at once, as [`defer`] runs a release.
#[inline(always)]
pub fn want_end_work() {
    let state = state();
    if state.releasing.get() {
        state.end_work.set(true);
    } else {
        end_work(state);
    }
}

/// Uses up the request and calls the end function: the drain is over (the
/// thread no longer drains, nothing is pending), so it may drop values,
/// whose drains then end on their own. A drain inside the end function
/// whose releases ask for the end work again runs it at its own end, inside
/// this call: one level of nesting per generation, as native Lean's frees
/// nest inside the dependents they run (deliberate; a host whose end work
/// can chain without bound bounds it itself).
#[cold]
#[inline(never)]
fn end_work(state: &State) {
    state.end_work.set(false);
    let f = END_WORK.load(Ordering::Acquire);
    if !f.is_null() {
        let f = unsafe { core::mem::transmute::<*mut (), unsafe extern "C" fn()>(f) };
        unsafe { f() };
    }
}

/// [`State::try_defer`] when a vector must grow first: the cells vector if
/// the drop joins the run on top, else the stack vector (for the run it
/// spills).
#[cold]
#[inline(never)]
unsafe extern "C" fn defer_slow(cell: *mut u8, release: Release) {
    let state = state();
    if state.joins(release) {
        state.cells.grow();
    } else {
        state.stack.grow();
    }
    let deferred = unsafe { state.try_defer(cell, release) };
    debug_assert!(deferred);
}

/// C entry point of [`defer`], called by compiled drop glue.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer(cell: *mut u8, release: Release) {
    unsafe { defer(cell, release) }
}

/// C entry point of [`defer_wide`], called by compiled drop glue.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer_wide(cell: *mut u8, release: Release) {
    unsafe { defer_wide(cell, release) }
}

/// C entry point of [`run`], called by compiled drop glue.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_run(object: *mut u8, walk: Release) {
    unsafe { run(object, walk) }
}

/// C entry point of [`defer_step`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer_step(p: usize, step: Step) {
    unsafe { defer_step(p, step) }
}

/// C entry point of [`run_step`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_run_step(p: usize, step: Step) {
    unsafe { run_step(p, step) }
}

/// C entry point of [`defer_indexed`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_drop_defer_indexed(
    object: *mut u8,
    cursor: usize,
    callback: Indexed,
) {
    unsafe { defer_indexed(object, cursor, callback) }
}

#[cfg(test)]
mod tests;
