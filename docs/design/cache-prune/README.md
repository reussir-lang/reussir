# Cache ledger protocol model

`CachePrune.tla` is a TLA+ model of the protocol `../cache-ledger.md`
describes: a single-opener TurboKV ledger holding entry rows, pins and
session rows; one OS lock per session that answers only "is this session
still alive"; one OS lock per entry that serializes its builders; and the
pruner. Every process may crash between any two steps, and a normal exit is
a crash as far as the ledger is concerned: a session ends by releasing its
lock, with no ledger operation.

Which entries are past their age is policy and is abstracted: the pruner
may pick any subset of the eligible entries (live, pinned by nobody alive).

## Checked properties

- `Safety`, an invariant:
  - `LiveIsComplete`: a live row has a complete directory.
  - `PinSafety`, `UseSafety`: nothing a session has found and pins, or is
    using, disappears.
  - `PrunedUnpinned`: only entries nobody alive pins are marked.
  - `Tracked`: everything on disk has a row, so nothing leaks.
  - `BuildExcl`: a live builder holds its entry's lock, and only the
    holder writes the directory.
- `Recovery`: no row stays `building` or `pruning` forever.
- `Progress`: a session that never crashes and wants an entry eventually
  uses it.

The ledger stores no running total: it is derived from the live rows, in
the model as in the implementation, so it is exact by construction.

## Correspondence with the implementation

| model | `crates/rene/src` |
|---|---|
| `Start` | `ledger::Session::start`, after taking `sessions/<id>.lock` |
| reaping (`RRow`, `RFs`, `RPins`, `RSessions`) | `ledger/reap.rs`, run by every transaction (`ledger/store.rs`) |
| `Lookup` | `Session::lookup`, called by `Cache::acquire` |
| `Acquire` | the entry lock taken in `Cache::acquire` (`cache/lock.rs`) |
| `Recheck` | `Session::recheck`, under that lock |
| `Unpack`, `Publish` | the consumer's build, then `Slot::publish` or `Slot::commit` |
| `Register` | `Session::register`, called by `Slot::publish`/`Slot::commit` before the lock is released |
| `Finish` | `Session::unpin` |
| `Mark`, `Remove` | `Session::prune` (`ledger/prune.rs`) |
| `Crash` | any exit: `Session`'s drop releases and deletes its lock file |

The model takes a session's lock atomically with its start, and treats a
free lock as a dead session. The implementation keeps that sound with one
contract on lock files, which `FileLock` (`cache/lock.rs`) enforces: a
lock file is deleted only by a process holding its lock, and a locker
checks after locking that the path still names the file it locked,
starting over if not. A reaper may delete a session's lock file in the
instant between its creation and its locking; the session then locks a
fresh file instead of the deleted one. So once a session has written its
row, its lock file stays where reapers look until the session deletes it
on exit, and a missing lock file proves the session gone.

## Rules found by model checking

Three rules of the protocol came out of checking this model rather than
the first draft: a session row outlives every row naming it, a session
pins a key before the entry exists, and the pruner renames inside its
transaction. `../cache-ledger.md` ("Why these rules") explains each.
