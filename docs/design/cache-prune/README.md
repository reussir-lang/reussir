# Cache ledger protocol model

`CachePrune.tla` is a TLA+ model of the protocol `../cache-ledger.md`
describes: a single-opener TurboKV ledger holding entry rows, pins and
session rows; one OS lock per session that answers only "is this session
still alive"; one OS lock per entry that serializes its builders; and the
pruner. Every process may crash between any two steps, and a normal exit is
a crash as far as the ledger is concerned: a session ends by releasing its
lock, with no ledger operation.

Which entries are past their age is policy and is abstracted: the pruner
may pick any subset of the eligible entries (live, pinned by nobody alive)
and of the untracked directories nobody pins.

The model also covers where the implementation departs from the happy
path: sessions that run without the ledger (`Degraded`: it could not be
opened, or a newer rene wrote it), whose entries have no row until a
session with the ledger adopts them or the pruner removes them; a
commit that fails after renaming a victim out, which renames it back and
makes the session give up; and entries read in place, which stay pinned
until their session ends.

Run with TLC (nixpkgs `tlaplus`), from this directory:

```
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePrune.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneLimit0.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoPins.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneRenameOutside.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoRenameBack.cfg CachePrune.tla
```

Every configuration has three sessions and two keys: `p1` never crashes
or fails, `p2` may crash or fail a commit, and `p3` runs without the
ledger; `k1` is read in place and `k2` is copied out.

## Checked properties

- `Safety`, an invariant:
  - `LiveIsComplete`: a live row has a complete directory.
  - `PinSafety`, `UseSafety`: nothing a session with the ledger has found
    and pins, is using, or used in place disappears, and an entry read in
    place stays pinned until its session ends. What a degraded session
    uses is not protected.
  - `PrunedUnpinned`: only entries nobody alive pins are marked.
  - `PruningPresent`: a row under prune keeps its directory while its
    pruner lives, so a session asking for it reclaims it.
  - `Tracked`: a directory without a row was written by a degraded
    session (and is adopted or pruned later), so nothing else leaks.
  - `BuildExcl`: a live builder holds its entry's lock, and only the
    holder writes the directory.
- `Recovery`: no row stays `building` or `pruning` forever.
- `Progress`: a session with the ledger that never crashes and wants an
  entry eventually uses it.

The ledger stores no running total: it is derived from the live rows, in
the model as in the implementation, so it is exact by construction.

## Results

Measured with TLC 2.19 (nixpkgs `tlaplus` 1.7.4):

| configuration | result | states generated | distinct | depth |
|---|---|---|---|---|
| `CachePrune.cfg` | no error | 959,443 | 253,556 | 65 |
| `CachePruneLimit0.cfg` | no error | 1,003,861 | 265,671 | 55 |
| `CachePruneNoPins.cfg` | `UseSafety` violated | | | |
| `CachePruneRenameOutside.cfg` | `LiveIsComplete`, `PinSafety`, `UseSafety`, `PruningPresent`, `Tracked` violated | | | |
| `CachePruneNoRenameBack.cfg` | `PruningPresent` violated | | | |

The three mutations must fail, and do:

- `NoPins` drops the reader's pin: a session uses an entry another
  session has just renamed away.
- `RenameOutside` renames victims outside the ledger transaction: a
  session reclaims an entry under prune, then the pruner renames it away
  anyway.
- `NoRenameBack` keeps a victim renamed out when the transaction that
  renamed it fails to commit: the row says "pruning" with no directory
  behind it while its pruner lives, so sessions asking for the entry wait
  for that process to exit instead of reclaiming it.

With the rename back, the two branches where a session waits for a row
under prune whose directory is gone (in `Lookup` and `Recheck`) are
unreachable; TLC's `-coverage` shows them unvisited. The implementation
keeps them for directories deleted from outside the protocol.

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
| `Finish` | `Session::unpin` after a copied entry (`Usage::Copied`); in-place entries keep their pins |
| `PruneNow`, `Mark`, `Remove`, `RemoveOrphan` | `Session::prune` (`ledger/prune.rs`) |
| `RemoveFailed` | a failed commit renaming its moves back (`ledger/store.rs`) |
| `Crash`, `Exit` | any exit: `Session`'s drop releases and deletes its lock file |
| `DLookup`, `DRecheck` | `Cache::acquire` with a disabled session |

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

Four rules of the protocol came out of checking this model rather than
the first draft: a session row outlives every row naming it, a session
pins a key before the entry exists, the pruner renames inside its
transaction, and a failed commit renames back. `../cache-ledger.md` ("Why
these rules") explains each.
