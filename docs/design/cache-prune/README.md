# Cache protocol model

`CachePrune.tla` is a TLA+ model of the protocol `../cache-ledger.md`
designs: a single-opener TurboKV ledger whose rows say which directories
are entries and whose pins say which session wants which entry; one OS
lock per session, which only tells a live session from a dead one; one OS
lock per entry, held while a session looks the entry up or builds it;
staging and trash directories named for the session that owns them;
publication and pruning as transactions that rename directories; the
hourly repair; the sweep; and the pruner.

## What the model covers

- Three sessions and two keys. `p1` never crashes and never fails a
  commit; `p2` and `p3` may crash between any two steps, or fail a commit.
  `k1` is read in place (sources, the runtime bake) and stays pinned until
  its session ends; `k2` is copied out (a compiled package) and unpinned at
  once.
- A crash releases the session's OS locks and, if it was inside a
  transaction, TurboKV's lock; its rows, pins and directories stay. A
  normal exit leaves the ledger alone just the same, so `Crash` covers it.
- A transaction that only reads and writes rows is one atomic step; a
  failed one is the step, or nothing, followed by the command's exit, which
  `Crash` covers. A transaction that renames directories takes two: the
  renames, with the ledger held, then the commit or the failed commit. A
  crash may fall between them, which is how live rows without directories
  and directories without rows arise.
- A failed commit renames back and may or may not have been applied:
  TurboKV can log a batch and then report an error, and replays the batch
  at the next open. An applied failure also stands for a power loss that
  kept a commit but not its renames; the reverse is a crash between the
  renames and the commit.
- After a commit, the session deletes its trash in a step of its own, so a
  crash can leave a committed transaction's trash behind.
- Every transaction reaps: the pins of dead sessions go.
- The pruner may remove any subset of the live entries that are there and
  that nobody alive pins, none included. Ages, sizes, the limit, the
  records and the `pruned` stamp are policy or payload, not safety, and
  are left out. The prune past the limit, the hourly repair and `rene
  cache prune` are the same action, `PruneNow`, from a session that keeps
  the pins of what it read in place. With `Repair`, a session that never
  crashes keeps pruning: the hourly repair, as the clock moves on while it
  builds.
- The staging directory and the trash carry their owner in their names;
  the sweep, after a prune and outside the ledger, deletes those of dead
  sessions.

Left out, besides policy: dry runs; several staging directories per
session and key; the deletion of lock files, covered by the contract
below; deletion from outside the protocol; durability of the files, which
the builder ensures before the rename, so a directory at an entry path is
always complete; a transaction undone because Windows refused a rename,
which returns to the state before it and runs again; and a victim the
pruner cannot rename, which it keeps, as if it had not chosen it.

## The lock-file contract

The model takes a session's lock atomically with its start, treats a free
or missing session lock as a dead session, and lets an entry lock be held
by one session at a time. The implementation keeps that sound with one
contract on lock files, which `FileLock` (`crates/rene/src/lock.rs`)
enforces:

1. a lock file is deleted only by a process holding its lock, before it
   releases it; on Windows, where an open lock file cannot be deleted, the
   holder releases first, and the deletion fails harmlessly if anyone has
   opened the file since;
2. after locking, the locker checks that the path still names the file it
   locked (device and inode on Unix; on Windows it always does), and
   starts over if not.

A reaper may delete a session's lock file in the instant between its
creation and its locking; the session then locks a fresh file instead of
the deleted one. Once a session holds its lock, the file stays where
reapers and sweepers look until the session removes it on exit, so a
missing file proves the session gone, and so does a free one. The sweep
deletes free entry lock files under the same contract, which a waiter
survives by locking the file the path names afterwards. Locks are only
trusted on local file systems, which the cache checks before it locks
anything.

## Checked properties

Invariants:

- `UseSafety`: nothing a session uses disappears or is replaced under it.
  What it is using, and every entry it used in place, stays there; the
  ghost `lost` records any session whose directory is renamed away.
- `PinsHeld`: an entry read in place stays pinned for the rest of its
  session.
- `RemovedUnpinned`: only live entries no live session pins are removed.
- `LiveHasDir`: a live row has its directory, unless an unclean
  transaction emptied its entry path: one still open, one whose session
  died, or one whose failed commit was applied.
- `RowlessIsDebris`: a directory at an entry path without a row was left
  there by an unclean transaction.
- `Owned`: a live session's staging directory exists only while it
  publishes (or is failing), and its trash only inside its transaction and
  until it deletes it after the commit. With the two above: everything on
  disk is a live entry, work in progress of a live session, or
  recognizable debris of an unclean transaction.
- `BuildExcl`: only the holder of an entry's lock builds or publishes it.

Temporal properties:

- `Progress`: a session that never crashes and wants an entry eventually
  uses it.
- `Recovery`: everything a dead session left (its staging and trash, its
  pins, debris it left, live rows whose directories its unclean
  transaction removed) is eventually cleaned up, given a session that
  keeps building and repairing.

## Commands

From this directory, with TLC from nixpkgs:

```
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePrune.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoPins.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePrunePruneOutside.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePrunePublishOutside.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoRenameBack.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoDirCheck.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneSweepLive.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoEntryLocks.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoRepair.cfg CachePrune.tla
```

TLC leaves its working files in `states/`; delete it afterwards.

## Results

Measured with TLC 2.19 (nixpkgs `tlaplus`), 20 workers:

| configuration | result | states generated | distinct | depth | time |
|---|---|---|---|---|---|
| `CachePrune.cfg` | no error: every invariant, `Progress`, `Recovery` | 1,082,044 | 305,433 | 57 | 88 s |

Larger runs of the same configuration, invariants only (a copy of
`CachePrune.cfg` without `PROPERTIES`), find no error either: with three
keys, 8,532,680 states generated, 2,533,798 distinct, depth 81; with four
sessions (`p4` may crash too), 95,667,884 generated, 20,503,498 distinct,
depth 73, in three minutes.

Each mutation must fail, and does (the counterexample's length in states;
with several workers TLC does not search strictly breadth-first, so a rerun
may report a trace a state or two longer). All fail within a second, except
`NoRepair`, a liveness failure, which takes a minute.

| configuration | rule dropped | reported | trace |
|---|---|---|---|
| `CachePruneNoPins.cfg` | a lookup pins its key | `UseSafety` | 10 |
| `CachePrunePruneOutside.cfg` | the pruner renames inside its transaction | `UseSafety` | 19 |
| `CachePrunePublishOutside.cfg` | publication renames inside its transaction | `UseSafety` | 12 |
| `CachePruneNoRenameBack.cfg` | a transaction that does not commit renames back | `LiveHasDir` | 12 |
| `CachePruneNoDirCheck.cfg` | a lookup checks a live row's directory | `UseSafety` | 10 |
| `CachePruneSweepLive.cfg` | the sweep deletes only dead sessions' names | `LiveHasDir` | 17 |
| `CachePruneNoEntryLocks.cfg` | entry locks | `BuildExcl` | 4 |
| `CachePruneNoRepair.cfg` | builds repair hourly | `Recovery` | 24, a lasso |

Checked against every invariant (dropping each one reported and running
again until the rest hold), the mutations violate:

| mutation | violated |
|---|---|
| `NoPins` | `UseSafety`, `PinsHeld` |
| `PruneOutside` | `UseSafety`, `RemovedUnpinned`, `LiveHasDir`, `RowlessIsDebris` |
| `PublishOutside` | `UseSafety`, `LiveHasDir`, `RowlessIsDebris`, `Owned` |
| `NoRenameBack` | `LiveHasDir`, `RowlessIsDebris`, `Owned` |
| `NoDirCheck` | `UseSafety` |
| `SweepLive` | `LiveHasDir`; with `Progress` checked instead, `Progress` |
| `NoEntryLocks` | `BuildExcl` only |
| `NoRepair` | no invariant; of the temporal properties, `Recovery` only |

The traces:

- `NoPins`: a session uses an entry it never pinned, and a pruner removes
  it.
- `PruneOutside`: a pruner deletes a row and commits; another session finds
  no row, builds the entry and publishes it, trashing the old directory as
  debris; the pruner's late rename then takes the new entry away from its
  user.
- `PublishOutside`: a publisher renames its directory into place; a prune
  takes the rowless directory for debris; the publisher's row then goes
  live with no directory, and it uses nothing.
- `NoRenameBack`: a prune's commit fails without being applied and leaves
  its victim in the trash, its row live, while the failing session still
  runs.
- `NoDirCheck`: a publication's commit fails after being applied, and its
  rename back leaves a live row without a directory; a lookup hands out the
  missing directory. A prune interrupted between its renames and its
  commit does the same in a longer trace.
- `SweepLive`: a sweep deletes the trash of a session whose prune is about
  to fail, so the rename back has nothing to put back; another trace
  deletes the staging directory of the session that never crashes, which
  then never gets its entry.
- `NoEntryLocks`: two sessions build the same entry at once. Nothing else
  breaks: with `BuildExcl` left out, the configuration checks every other
  invariant, `Progress` and `Recovery` without error (1,429,242 states
  generated, 390,689 distinct, depth 57), because a publication that finds
  a live entry keeps it. The entry lock buys exclusivity, which saves
  duplicate builds, and cheap waiting, which the model does not measure.
- `NoRepair`: a session crashes holding a staging directory; the session
  that never crashes then only ever reuses entries that exist, and since
  nothing takes the cache past its limit, nothing prunes and nothing sweeps.
  Every invariant and `Progress` still hold (the state space is the main
  one): the repair is what makes `Recovery` true, not the size limit.

To make sure the main configuration is not vacuous, a scratch module that
negated each interesting situation, as an invariant or an action property,
found every one reachable: a live row without its directory and debris
with the ledger free; failed commits of publications and of prunes, applied
and not; a session waiting on a builder's entry lock; a lookup forgetting a
row; a dead session's staging directory; a session dying between its
commit and deleting its trash; a repair trashing debris; a prune removing a
victim; a prune running while another session uses an entry; reaping a
dead session's pins; and a sweep deleting a dead session's trash.

## Correspondence with the implementation

| model | `crates/rene/src` |
|---|---|
| a session's start | `ledger::Session::start`: check `lock::remote_file_system`, create and lock `sessions/<sid>.lock` |
| `RPins` (reaping) | `ledger::session::reap`, the first step of `Session::transact` |
| `Pick` | a consumer asking for an entry: `bundled::Bundle::unpack`, `rt::prepare`, `shared::Shared::claim` |
| `Acquire` | `FileLock::lock` on `<kind>/<key>.lock` in `cache::Cache::acquire` |
| `Lookup` | `ledger::Session::lookup`, called by `Cache::acquire` under the entry lock |
| `Build` | the consumer filling `cache::Slot::stage`'s directory; `cache::durable` makes it durable and measures it |
| `PublishBegin`, `PublishCommit` | `cache::Slot::publish`, through `ledger::Session::publish`: `Txn::discard` the debris, `Txn::rename` the staging directory into place, write the row, commit |
| `PublishFail`, `PruneFail` | `ledger::Ledger::transact` renaming back, newest first, whenever the transaction does not commit |
| `DeleteTrash` | `Ledger::transact` deleting the trash after closing the ledger |
| `PublishMissing` | `Session::publish` failing on a staging directory that is gone |
| `Finish` | `Cache::acquire` and `Slot::publish` unpinning a `Usage::Copied` entry (`Session::unpin`); `Usage::InPlace` entries keep their pins |
| `PruneNow` | `main::maintain` after a successful build (past the limit, or a repair when `Session::repair_due`), and `rene cache prune` |
| `PruneBegin`, `PruneCommit` | `ledger::Session::prune`, one transaction |
| `Sweep` | `cache::sweep`, called by `Cache::prune` |
| `Crash`, `Exit` | the process ending: `Drop for Session` removes the session lock (`FileLock::remove`) |
| `PublishOutside`, `PublishRow`, `Late` | mutations only |
