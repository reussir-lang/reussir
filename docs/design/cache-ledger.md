# The Cache Ledger

`rene` keeps the dependencies every build shares (the `reussir-rt` bake, the
bundled `core` sources, `core`'s compiled artifacts) in a per-user cache
(`crates/rene/src/cache.rs`; see also `system-rust-runtime.md` §4). Entries
are content-keyed and immutable once published, so sharing them needs no
coordination beyond a per-key build lock. Removing them does: a build links
the runtime and reads the core sources in place, for minutes, and no
directory listing says which build is doing so or when an entry was last
wanted.

This document describes the ledger that records those facts, and the prune
protocol built on it. The protocol is specified and model-checked in
`cache-prune/CachePrune.tla`. The ledger itself is implemented in
`crates/rene/src/ledger.rs`; pruning and its configuration follow in a
separate change.

## Goals

- Removing an entry never breaks a running build, on Linux, macOS or
  Windows.
- A crash at any point leaves nothing that needs manual repair.
- No ledger failure ever fails a build.
- Pruning is by access age, triggered by size: once the cache is over its
  size limit, a session that just added an entry removes entries whose last
  access is older than a configured age and that no running build uses. The
  cache may stay over the limit when nothing qualifies.

## Two mechanisms, deliberately separate

**Facts live in a TurboKV database**, `<root>/ledger`:

| key | value |
|---|---|
| `entry/<kind>/<key>` | state (`building`, `live`, `pruning`), owning session, size, last use |
| `pin/<session>/<kind>/<key>` | this session wants this entry |
| `session/<id>` | a running (or crashed) rene |
| `total` | summed size of the live entries |
| `schema` | the ledger's schema version |

TurboKV holds an exclusive directory lock while a database is open, so the
ledger is opened for one operation at a time: open, read, one atomic batch,
close. Nothing keeps it open across a build, and a session that finds it held
waits ten milliseconds and retries. The exclusive open makes every operation
serialisable, and lets an operation consult the file system (is this entry's
directory still there?) knowing nobody changes it underneath.

**Liveness lives in OS locks.** Every rene run is a session holding an
exclusive lock on `<root>/sessions/<id>.lock` for its whole life. The
operating system releases it when the process exits, crashed or not. Any
session can tell a dead session from a live one with a single non-blocking
lock attempt. A database cannot observe a crash, so it never decides
liveness; it only records who claimed what, and the lock says whether that
claimant is still there.

## Operations

Every operation begins by **reaping**. For each session row whose lock can be
taken, the session is dead: its pins go, and every row it owned is repaired
from two facts, whether the entry's build lock is free and whether its
directory still exists:

| row owned by a dead session | directory | repair |
|---|---|---|
| `building` | any, build lock free | delete the directory and the row |
| `building` | any, build lock held | leave it: the holder takes the row over on its recheck |
| `pruning` | present | back to `live`, size added to the total |
| `pruning` | gone | delete the row |

The session row itself is removed only once no entry row names it. Otherwise
a later reaper could meet a `building` row whose owner it can no longer test.

**Lookup.** A session that wants an entry pins it and stamps its access time
in one batch, whether or not the entry exists yet. A pin means "I want this
key", so a pruner never selects it. Then:

- `live`: use the entry in place, after checking its own completeness marker
  as before (the ledger promises the entry will not be removed, not that
  every file is intact).
- `pruning`, directory present: reclaim it. The row goes back to `live`, the
  size back into the total. The request is a fresh access, so the entry is
  no longer past its age.
- `pruning`, directory gone: wait, then look up again.
- otherwise: take the entry's build lock and **recheck**.

**Recheck**, under the build lock, re-reads the row, since whoever held the
lock may have published. A usable live entry is used. Otherwise the row
becomes `building`, owned by this session; an unusable live entry is taken
over the same way, its size leaving the total. The caller clears the
directory, builds, publishes, and **registers** the entry: `live`, sized,
counted. Registration returns the new total, which is what decides whether
this session prunes.

**Unpin** drops one pin: `core`'s artifacts are copied into the build
directory, so their entry is unpinned right after the copy. Entries used in
place (the bake, the core sources) stay pinned until the session ends, which
drops all its pins and its row.

**Prune** (next change). Only when the total is over the limit, and only one
pruner at a time (`<root>/prune.lock`, tried, never waited on). In one open,
the pruner selects `live` rows past the age limit with no pin from a live
session and flips them to `pruning`, owned by itself, subtracting their
sizes. Then, one victim per open: re-read the row, and only if it is still
`pruning` and still this pruner's, rename the directory out (one atomic
syscall, inside the critical section) and delete the row in the same batch.
A row that a waiter reclaimed meanwhile reads as `live` and is skipped. The
renamed-out trees are deleted afterwards, outside any lock. A rename that
fails, which Windows allows transiently, puts the row back to `live` in the
same batch.

## Why these rules

Three of them came out of model checking rather than the first draft.

1. **A session row outlives every row that names it.** Deleting it as soon
   as its pins were gone left a `building` row, owned by a dead session whose
   build lock a live session happened to hold at reap time, that nobody could
   ever prove orphaned once that live session also died.
2. **Pin at lookup, not after the entry is found.** Pinning only a found
   entry left a window between taking the build lock and the recheck in which
   a competitor could rebuild and prune the same key, forever.
3. **The rename happens inside the ledger transaction.** Reclaim and removal
   both decide from the row and the directory. With the rename outside the
   ledger, a reclaim could commit between the pruner's mark and its rename,
   and the directory would vanish under its new user.

## Degradation and compatibility

- A ledger that cannot be opened, or that is unlockable, disables itself for
  that session with one warning. The session then trusts entries from their
  on-disk markers alone, as builds did before the ledger, and never removes
  anything.
- A ledger written by a newer rene (`schema` above what this rene knows) is
  treated the same way. Additions to row values are not schema changes:
  unknown fields are ignored, unreadable rows are dropped and re-registered
  by the next session that needs their entry.
- Deleting the whole cache directory is still safe while no rene runs.

## Model

`cache-prune/CachePrune.tla` models sessions, the ledger, the per-key build
lock, the file system state of every entry, and a crash between any two
steps. TLC checks that a live row always has a complete directory, that
nothing a session uses is ever removed, that the total is exact, that
everything on disk has a row, that no row stays transient forever, and that
a session which never crashes eventually gets the entry it asked for. Two
mutation configurations, readers that do not pin and a rename outside the
transaction, must and do fail. The directory's README has the commands.
