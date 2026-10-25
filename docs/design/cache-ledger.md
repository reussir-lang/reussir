# The Cache Ledger

Status: implemented in `crates/rene/src/ledger/`, with the configuration
in `crates/rene/src/config.rs`.

`rene` keeps what every build has in common (the bundled libraries'
sources, the `reussir-rt` bake, the bundled libraries' compiled artifacts)
in a per-user cache (`crates/rene/src/cache/`; `system-rust-runtime.md`
§4). Entries are content-keyed and never change once complete, and every
consumer uses one through the same protocol, `Cache::acquire`: use the
entry if it is complete, otherwise take its lock, check again, and build
it. Sharing entries needs nothing more. Removing them does: a build links
the runtime and reads the library sources in place for minutes, and no
directory listing says which build is doing so, or when an entry was last
wanted.

This document describes the ledger that records those facts and the
pruning built on it. The protocol is specified and model-checked in
`cache-prune/CachePrune.tla`.

## Goals

- Removing an entry never breaks a running build, on Linux, macOS, or
  Windows.
- A crash at any point leaves nothing that needs manual repair.
- No ledger failure fails a build.
- Pruning is by access age, triggered by size: a build that took the cache
  over its size limit removes, once it is done, the entries that went
  unused for longer than a configured age and that no running build uses.
  The cache may stay over the limit when nothing qualifies.

## Facts and liveness, kept apart

**Facts live in a TurboKV database**, `<root>/ledger`:

| key | value |
|---|---|
| `schema` | the ledger's schema version |
| `session/<id>` | a running (or ended) rene: its pid and start time |
| `pin/<session>/<kind>/<key>` | this session wants this entry |
| `entry/<kind>/<key>` | state (`building`, `live`, `pruning`), owning session for the first and last, size, last use |

There is no running total. The rows are few and every operation loads all
of them, so the total is summed from the live rows each time and cannot
drift from them.

TurboKV holds an exclusive directory lock while a database is open, so the
ledger is opened for one operation at a time: open, load, reap, operate,
write one atomic batch, close. Nothing keeps it open across a build, and a
session that finds it open elsewhere polls every ten milliseconds. The
exclusive open makes every operation serializable, and lets an operation
consult the file system (is this directory still there? is this lock
free?) knowing that no other ledger operation runs meanwhile.

**Liveness lives in OS locks.** Every rene run is a session holding the
lock `<root>/sessions/<id>.lock` for its whole life, and that lock is its
only liveness signal: the operating system releases it when the process
exits, crashed or not. A session whose lock is free, or whose lock file is
gone, is dead. Nothing is inferred from pids or timestamps.

Lock files can be deleted (a pruner removes the entry locks of entries it
removed, a session removes its own on exit), which is sound because of the
contract `FileLock` keeps (`crates/rene/src/cache/lock.rs`):

1. a lock file is deleted only by a process holding its lock;
2. after locking, the locker checks that the path still names the file it
   locked (device and inode on Unix; on Windows lock files are opened
   without `FILE_SHARE_DELETE`, so they cannot be deleted while open), and
   starts over if not.

So no two processes ever hold "the same" lock on different files. A
session takes its lock before it writes its row; if a reaper deletes the
fresh file in between, the session locks a new one. Once the row exists,
the lock file stays where reapers look until the session deletes it, and a
missing lock file proves the session gone.

A session ends by releasing its lock and deleting the file, with no ledger
operation. The next operation of any session reaps it exactly as it would
reap a crashed one: one code path, exercised by every build.

## Transactions

A ledger operation runs between the open and the close, and follows three
rules.

- It never awaits and does only bounded I/O: lock probes, `stat`, listing
  the kind directories, and renames. It deletes no directory tree.
- A directory leaves the cache by a rename to a trash sibling,
  `<key>.rm-<pid>-<n>`, inside the operation. The trash is deleted after
  the commit, once the ledger is closed.
- If the commit fails, every rename the operation did is undone before it
  returns, and the session disables itself (see Degradation).

Every operation begins by **reaping**. Each session row other than the
caller's whose lock is free or gone belongs to a dead session: its pins go,
and each row it owned is repaired from two facts, whether the entry's lock
is free and whether its directory is there.

| row owned by a dead session | condition | repair |
|---|---|---|
| `building` | entry lock free | rename the directory out, delete the row |
| `building` | entry lock held | leave it: the holder takes the row over |
| `pruning` | directory present | back to `live`, its last use kept |
| `pruning` | directory gone | delete the row |

The session row goes only once no entry row names it, and its lock file is
deleted while held. A lock file in `sessions/` with no row (a rene that
crashed between taking its lock and writing its row) is deleted once free.

## Using an entry

`Cache::acquire` consults the build's session; the consumers do not know
the ledger exists.

1. **Lookup.** Pin the key and stamp its last use, whether or not the
   entry exists yet. A pin means "I want this key", so a pruner never
   selects it. Then, by the row:
   - `live`: run the consumer's `ready` check, with no lock; a complete
     entry is used. (The ledger promises the entry will not be removed,
     not that every file is intact, so `ready` stays the judge.)
   - `pruning`, directory present: reclaim it. The row goes back to `live`;
     the request is a fresh use, so the entry is no longer old. Use it as
     a live entry.
   - `pruning`, directory gone: wait for the pruner, saying so once, and
     look up again.
   - `building`, or no row: go on to the lock.
2. **Lock.** Take the entry's lock, waiting for the rene that holds it and
   saying so once.
3. **Recheck**, under the lock, with `ready`'s verdict, since whoever held
   the lock may have completed the entry meanwhile:
   - complete: the row becomes `live` and the entry is used. A live row
     stays as it is, a `pruning` row is reclaimed, and a `building` row (a
     builder that died after completing the entry) or a missing row (an
     entry completed without the ledger) is **adopted** with its measured
     size. A complete entry is never rebuilt.
   - incomplete, `pruning`, directory gone: release the lock, wait, and
     look up again.
   - otherwise the row becomes `building`, owned by this session, and the
     consumer builds the entry through its slot.
4. **Register.** Completing the entry (`Slot::publish`, `Slot::commit`)
   marks the row `live` with its measured size before the lock is
   released. The session remembers the total it saw.

Entries copied out of the cache (compiled packages) are unpinned right
after the copy or the publication. Entries read in place (sources, the
runtime bake) stay pinned until the session ends.

## Pruning

`Session::prune(max_age, dry_run)` removes the live entries last used more
than `max_age` ago that nobody pins, and the untracked directories (no row)
whose modification time is as old and that nobody pins or locks:

1. Only one pruner runs at a time: `<root>/prune.lock` is tried, never
   waited on, and a second pruner simply skips.
2. **Mark**, in one operation: flip the aged, unpinned `live` rows to
   `pruning`, owned by the pruner, and list the aged, unpinned untracked
   directories. Count what is kept because it is pinned or recent.
3. **Remove**, one victim per operation: re-read the row, and only if it
   is still `pruning` and still this pruner's, rename the directory out
   and delete the row in the same batch, then delete the entry's lock file
   if it is free. A row a session reclaimed meanwhile reads as `live` and
   is skipped. A rename that fails, which Windows allows transiently, puts
   the row back to `live`.
4. **Remove untracked**, one per operation: still no row, still unpinned,
   and the entry lock free (held while renaming): rename the directory
   out.
5. **Sweep**, after the operations: delete trash siblings, staging
   siblings (`<key>.tmp-<pid>-<n>`) whose entry lock is free (left by a
   builder that crashed), and the free lock files of entries that are
   gone. Only the kind directories are walked, never `ledger/` or
   `sessions/`, and nothing is removed there but lock files and the
   siblings `cache::parse_sibling` recognizes.

A dry run marks and removes nothing and reports what would go; like every
operation it still reaps.

A build prunes once, after it succeeded, if registering an entry showed it
the total over `cache.limit`. It holds no entry lock then, and its session
still pins everything it used in place. `rene cache prune [--max-age AGE]
[--dry-run]` prunes on demand, whatever the size.

## Configuration

`config.ncl` in the platform configuration directory (`~/.config/rene` on
Linux via XDG, `~/Library/Application Support/rene` on macOS,
`%APPDATA%\rene` on Windows; `RENE_CONFIG_DIR` overrides it) is a Nickel
program evaluated like a manifest. A missing file means the defaults; an
unknown field, a bad unit, or a value out of range is an error, so a typo
cannot silently restore defaults that delete things.

```nickel
{
  cache = {
    limit = "10 GiB",   # a build prunes once the cache is larger than this
    max_age = "120d",   # entries unused this long may be removed
  },
}
```

Either set to `null` turns automatic pruning off; `rene cache prune
--max-age` still works. The last use is the last lookup, so an entry a
build asked for is young again even if the build then found it incomplete
and rebuilt it.

## Why these rules

The first four came out of model checking.

1. **A session row outlives every row that names it.** Deleting it as soon
   as its pins were gone left a `building` row, owned by a dead session
   whose entry lock a live session held at reap time, that nobody could
   ever prove orphaned once that session also died.
2. **Pin at lookup, before the entry exists.** Pinning only a found entry
   left a window between taking the lock and the recheck in which a
   competitor could rebuild and prune the same key, forever.
3. **The rename happens inside the transaction.** Reclaiming and removing
   both decide from the row and the directory. With the rename outside,
   a reclaim could commit between the pruner's mark and its rename, and the
   directory would vanish under its new user.
4. **A failed commit renames back.** Otherwise a row under prune would
   have no directory while its pruner lives, and every session asking for
   the entry would wait for that process to exit.
5. **Adopt, never rebuild, a complete entry.** An entry without a row
   (published by a session without the ledger, or from before it) is
   recognized by the consumer's `ready` check, under the lock, like any
   other.
6. **Prune outside every entry lock**, after the build: a build waiting
   for an entry never waits out someone else's prune.

## Degradation and compatibility

- A ledger that cannot be opened or written, or that a newer rene wrote
  (`schema` above what this rene knows), disables the session with one
  warning. `Cache::acquire` then behaves as without a ledger: `ready`, the
  lock, `ready` again. A disabled session never prunes, and `rene cache
  prune` refuses to run without the ledger.
- What a disabled session uses carries no pins, so another session's prune
  may remove an entry it reads in place, once the entry is old by its row
  or its modification time. The entries it completes have no row: the next
  session with the ledger that asks for one adopts it, and the pruner
  removes those nobody asks for once they are old.
- Rows this rene cannot read are dropped; their entries are then adopted
  or pruned as untracked. Fields added to a row are not schema changes,
  since unknown fields are ignored; a new entry kind, or a changed meaning,
  is.
- Deleting the whole cache directory is safe while no rene runs.

## Model

`cache-prune/CachePrune.tla` models sessions, the ledger, the entry locks,
the state of every entry's directory, and a crash between any two steps.
TLC checks that a live row always has a complete directory, that nothing a
session has found or uses is removed, that only unpinned entries are
marked, that interrupted work is always repaired, and that a session which
never crashes eventually gets the entry it asked for; its README has the
commands and results, and maps the model's actions to the code.
