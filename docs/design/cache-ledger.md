# The Per-User Cache and Its Ledger

Status: the design of `crates/rene/src/lock.rs`, `crates/rene/src/ledger/`
and `crates/rene/src/cache/`, and of the consumers of the cache. The
protocol is model-checked in `cache-prune/`.

Some of what a `rene` build needs does not depend on the package being
built: the sources of the libraries bundled inside rene (`core`, `std`),
the `reussir-rt` runtime baked with the user's toolchain
(`system-rust-runtime.md` §4), and those libraries' compiled artifacts.
They live once per user, in a cache every build on the machine shares.

Sharing is the easy half. The hard half is removing entries: a build reads
library sources and links the runtime bake in place for minutes, several
renes may build the same entry at once, any of them may crash or lose power
at any point, and no directory listing says which build uses which entry or
when an entry was last wanted. This document designs the cache around a
ledger that records exactly those facts, and the protocol every consumer
uses an entry through. The protocol is specified in TLA+ and model-checked
with TLC (`cache-prune/`).

## Goals

- Every build shares one copy of each entry; concurrent builds that need
  the same missing entry build it once, and the others wait cheaply.
- Removing an entry never breaks a running build, on Linux, macOS, or
  Windows.
- A crash or a power loss at any point leaves nothing that needs manual
  repair: whatever it leaves is recognized and cleaned up by later renes.
- Pruning is by access age, triggered by size.
- When the cache cannot be used safely, the command fails and says why. It
  never runs in a weaker mode.
- Few mechanisms and few states.

## Layout

```
<root>/
  CACHEDIR.TAG                    so backup tools skip the cache
  ledger/                         the TurboKV database
  sessions/<sid>.lock             one per running rene
  <kind>/<key>                    an entry's directory
  <kind>/<key>.lock               the entry's lock
  <kind>/<key>.stage-<sid>-<n>    an entry being built by session <sid>
  <kind>/<key>.trash-<sid>-<n>    a directory session <sid> renamed out
```

The root is the platform cache directory's `rene` (`$XDG_CACHE_HOME/rene`
or `~/.cache/rene` on Linux, `~/Library/Caches/rene` on macOS,
`%LOCALAPPDATA%\rene` on Windows) unless `RENE_CACHE_DIR` names another;
it is made absolute once, since entry paths end up in build records. The
kinds are `core-src` and `std-src` (bundled library sources), `rt`
(runtime bakes), and `deps` (compiled bundled packages). A key is 32 hex
digits of a blake3 hash over length-prefixed components, which are
everything the entry's contents depend on: equal keys mean interchangeable
contents, and an entry is never changed once it exists.

Nothing else is ever created or deleted under the root.

## Layers

Each layer uses only the ones above it: **file locks** (`lock`); **the
ledger database** (`ledger`: transactions, sessions, reaping); **entries in
the ledger** (`ledger::entry`: rows, pins, lookup, publication, repair);
**the entry protocol** (`cache`: the root, entry locks, `Cache::acquire`);
the **consumers**; **configuration** (`config`); and **pruning**
(`ledger::prune`, the sweep in `cache`, `rene cache prune`).

## File locks and their contract

A lock is the operating system's advisory lock on an open file (`flock` on
Unix, `LockFileEx` on Windows). The OS releases it when its holder closes
the file or exits, crashed or not, so a stale lock never wedges the cache.
`FileLock` is the only lock helper: `try_lock` and `lock` (which waits,
polling with a backoff from 5 ms up to 100 ms, and tells the caller once
if it is still waiting after a second), `probe` (held, free, or absent;
never creates the file), and `remove`.

Lock files may be deleted, which is sound under two rules:

1. a lock file is deleted only by a process that holds its lock, which
   deletes it before releasing it. Windows cannot delete a file that is
   open without `FILE_SHARE_DELETE`, as lock files are, so there the
   holder releases first and the deletion fails harmlessly (a sharing
   violation) if anyone has opened the file since;
2. after locking, the locker checks that the path still names the file it
   locked (device and inode on Unix; on Windows the path always does,
   since an open lock file cannot be deleted), and starts over if not.

So no two processes ever hold "the same" lock on different files, and the
lock file of a process that has locked it stays at its path until that
process deletes it: once a session has taken its lock, a missing lock file
proves the session ended.

**Locks are trusted only on local file systems.** On network file systems
`flock` may block forever even when asked not to (cargo skips locking on
NFS for that reason, rust-lang/cargo#2615), or hold only on one host, so
two hosts sharing a home directory would both hold the same lock. No probe
can detect either, so `lock::remote_file_system` names the file system
from the OS before anything is locked: on Linux the `statfs` type (NFS,
SMB/CIFS, AFS, Ceph, Lustre, GPFS), on macOS any file system `statfs` does
not mark `MNT_LOCAL`, on Windows a volume whose drive type is
`DRIVE_REMOTE`. The cache refuses such a root (see Failure).

## Facts and liveness

**Facts live in the ledger**, a TurboKV database at `<root>/ledger`:

| key | value |
|---|---|
| `schema` | the ledger's schema version, `1` |
| `entry/<kind>/<key>` | `{"size", "last_used", "record"}`: a live entry |
| `pin/<sid>/<kind>/<key>` | empty: session `<sid>` wants this entry |
| `pruned` | Unix seconds of the last prune that committed |

There are no other rows. An entry row exists only for a complete entry,
so a row has no state field: it is live by existing. The total size is
summed from the rows whenever it is needed and cannot drift from them.

**Liveness lives in OS locks**, never in pids or timestamps. Every rene
that uses the cache is a session holding `sessions/<sid>.lock` for its
whole life. A session whose lock is free, or whose lock file is gone, has
ended. Every entry has a lock, `<kind>/<key>.lock`, held by whoever is
deciding about the entry outside the ledger: looking it up, or building it.

## Transactions

TurboKV holds an exclusive lock on its directory while a database is open,
so the ledger is opened for one transaction at a time and nobody keeps it
open across a build. A transaction opens the database (polling every
10 ms while another session has it, and saying so once if that lasts a
second), checks the schema, reads the pin rows and reaps (below), reads
`pruned`, operates, commits one atomic batch (TurboKV's paranoid mode
syncs it before acknowledging), closes, and then deletes its trash. It
reads only the rows it needs: a lookup or a publication its entry's row,
a publication the sizes of all entries, and only a prune everything.

Four rules:

- **Bounded I/O only.** A transaction stats directories, renames them,
  probes locks, and (pruning only) lists the kind directories. It never
  deletes a tree, never waits for a lock, and never runs a build.
- **A directory enters or leaves an entry path only by a rename inside the
  transaction that writes or deletes its row.** So no transaction ever sees
  a row and a directory that another transaction is halfway through
  changing.
- **A directory leaving an entry path goes to the transaction's trash**,
  `<key>.trash-<sid>-<n>`, deleted once the transaction has committed and
  closed. Failing to delete it is harmless: once its session ends, the
  sweep deletes it.
- **A transaction that does not commit renames back.** If the operation
  fails, or the commit reports an error, its renames are undone, newest
  first, and the command fails. A commit error leaves the outcome unknown:
  TurboKV may have logged the batch before the error and then replays it
  at the next open. The renames are undone either way, and either outcome
  leaves only the disagreements below.

On Windows a rename fails while any file under the directory is open, as
when an antivirus scans files just written. A transaction whose rename
fails that way (access denied, a sharing or a lock violation) is undone,
the ledger is closed, and the transaction is run again after a backoff,
for up to ten seconds before the command fails. A prune instead keeps a
victim it cannot rename.

A process that dies between a rename and the commit leaves the ledger
closed (the OS releases TurboKV's lock) and the rename done. That leaves
one of two disagreements between the rows and the disk, and both are
recognized and repaired wherever they are found:

- a live row whose directory is gone (it sits in a dead session's trash):
  the row is deleted, as if the entry had been pruned;
- a directory at an entry path without a row (a publication that never
  committed): it is renamed to the trash, as debris.

The same two repairs cover a commit that failed after being applied, a
power loss that kept a commit but not its renames or the reverse, and an
entry directory deleted by hand. So neither a commit nor a rename has to
be durable for safety; only an entry's files must be (see Entries).

## Sessions and reaping

A session is one rene process using the cache. Its id is 16 hex digits
hashed from its pid, its start time, and a counter: unique across reboots,
and never containing a separator. A session starts, with no ledger
operation, by checking that the root's file system is local, then creating
and locking `sessions/<sid>.lock` (if a reaper deletes the fresh file
first, the identity check starts over); that lock is also the probe that
the file system can lock at all. It ends, again without the ledger, by
removing the lock file (`FileLock::remove`), so ending and crashing differ
only in whether the file stays.

**Every transaction reaps first.** For each session other than the
caller's that has pins, it probes that session's lock: held means alive;
free means dead, and the lock file is removed while held; absent means
dead. A dead session's pins are deleted. A probe that fails counts as held,
since nothing acts on a session it has not proven dead. Reaping is one code
path for every way a session ends, and every build exercises it.

Everything else a dead session leaves is named for it (staging and
trash, deleted by the sweep) or is one of the disagreements above.

## Entries

An **entry** is a live row together with its directory at `<kind>/<key>`.
The row holds:

- `size`: the bytes of the directory, measured when it was published;
- `last_used`: Unix seconds of the last lookup;
- `record`: the consumer's JSON describing the directory: artifact paths
  relative to the entry, a source graph whose paths are relative to the
  root. A record must be a function of the key, like the directory, so any
  complete directory of the key matches it, wherever the root is.

Entries are **staged and published**, all of them. A builder fills a fresh
directory `<key>.stage-<sid>-<n>` that only it touches, then makes it
durable while summing its size. Publication is one transaction: rename the
staging directory to `<kind>/<key>`, write the row, commit. The row is
written only after every file it describes is on disk, so no row ever
describes a file cut short. The rename need not be durable before the
commit: a power loss that keeps the row but not the rename leaves a live
row without its directory, which the next lookup forgets.

Making a tree durable depends on the platform. On Linux and the other
Unixes every file and directory in it is fsynced, bottom-up. On macOS each
gets a plain `fsync(2)` and the tree one `F_FULLFSYNC` at the end, since
std's `sync_all` would flush the whole drive cache once per file. On
Windows every file is opened for writing and flushed (`FlushFileBuffers`);
directories are not, since std cannot open them, and NTFS journals their
entries. A 108 MB, 591-file runtime bake takes 0.7 to 2.4 s to make
durable on ext4 and NVMe, once per bake.

## The entry protocol

Consumers use an entry only through `Cache::acquire(request, use)`, which
runs the protocol; they never see the ledger.

1. **Lock.** Take the entry's lock, waiting for whoever holds it: a rene
   building the entry, or one looking it up. If the wait lasts a second,
   say once what is being waited for.
2. **Look up** (a transaction). Pin the key, whether or not the entry
   exists, and stamp its last use. Then:
   - a live row whose directory is there: release the lock, and hand the
     directory and its record to `use`;
   - a live row whose directory is missing: delete the row, and build;
   - no row: build.
3. **Build**, still holding the lock: the consumer fills a staging
   directory, which `Slot::publish` makes durable and measures. A build
   that fails or is abandoned deletes its staging directory.
4. **Publish** (a transaction). If a live entry is there already, keep it
   and delete the staging directory; this cannot happen while the lock is
   held, but publication does not rely on the lock. Otherwise rename any
   debris at the entry path to the trash, rename the staging directory into
   place, write the row, and commit. Release the lock.
5. **Unpin.** An entry copied out of the cache (a compiled package) is
   unpinned, in a transaction, as soon as it has been copied or published.
   An entry read in place (sources, the runtime bake) stays pinned until
   the session ends and its pins are reaped.

Two ordering rules keep the protocol free of deadlocks: a session never
waits for an entry lock while it holds another one, and nobody waits for
anything while holding the ledger.

## Consumers

- `core-src/<key>`, `std-src/<key>`: a bundled library's manifest and
  sources, keyed by the bundle's content and built by unpacking it. No
  record; read in place.
- `rt/<key>`: a `reussir-rt` bake, keyed by the bundle (with the rcgmp
  archive), the target, the toolchain the package directory selects (its
  `rustc`, sysroot and version), the cargo binary, the allocator backend,
  and the rustflags environment. Built by `cargo build` with its target and
  build directories inside the staging directory, then the rcgmp archive
  staged beside the dependencies. The record holds the artifacts, relative
  to the entry, and the sysroot. Read in place.
- `deps/<key>`: a compiled bundled package, keyed by the package's content
  hash, the planned `rrc` argv with the build directory's `deps/`
  abstracted (it keeps `--package-root`), the digests of the upstream
  artifacts, the bake, the `rrc` binary's identity, and rene's version.
  Built by the build's own compile, whose declared outputs are staged. The
  record holds the file names and the package's source graph. Copied out.

Three properties of the consumers matter to the protocol:

- The bundled libraries are a table (`bundled`): `core`, injected into
  every package unless it sets `package.no_core`, and `std`, which a
  package asks for with `dependencies.std = { bundled = true }`. The table
  is the only list of reserved package names (`rrc --builtin`). A graph is
  loaded by a sans-IO `resolve::Loader` that names each bundle the first
  time a package needs it, so a build unpacks, pins and stamps only what it
  uses. The bundles are packed with a fixed mtime, so their unpacked files,
  and the source graphs recorded against them, depend only on the key.
- The runtime bake is relocatable: cargo records absolute paths only in
  what it reads to rebuild, and an entry is never rebuilt. Consumers find
  the rlibs, the staticlib and the rcgmp archive by the record's relative
  paths.
- A compiled bundled package is restored by copying its files into the
  build directory's `deps/`; from there on it is a local compile.

## Pruning, repair and sweeping

`Cache::prune(max_age, dry_run)` is **one transaction** over the whole
ledger. After reaping, for each live row:

- its directory is missing: delete the row;
- used within `max_age`: keep it ("recent");
- pinned by any session: keep it ("in use");
- otherwise: rename the directory to the trash and delete the row.

It also lists the kind directories and renames every rowless directory at
an entry path to the trash, and stamps `pruned`. It commits, renaming
everything back if it does not commit, closes, and deletes its trash. A
dry run does the same without renaming or deleting anything, and reports
what would go. A **repair** is a prune with no `max_age`: it removes no
entry, only what disagrees.

Then, outside the ledger, the **sweep** deletes in each kind directory the
staging and trash directories whose owning session is dead (its lock is
free or absent), and the free lock files of keys whose entry is not there
(taken first, as the contract requires); in `sessions/` it deletes the
free lock files. Nothing but a dead session touches a dead session's
directories, so the sweep needs no transaction, and concurrent sweeps are
harmless.

**When.** Once a build has succeeded, outside every entry lock, while its
session still pins everything it reads in place: if its publications took
the total over `cache.limit`, it prunes at `cache.max_age`; otherwise, if
the `pruned` row (which every transaction reads) is missing or more than
an hour old, it repairs. Either is followed by the sweep. `rene cache prune
[--max-age AGE] [--dry-run]` prunes on demand, whatever the size, then
sweeps, and lists what it removed on stdout. The cache may stay over the
limit when nothing qualifies.

**One at a time, by the ledger.** There is no prune lock. Renames are
cheap, so a prune decides and acts inside one transaction, which the
ledger serializes with every other; a second pruner finds nothing left to
remove, and a second repair that finds `pruned` fresh does nothing.

## Configuration

`config.ncl` in the platform configuration directory (`$XDG_CONFIG_HOME/rene`
or `~/.config/rene` on Linux, `~/Library/Application Support/rene` on
macOS, `%APPDATA%\rene` on Windows; `RENE_CONFIG_DIR` overrides it) is a
Nickel program evaluated like a manifest:

```nickel
{
  cache = {
    limit = "10 GiB",   # a build prunes once the cache is larger than this
    max_age = "120d",   # entries unused this long may be removed
  },
}
```

A missing file means these defaults. Sizes take bytes or a unit (`B`, `KB`
to `TB`, `KiB` to `TiB`); ages take seconds or a unit (`s`, `min`, `h`,
`d`, `w`, `months`, `y`). An unknown field, an unknown unit, a negative or
out-of-range value is an error, so a typo cannot silently restore defaults
that delete things. `null` for either setting stops builds from removing
entries; the hourly repair still runs, since it removes only what crashes
left, and `rene cache prune --max-age` still works. The CLI tests and lit
point both `RENE_CACHE_DIR` and `RENE_CONFIG_DIR` into their own trees.

## Failure

Whenever the cache cannot be used safely, the command fails with an error
that names the path and suggests setting `RENE_CACHE_DIR` to a directory on
a local file system rene can write:

- the root cannot be created or written;
- the root is on a network file system;
- the session lock cannot be taken: the file system cannot lock files;
- the ledger cannot be opened, read, or written, or holds a row this rene
  cannot read;
- the ledger's schema is newer than this rene's.

There is no degraded mode, no private cache inside the build directory, no
unlocked mode, and no consumer working without the cache: an error from
the cache is the command's error. Deleting the whole root is safe while no
rene runs; deleting one entry's directory is safe while no build uses it.

## Why these rules

**The record lives in the row.** The row already decides whether a
directory is an entry; with the record beside it, one transaction tells a
reader that the entry exists and what it holds, and pins it. A record file
in the entry would be a second claim of completeness, to write last and to
find cut short after a power loss. Records are small (a compiled `std`'s
source graph, the largest, is a few KB) and functions of the key, so a row
and any complete directory of its key agree. Scanning 3,000 rows of 2 KB
adds about 3 ms to a 35 ms transaction, so records need no keys of their
own; only publication and pruning scan.

**Durable files, not recorded sizes.** A row goes live only after every
byte it describes is fsynced. Recorded sizes catch only truncation, not
reordered or zero-filled blocks, and would need a "damaged entry" path
through the protocol. Nothing else needs to be durable: a lost rename or a
lost commit is one of the two disagreements, which are repaired. The
ledger stays in paranoid mode anyway, since every transaction's close
flushes and syncs the database: a transaction measured about 30 ms in
paranoid and durable mode alike. Damage from outside the protocol is left
to the user: a file missing inside an entry surfaces as the consumer's
error, whose path names the entry directory to delete.

**One publication mode: stage, then rename in the transaction.** The bake
was the only candidate for building in place, and it is relocatable (a
bake copied to one path, then moved to another, still links a staticlib, an
executable, and a cdylib with the rustc that baked it). With every entry
staged, the entry path holds a directory only while its row is live, and a
builder's unfinished work is named for its owner: no in-place marker, no
clearing, no reaper renaming a dead builder's half-built entry.

**Publication in the transaction that renames.** The row goes live in the
same transaction that renames the directory into place, so no transaction
ever sees a directory at an entry path whose row is still coming: a pruner
or a publisher may then treat a rowless directory there as debris. Model
checking confirms it: publishing outside the transaction lets a prune
remove the directory just before its row goes live.

**Rows exist only for complete entries.** A build is owned by the entry
lock and a staging name, a prune happens inside one transaction, so a row
has no transient states, reapers repair nothing but pins, and a lookup
waits on nothing but the entry lock.

**Entry locks, not a building row.** The ledger has one opener, and every
transaction costs an open and a close (15 to 35 ms, measured). Twenty lit
workers polling a building row for minutes would starve the bake's own
publication; waiting on the entry lock touches nothing shared, and the OS
releases it if the builder dies. Safety does not rest on it: without entry
locks the model loses builder exclusivity and nothing else, since
publication keeps the first entry.

**Lookup under the entry lock.** Taking the lock first makes the lookup
the recheck: whoever held the lock has published or given up. Behind
another session's lookup, a reader waits about as long as that lookup,
which the ledger would impose anyway (the poll starts at 5 ms and
doubles), and the notice waits a second so such waits stay silent. No
session waits for an entry lock while holding another: a build acquires
its sources and its bake before it claims compiled packages, one at a time.

**Pin at lookup, before the entry exists.** A pin means "this session wants
this key". Taken in the transaction that finds the entry, it covers every
moment the session uses it; taken before the entry exists, it also keeps a
pruner from removing the entry between its publication and its use.
Readers that do not pin lose entries to the pruner in the model.

**The pruner renames inside its transaction.** It decides from the row and
the pins. If it deleted the rows in its transaction and renamed the
directories after it, a session asking for the key in between would build
and publish it anew, and the late rename would take the new entry from
under it; the model finds exactly that.

**A transaction that does not commit renames back.** Usually the batch was
not applied (a write failed), and the rename back restores the rows' view
of the disk. Without it, a failed prune leaves live rows without
directories while its session lives, and a failed publication leaves its
directory at the entry path without a row. When the batch was applied
after all, the rename back causes the same disagreements, and they are
repaired; the model checks both outcomes.

**Names carry the session id.** Staging and trash directories belong to
the session in their name, so the sweep decides by liveness alone, without
the ledger and without guessing from ages. In the model, a sweep that also
deletes a live session's names breaks the rename back of a failing commit,
and starves a session that never crashes of its entry.

**Disagreements are repaired, never adopted.** A directory without a row is
debris and a row without a directory is forgotten: the ledger is the only
truth about entries. So a lookup checks a live row's directory; without
that, a session uses a directory an interrupted prune, or a failed but
applied publication, left missing.

**Repairs run hourly, not only past the limit.** Staging and trash
directories and debris do not count toward the total, so a prune triggered
by size alone may never come, and a Ctrl-C during a first bake would leave
up to a hundred megabytes for good. A repair costs one transaction an hour,
and the model's recovery needs it: without it, a dead session's staging
directory outlives every later build.

**Prune after the build, outside every entry lock.** A build waiting for an
entry never waits out someone's prune, and the pruning build still pins
what it read in place.

**No session rows.** A session's lock is its liveness and its pins name
it, so it needs no row and no transaction to start or end.

**Fail rather than degrade.** A session without the ledger could not pin,
a private cache would duplicate every entry, an unlocked cache every build,
and each is a second mode to design, test, and model. A network file
system is refused because its locks may hang or hold on one host only.

## Model

`cache-prune/CachePrune.tla` models all of the above, with crashes
anywhere and failed commits of unknown outcome. TLC checks that nothing in
use is removed or replaced, that rows and directories disagree only by
what unclean transactions leave, that a session that never crashes gets
every entry, and that whatever a dead session leaves is cleaned up. Eight
mutations, one per rule above, each break one of these;
`cache-prune/README.md` has the commands, results, and the map to the code.
