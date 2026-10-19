# Cache prune protocol model

`CachePrune.tla` is a TLA+ model of the shared-cache prune protocol (see `../cache-ledger.md`): a
single-opener TurboKV ledger holding entry rows, pins, session rows and the
running size total, plus one OS lock per session that answers only "is this
session still alive". Every process may crash between any two steps.

Pruning is by access deadline and triggered by size. Which entries are past
the deadline is policy and is abstracted: the model lets the pruner pick any
subset of the eligible entries (live, pinned by nobody alive).

Run with TLC (nixpkgs `tlaplus`):

```
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePrune.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneLimit0.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneNoPins.cfg CachePrune.tla
nix shell nixpkgs#tlaplus --command tlc -workers auto -config CachePruneRenameOutside.cfg CachePrune.tla
```

Checked properties:

- `Safety`: a live row has a complete directory; nothing a session is using
  is removed; the total is exact; only unpinned entries are pruned;
  everything on disk has a row; a live builder holds its build lock.
- `Recovery`: no row stays `building` or `pruning` forever (a dead owner is
  reaped by the next ledger open).
- `Progress`: a session that never crashes and wants an entry eventually
  uses it.

The two mutation configurations must fail `Safety`, and do:

- `CachePruneNoPins.cfg` drops the reader pin: a session uses an entry
  another session has just renamed away.
- `CachePruneRenameOutside.cfg` renames victims outside the ledger
  transaction: a session reclaims an entry under prune, then the pruner
  renames it away anyway.

Three protocol rules came out of model checking rather than the first draft:

1. A dead session's row (and so its lock file) is removed only once no entry
   row names it. Removing it earlier, while a live session still held the
   entry's build lock, left a `building` row that nobody could ever prove
   orphaned.
2. A session pins a key when it first looks it up, before the entry exists.
   Pinning only once the entry is found let a competitor rebuild and prune
   the same key between a waiter's lock acquisition and its recheck, forever.
3. A session that looks up a key under prune reclaims it if the directory is
   still complete, and the pruner's rename therefore happens inside its
   ledger transaction, after re-reading the row. Renaming outside the ledger
   races the reclaim.
