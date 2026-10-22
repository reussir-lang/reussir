------------------------------ MODULE CachePrune ------------------------------
(***************************************************************************)
(* The rene shared-cache prune protocol: a TurboKV ledger that records     *)
(* entries, pins and prune intents, plus one OS lock per session whose     *)
(* only job is to tell a live session from a dead one.                     *)
(*                                                                         *)
(* Modelling decisions:                                                    *)
(*                                                                         *)
(* - The ledger is single-opener (TurboKV holds an exclusive directory     *)
(*   lock while open), so every ledger operation is one atomic step.       *)
(*   Each such step first reaps dead sessions, as the implementation does. *)
(* - Every process may crash between any two steps (`Crash`).  A crash     *)
(*   releases the process's OS locks (build lock, session lock) and        *)
(*   nothing else: its ledger rows stay behind for the reaper.             *)
(* - The file system state of an entry is "absent", "partial" or           *)
(*   "complete".  A renamed-out victim is "absent": no path reaches it.    *)
(* - Every entry has size 1, so `total` counts live entries, and the       *)
(*   victim set is any subset of the eligible entries.  LRU order and the  *)
(*   watermark are policy, not safety, and are left out.                   *)
(* - Pruning is by access deadline, triggered by size: a session asking    *)
(*   for an entry under prune reclaims it if its directory is still there, *)
(*   instead of waiting to rebuild it.  That is only sound if the pruner's *)
(*   rename happens inside its ledger transaction, so reclaim and rename   *)
(*   are serialised.                                                       *)
(* - `ReadersPin` is a mutation switch: FALSE drops the reader pin, which  *)
(*   must make TLC find a pruned-under-use violation.                      *)
(* - `RenameInLedger` is a mutation switch: FALSE renames victims outside  *)
(*   the ledger transaction, which must make TLC find a reclaimed entry    *)
(*   renamed away under its user.                                          *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS Procs,      \* sessions
          Reliable,   \* the sessions that never crash (for liveness)
          Keys,       \* cache entry keys
          Limit,      \* prune when total > Limit
          NULL,
          ReadersPin,
          RenameInLedger

ASSUME Reliable \subseteq Procs

VARIABLES
  fs,        \* [Keys -> {"absent", "partial", "complete"}]: the entry dir
  row,       \* [Keys -> [state, owner]]: the ledger's entry rows
  pins,      \* SUBSET (Procs \X Keys): the ledger's pin rows
  sessions,  \* SUBSET Procs: the ledger's session rows
  total,     \* the ledger's running size total
  buildLock, \* [Keys -> Procs \cup {NULL}]: the per-key OS build lock
  alive,     \* [Procs -> BOOLEAN]: FALSE iff crashed (session lock free)
  pc,        \* [Procs -> label]
  cur,       \* [Procs -> Keys \cup {NULL}]: the key a session works on
  victims    \* [Procs -> SUBSET Keys]: the entries a session is pruning

vars == <<fs, row, pins, sessions, total, buildLock, alive, pc, cur, victims>>

Labels == {"start", "idle", "lookup", "acquire", "recheck", "unpack",
           "publish", "register", "mark", "remove", "unmark", "using", "dead"}

NoRow == [state |-> "none", owner |-> NULL]
Row(s, o) == [state |-> s, owner |-> o]

-----------------------------------------------------------------------------
(* Reaping: the first thing every ledger open does.  A session is dead     *)
(* when its session lock can be taken, i.e. the process is gone.           *)

Dead == {s \in sessions : ~alive[s]}

\* An interrupted build whose build lock is free: delete the directory.
Sweep(k)  == row[k].state = "building" /\ row[k].owner \in Dead
             /\ buildLock[k] = NULL
\* An interrupted prune that never removed the directory: back to live.
Revive(k) == row[k].state = "pruning" /\ row[k].owner \in Dead
             /\ fs[k] = "complete"
\* An interrupted prune that did remove the directory: finish it.
Drop(k)   == row[k].state = "pruning" /\ row[k].owner \in Dead
             /\ fs[k] # "complete"

RRow == [k \in Keys |-> IF Sweep(k) \/ Drop(k) THEN NoRow
                        ELSE IF Revive(k) THEN Row("live", NULL)
                        ELSE row[k]]
RFs == [k \in Keys |-> IF Sweep(k) THEN "absent" ELSE fs[k]]
RPins == {pk \in pins : pk[1] \notin Dead}
\* A dead session's row (and so its lock file) goes only once no entry row
\* names it any more; otherwise a later reaper could not tell it was dead.
RSessions == sessions \ {s \in Dead : \A k \in Keys : RRow[k].owner # s}
RTotal == total + Cardinality({k \in Keys : Revive(k)})

-----------------------------------------------------------------------------
Init == /\ fs = [k \in Keys |-> "absent"]
        /\ row = [k \in Keys |-> NoRow]
        /\ pins = {}
        /\ sessions = {}
        /\ total = 0
        /\ buildLock = [k \in Keys |-> NULL]
        /\ alive = [p \in Procs |-> TRUE]
        /\ pc = [p \in Procs |-> "start"]
        /\ cur = [p \in Procs |-> NULL]
        /\ victims = [p \in Procs |-> {}]

\* Ledger op: create the session row (the session lock is already held).
Start(p) == /\ alive[p] /\ pc[p] = "start"
            /\ sessions' = RSessions \cup {p}
            /\ row' = RRow /\ fs' = RFs /\ pins' = RPins /\ total' = RTotal
            /\ pc' = [pc EXCEPT ![p] = "idle"]
            /\ UNCHANGED <<buildLock, alive, cur, victims>>

Pick(p) == /\ alive[p] /\ pc[p] = "idle"
           /\ \E k \in Keys : cur' = [cur EXCEPT ![p] = k]
           /\ pc' = [pc EXCEPT ![p] = "lookup"]
           /\ UNCHANGED <<fs, row, pins, sessions, total, buildLock, alive,
                          victims>>

\* Ledger op: pin the key first (a pin means "I want this entry", whether
\* or not it exists yet, so a pruner never takes it from under a waiter),
\* then use a live entry, wait out a prune, or go build.
Lookup(p) == /\ alive[p] /\ pc[p] = "lookup"
             /\ LET k == cur[p]
                    r == RRow[k]
                IN /\ sessions' = RSessions /\ fs' = RFs
                   /\ pins' = IF ReadersPin THEN RPins \cup {<<p, k>>} ELSE RPins
                   /\ CASE r.state = "live" ->
                           /\ row' = RRow /\ total' = RTotal
                           /\ pc' = [pc EXCEPT ![p] = "using"]
                        [] r.state = "pruning" /\ RFs[k] = "complete" ->
                           \* Reclaim: the entry is wanted after all.  The
                           \* pruner finds the row live and skips it.
                           /\ row' = [RRow EXCEPT ![k] = Row("live", NULL)]
                           /\ total' = RTotal + 1
                           /\ pc' = [pc EXCEPT ![p] = "using"]
                        [] r.state = "pruning" ->
                           /\ row' = RRow /\ total' = RTotal
                           /\ pc' = pc
                        [] OTHER ->
                           /\ row' = RRow /\ total' = RTotal
                           /\ pc' = [pc EXCEPT ![p] = "acquire"]
             /\ UNCHANGED <<buildLock, alive, cur, victims>>

\* OS lock: the per-key build lock.
Acquire(p) == /\ alive[p] /\ pc[p] = "acquire"
              /\ buildLock[cur[p]] = NULL
              /\ buildLock' = [buildLock EXCEPT ![cur[p]] = p]
              /\ pc' = [pc EXCEPT ![p] = "recheck"]
              /\ UNCHANGED <<fs, row, pins, sessions, total, alive, cur,
                             victims>>

\* Ledger op under the build lock: whoever held it may have published.
\* Otherwise claim the row and clear whatever is on disk.
Recheck(p) ==
  /\ alive[p] /\ pc[p] = "recheck"
  /\ LET k == cur[p]
         r == RRow[k]
     IN /\ sessions' = RSessions /\ total' = RTotal
        /\ CASE r.state = "live" ->
                /\ pins' = RPins
                /\ row' = RRow /\ fs' = RFs
                /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
                /\ pc' = [pc EXCEPT ![p] = "using"]
             [] r.state = "pruning" ->
                /\ pins' = RPins /\ row' = RRow /\ fs' = RFs
                /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
                /\ pc' = [pc EXCEPT ![p] = "lookup"]
             [] OTHER ->
                /\ pins' = RPins
                /\ row' = [RRow EXCEPT ![k] = Row("building", p)]
                /\ fs' = [RFs EXCEPT ![k] = "absent"]
                /\ buildLock' = buildLock
                /\ pc' = [pc EXCEPT ![p] = "unpack"]
  /\ UNCHANGED <<alive, cur, victims>>

Unpack(p) == /\ alive[p] /\ pc[p] = "unpack"
             /\ fs' = [fs EXCEPT ![cur[p]] = "partial"]
             /\ pc' = [pc EXCEPT ![p] = "publish"]
             /\ UNCHANGED <<row, pins, sessions, total, buildLock, alive, cur,
                            victims>>

\* The atomic rename / marker write that completes the entry on disk.
Publish(p) == /\ alive[p] /\ pc[p] = "publish"
              /\ fs' = [fs EXCEPT ![cur[p]] = "complete"]
              /\ pc' = [pc EXCEPT ![p] = "register"]
              /\ UNCHANGED <<row, pins, sessions, total, buildLock, alive, cur,
                             victims>>

\* Ledger op: the entry becomes live, sized and pinned; read the total.
\* Then release the build lock.  Over the limit, this session prunes.
Register(p) ==
  /\ alive[p] /\ pc[p] = "register"
  /\ LET k == cur[p]
     IN /\ row' = [RRow EXCEPT ![k] = Row("live", NULL)]
        /\ pins' = RPins \cup {<<p, k>>}
        /\ sessions' = RSessions /\ fs' = RFs
        /\ total' = RTotal + 1
        /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
        /\ pc' = [pc EXCEPT ![p] = IF total' > Limit THEN "mark" ELSE "using"]
  /\ UNCHANGED <<alive, cur, victims>>

\* Ledger op: choose victims among live entries nobody alive pins, and
\* announce the prune by flipping them to "pruning".
Mark(p) ==
  /\ alive[p] /\ pc[p] = "mark"
  /\ LET eligible == {k \in Keys : RRow[k].state = "live"
                                   /\ \A q \in Procs : <<q, k>> \notin RPins}
     IN \E V \in SUBSET eligible :
          /\ row' = [k \in Keys |-> IF k \in V THEN Row("pruning", p)
                                    ELSE RRow[k]]
          /\ total' = RTotal - Cardinality(V)
          /\ victims' = [victims EXCEPT ![p] = V]
  /\ pins' = RPins /\ sessions' = RSessions /\ fs' = RFs
  /\ pc' = [pc EXCEPT ![p] = "remove"]
  /\ UNCHANGED <<buildLock, alive, cur>>

\* Ledger op, one victim at a time: re-read the row under the ledger lock;
\* if it is still ours, rename the directory out (one atomic syscall, inside
\* the critical section) and delete the row in the same batch.  A row a
\* waiter reclaimed is live again and is left alone.  (Deleting the
\* renamed-out tree happens afterwards and is invisible to the protocol.)
RemoveInLedger(p) ==
  /\ RenameInLedger
  /\ \E k \in victims[p] :
       /\ IF RRow[k].state = "pruning" /\ RRow[k].owner = p
          THEN /\ fs' = [RFs EXCEPT ![k] = "absent"]
               /\ row' = [RRow EXCEPT ![k] = NoRow]
          ELSE /\ fs' = RFs
               /\ row' = RRow
       /\ victims' = [victims EXCEPT ![p] = victims[p] \ {k}]
  /\ pins' = RPins /\ sessions' = RSessions /\ total' = RTotal
  /\ pc' = pc

\* Mutation: rename outside any ledger open, then delete rows afterwards.
RemoveOutside(p) ==
  /\ ~RenameInLedger
  /\ \E k \in victims[p] : /\ fs[k] # "absent"
                           /\ fs' = [fs EXCEPT ![k] = "absent"]
  /\ UNCHANGED <<row, pins, sessions, total, pc, victims>>

Remove(p) == /\ alive[p] /\ pc[p] = "remove"
             /\ RemoveInLedger(p) \/ RemoveOutside(p)
             /\ UNCHANGED <<buildLock, alive, cur>>

Removed(p) == /\ alive[p] /\ pc[p] = "remove"
              /\ IF RenameInLedger THEN victims[p] = {}
                 ELSE \A k \in victims[p] : fs[k] = "absent"
              /\ pc' = [pc EXCEPT ![p] = IF RenameInLedger THEN "using"
                                         ELSE "unmark"]
              /\ UNCHANGED <<fs, row, pins, sessions, total, buildLock, alive,
                             cur, victims>>

\* Mutation only: the ledger op that deletes the victims' rows afterwards.
Unmark(p) == /\ alive[p] /\ pc[p] = "unmark"
             /\ row' = [k \in Keys |-> IF k \in victims[p]
                                          /\ RRow[k].state = "pruning"
                                       THEN NoRow ELSE RRow[k]]
             /\ pins' = RPins /\ sessions' = RSessions /\ fs' = RFs
             /\ total' = RTotal
             /\ victims' = [victims EXCEPT ![p] = {}]
             /\ pc' = [pc EXCEPT ![p] = "using"]
             /\ UNCHANGED <<buildLock, alive, cur>>

\* Ledger op: the build is over, drop the pin.
Finish(p) == /\ alive[p] /\ pc[p] = "using"
             /\ pins' = RPins \ {<<p, cur[p]>>}
             /\ row' = RRow /\ sessions' = RSessions /\ fs' = RFs
             /\ total' = RTotal
             /\ pc' = [pc EXCEPT ![p] = "idle"]
             /\ cur' = [cur EXCEPT ![p] = NULL]
             /\ UNCHANGED <<buildLock, alive, victims>>

\* The process dies: its OS locks are released, its ledger rows are not.
Crash(p) == /\ alive[p] /\ p \notin Reliable
            /\ alive' = [alive EXCEPT ![p] = FALSE]
            /\ pc' = [pc EXCEPT ![p] = "dead"]
            /\ buildLock' = [k \in Keys |-> IF buildLock[k] = p THEN NULL
                                            ELSE buildLock[k]]
            /\ UNCHANGED <<fs, row, pins, sessions, total, cur, victims>>

Step(p) == \/ Start(p) \/ Pick(p) \/ Lookup(p) \/ Acquire(p) \/ Recheck(p)
           \/ Unpack(p) \/ Publish(p) \/ Register(p) \/ Mark(p) \/ Remove(p)
           \/ Removed(p) \/ Unmark(p) \/ Finish(p)

Next == \E p \in Procs : Step(p) \/ Crash(p)

Fairness == \A p \in Procs : SF_vars(Step(p))

Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Safety *)

TypeOK ==
  /\ fs \in [Keys -> {"absent", "partial", "complete"}]
  /\ row \in [Keys -> [state : {"none", "building", "live", "pruning"},
                       owner : Procs \cup {NULL}]]
  /\ pins \subseteq Procs \X Keys
  /\ sessions \subseteq Procs
  /\ total \in Nat
  /\ buildLock \in [Keys -> Procs \cup {NULL}]
  /\ alive \in [Procs -> BOOLEAN]
  /\ pc \in [Procs -> Labels]
  /\ cur \in [Procs -> Keys \cup {NULL}]
  /\ victims \in [Procs -> SUBSET Keys]

\* A live row always has a complete directory behind it.
LiveIsComplete == \A k \in Keys : row[k].state = "live" => fs[k] = "complete"

\* A pin taken before the entry exists is intent; once its holder has
\* found the entry (it is past lookup), the entry stays on disk.
PinSafety == \A pk \in pins :
  alive[pk[1]] /\ pc[pk[1]] \notin {"lookup", "acquire", "recheck", "unpack",
                                     "publish", "register"}
    => fs[pk[2]] = "complete"

\* The same, seen from the process: while it uses (or prunes next to) its
\* entry, that entry is on disk.
UseSafety == \A p \in Procs :
  pc[p] \in {"using", "mark", "remove", "unmark"} => fs[cur[p]] = "complete"

\* The ledger's running total is exact.
TotalExact == total = Cardinality({k \in Keys : row[k].state = "live"})

\* Only entries nobody alive uses or waits for are ever marked for pruning.
\* (A waiter in lookup may pin a pruning row: it is waiting for the row
\* to go away so it can rebuild, and its pin keeps the rebuilt entry.)
PrunedUnpinned == \A k \in Keys : row[k].state = "pruning" =>
  \A pk \in pins : pk[2] = k => ~alive[pk[1]] \/ pc[pk[1]] = "lookup"

\* Everything on disk is tracked by a row, so nothing can leak.
Tracked == \A k \in Keys : fs[k] # "absent" => row[k].state # "none"

\* A live builder holds the build lock of the row it owns.
BuildExcl == \A k \in Keys :
  row[k].state = "building" /\ alive[row[k].owner] => buildLock[k] = row[k].owner

Safety == /\ TypeOK /\ LiveIsComplete /\ PinSafety /\ UseSafety /\ TotalExact
          /\ PrunedUnpinned /\ Tracked /\ BuildExcl

-----------------------------------------------------------------------------
(* Liveness, given at least one session that never crashes. *)

\* Interrupted work is always repaired: no row stays in a transient state.
Recovery == \A k \in Keys :
  [](row[k].state \in {"building", "pruning"} =>
       <>(row[k].state \in {"live", "none"}))

\* A reliable session that wants an entry eventually uses it.
Progress == \A p \in Reliable : [](pc[p] = "lookup" => <>(pc[p] = "using"))

=============================================================================
