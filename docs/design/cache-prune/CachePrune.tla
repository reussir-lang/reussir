------------------------------ MODULE CachePrune ------------------------------
(***************************************************************************)
(* The rene shared-cache protocol (../cache-ledger.md): a TurboKV ledger   *)
(* of entry rows, pins and session rows; one OS lock per session, whose    *)
(* only job is to tell a live session from a dead one; one OS lock per     *)
(* entry, serializing its builders; and the pruner that removes entries.   *)
(*                                                                         *)
(* Modelling decisions:                                                    *)
(*                                                                         *)
(* - The ledger is single-opener (TurboKV holds an exclusive directory     *)
(*   lock while open), so every ledger operation is one atomic step.       *)
(*   Each such step first reaps dead sessions, as the implementation does. *)
(* - Every process may crash between any two steps (`Crash`).  A crash     *)
(*   releases the process's OS locks and nothing else: its ledger rows     *)
(*   stay behind for the reaper.  A session that ends normally releases    *)
(*   its session lock without a ledger operation, so `Crash` covers normal *)
(*   exits as well.                                                        *)
(* - The directory of an entry is "absent", "partial" or "complete";       *)
(*   "complete" stands for the consumer's own `ready` check.  A directory  *)
(*   renamed to the trash is "absent": no path reaches it.                 *)
(* - Every entry has size 1, so the total, derived from the live rows as   *)
(*   in the implementation, counts them, and the pruner may pick any       *)
(*   subset of the eligible entries.  Ages and LRU order are policy, not   *)
(*   safety, and are left out.                                             *)
(* - Every session can use the ledger, every commit succeeds, and a        *)
(*   session drops its pin once it is done with an entry.                  *)
(* - Mutation switches, each of which must make TLC find a violation:      *)
(*   `ReadersPin` = FALSE drops the reader's pin; `RenameInLedger` = FALSE *)
(*   renames victims outside the ledger transaction.                       *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS Procs,      \* sessions
          Reliable,   \* the sessions that never crash (for liveness)
          Keys,       \* cache entry keys
          Limit,      \* prune when the total exceeds it
          NULL,
          ReadersPin,
          RenameInLedger

ASSUME Reliable \subseteq Procs

VARIABLES
  fs,        \* [Keys -> {"absent", "partial", "complete"}]: the entry dir
  row,       \* [Keys -> [state, owner]]: the ledger's entry rows
  pins,      \* SUBSET (Procs \X Keys): the ledger's pin rows
  sessions,  \* SUBSET Procs: the ledger's session rows
  buildLock, \* [Keys -> Procs \cup {NULL}]: the per-entry OS lock
  alive,     \* [Procs -> BOOLEAN]: FALSE once ended (session lock free)
  pc,        \* [Procs -> label]
  cur,       \* [Procs -> Keys \cup {NULL}]: the key a session works on
  victims    \* [Procs -> SUBSET Keys]: rows a pruner marked

vars == <<fs, row, pins, sessions, buildLock, alive, pc, cur, victims>>

Labels == {"start", "idle", "lookup", "acquire", "recheck", "unpack",
           "publish", "register", "using", "mark", "remove", "unmark",
           "dead"}

\* Where a session is still getting its current entry.
Getting == {"lookup", "acquire", "recheck", "unpack", "publish", "register"}

NoRow == [state |-> "none", owner |-> NULL]
Row(s, o) == [state |-> s, owner |-> o]

-----------------------------------------------------------------------------
(* Reaping: the first thing every ledger open does.  A session is dead     *)
(* when its session lock is free (or its lock file is gone): the process   *)
(* is gone.                                                                *)

Dead == {s \in sessions : ~alive[s]}

\* An interrupted build whose entry lock is free: rename the directory out.
Sweep(k)  == row[k].state = "building" /\ row[k].owner \in Dead
             /\ buildLock[k] = NULL
\* An interrupted prune that never removed the directory: back to live.
Revive(k) == row[k].state = "pruning" /\ row[k].owner \in Dead
             /\ fs[k] = "complete"
\* An interrupted prune whose directory is gone: finish it.
Drop(k)   == row[k].state = "pruning" /\ row[k].owner \in Dead
             /\ fs[k] # "complete"

RRow == [k \in Keys |-> IF Sweep(k) \/ Drop(k) THEN NoRow
                        ELSE IF Revive(k) THEN Row("live", NULL)
                        ELSE row[k]]
RFs == [k \in Keys |-> IF Sweep(k) THEN "absent" ELSE fs[k]]
RPins == {pk \in pins : pk[1] \notin Dead}
\* A dead session's row goes only once no entry row names it any more;
\* otherwise a later reaper could not tell that the owner is dead.
RSessions == sessions \ {s \in Dead : \A k \in Keys : RRow[k].owner # s}

\* The total, derived from the rows (the ledger stores no running total).
Total(r) == Cardinality({k \in Keys : r[k].state = "live"})

\* Nobody alive pins `k` (after reaping).
Unpinned(k) == \A q \in Procs : <<q, k>> \notin RPins

-----------------------------------------------------------------------------
Init == /\ fs = [k \in Keys |-> "absent"]
        /\ row = [k \in Keys |-> NoRow]
        /\ pins = {}
        /\ sessions = {}
        /\ buildLock = [k \in Keys |-> NULL]
        /\ alive = [p \in Procs |-> TRUE]
        /\ pc = [p \in Procs |-> "start"]
        /\ cur = [p \in Procs |-> NULL]
        /\ victims = [p \in Procs |-> {}]

\* Ledger op: create the session row.  The session lock is already held:
\* it is taken before the row exists, so no row ever names a session whose
\* lock was never held.
Start(p) == /\ alive[p] /\ pc[p] = "start"
            /\ sessions' = RSessions \cup {p}
            /\ row' = RRow /\ fs' = RFs /\ pins' = RPins
            /\ pc' = [pc EXCEPT ![p] = "idle"]
            /\ UNCHANGED <<buildLock, alive, cur, victims>>

Pick(p) == /\ alive[p] /\ pc[p] = "idle"
           /\ \E k \in Keys : cur' = [cur EXCEPT ![p] = k]
           /\ pc' = [pc EXCEPT ![p] = "lookup"]
           /\ UNCHANGED <<fs, row, pins, sessions, buildLock, alive, victims>>

\* Ledger op: pin the key first (a pin means "I want this entry", whether
\* or not it exists yet, so a pruner never takes it from under a waiter),
\* then use a live entry, reclaim one under prune, wait out a prune whose
\* directory is gone, or go and take the entry lock.
Lookup(p) == /\ alive[p] /\ pc[p] = "lookup"
             /\ LET k == cur[p]
                    r == RRow[k]
                IN /\ sessions' = RSessions /\ fs' = RFs
                   /\ pins' = IF ReadersPin THEN RPins \cup {<<p, k>>}
                                            ELSE RPins
                   /\ CASE r.state = "live" ->
                           /\ row' = RRow
                           /\ pc' = [pc EXCEPT ![p] = "using"]
                        [] r.state = "pruning" /\ RFs[k] = "complete" ->
                           \* Reclaim: the entry is wanted after all.  The
                           \* pruner finds the row live and skips it.
                           /\ row' = [RRow EXCEPT ![k] = Row("live", NULL)]
                           /\ pc' = [pc EXCEPT ![p] = "using"]
                        [] r.state = "pruning" ->
                           /\ row' = RRow
                           /\ pc' = pc
                        [] OTHER ->
                           /\ row' = RRow
                           /\ pc' = [pc EXCEPT ![p] = "acquire"]
             /\ UNCHANGED <<buildLock, alive, cur, victims>>

\* OS lock: the entry lock.
Acquire(p) == /\ alive[p] /\ pc[p] = "acquire"
              /\ buildLock[cur[p]] = NULL
              /\ buildLock' = [buildLock EXCEPT ![cur[p]] = p]
              /\ pc' = [pc EXCEPT ![p] = "recheck"]
              /\ UNCHANGED <<fs, row, pins, sessions, alive, cur, victims>>

\* Ledger op under the entry lock: whoever held it may have completed the
\* entry.  A complete entry is used, whatever its row says: live, under
\* prune (reclaimed), or building by a session that crashed after
\* completing it (adopted).  Otherwise claim the row and clear the
\* directory.
Recheck(p) ==
  /\ alive[p] /\ pc[p] = "recheck"
  /\ LET k == cur[p]
         r == RRow[k]
     IN /\ sessions' = RSessions /\ pins' = RPins
        /\ CASE RFs[k] = "complete" ->
                /\ row' = [RRow EXCEPT ![k] = Row("live", NULL)]
                /\ fs' = RFs
                /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
                /\ pc' = [pc EXCEPT ![p] = "using"]
             [] r.state = "pruning" ->
                /\ row' = RRow /\ fs' = RFs
                /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
                /\ pc' = [pc EXCEPT ![p] = "lookup"]
             [] OTHER ->
                /\ row' = [RRow EXCEPT ![k] = Row("building", p)]
                /\ fs' = [RFs EXCEPT ![k] = "absent"]
                /\ buildLock' = buildLock
                /\ pc' = [pc EXCEPT ![p] = "unpack"]
  /\ UNCHANGED <<alive, cur, victims>>

Unpack(p) == /\ alive[p] /\ pc[p] = "unpack"
             /\ fs' = [fs EXCEPT ![cur[p]] = "partial"]
             /\ pc' = [pc EXCEPT ![p] = "publish"]
             /\ UNCHANGED <<row, pins, sessions, buildLock, alive, cur,
                            victims>>

\* The atomic rename or marker write that completes the entry on disk.
Publish(p) == /\ alive[p] /\ pc[p] = "publish"
              /\ fs' = [fs EXCEPT ![cur[p]] = "complete"]
              /\ pc' = [pc EXCEPT ![p] = "register"]
              /\ UNCHANGED <<row, pins, sessions, buildLock, alive, cur,
                             victims>>

\* Ledger op: the entry becomes live, then the entry lock is released.
\* A session that saw the total over the limit prunes, holding no entry
\* lock (the implementation does so once its build is done; its pins are
\* still held either way).
Register(p) ==
  /\ alive[p] /\ pc[p] = "register"
  /\ LET k == cur[p]
         r == [RRow EXCEPT ![k] = Row("live", NULL)]
     IN /\ row' = r
        /\ pins' = RPins /\ sessions' = RSessions /\ fs' = RFs
        /\ buildLock' = [buildLock EXCEPT ![k] = NULL]
        /\ pc' = [pc EXCEPT ![p] = IF Total(r) > Limit THEN "mark"
                                                        ELSE "using"]
  /\ UNCHANGED <<alive, cur, victims>>

\* Ledger op: choose victims among the live rows nobody alive pins, and
\* announce the prune by flipping them to "pruning".
Mark(p) ==
  /\ alive[p] /\ pc[p] = "mark"
  /\ LET eligible == {k \in Keys : RRow[k].state = "live" /\ Unpinned(k)}
     IN \E V \in SUBSET eligible :
          /\ row' = [k \in Keys |-> IF k \in V THEN Row("pruning", p)
                                    ELSE RRow[k]]
          /\ victims' = [victims EXCEPT ![p] = V]
  /\ pins' = RPins /\ sessions' = RSessions /\ fs' = RFs
  /\ pc' = [pc EXCEPT ![p] = "remove"]
  /\ UNCHANGED <<buildLock, alive, cur>>

\* Ledger op, one victim at a time: re-read the row; if it is still ours,
\* rename the directory out (one atomic syscall, inside the critical
\* section) and delete the row in the same batch.  A row a waiter reclaimed
\* is live again and is left alone.  (Deleting the renamed-out tree happens
\* after the commit and is invisible to the protocol.)
RemoveInLedger(p) ==
  /\ RenameInLedger
  /\ \E k \in victims[p] :
       /\ IF RRow[k] = Row("pruning", p)
          THEN /\ fs' = [RFs EXCEPT ![k] = "absent"]
               /\ row' = [RRow EXCEPT ![k] = NoRow]
          ELSE /\ fs' = RFs
               /\ row' = RRow
       /\ victims' = [victims EXCEPT ![p] = victims[p] \ {k}]
  /\ pins' = RPins /\ sessions' = RSessions
  /\ UNCHANGED pc

\* Mutation: rename outside any ledger open, then delete rows afterwards.
RemoveOutside(p) ==
  /\ ~RenameInLedger
  /\ \E k \in victims[p] : /\ fs[k] # "absent"
                           /\ fs' = [fs EXCEPT ![k] = "absent"]
  /\ UNCHANGED <<row, pins, sessions, pc, victims>>

Remove(p) == /\ alive[p] /\ pc[p] = "remove"
             /\ RemoveInLedger(p) \/ RemoveOutside(p)
             /\ UNCHANGED <<buildLock, alive, cur>>

Removed(p) == /\ alive[p] /\ pc[p] = "remove"
              /\ IF RenameInLedger THEN victims[p] = {}
                 ELSE \A k \in victims[p] : fs[k] = "absent"
              /\ pc' = [pc EXCEPT ![p] = IF RenameInLedger THEN "using"
                                         ELSE "unmark"]
              /\ UNCHANGED <<fs, row, pins, sessions, buildLock, alive, cur,
                             victims>>

\* Mutation only: the ledger op that deletes the victims' rows afterwards.
Unmark(p) == /\ alive[p] /\ pc[p] = "unmark"
             /\ row' = [k \in Keys |-> IF k \in victims[p]
                                          /\ RRow[k].state = "pruning"
                                       THEN NoRow ELSE RRow[k]]
             /\ pins' = RPins /\ sessions' = RSessions /\ fs' = RFs
             /\ victims' = [victims EXCEPT ![p] = {}]
             /\ pc' = [pc EXCEPT ![p] = "using"]
             /\ UNCHANGED <<buildLock, alive, cur>>

\* Ledger op: done with the entry, drop the pin.
Finish(p) == /\ alive[p] /\ pc[p] = "using"
             /\ pins' = RPins \ {<<p, cur[p]>>}
             /\ row' = RRow /\ sessions' = RSessions /\ fs' = RFs
             /\ pc' = [pc EXCEPT ![p] = "idle"]
             /\ cur' = [cur EXCEPT ![p] = NULL]
             /\ UNCHANGED <<buildLock, alive, victims>>

\* The process dies, or ends normally, which leaves the ledger alone just
\* the same: the OS releases its locks; its rows stay.
Crash(p) == /\ alive[p] /\ p \notin Reliable
            /\ alive' = [alive EXCEPT ![p] = FALSE]
            /\ pc' = [pc EXCEPT ![p] = "dead"]
            /\ buildLock' = [k \in Keys |-> IF buildLock[k] = p THEN NULL
                                            ELSE buildLock[k]]
            /\ UNCHANGED <<fs, row, pins, sessions, cur, victims>>

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
  /\ buildLock \in [Keys -> Procs \cup {NULL}]
  /\ alive \in [Procs -> BOOLEAN]
  /\ pc \in [Procs -> Labels]
  /\ cur \in [Procs -> Keys \cup {NULL}]
  /\ victims \in [Procs -> SUBSET Keys]

\* A live row always has a complete directory behind it.
LiveIsComplete == \A k \in Keys : row[k].state = "live" => fs[k] = "complete"

\* A pin taken before the entry exists is intent; once its holder has
\* found the entry, the entry stays on disk for as long as the pin.
PinSafety == \A pk \in pins :
  alive[pk[1]] /\ ~(cur[pk[1]] = pk[2] /\ pc[pk[1]] \in Getting)
    => fs[pk[2]] = "complete"

\* The same, seen from the session: what it is using is on disk.
UseSafety == \A p \in Procs :
  alive[p] /\ pc[p] \in {"using", "mark", "remove", "unmark"}
    => fs[cur[p]] = "complete"

\* Only entries nobody alive uses or waits for are ever marked for pruning.
\* (A waiter in lookup may pin a pruning row whose directory is gone: it
\* waits for the row to go away so it can rebuild.)
PrunedUnpinned == \A k \in Keys : row[k].state = "pruning" =>
  \A pk \in pins : pk[2] = k =>
    ~alive[pk[1]] \/ (pc[pk[1]] = "lookup" /\ cur[pk[1]] = k)

\* Everything on disk is tracked by a row, so nothing can leak.
Tracked == \A k \in Keys : fs[k] # "absent" => row[k].state # "none"

\* A live builder holds the entry lock of the row it owns, and only the
\* holder of an entry's lock writes its directory.
BuildExcl ==
  /\ \A k \in Keys : row[k].state = "building" /\ alive[row[k].owner]
                       => buildLock[k] = row[k].owner
  /\ \A p \in Procs : pc[p] \in {"unpack", "publish", "register"}
                        => buildLock[cur[p]] = p

Safety == /\ TypeOK /\ LiveIsComplete /\ PinSafety /\ UseSafety
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
