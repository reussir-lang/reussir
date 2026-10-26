------------------------------ MODULE CachePrune ------------------------------
(***************************************************************************)
(* The protocol of rene's per-user cache (../cache-ledger.md): ledger rows *)
(* say which directories are entries, pins which session wants which      *)
(* entry; session locks tell live sessions from dead ones; entry locks are *)
(* held while a session looks an entry up or builds it; staging and trash  *)
(* are named for their session.  A transaction that only touches rows is   *)
(* one step; one that renames (publication, pruning) takes two, the        *)
(* renames with the ledger held and then the commit or its failure, whose  *)
(* outcome is unknown (TurboKV may log a batch, report an error, and       *)
(* replay it): the renames are undone either way.  Sessions outside        *)
(* `Reliable` may crash between any two steps or fail a commit.  Only      *)
(* complete, durable directories reach an entry path.  The pruner may      *)
(* remove any subset of the unpinned entries, none included (a repair).    *)
(* Ghosts: `moved`, `author`, `prev`, `unknown`, `lost`, `pinnedGone`.     *)
(* Each switch is TRUE in the protocol; FALSE is a mutation (README.md).   *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS Procs,          \* sessions
          Reliable,       \* the sessions that never crash or fail a commit
          Keys,           \* entry keys
          InPlace,        \* the keys whose entries are read in place
          NULL,
          ReadersPin,     \* a lookup pins the key it asks for
          PruneInTxn,     \* the pruner renames victims inside its transaction
          PublishInTxn,   \* the publisher renames inside its transaction
          RenameBack,     \* a failed commit undoes its transaction's renames
          DirCheck,       \* a lookup checks a live row's directory
          SweepDeadOnly,  \* the sweep deletes only what dead sessions own
          EntryLocks,     \* lookups and builds hold the entry's lock
          Repair          \* builds prune hourly, if only to repair

ASSUME /\ Reliable \subseteq Procs
       /\ InPlace \subseteq Keys
       /\ NULL \notin Procs

VARIABLES
  dir,        \* [Keys -> {"absent", "complete"}]: the entry path
  row,        \* [Keys -> BOOLEAN]: the ledger holds a live row for the key
  pins,       \* SUBSET (Procs \X Keys): the ledger's pin rows
  stage,      \* [Procs -> BOOLEAN]: the session's staging directory exists
  trash,      \* [Procs -> SUBSET Keys]: entry directories in its trash
  elock,      \* [Keys -> Procs \cup {NULL}]: the entry locks
  ledger,     \* Procs \cup {NULL}: the session inside a renaming transaction
  alive,      \* [Procs -> BOOLEAN]: FALSE once its session lock is free
  pc,         \* [Procs -> Labels]
  cur,        \* [Procs -> Keys \cup {NULL}]: the key the session works on
  held,       \* [Procs -> SUBSET Keys]: entries it used in place
  moved,      \* ghost: [Procs -> SUBSET Keys], entry paths it emptied
  author,     \* ghost: [Keys -> Procs \cup {NULL}], who left dir[k] rowless
  prev,       \* ghost: [Procs -> Procs \cup {NULL}], author of what it trashed
  unknown,    \* ghost: the sessions whose failed commit was applied
  lost,       \* ghost: the sessions that lost a directory they used
  pinnedGone  \* ghost: an entry pinned by a live session was removed

vars == <<dir, row, pins, stage, trash, elock, ledger, alive, pc, cur, held,
          moved, author, prev, unknown, lost, pinnedGone>>

Labels == {"idle", "acquire", "lookup", "build", "publish", "pubcommit",
           "pubrow", "using", "prune", "prunecommit", "late", "deltrash",
           "sweep", "exit", "dead"}

-----------------------------------------------------------------------------
\* Reaping, the first step of every transaction: dead sessions' pins go.
RPins == {pk \in pins : alive[pk[1]]}
Unpinned(k) == \A q \in Procs : <<q, k>> \notin RPins

\* The sessions reading the directory at k right now.
UsedBy(k) == {q \in Procs : alive[q] /\ ((pc[q] = "using" /\ cur[q] = k)
                                         \/ k \in held[q])}

\* A live entry in S is pinned by a live session.
PinnedByLive(S) ==
  \E k \in S, q \in Procs : row[k] /\ alive[q] /\ <<q, k>> \in pins

\* The ledger is free: an atomic transaction may run.
Free == ledger = NULL
Set(f, x, v) == [f EXCEPT ![x] = v]
\* Release the entry lock of k if p holds it.
Unlock(p, k) == IF elock[k] = p THEN Set(elock, k, NULL) ELSE elock

\* The rows a prune by p leaves: none whose directory is missing (victims
\* included); the mutant pruner drops its victims' before renaming them.
PrunedRows(p) == [k \in Keys |-> IF row[k] /\ (dir[k] = "absent"
                                     \/ (~PruneInTxn /\ k \in moved[p]))
                                 THEN FALSE ELSE row[k]]

-----------------------------------------------------------------------------
Init == /\ dir = [k \in Keys |-> "absent"]
        /\ row = [k \in Keys |-> FALSE]
        /\ pins = {}
        /\ stage = [p \in Procs |-> FALSE]
        /\ trash = [p \in Procs |-> {}]
        /\ elock = [k \in Keys |-> NULL]
        /\ alive = [p \in Procs |-> TRUE]
        /\ pc = [p \in Procs |-> "idle"]
        /\ cur = [p \in Procs |-> NULL]
        /\ held = [p \in Procs |-> {}]
        /\ moved = [p \in Procs |-> {}]
        /\ author = [k \in Keys |-> NULL]
        /\ prev = [p \in Procs |-> NULL]
        /\ ledger = NULL /\ unknown = {} /\ lost = {} /\ pinnedGone = FALSE

\* A build wants an entry.
Pick(p) == /\ alive[p] /\ pc[p] = "idle"
           /\ \E k \in Keys : cur' = Set(cur, p, k)
           /\ pc' = Set(pc, p, "acquire")
           /\ UNCHANGED <<dir, row, pins, stage, trash, elock, ledger, alive,
                          held, moved, author, prev, unknown, lost,
                          pinnedGone>>

\* OS lock: the entry's lock, waited for.
Acquire(p) == /\ alive[p] /\ pc[p] = "acquire"
              /\ IF EntryLocks
                 THEN /\ elock[cur[p]] = NULL
                      /\ elock' = Set(elock, cur[p], p)
                 ELSE elock' = elock
              /\ pc' = Set(pc, p, "lookup")
              /\ UNCHANGED <<dir, row, pins, stage, trash, ledger, alive, cur,
                             held, moved, author, prev, unknown, lost,
                             pinnedGone>>

\* Transaction, under the entry lock: pin the key, then use the entry if its
\* row is live and its directory there; else forget the row and build.
Lookup(p) ==
  /\ alive[p] /\ pc[p] = "lookup" /\ Free
  /\ LET k == cur[p] IN
     /\ pins' = IF ReadersPin THEN RPins \cup {<<p, k>>} ELSE RPins
     /\ IF row[k] /\ (dir[k] = "complete" \/ ~DirCheck)
        THEN /\ row' = row
             /\ elock' = Unlock(p, k)
             /\ pc' = Set(pc, p, "using")
        ELSE /\ row' = Set(row, k, FALSE)
             /\ elock' = elock
             /\ pc' = Set(pc, p, "build")
  /\ UNCHANGED <<dir, stage, trash, ledger, alive, cur, held, moved, author,
                 prev, unknown, lost, pinnedGone>>

\* Build the entry in the session's staging directory and make it durable.
Build(p) == /\ alive[p] /\ pc[p] = "build"
            /\ stage' = Set(stage, p, TRUE)
            /\ pc' = Set(pc, p, "publish")
            /\ UNCHANGED <<dir, row, pins, trash, elock, ledger, alive, cur,
                           held, moved, author, prev, unknown, lost,
                           pinnedGone>>

\* Transaction, first step: trash any debris at the entry path and rename
\* the staging directory into place.  If a live entry is there (only
\* possible without entry locks), use it and drop the staging directory.
PublishBegin(p) ==
  /\ alive[p] /\ pc[p] = "publish" /\ Free /\ PublishInTxn /\ stage[p]
  /\ LET k == cur[p] IN
     IF row[k] /\ dir[k] = "complete"
     THEN /\ stage' = Set(stage, p, FALSE)
          /\ pins' = RPins
          /\ elock' = Unlock(p, k)
          /\ pc' = Set(pc, p, "using")
          /\ UNCHANGED <<dir, trash, ledger, moved, author, prev, lost>>
     ELSE /\ ledger' = p
          /\ moved' = Set(moved, p, IF dir[k] = "complete" THEN {k} ELSE {})
          /\ trash' = IF dir[k] = "complete"
                      THEN Set(trash, p, trash[p] \cup {k}) ELSE trash
          /\ prev' = Set(prev, p, author[k])
          /\ lost' = IF dir[k] = "complete" THEN lost \cup UsedBy(k) ELSE lost
          /\ dir' = Set(dir, k, "complete")
          /\ author' = Set(author, k, p)
          /\ stage' = Set(stage, p, FALSE)
          /\ pc' = Set(pc, p, "pubcommit")
          /\ UNCHANGED <<pins, elock>>
  /\ UNCHANGED <<row, alive, cur, held, unknown, pinnedGone>>

\* The staging directory is gone (a mutant sweep): the command fails.
PublishMissing(p) ==
  /\ alive[p] /\ pc[p] = "publish" /\ ~stage[p]
  /\ elock' = Unlock(p, cur[p])
  /\ pc' = Set(pc, p, "exit")
  /\ UNCHANGED <<dir, row, pins, stage, trash, ledger, alive, cur, held,
                 moved, author, prev, unknown, lost, pinnedGone>>

\* The commit: the row goes live with the rename.  The trash goes next.
PublishCommit(p) ==
  /\ alive[p] /\ pc[p] = "pubcommit" /\ ledger = p
  /\ row' = Set(row, cur[p], TRUE)
  /\ pins' = RPins
  /\ ledger' = NULL
  /\ moved' = Set(moved, p, {})
  /\ author' = Set(author, cur[p], NULL)
  /\ prev' = Set(prev, p, NULL)
  /\ elock' = Unlock(p, cur[p])
  /\ pc' = Set(pc, p, "deltrash")
  /\ UNCHANGED <<dir, stage, trash, alive, cur, held, unknown, lost,
                 pinnedGone>>

\* The commit fails, applied or not; its renames are undone either way, and
\* the command fails.  Applied, the row is live over the debris that went
\* back, or over nothing.
PublishFail(p) ==
  /\ alive[p] /\ pc[p] = "pubcommit" /\ ledger = p /\ p \notin Reliable
  /\ LET k == cur[p]
         back == k \in moved[p] /\ k \in trash[p]
     IN \E applied \in BOOLEAN :
          /\ row' = IF applied THEN Set(row, k, TRUE) ELSE row
          /\ pins' = IF applied THEN RPins ELSE pins
          /\ unknown' = IF applied THEN unknown \cup {p} ELSE unknown
          /\ moved' = Set(moved, p, IF applied THEN {k} ELSE {})
          /\ IF RenameBack
             THEN /\ stage' = Set(stage, p, TRUE)
                  /\ dir' = Set(dir, k, IF back THEN "complete" ELSE "absent")
                  /\ author' = Set(author, k, IF back /\ ~applied
                                              THEN prev[p] ELSE NULL)
                  /\ trash' = Set(trash, p, trash[p] \ moved[p])
                  /\ lost' = lost \cup UsedBy(k)
             ELSE /\ author' = IF applied THEN Set(author, k, NULL)
                                          ELSE author
                  /\ UNCHANGED <<stage, dir, trash, lost>>
  /\ ledger' = NULL
  /\ prev' = Set(prev, p, NULL)
  /\ elock' = Unlock(p, cur[p])
  /\ pc' = Set(pc, p, "exit")
  /\ UNCHANGED <<alive, cur, held, pinnedGone>>

\* Mutation: rename into place outside the ledger, then write the row in a
\* transaction of its own.
PublishOutside(p) ==
  /\ alive[p] /\ pc[p] = "publish" /\ ~PublishInTxn /\ stage[p]
  /\ LET k == cur[p] IN
     /\ trash' = IF dir[k] = "complete"
                 THEN Set(trash, p, trash[p] \cup {k}) ELSE trash
     /\ lost' = IF dir[k] = "complete" THEN lost \cup UsedBy(k) ELSE lost
     /\ dir' = Set(dir, k, "complete")
     /\ author' = Set(author, k, p)
  /\ stage' = Set(stage, p, FALSE)
  /\ pc' = Set(pc, p, "pubrow")
  /\ UNCHANGED <<row, pins, elock, ledger, alive, cur, held, moved, prev,
                 unknown, pinnedGone>>

PublishRow(p) ==
  /\ alive[p] /\ pc[p] = "pubrow" /\ Free
  /\ row' = Set(row, cur[p], TRUE)
  /\ pins' = RPins
  /\ author' = Set(author, cur[p], NULL)
  /\ elock' = Unlock(p, cur[p])
  /\ pc' = Set(pc, p, "deltrash")
  /\ UNCHANGED <<dir, stage, trash, ledger, alive, cur, held, moved, prev,
                 unknown, lost, pinnedGone>>

\* After the commit and the close, delete the trash; then a publisher uses
\* its entry and a pruner sweeps.
DeleteTrash(p) == /\ alive[p] /\ pc[p] = "deltrash"
                  /\ trash' = Set(trash, p, {})
                  /\ pc' = Set(pc, p, IF cur[p] = NULL THEN "sweep"
                                                       ELSE "using")
                  /\ UNCHANGED <<dir, row, pins, stage, elock, ledger, alive,
                                 cur, held, moved, author, prev, unknown,
                                 lost, pinnedGone>>

\* Done: an entry read in place stays pinned; one copied out is unpinned.
Finish(p) ==
  /\ alive[p] /\ pc[p] = "using"
  /\ IF cur[p] \in InPlace
     THEN /\ held' = Set(held, p, held[p] \cup {cur[p]})
          /\ pins' = pins
     ELSE /\ Free
          /\ held' = held
          /\ pins' = RPins \ {<<p, cur[p]>>}
  /\ cur' = Set(cur, p, NULL)
  /\ pc' = Set(pc, p, "idle")
  /\ UNCHANGED <<dir, row, stage, trash, elock, ledger, alive, moved, author,
                 prev, unknown, lost, pinnedGone>>

\* A done build prunes past the size limit, or hourly to repair; so does
\* `rene cache prune`.  It holds no entry lock and keeps its pins.
PruneNow(p) == /\ alive[p] /\ pc[p] = "idle"
               /\ pc' = Set(pc, p, "prune")
               /\ UNCHANGED <<dir, row, pins, stage, trash, elock, ledger,
                              alive, cur, held, moved, author, prev, unknown,
                              lost, pinnedGone>>

\* Transaction, first step: rename to the trash some unpinned live entries
\* and all debris.  The rows go at the commit, with every row whose
\* directory is missing.
PruneBegin(p) ==
  /\ alive[p] /\ pc[p] = "prune" /\ Free
  /\ LET eligible == {k \in Keys : /\ row[k] /\ dir[k] = "complete"
                                    /\ Unpinned(k)}
         debris == {k \in Keys : ~row[k] /\ dir[k] = "complete"}
     IN \E V \in SUBSET eligible :
          IF PruneInTxn
          THEN LET out == V \cup debris IN
               /\ dir' = [k \in Keys |-> IF k \in out THEN "absent"
                                                    ELSE dir[k]]
               /\ trash' = Set(trash, p, trash[p] \cup out)
               /\ moved' = Set(moved, p, out)
               /\ lost' = lost \cup UNION {UsedBy(k) : k \in out}
               /\ pinnedGone' = (pinnedGone \/ PinnedByLive(V))
          ELSE \* Mutation: decide now, rename after the commit.
               /\ moved' = Set(moved, p, V)
               /\ UNCHANGED <<dir, trash, lost, pinnedGone>>
  /\ ledger' = p
  /\ pc' = Set(pc, p, "prunecommit")
  /\ UNCHANGED <<row, pins, stage, elock, alive, cur, held, author, prev,
                 unknown>>

PruneCommit(p) ==
  /\ alive[p] /\ pc[p] = "prunecommit" /\ ledger = p
  /\ row' = PrunedRows(p)
  /\ pins' = RPins
  /\ ledger' = NULL
  /\ moved' = IF PruneInTxn THEN Set(moved, p, {}) ELSE moved
  /\ author' = [k \in Keys |-> IF dir[k] = "absent" THEN NULL ELSE author[k]]
  /\ pc' = Set(pc, p, IF PruneInTxn THEN "deltrash" ELSE "late")
  /\ UNCHANGED <<dir, stage, trash, elock, alive, cur, held, prev, unknown,
                 lost, pinnedGone>>

\* The commit fails, applied or not; the renames are undone either way.
\* Applied, the victims come back as debris of the failing session.
PruneFail(p) ==
  /\ alive[p] /\ pc[p] = "prunecommit" /\ ledger = p /\ p \notin Reliable
  /\ \E applied \in BOOLEAN :
       LET rows == IF applied THEN PrunedRows(p) ELSE row
           dirs == IF RenameBack
                   THEN [k \in Keys |-> IF k \in moved[p] /\ k \in trash[p]
                                        THEN "complete" ELSE dir[k]]
                   ELSE dir
       IN /\ row' = rows
          /\ dir' = dirs
          /\ pins' = IF applied THEN RPins ELSE pins
          /\ unknown' = IF applied THEN unknown \cup {p} ELSE unknown
          /\ author' = [k \in Keys |->
                          IF row[k] /\ ~rows[k] /\ dirs[k] = "complete"
                          THEN p ELSE author[k]]
          /\ trash' = IF RenameBack THEN Set(trash, p, trash[p] \ moved[p])
                                    ELSE trash
  /\ ledger' = NULL
  /\ moved' = Set(moved, p, {})
  /\ pc' = Set(pc, p, "exit")
  /\ UNCHANGED <<stage, elock, alive, cur, held, prev, lost, pinnedGone>>

\* Mutation only: the victims' directories are renamed away after the
\* commit, outside any transaction.
Late(p) ==
  /\ alive[p] /\ pc[p] = "late"
  /\ LET gone == {k \in moved[p] : dir[k] = "complete"} IN
     /\ dir' = [k \in Keys |-> IF k \in gone THEN "absent" ELSE dir[k]]
     /\ lost' = lost \cup UNION {UsedBy(k) : k \in gone}
     /\ pinnedGone' = (pinnedGone \/ PinnedByLive(gone))
  /\ moved' = Set(moved, p, {})
  /\ pc' = Set(pc, p, "sweep")
  /\ UNCHANGED <<row, pins, stage, trash, elock, ledger, alive, cur, held,
                 author, prev, unknown>>

\* After the prune, outside the ledger: delete dead sessions' staging and
\* trash, which nobody else touches.
Sweep(p) ==
  /\ alive[p] /\ pc[p] = "sweep"
  /\ LET owners == IF SweepDeadOnly THEN {q \in Procs : ~alive[q]}
                                  ELSE Procs \ {p} IN
     /\ stage' = [q \in Procs |-> IF q \in owners THEN FALSE ELSE stage[q]]
     /\ trash' = [q \in Procs |-> IF q \in owners THEN {} ELSE trash[q]]
  /\ pc' = Set(pc, p, "idle")
  /\ UNCHANGED <<dir, row, pins, elock, ledger, alive, cur, held, moved,
                 author, prev, unknown, lost, pinnedGone>>

\* The OS releases a dead process's locks; its rows, pins and dirs stay.
Die(p) == /\ alive' = Set(alive, p, FALSE)
          /\ pc' = Set(pc, p, "dead")
          /\ elock' = [k \in Keys |-> IF elock[k] = p THEN NULL ELSE elock[k]]
          /\ ledger' = IF ledger = p THEN NULL ELSE ledger
          /\ cur' = Set(cur, p, NULL)
          /\ held' = Set(held, p, {})
          /\ prev' = Set(prev, p, NULL)
          /\ UNCHANGED <<dir, row, pins, stage, trash, moved, author, unknown,
                         lost, pinnedGone>>

Crash(p) == alive[p] /\ p \notin Reliable /\ Die(p)
\* The exit of a command that failed.
Exit(p) == alive[p] /\ pc[p] = "exit" /\ Die(p)

Step(p) == \/ Pick(p) \/ Acquire(p) \/ Lookup(p) \/ Build(p)
           \/ PublishBegin(p) \/ PublishMissing(p) \/ PublishCommit(p)
           \/ PublishFail(p) \/ PublishOutside(p) \/ PublishRow(p)
           \/ DeleteTrash(p) \/ Finish(p) \/ PruneNow(p) \/ PruneBegin(p)
           \/ PruneCommit(p) \/ PruneFail(p) \/ Late(p) \/ Sweep(p)
           \/ Exit(p)

Next == \E p \in Procs : Step(p) \/ Crash(p)

\* A reliable session keeps wanting entries and, with `Repair`, pruning (the
\* hourly repair); otherwise only the size limit prunes, maybe never.
Fairness == /\ \A p \in Procs : SF_vars(Step(p))
            /\ \A p \in Reliable : /\ SF_vars(Pick(p))
                                   /\ SF_vars(Repair /\ PruneNow(p))
Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Safety *)

TypeOK ==
  /\ dir \in [Keys -> {"absent", "complete"}]
  /\ row \in [Keys -> BOOLEAN]
  /\ pins \subseteq Procs \X Keys
  /\ stage \in [Procs -> BOOLEAN]
  /\ trash \in [Procs -> SUBSET Keys]
  /\ elock \in [Keys -> Procs \cup {NULL}]
  /\ ledger \in Procs \cup {NULL}
  /\ alive \in [Procs -> BOOLEAN]
  /\ pc \in [Procs -> Labels]
  /\ cur \in [Procs -> Keys \cup {NULL}]
  /\ held \in [Procs -> SUBSET InPlace]
  /\ moved \in [Procs -> SUBSET Keys]
  /\ author \in [Keys -> Procs \cup {NULL}]
  /\ prev \in [Procs -> Procs \cup {NULL}]
  /\ unknown \subseteq Procs
  /\ lost \subseteq Procs
  /\ pinnedGone \in BOOLEAN

\* Nothing a session uses (now, or in place) disappears or is replaced.
UseSafety ==
  /\ lost = {}
  /\ \A p \in Procs : alive[p] =>
       /\ pc[p] = "using" => dir[cur[p]] = "complete"
       /\ \A k \in held[p] : dir[k] = "complete"

\* An entry read in place stays pinned for the rest of its session.
PinsHeld == \A p \in Procs : alive[p] => \A k \in held[p] : <<p, k>> \in pins

\* Only entries no live session pins are removed.
RemovedUnpinned == ~pinnedGone

\* Unclean: q holds the ledger, has died, or had a failed commit applied.
Unclean(q) == ledger = q \/ ~alive[q] \/ q \in unknown

\* A live row has its directory unless an unclean transaction took it.
LiveHasDir == \A k \in Keys :
  row[k] /\ dir[k] = "absent" => \E q \in Procs : k \in moved[q] /\ Unclean(q)

\* A rowless directory at an entry path was left by an unclean transaction.
RowlessIsDebris == \A k \in Keys :
  ~row[k] /\ dir[k] = "complete" => author[k] \in Procs /\ Unclean(author[k])

\* A live session's staging exists while it publishes or fails, its trash
\* inside its transaction and until it deletes it after the commit.
Owned == \A p \in Procs : alive[p] =>
  /\ stage[p] => pc[p] \in {"publish", "exit"}
  /\ trash[p] # {} => ledger = p \/ pc[p] = "deltrash"

\* Only the holder of an entry's lock builds or publishes it.
BuildExcl == \A p \in Procs :
  alive[p] /\ pc[p] \in {"build", "publish", "pubcommit", "pubrow"}
    => elock[cur[p]] = p

-----------------------------------------------------------------------------
(* Liveness, given a session that never crashes. *)

\* A reliable session that wants an entry eventually uses it.
Progress == \A p \in Reliable : [](pc[p] = "acquire" => <>(pc[p] = "using"))

\* What a dead session left: staging, trash, pins, debris, and live rows
\* whose directories its unclean transaction removed.
Clean(q) == /\ ~stage[q]
            /\ trash[q] = {}
            /\ \A k \in Keys : <<q, k>> \notin pins
            /\ \A k \in Keys : ~row[k] /\ dir[k] = "complete" => author[k] # q
            /\ \A k \in moved[q] : row[k] => dir[k] = "complete"

\* Everything a dead session left is eventually cleaned up.
Recovery == \A q \in Procs : [](~alive[q] => <>Clean(q))

=============================================================================
