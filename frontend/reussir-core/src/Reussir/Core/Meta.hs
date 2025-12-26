{- |
Module      : Reussir.Core.Meta
Description : Meta-variable constraint graph, growing-cycle detection, and solution propagation.

This module implements a small constraint solver over *meta variables* (unification-ish
placeholders) by building a directed graph of "flows" between metas and optionally
annotating edges with a constructor / type template.

The key ideas:

1. We maintain a directed graph over 'MetaID's.
   Each node is a meta variable, and each directed edge @u -> v@ means:
   "information about @u@ flows into @v@".

2. Edges optionally carry a constructor (a 'Type' template):
   - @u -[Nothing]-> v@: plain flow (solutions of @u@ are also solutions of @v@)
   - @u -[Just t]-> v@: constructor flow (solutions of @u@ can be /used to instantiate/ @t@
     and the instantiated result flows into @v@)

3. We also record direct evidence of concrete types per meta via 'addConcreteFlow'.

4. A /growing cycle/ is a directed cycle that contains at least one constructor edge.
   Such a cycle implies an infinite growth (e.g. @a = List a = List (List a) = ...@),
   so the system is unsatisfiable (or would require recursive types / μ-types,
   which we currently do not model). If any growing cycle exists, we return @Nothing@.

5. If there is no growing cycle:
   - SCCs (strongly connected components) formed only by non-constructor edges represent
     metas that must share the same solution set.
   - We compute SCCs, process them in a topological order, and propagate/instantiate
     types along incoming edges.

-------------------------------------------------------------------------------

Graph model
===========

Nodes:
  - Each meta variable is a node identified by 'MetaID'.

Edges:
  - @u -> v@ (plain edge): solutions(u) ⊆ solutions(v)
  - @u -(t)-> v@ (ctor edge): instantiate template @t@ using solutions of metas referenced
    inside @t@ (including possibly @u@ and others), and add those instantiated types to
    solutions(v).

Concrete evidence:
  - A meta can be seeded with concrete types (only if 'isConcrete' holds).

Example (ASCII)
---------------

Suppose we have metas: m0, m1, m2

Plain edges:
  m0 -----> m1
  m1 -----> m2

Ctor edge:
  m2 -(List m2)-> m0

ASCII sketch:

  [m0] -----> [m1] -----> [m2]
    ^                      |
    |                      |
    +------(List m2)-------+

This contains a cycle and it contains a constructor edge, therefore it is a *growing cycle*.
We reject it.

Non-growing SCC example:

  [m0] <----> [m1] -----> [m2]

If edges m0->m1 and m1->m0 are both plain edges, then {m0,m1} is an SCC.
They must share the same solution set, and their combined solutions flow into m2.

-------------------------------------------------------------------------------

Algorithm overview
==================

detectGrowingCycles
-------------------

We run DFS with node states:
  * Visiting depth : currently in recursion stack, remembering DFS depth
  * Done          : fully processed

While exploring edges on the current DFS path, we additionally remember the most recent
constructor edge encountered (and its depth), called "latest ctor edge".

When we see a back edge @u -> v@ (i.e. v is Visiting), we found a directed cycle.
Then:

  1) If the back edge itself has a constructor, we immediately found a growing cycle.
  2) Otherwise, if our latest ctor edge is deeper than the depth of v, that ctor edge lies
     within the cycle segment v..u, so it's a growing cycle. Return that ctor edge.

This returns *one* witness ctor-edge in some growing cycle.

solveMeta
---------

1) Reject if any growing cycle exists.

2) Compute SCCs with 'stronglyConnComp' over the plain adjacency (we ignore ctor annotation
   for SCC computation; ctor edges still connect nodes, but SCCs with ctor edges are safe
   only because we pre-checked "no growing cycles"; cycles may exist, but must contain
   no ctor edges).

3) Build a reverse graph (incomingEdges) so that for any node v we can efficiently enumerate
   all incoming edges (u, ctorAnn).

4) Initialize solutions with the concrete flow table.

5) Process SCCs in topological order:
   For each SCC:
     - Let nodes = all metas in this SCC.
     - baseTypes = union of existing solutions already present for nodes
     - flowTypes = union of contributions from incoming edges from outside the SCC
       * For plain edges: solutions(u)
       * For ctor edges: instantiate(template) using solutions of metas appearing in template
     - allTypes = nub(baseTypes ++ flowTypes)
     - Write allTypes as the solution set for each node in SCC.

Instantiation details
---------------------

instantiate template t:
  - collectVars t = all MetaID occurring in t (as TypeMeta nodes)
  - for each collected meta v, look up its solution set sol[v] (possibly empty)
  - generateAssignments builds the Cartesian product of choices for all vars
  - substituteMeta uses each assignment to produce a fully instantiated Type

Note: if some meta in template has no solutions yet, it contributes no assignments and thus
produces no instantiated results. This naturally delays ctor instantiation until prerequisites
arrive from earlier SCCs.

-------------------------------------------------------------------------------

Performance notes
=================

- We store meta vars in a Seq inside an IORef for append-only allocation.
- Each MetaVar stores its outgoing edges in a mutable HashTable.
- We use a HashTable for solution maps as well.
- SCC computation uses Data.Graph over an adjacency list snapshot (O(V+E)).
- Instantiation can blow up combinatorially due to Cartesian products; this is intended
  because the solver returns *all* possible fully-instantiated types for codegen.

-------------------------------------------------------------------------------
-}
module Reussir.Core.Meta where

import Control.Monad (forM, forM_, when)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (modifyIORef', newIORef', readIORef')
import Reussir.Core.Type (isConcrete, substituteMeta)
import Reussir.Core.Types.Meta
import Reussir.Core.Types.MetaID
import Reussir.Core.Types.Type (Type (..))
import Reussir.Parser.Types.Lexer (Path)

{- |
Create an empty 'MetaState'.

This allocates:
  - an IORef holding a Seq of all allocated 'MetaVar' records
  - a hash table for 'concreteFlow' seeding

Allocation model
----------------
Meta vars are allocated by appending to a Seq. Their 'MetaID' is the index in that Seq
at allocation time. This means IDs are stable for the lifetime of the MetaState.
-}
emptyMetaState :: (IOE :> es, Prim :> es) => Eff es MetaState
emptyMetaState = MetaState <$> newIORef' Seq.empty <*> liftIO H.new

{- |
Allocate a new meta variable and append it to the global sequence.

Parameters:
  * metaName   : pretty/debug name
  * metaSpan   : (start,end) span in source, if available
  * metaBounds : a list of paths used as bounds/constraints (domain-specific)
  * state      : global state

Returns:
  A fresh 'MetaID' corresponding to its index in the internal Seq.

Notes:
  Each meta var has its own mutable hash table of outgoing edges ('metaLinks').
-}
newMetaVar ::
    (IOE :> es, Prim :> es) =>
    T.Text ->
    Maybe (Int64, Int64) ->
    [Path] ->
    MetaState ->
    Eff es MetaID
newMetaVar metaName metaSpan metaBounds state = do
    links <- liftIO H.new
    let var = MetaVar{metaName, metaSpan, metaLinks = links, metaBounds}
        ref = getStateRef state
    varID <- fromIntegral . Seq.length <$> readIORef' ref
    modifyIORef' ref (Seq.|> var)
    pure $ MetaID varID

{- |
Add an edge @src -> tgt@ into the meta graph.

If @mType@ is:
  * Nothing    : plain flow edge
  * Just tyTpl : constructor edge carrying a template type

Semantics:
  - plain: solutions(src) flow into solutions(tgt)
  - ctor : instantiate tyTpl with current solutions for metas occurring inside tyTpl,
           and the instantiated results flow into solutions(tgt)

Implementation:
  Outgoing edges are stored in 'metaLinks' of the source node.
-}
addLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaID ->
    Maybe Type ->
    MetaState ->
    Eff es ()
addLink (MetaID srcID) (MetaID tgtID) mType state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral srcID)
    liftIO $ H.insert (metaLinks var) (MetaID tgtID) mType

{- |
Add a plain flow edge @src -> tgt@.
-}
addDirectLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaID ->
    MetaState ->
    Eff es ()
addDirectLink src tgt state = addLink src tgt Nothing state

{- |
Add a constructor edge @src -(tyTpl)-> tgt@.
-}
addCtorLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaID ->
    Type ->
    MetaState ->
    Eff es ()
addCtorLink src tgt mType state = addLink src tgt (Just mType) state

{- |
Get the human-friendly name of a meta variable.
-}
getName ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaState ->
    Eff es T.Text
getName (MetaID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ metaName var

{- |
Get the source span of a meta variable, if any.
-}
getSpan ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaState ->
    Eff es (Maybe (Int64, Int64))
getSpan (MetaID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ metaSpan var

{- |
Seed a meta variable with a concrete type.

This only inserts the type if 'isConcrete ty' holds.
If the meta has an existing list in 'concreteFlow', we cons onto it.

This is an *input* to the solver: it represents "we already learned that this meta
can be (or must be) this concrete type".

Note:
  We store a list of concrete types per meta, allowing multiple candidates.
-}
addConcreteFlow ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    Type ->
    MetaState ->
    Eff es ()
addConcreteFlow metaID ty state = do
    when (isConcrete ty) $ do
        let table = concreteFlow state
        existing <- liftIO $ H.lookup table metaID
        case existing of
            Nothing -> liftIO $ H.insert table metaID [ty]
            Just tys -> liftIO $ H.insert table metaID (ty : tys)

-- Internal DFS visitation state used by 'detectGrowingCycles'.
data VisitState
    = Visiting {-# UNPACK #-} !Int
    | Done
    deriving (Show)

{- | Detects a cycle in the graph that contains at least one constructor edge.
If such a cycle exists, returns /one witness constructor edge/ that is part of such a cycle.

Why this matters
----------------
A constructor edge is like a "type grows by applying a constructor".
If you can return to the same meta through a path that includes a constructor edge,
you can keep growing types indefinitely:

  a  ->  ...  ->  a
  where some edge is (List _), (Pair _ _), (Arrow _ _), ...

This is the classic "occurs check" style failure in unification, generalized to a graph
with multiple metas.

What is returned
----------------
The returned triple @(src, tgt, mCtor)@ is always an edge that has a constructor
(i.e. @mCtor == Just ...@) and lies on some directed cycle.

Algorithm sketch (DFS + latest ctor edge)
-----------------------------------------
We run DFS over all nodes, maintaining:
  - visited map: MetaID -> VisitState (Visiting depth | Done)
  - depth: current DFS depth
  - latestCtorDepth: depth at which we last traversed a ctor edge on the current recursion path
  - latestCtorEdge: that edge (u,v,Just t)

When exploring edge (u -> v):
  - If v is Visiting(vDepth), then u->v is a back edge forming a cycle.
    * If current edge is ctor, it is a growing cycle.
    * Else, if latestCtorDepth > vDepth, then latestCtorEdge lies within the cycle segment.
-}
detectGrowingCycles ::
    (IOE :> es, Prim :> es) =>
    MetaState ->
    Eff es (Maybe (MetaID, MetaID, Maybe Type))
detectGrowingCycles state = do
    vars <- readIORef' (getStateRef state)
    let nodes = [MetaID (fromIntegral i) | i <- [0 .. Seq.length vars - 1]]

    let loop [] _ = pure Nothing
        loop (n : ns) visited = do
            let isVisited = Map.member n visited
            if isVisited
                then loop ns visited
                else do
                    (res, visited') <- dfs n 0 0 Nothing visited
                    case res of
                        Just edge -> pure $ Just edge
                        Nothing -> loop ns visited'

        dfs u depth latestCtorDepth latestCtorEdge visited = do
            let visited' = Map.insert u (Visiting depth) visited
            let var = Seq.index vars (fromIntegral (case u of MetaID i -> i))

            edgesList <- liftIO $ H.toList (metaLinks var)

            let processEdges [] vMap = pure (Nothing, Map.insert u Done vMap)
                processEdges ((v, ctor) : es) vMap = do
                    let hasCtor = isJust ctor
                        newDepth = depth + 1
                        (newLatestDepth, newLatestEdge) =
                            if hasCtor
                                then (newDepth, Just (u, v, ctor))
                                else (latestCtorDepth, latestCtorEdge)

                    case Map.lookup v vMap of
                        Just (Visiting vDepth) ->
                            if hasCtor
                                then pure (Just (u, v, ctor), vMap)
                                else
                                    if newLatestDepth > vDepth
                                        then pure (newLatestEdge, vMap)
                                        else processEdges es vMap
                        Just Done -> processEdges es vMap
                        Nothing -> do
                            (res, vMap') <- dfs v newDepth newLatestDepth newLatestEdge vMap
                            case res of
                                Just found -> pure (Just found, vMap')
                                Nothing -> processEdges es vMap'

            processEdges edgesList visited'

    loop nodes Map.empty

{- | Solves the meta variable constraints and returns a map @MetaID -> [Type]@.

Return value:
  - @Nothing@ if a growing cycle is detected.
  - @Just table@ where table maps each meta to a list of possible fully instantiated types.

High-level procedure
--------------------
1. Reject if there is a growing cycle (see 'detectGrowingCycles').

2. Otherwise:
   - Compute SCCs in the meta graph.
   - Process SCCs in topological order.
   - Each SCC shares one solution set (we assign the same list to each node in SCC).
   - For each SCC, collect:
       a) existing solutions already known for nodes in SCC
       b) contributions from incoming edges from outside the SCC
   - Union them (dedup with 'nub') and write back.

Important detail:
  Cycles are allowed as long as they contain no constructor edge (the "no growing cycle"
  condition). Such SCCs are handled by giving them a shared solution set.

Instantiation along ctor edges
------------------------------
If we have incoming edge @u -(tTpl)-> v@ (and u is outside the SCC):
  - We instantiate @tTpl@ by substituting each meta appearing in @tTpl@ with each of its
    current solutions (cartesian product).
  - Each instantiated result is added to solutions(v) (and thus to the SCC’s shared set).

This is why the solver returns *lists* of types: it explicitly enumerates alternatives.

Caveat:
  Instantiation may be large (cartesian product). That's intended for codegen enumeration.
-}
solveMeta ::
    (IOE :> es, Prim :> es) =>
    MetaState ->
    Eff es (Maybe (H.CuckooHashTable MetaID [Type]))
solveMeta state = do
    cycle' <- detectGrowingCycles state
    if isJust cycle'
        then pure Nothing
        else Just <$> runSolve
  where
    runSolve = do
        vars <- readIORef' (getStateRef state)
        let allNodes = [MetaID (fromIntegral i) | i <- [0 .. Seq.length vars - 1]]

        -- Build adjacency list for SCC
        -- stronglyConnComp expects triples (node, key, [key]).
        adjList <- forM allNodes $ \u -> do
            let var = Seq.index vars (fromIntegral (case u of MetaID i -> i))
            edges <- liftIO $ H.toList (metaLinks var)
            let neighbors = map fst edges
            pure (u, u, neighbors)

        let sccs = stronglyConnComp adjList
        let topo = reverse sccs

        -- Build reverse graph: for each v, store incoming edges (u, ctorAnn)
        incomingEdges <- liftIO (H.new :: IO (H.CuckooHashTable MetaID [(MetaID, Maybe Type)]))
        forM_ allNodes $ \u -> do
            let var = Seq.index vars (fromIntegral (case u of MetaID i -> i))
            edges <- liftIO $ H.toList (metaLinks var)
            forM_ edges $ \(v, ctor) -> do
                prev <- liftIO $ H.lookup incomingEdges v
                let new = (u, ctor) : fromMaybe [] prev
                liftIO $ H.insert incomingEdges v new

        -- Initialize solution with concrete flow
        currentSol <- liftIO (H.new :: IO (H.CuckooHashTable MetaID [Type]))
        concrete <- liftIO $ H.toList (concreteFlow state)
        forM_ concrete $ \(k, v) -> liftIO $ H.insert currentSol k v

        let processSCC comp = do
                let nodes = flattenSCC comp

                -- 1) Existing types already in currentSol for nodes in this SCC
                baseTypes <- fmap concat $ forM nodes $ \n -> do
                    res <- liftIO $ H.lookup currentSol n
                    pure $ fromMaybe [] res

                -- 2) Types contributed by incoming edges from outside this SCC
                flowTypes <- fmap concat $ forM nodes $ \n ->
                    getIncomingTypes currentSol incomingEdges nodes n

                let allTypes = nub (baseTypes ++ flowTypes)

                -- Assign the shared set to all nodes in SCC
                forM_ nodes $ \n -> liftIO $ H.insert currentSol n allTypes

        forM_ topo processSCC
        pure currentSol

    -- \|
    --    Collect contributions from all incoming edges to @node@.
    --
    --    - If edge comes from within the same SCC: ignore it, since SCC shares a solution set
    --      and we want only "external" contributions to compute it.
    --    - Else:
    --        * plain edge: take solutions(u)
    --        * ctor edge : instantiate template with solutions of metas appearing in it
    --
    getIncomingTypes sol incomingEdges sccNodes node = do
        edges <- liftIO $ H.lookup incomingEdges node
        case edges of
            Nothing -> pure []
            Just es -> fmap concat $ forM es $ \(u, ctor) ->
                resolveEdge sol sccNodes u ctor

    -- \|
    --    Resolve a single incoming edge into a list of contributed types.
    --
    resolveEdge sol sccNodes u ctor = do
        if u `elem` sccNodes
            then pure []
            else case ctor of
                Nothing -> do
                    res <- liftIO $ H.lookup sol u
                    pure $ fromMaybe [] res
                Just t -> instantiate t sol

    -- \|
    --    Instantiate a template type by enumerating solutions for all metas occurring inside it.
    --
    --    Steps:
    --      1) collectVars t: list all MetaID referenced as TypeMeta in t
    --      2) generateAssignments: cartesian product picking one concrete type per meta
    --      3) substituteMeta: apply assignment env to t, producing fully instantiated types
    --
    instantiate t sol = do
        let vars = nub $ collectVars t
        assignments <- generateAssignments vars sol
        pure $ map (\env -> substituteMeta t (\m -> Map.lookup m env)) assignments

    -- Collect all metas referenced in a Type tree.
    collectVars (TypeMeta m) = [m]
    collectVars (TypeExpr _ args) = concatMap collectVars args
    collectVars (TypeArrow t1 t2) = collectVars t1 ++ collectVars t2
    collectVars _ = []

    -- \|
    --    Enumerate all environments (MetaID -> Type) for the given meta variables.
    --
    --    If some meta has no known solution yet (lookup returns Nothing or empty list),
    --    it yields no assignments and thus blocks instantiation (returns []).
    generateAssignments [] _ = pure [Map.empty]
    generateAssignments (v : vs) sol = do
        rest <- generateAssignments vs sol
        options <- liftIO $ H.lookup sol v
        let opts = fromMaybe [] options
        pure [Map.insert v opt r | opt <- opts, r <- rest]
