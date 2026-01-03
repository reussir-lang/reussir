{- |
Module      : Reussir.Core.Generic
Description : Generic-variable constraint graph, growing-cycle detection, and solution propagation.

This module implements a small constraint solver over *generic variables* (unification-ish
placeholders) by building a directed graph of "flows" between generics and optionally
annotating edges with a constructor / type template.

The key ideas:

1. We maintain a directed graph over 'GenericID's.
   Each node is a generic variable, and each directed edge @u -> v@ means:
   "information about @u@ flows into @v@".

2. Edges optionally carry a constructor (a 'Type' template):
   - @u -[Nothing]-> v@: plain flow (solutions of @u@ are also solutions of @v@)
   - @u -[Just t]-> v@: constructor flow (solutions of @u@ can be /used to instantiate/ @t@
     and the instantiated result flows into @v@)

3. We also record direct evidence of concrete types per generic via 'addConcreteFlow'.

4. A /growing cycle/ is a directed cycle that contains at least one constructor edge.
   Such a cycle implies an infinite growth (e.g. @a = List a = List (List a) = ...@),
   so the system is unsatisfiable (or would require recursive types / μ-types,
   which we currently do not model). If any growing cycle exists, we return @Nothing@.

5. If there is no growing cycle:
   - SCCs (strongly connected components) formed only by non-constructor edges represent
     generics that must share the same solution set.
   - We compute SCCs, process them in a topological order, and propagate/instantiate
     types along incoming edges.

-------------------------------------------------------------------------------

Graph model
===========

Nodes:
  - Each generic variable is a node identified by 'GenericID'.

Edges:
  - @u -> v@ (plain edge): solutions(u) ⊆ solutions(v)
  - @u -(t)-> v@ (ctor edge): instantiate template @t@ using solutions of generics referenced
    inside @t@ (including possibly @u@ and others), and add those instantiated types to
    solutions(v).

Concrete evidence:
  - A generic can be seeded with concrete types (only if 'isConcrete' holds).

Example (ASCII)
---------------

Suppose we have generics: m0, m1, m2

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

solveGeneric
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
     - Let nodes = all generics in this SCC.
     - baseTypes = union of existing solutions already present for nodes
     - flowTypes = union of contributions from incoming edges from outside the SCC
       * For plain edges: solutions(u)
       * For ctor edges: instantiate(template) using solutions of generics appearing in template
     - allTypes = nub(baseTypes ++ flowTypes)
     - Write allTypes as the solution set for each node in SCC.

Instantiation details
---------------------

instantiate template t:
  - collectVars t = all GenericID occurring in t (as TypeGeneric nodes)
  - for each collected generic v, look up its solution set sol[v] (possibly empty)
  - generateAssignments builds the Cartesian product of choices for all vars
  - substituteGeneric uses each assignment to produce a fully instantiated Type

Note: if some generic in template has no solutions yet, it contributes no assignments and thus
produces no instantiated results. This naturally delays ctor instantiation until prerequisites
arrive from earlier SCCs.

-------------------------------------------------------------------------------

Performance notes
=================

- We store generic vars in a Seq inside an IORef for append-only allocation.
- Each GenericVar stores its outgoing edges in a mutable HashTable.
- We use a HashTable for solution maps as well.
- SCC computation uses Data.Graph over an adjacency list snapshot (O(V+E)).
- Instantiation can blow up combinatorially due to Cartesian products; this is intended
  because the solver returns *all* possible fully-instantiated types for codegen.

-------------------------------------------------------------------------------
-}
module Reussir.Core.Generic where

import Control.Monad (forM, forM_, when)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence qualified as Seq
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (modifyIORef', newIORef', readIORef')
import Reussir.Core.Type (isConcrete, substituteGeneric)
import Reussir.Core.Types.Generic
import Reussir.Core.Types.GenericID
import Reussir.Core.Types.Type (Type (..))
import Reussir.Parser.Types.Lexer (Identifier, Path)

{- |
Create an empty 'GenericState'.

This allocates:
  - an IORef holding a Seq of all allocated 'GenericVar' records
  - a hash table for 'concreteFlow' seeding

Allocation model
----------------
Generic vars are allocated by appending to a Seq. Their 'GenericID' is the index in that Seq
at allocation time. This means IDs are stable for the lifetime of the GenericState.
-}
emptyGenericState :: (IOE :> es, Prim :> es) => Eff es GenericState
emptyGenericState = GenericState <$> newIORef' Seq.empty <*> liftIO H.new

{- |
Allocate a new generic variable and append it to the global sequence.

Parameters:
  * genericName   : pretty/debug name
  * genericSpan   : (start,end) span in source, if available
  * genericBounds : a list of paths used as bounds/constraints (domain-specific)
  * state      : global state

Returns:
  A fresh 'GenericID' corresponding to its index in the internal Seq.

Notes:
  Each generic var has its own mutable hash table of outgoing edges ('genericLinks').
-}
newGenericVar ::
    (IOE :> es, Prim :> es) =>
    Identifier ->
    Maybe (Int64, Int64) ->
    [Path] ->
    GenericState ->
    Eff es GenericID
newGenericVar genericName genericSpan genericBounds state = do
    links <- liftIO H.new
    let var = GenericVar{genericName, genericSpan, genericLinks = links, genericBounds}
        ref = getStateRef state
    varID <- fromIntegral . Seq.length <$> readIORef' ref
    modifyIORef' ref (Seq.|> var)
    pure $ GenericID varID

{- |
Add an edge @src -> tgt@ into the generic graph.

If @mType@ is:
  * Nothing    : plain flow edge
  * Just tyTpl : constructor edge carrying a template type

Semantics:
  - plain: solutions(src) flow into solutions(tgt)
  - ctor : instantiate tyTpl with current solutions for generics occurring inside tyTpl,
           and the instantiated results flow into solutions(tgt)

Implementation:
  Outgoing edges are stored in 'genericLinks' of the source node.
-}
addLink ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    GenericID ->
    Maybe Type ->
    GenericState ->
    Eff es ()
addLink (GenericID srcID) (GenericID tgtID) mType state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral srcID)
    liftIO $ H.insert (genericLinks var) (GenericID tgtID) mType

{- |
Add a plain flow edge @src -> tgt@.
-}
addDirectLink ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    GenericID ->
    GenericState ->
    Eff es ()
addDirectLink src tgt state = addLink src tgt Nothing state

{- |
Add a constructor edge @src -(tyTpl)-> tgt@.
-}
addCtorLink ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    GenericID ->
    Type ->
    GenericState ->
    Eff es ()
addCtorLink src tgt mType state = addLink src tgt (Just mType) state

{- |
Get the human-friendly name of a generic variable.
-}
getName ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    GenericState ->
    Eff es Identifier
getName (GenericID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ genericName var

{- |
Get the source span of a generic variable, if any.
-}
getSpan ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    GenericState ->
    Eff es (Maybe (Int64, Int64))
getSpan (GenericID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ genericSpan var

{- |
Seed a generic variable with a concrete type.

This only inserts the type if 'isConcrete ty' holds.
If the generic has an existing list in 'concreteFlow', we cons onto it.

This is an *input* to the solver: it represents "we already learned that this generic
can be (or must be) this concrete type".

Note:
  We store a list of concrete types per generic, allowing multiple candidates.
-}
addConcreteFlow ::
    (IOE :> es, Prim :> es) =>
    GenericID ->
    Type ->
    GenericState ->
    Eff es ()
addConcreteFlow genericID ty state = do
    when (isConcrete ty) $ do
        let table = concreteFlow state
        existing <- liftIO $ H.lookup table genericID
        case existing of
            Nothing -> liftIO $ H.insert table genericID [ty]
            Just tys -> liftIO $ H.insert table genericID (ty : tys)

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
If you can return to the same generic through a path that includes a constructor edge,
you can keep growing types indefinitely:

  a  ->  ...  ->  a
  where some edge is (List _), (Pair _ _), (Arrow _ _), ...

This is the classic "occurs check" style failure in unification, generalized to a graph
with multiple generics.

What is returned
----------------
The returned triple @(src, tgt, mCtor)@ is always an edge that has a constructor
(i.e. @mCtor == Just ...@) and lies on some directed cycle.

Algorithm sketch (DFS + latest ctor edge)
-----------------------------------------
We run DFS over all nodes, maintaining:
  - visited map: GenericID -> VisitState (Visiting depth | Done)
  - depth: current DFS depth
  - latestCtorDepth: depth at which we last traversed a ctor edge on the current recursion path
  - latestCtorEdge: that edge (u,v,Just t)

When exploring edge (u -> v):
  - If v is Visiting(vDepth), then u->v is a back edge forming a cycle.
    * If current edge is ctor, it is a growing cycle.
    * Else, if latestCtorDepth > vDepth, then latestCtorEdge lies within the cycle segment.
-}
type Cycle = (GenericID, GenericID, Maybe Type)

detectGrowingCycles ::
    (IOE :> es, Prim :> es) =>
    GenericState ->
    Eff es (Maybe (GenericID, GenericID, Maybe Type))
detectGrowingCycles state = do
    vars <- readIORef' (getStateRef state)
    let nodes = [GenericID (fromIntegral i) | i <- [0 .. Seq.length vars - 1]]

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
            let var = Seq.index vars (fromIntegral (case u of GenericID i -> i))

            edgesList <- liftIO $ H.toList (genericLinks var)

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

{- | Solves the generic variable constraints and returns a map @GenericID -> [Type]@.

Return value:
  - @Nothing@ if a growing cycle is detected.
  - @Just table@ where table maps each generic to a list of possible fully instantiated types.

High-level procedure
--------------------
1. Reject if there is a growing cycle (see 'detectGrowingCycles').

2. Otherwise:
   - Compute SCCs in the generic graph.
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
  - We instantiate @tTpl@ by substituting each generic appearing in @tTpl@ with each of its
    current solutions (cartesian product).
  - Each instantiated result is added to solutions(v) (and thus to the SCC’s shared set).

This is why the solver returns *lists* of types: it explicitly enumerates alternatives.

Caveat:
  Instantiation may be large (cartesian product). That's intended for codegen enumeration.
-}
type GenericSolution = H.CuckooHashTable GenericID [Type]

solveGeneric ::
    (IOE :> es, Prim :> es) =>
    GenericState ->
    Eff es (Either GenericSolution Cycle)
solveGeneric state = do
    cycle' <- detectGrowingCycles state
    case cycle' of
        Just edge -> pure $ Right edge
        Nothing -> do
            sol <- runSolve
            pure $ Left sol
  where
    runSolve = do
        vars <- readIORef' (getStateRef state)
        let allNodes = [GenericID (fromIntegral i) | i <- [0 .. Seq.length vars - 1]]

        -- Build adjacency list for SCC
        -- stronglyConnComp expects triples (node, key, [key]).
        adjList <- forM allNodes $ \u -> do
            let var = Seq.index vars (fromIntegral (case u of GenericID i -> i))
            edges <- liftIO $ H.toList (genericLinks var)
            let neighbors = map fst edges
            pure (u, u, neighbors)

        let sccs = stronglyConnComp adjList
        let topo = reverse sccs

        -- Build reverse graph: for each v, store incoming edges (u, ctorAnn)
        incomingEdges <- liftIO (H.new :: IO (H.CuckooHashTable GenericID [(GenericID, Maybe Type)]))
        forM_ allNodes $ \u -> do
            let var = Seq.index vars (fromIntegral (case u of GenericID i -> i))
            edges <- liftIO $ H.toList (genericLinks var)
            forM_ edges $ \(v, ctor) -> do
                prev <- liftIO $ H.lookup incomingEdges v
                let new = (u, ctor) : fromMaybe [] prev
                liftIO $ H.insert incomingEdges v new

        -- Initialize solution with concrete flow
        currentSol <- liftIO (H.new :: IO (H.CuckooHashTable GenericID [Type]))
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
    --        * ctor edge : instantiate template with solutions of generics appearing in it
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
    --    Instantiate a template type by enumerating solutions for all generics occurring inside it.
    --
    --    Steps:
    --      1) collectVars t: list all GenericID referenced as TypeGeneric in t
    --      2) generateAssignments: cartesian product picking one concrete type per generic
    --      3) substituteGeneric: apply assignment env to t, producing fully instantiated types
    --
    instantiate t sol = do
        let vars = nub $ collectVars t
        assignments <- generateAssignments vars sol
        pure $ map (\env -> substituteGeneric t (\m -> Map.lookup m env)) assignments

    -- Collect all generics referenced in a Type tree.
    collectVars (TypeGeneric m) = [m]
    collectVars (TypeRecord _ args) = concatMap collectVars args
    collectVars (TypeClosure args ret) = concatMap collectVars args ++ collectVars ret
    collectVars (TypeRc t _) = collectVars t
    collectVars (TypeRef t _) = collectVars t
    collectVars _ = []

    -- \|
    --    Enumerate all environments (GenericID -> Type) for the given generic variables.
    --
    --    If some generic has no known solution yet (lookup returns Nothing or empty list),
    --    it yields no assignments and thus blocks instantiation (returns []).
    generateAssignments [] _ = pure [Map.empty]
    generateAssignments (v : vs) sol = do
        rest <- generateAssignments vs sol
        options <- liftIO $ H.lookup sol v
        let opts = fromMaybe [] options
        pure [Map.insert v opt r | opt <- opts, r <- rest]
