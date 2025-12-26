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

emptyMetaState :: (IOE :> es, Prim :> es) => Eff es MetaState
emptyMetaState = MetaState <$> newIORef' Seq.empty <*> liftIO H.new

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

addDirectLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaID ->
    MetaState ->
    Eff es ()
addDirectLink src tgt state = addLink src tgt Nothing state

addCtorLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaID ->
    Type ->
    MetaState ->
    Eff es ()
addCtorLink src tgt mType state = addLink src tgt (Just mType) state

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

-- Check if a type is concrete
-- If it is, add it to the list of concrete types for the given MetaID
-- Create empty list if none exists prior to the addition
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

data VisitState
    = Visiting {-# UNPACK #-} !Int
    | Done
    deriving (Show)

{- | Detects a cycle in the graph that contains at least one edge with a constructor.
If such a cycle exists, returns one edge with a constructor that is part of the cycle.

The algorithm uses Depth First Search (DFS) to traverse the graph.
It maintains the state of each node (Visiting or Done) to detect cycles.
Additionally, it tracks the "latest constructor edge" encountered on the current path.

When a back edge (u -> v) is found (indicating a cycle):
1. If the back edge itself has a constructor, we found a growing cycle. Return this edge.
2. If not, we check if there was any constructor edge on the path from v to u.
   We do this by comparing the depth of the latest constructor edge with the depth of v.
   If latestCtorDepth > depth(v), then the constructor edge is within the cycle.
   Return it.

Complexity: O(V + E) time and space, where V is vertices and E is edges.
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

{- | Solves the meta variable constraints.

1. No solution if there is a growing cycle.
2. Otherwise, we can construct a solution by assigning basic types to given meta vars.
3. Since there is no growing cycle, if a cycle ever exists, it just mean that
   the meta vars in the SCC have the same solution.
4. Hence we construct the topological order of SCCs and solve them in order.
5. If x flows into y, then x's solutions are also y's solutions.
6. The returned solution should map meta to all fully instantiated types.
   That is, all metas in the body should be substituted with their solutions.
   Multiple subsitutions are possible, we return all of them as this solution
   is used to compute all types that out to be emitted for codegen.
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
        adjList <- forM allNodes $ \u -> do
            let var = Seq.index vars (fromIntegral (case u of MetaID i -> i))
            edges <- liftIO $ H.toList (metaLinks var)
            let neighbors = map fst edges
            pure (u, u, neighbors)

        let sccs = stronglyConnComp adjList
        let topo = reverse sccs

        -- Build reverse graph
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

                -- 1. Existing types (already in currentSol)
                baseTypes <- fmap concat $ forM nodes $ \n -> do
                    res <- liftIO $ H.lookup currentSol n
                    pure $ fromMaybe [] res

                -- 2. Types from incoming edges
                flowTypes <- fmap concat $ forM nodes $ \n -> getIncomingTypes currentSol incomingEdges nodes n

                let allTypes = nub (baseTypes ++ flowTypes)

                forM_ nodes $ \n -> liftIO $ H.insert currentSol n allTypes

        forM_ topo processSCC
        pure currentSol

    getIncomingTypes sol incomingEdges sccNodes node = do
        edges <- liftIO $ H.lookup incomingEdges node
        case edges of
            Nothing -> pure []
            Just es -> fmap concat $ forM es $ \(u, ctor) -> resolveEdge sol sccNodes u ctor

    resolveEdge sol sccNodes u ctor = do
        if u `elem` sccNodes
            then pure []
            else case ctor of
                Nothing -> do
                    res <- liftIO $ H.lookup sol u
                    pure $ fromMaybe [] res
                Just t -> instantiate t sol

    instantiate t sol = do
        let vars = nub $ collectVars t
        assignments <- generateAssignments vars sol
        pure $ map (\env -> substituteMeta t (\m -> Map.lookup m env)) assignments

    collectVars (TypeMeta m) = [m]
    collectVars (TypeExpr _ args) = concatMap collectVars args
    collectVars (TypeArrow t1 t2) = collectVars t1 ++ collectVars t2
    collectVars _ = []

    generateAssignments [] _ = pure [Map.empty]
    generateAssignments (v : vs) sol = do
        rest <- generateAssignments vs sol
        options <- liftIO $ H.lookup sol v
        let opts = fromMaybe [] options
        pure [Map.insert v opt r | opt <- opts, r <- rest]
