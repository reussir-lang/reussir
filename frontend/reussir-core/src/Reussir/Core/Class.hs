module Reussir.Core.Class where

import Control.Monad (forM_, when)
import Data.HashTable.IO qualified as H
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Reussir.Core.Types.Class (Class, ClassDAG (..), ClassNode (..))

newDAG :: (IOE :> es) => Eff es ClassDAG
newDAG = do
    chainHeadTable <- liftIO H.new
    classesTable <- liftIO H.new
    pure $
        ClassDAG
            { chainHead = chainHeadTable
            , classes = classesTable
            }

addClass ::
    (IOE :> es) =>
    Class ->
    [Class] ->
    ClassDAG ->
    Eff es ()
addClass cls parentCls dag = do
    let node = ClassNode{chain = (-1, -1), parents = parentCls}
    liftIO $ H.insert (classes dag) cls node

{- |
    Populates the 'ClassDAG' with chain decomposition information.

    This function performs a Heavy-Light Decomposition (HLD) on a spanning forest of the inheritance DAG.

    Note on DAG support:
    Although HLD is typically defined for trees, we apply it here to a spanning forest of the DAG
    (constructed by arbitrarily picking the first parent of each class as its tree parent).
    This decomposition is used purely as an optimization structure to accelerate 'isSuperClass' queries.
    It does not restrict the inheritance relationship to a tree; the 'isSuperClass' function
    correctly handles the full DAG structure by falling back to graph traversal when necessary.

    The "heavy" edges are selected based on the subtree size within the spanning forest.
-}
populateDAG ::
    (IOE :> es) =>
    ClassDAG ->
    Eff es ()
populateDAG dag = do
    entries <- liftIO $ H.toList (classes dag)

    -- 1. Build Spanning Forest (Parent -> Children)
    -- We pick the first parent as the tree parent.
    let (childrenMap, roots) = foldr buildTree (Map.empty, []) entries
        buildTree (cls, node) (cm, rs) =
            case listToMaybe (parents node) of
                Nothing -> (cm, cls : rs)
                Just p -> (Map.insertWith (++) p [cls] cm, rs)

    -- 2. Compute Subtree Sizes
    let buildSizeMap cls m =
            let children = Map.findWithDefault [] cls childrenMap
                (s, m') =
                    foldr
                        ( \c (accSize, accMap) ->
                            let (cSize, m'') = buildSizeMap c accMap
                             in (accSize + cSize, m'')
                        )
                        (0, m)
                        children
                mySize = (1 :: Int) + s
             in (mySize, Map.insert cls mySize m')

    let sizeMap = foldr (\r m -> snd (buildSizeMap r m)) Map.empty roots

    -- 3. Decompose into Chains
    chainIDCounter <- liftIO $ newIORef (0 :: Int64)

    let decompose cls currentChainID currentPos = do
            -- Update node in DAG
            maybeNode <- liftIO $ H.lookup (classes dag) cls
            case maybeNode of
                Nothing -> pure ()
                Just node -> do
                    let newNode = node{chain = (currentChainID, currentPos)}
                    liftIO $ H.insert (classes dag) cls newNode

            -- If head of chain, update chainHead
            when (currentPos == 0) $ do
                liftIO $ H.insert (chainHead dag) currentChainID cls

            let children = Map.findWithDefault [] cls childrenMap
            case children of
                [] -> pure ()
                _ -> do
                    -- Find heavy child
                    let heavyChild = maximumBy (comparing (\c -> Map.findWithDefault 0 c sizeMap)) children

                    -- Continue chain with heavy child
                    decompose heavyChild currentChainID (currentPos + 1)

                    -- Start new chains for other children
                    forM_ children $ \c -> do
                        when (c /= heavyChild) $ do
                            newID <- liftIO $ do
                                modifyIORef' chainIDCounter (+ 1)
                                readIORef chainIDCounter
                            decompose c newID 0

    -- Run decomposition from roots
    forM_ roots $ \r -> do
        newID <- liftIO $ do
            modifyIORef' chainIDCounter (+ 1)
            readIORef chainIDCounter
        decompose r newID 0

{- |
    Checks if a class is a superclass of another class.

    This function uses the chain decomposition computed by 'populateDAG' to optimize the reachability check.
    If the two classes lie on the same chain, the check is performed in O(1) by comparing chain positions.
    Otherwise, a graph traversal is performed, pruning branches that cannot lead to the parent class based on chain information.

    The algorithm correctly handles multiple inheritance (DAG structure) by traversing all parents,
    while using the spanning-forest-based chains to prune the search space where possible.

    @isSuperClass dag parent child@ returns 'True' if @parent@ is an ancestor of @child@ (or equal), 'False' otherwise.
-}
isSuperClass ::
    (IOE :> es) =>
    ClassDAG ->
    Class ->
    Class ->
    Eff es Bool
isSuperClass dag parent child = do
    -- Look up the target parent node.
    -- If it does not exist in the DAG, it cannot be a superclass.
    maybeParentNode <- liftIO $ H.lookup (classes dag) parent
    case maybeParentNode of
        Nothing -> pure False
        Just parentNode -> do
            -- Extract chain metadata of the target parent.
            -- (chain id, position in chain)
            let (targetChain, targetPos) = chain parentNode

            -- DFS search using an explicit worklist (Seq)
            --
            -- worklist : nodes that still need to be explored
            -- visited  : nodes already explored (prevents re-visiting in DAG)
            let search :: (IOE :> es) => Seq Class -> Set.Set Class -> Eff es Bool
                -- No more nodes to explore ⇒ parent not reachable
                search Empty _ = pure False
                -- Pop next node from the front of the worklist
                search (u :<| worklist) visited
                    -- Found the parent directly
                    | u == parent =
                        pure True
                    -- Already visited ⇒ skip to next
                    | Set.member u visited =
                        search worklist visited
                    | otherwise = do
                        maybeNode <- liftIO $ H.lookup (classes dag) u
                        case maybeNode of
                            -- Defensive case: node missing from DAG
                            -- Mark visited and continue
                            Nothing ->
                                search worklist (Set.insert u visited)
                            Just node -> do
                                let (uChain, uPos) = chain node

                                -- FAST PATH:
                                -- If current node lies on the same HLD chain
                                -- as the target parent, we can decide immediately.
                                if uChain == targetChain
                                    then
                                        -- Chain positions increase downward.
                                        -- If parent position ≤ u position,
                                        -- parent must be an ancestor of u.
                                        if targetPos <= uPos
                                            then pure True
                                            -- Otherwise, parent is below u on the chain.
                                            -- Since we only move upward in the DAG,
                                            -- this branch cannot reach parent.
                                            else
                                                search
                                                    worklist
                                                    (Set.insert u visited)
                                    else do
                                        -- GENERAL DAG CASE:
                                        -- We cannot decide based on chain info.
                                        -- Explore all parents of u.
                                        let ps = parents node

                                        -- Push parents to the *front* of the worklist
                                        -- to preserve DFS-like traversal.
                                        --
                                        -- foldr (<|) ensures O(#parents) total cost,
                                        -- avoiding any list append or Seq concatenation.
                                        let worklist' = foldr (<|) worklist ps

                                        search
                                            worklist'
                                            (Set.insert u visited)

            -- Start DFS from the child
            search (Seq.singleton child) Set.empty
