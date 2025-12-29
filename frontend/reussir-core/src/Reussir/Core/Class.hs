module Reussir.Core.Class where

import Control.Monad (filterM, forM, forM_, when)
import Data.Array (Array, listArray, (!))
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.IntSet qualified as IntSet
import Data.List (maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Effectful (Eff, IOE, MonadIO (liftIO), subsume, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (modifyIORef', newIORef', readIORef', writeIORef')
import Reussir.Core.Types.Class (Class, ClassDAG (..), ClassNode (..), TypeBound)

newDAG :: (IOE :> es, Prim :> es) => Eff es ClassDAG
newDAG = do
    nm <- liftIO H.new
    im <- liftIO H.new
    nmap <- liftIO H.new
    chm <- liftIO H.new
    cnt <- newIORef' 0
    fg <- newIORef' Nothing
    pure $
        ClassDAG
            { nameMap = nm
            , idMap = im
            , nodeMap = nmap
            , chainHeadMap = chm
            , counter = cnt
            , finalizedGraph = fg
            }

getClassID :: (IOE :> es, Prim :> es) => ClassDAG -> Class -> Eff es Int
getClassID dag cls = do
    maybeID <- liftIO $ H.lookup (nameMap dag) cls
    case maybeID of
        Just i -> pure i
        Nothing -> do
            i <- do
                c <- readIORef' (counter dag)
                modifyIORef' (counter dag) (+ 1)
                pure c
            liftIO $ H.insert (nameMap dag) cls i
            liftIO $ H.insert (idMap dag) i cls
            pure i

addClass ::
    (IOE :> es, Prim :> es) =>
    Class ->
    [Class] ->
    ClassDAG ->
    Eff es ()
addClass cls parentCls dag = do
    clsID <- getClassID dag cls
    parentIDs <- mapM (getClassID dag) parentCls
    let node = ClassNode{chain = (-1, -1), parents = parentIDs}
    liftIO $ H.insert (nodeMap dag) clsID node

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
    (IOE :> es, Prim :> es) =>
    ClassDAG ->
    Eff es ()
populateDAG dag = do
    entries <- liftIO $ H.toList (nodeMap dag)

    -- 1. Build Spanning Forest (Parent -> Children)
    -- We pick the first parent as the tree parent.
    let (childrenMap, roots) = foldr buildTree (Map.empty, []) entries
        buildTree (cid, node) (cm, rs) =
            case listToMaybe (parents node) of
                Nothing -> (cm, cid : rs)
                Just p -> (Map.insertWith (++) p [cid] cm, rs)

    -- 2. Compute Subtree Sizes
    let buildSizeMap cid m =
            let children = Map.findWithDefault [] cid childrenMap
                (s, m') =
                    foldr
                        ( \c (accSize, accMap) ->
                            let (cSize, m'') = buildSizeMap c accMap
                             in (accSize + cSize, m'')
                        )
                        (0, m)
                        children
                mySize = (1 :: Int) + s
             in (mySize, Map.insert cid mySize m')

    let sizeMap = foldr (\r m -> snd (buildSizeMap r m)) Map.empty roots

    -- 3. Decompose into Chains
    chainIDCounter <- newIORef' (0 :: Int64)

    let decompose cid currentChainID currentPos = do
            -- Update node in DAG
            maybeNode <- liftIO $ H.lookup (nodeMap dag) cid
            case maybeNode of
                Nothing -> pure ()
                Just node -> do
                    let newNode = node{chain = (currentChainID, currentPos)}
                    liftIO $ H.insert (nodeMap dag) cid newNode

            -- If head of chain, update chainHead
            when (currentPos == 0) $ do
                liftIO $ H.insert (chainHeadMap dag) currentChainID cid

            let children = Map.findWithDefault [] cid childrenMap
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
                            newID <- do
                                modifyIORef' chainIDCounter (+ 1)
                                readIORef' chainIDCounter
                            decompose c newID 0

    -- Run decomposition from roots
    forM_ roots $ \r -> do
        newID <- do
            modifyIORef' chainIDCounter (+ 1)
            readIORef' chainIDCounter
        decompose r newID 0

    -- 4. Finalize Graph into Array
    maxID <- readIORef' (counter dag)
    -- Note: maxID is the count, so IDs are 0 .. maxID-1
    if maxID == 0
        then writeIORef' (finalizedGraph dag) (Just (listArray (0, -1) []))
        else do
            -- We need to construct the array.
            -- We iterate 0..maxID-1 and lookup in nodeMap.
            -- If a node is missing (e.g. referenced but not defined), we need a placeholder or error.
            -- Assuming all referenced classes are defined or we handle it.
            -- If a class ID exists (assigned in getClassID), but addClass was never called for it,
            -- it won't be in nodeMap.
            -- We should probably insert a dummy node or handle it.
            -- Let's insert a dummy node for missing IDs to avoid crash.
            nodes <- forM [0 .. maxID - 1] $ \i -> do
                maybeNode <- liftIO $ H.lookup (nodeMap dag) i
                case maybeNode of
                    Just n -> pure n
                    Nothing -> pure $ ClassNode (-1, -1) [] -- Dummy node
            let arr = listArray (0, maxID - 1) nodes
            writeIORef' (finalizedGraph dag) (Just arr)

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
    (IOE :> es, Prim :> es) =>
    ClassDAG ->
    Class ->
    Class ->
    Eff es Bool
isSuperClass dag parent child = do
    maybeParentID <- liftIO $ H.lookup (nameMap dag) parent
    maybeChildID <- liftIO $ H.lookup (nameMap dag) child

    case (maybeParentID, maybeChildID) of
        (Just parentID, Just childID) -> do
            maybeArr <- readIORef' (finalizedGraph dag)
            case maybeArr of
                Just arr -> pure $ isSuperClassFast arr parentID childID
                Nothing -> isSuperClassSlow dag parentID childID
        _ -> pure False

isSuperClassFast :: Array Int ClassNode -> Int -> Int -> Bool
isSuperClassFast arr parentID childID = search (Seq.singleton childID) IntSet.empty
  where
    -- We can access array directly.
    -- But we need to be careful about bounds if IDs are somehow out of sync, but they shouldn't be.
    targetNode = arr ! parentID
    (targetChain, targetPos) = chain targetNode

    search Empty _ = False
    search (u :<| worklist) visited
        | u == parentID = True
        | IntSet.member u visited = search worklist visited
        | otherwise =
            let node = arr ! u
                (uChain, uPos) = chain node
             in if uChain == targetChain
                    then
                        if targetPos <= uPos
                            then True
                            else search worklist (IntSet.insert u visited)
                    else
                        let ps = parents node
                            worklist' = foldr (<|) worklist ps
                         in search worklist' (IntSet.insert u visited)

isSuperClassSlow :: (IOE :> es) => ClassDAG -> Int -> Int -> Eff es Bool
isSuperClassSlow dag parentID childID = do
    maybeParentNode <- liftIO $ H.lookup (nodeMap dag) parentID
    case maybeParentNode of
        Nothing -> pure False
        Just parentNode -> do
            let (targetChain, targetPos) = chain parentNode
            let search Empty _ = pure False
                search (u :<| worklist) visited
                    | u == parentID = pure True
                    | IntSet.member u visited = search worklist visited
                    | otherwise = do
                        maybeNode <- liftIO $ H.lookup (nodeMap dag) u
                        case maybeNode of
                            Nothing -> search worklist (IntSet.insert u visited)
                            Just node -> do
                                let (uChain, uPos) = chain node
                                if uChain == targetChain
                                    then
                                        if targetPos <= uPos
                                            then pure True
                                            else search worklist (IntSet.insert u visited)
                                    else do
                                        let ps = parents node
                                        let worklist' = foldr (<|) worklist ps
                                        search worklist' (IntSet.insert u visited)
            search (Seq.singleton childID) IntSet.empty

meetClass :: (IOE :> es, Prim :> es) => ClassDAG -> Class -> Class -> Eff es TypeBound
meetClass dag c1 c2 = do
    sup12 <- isSuperClass dag c1 c2
    if sup12
        then pure [c2]
        else do
            sup21 <- isSuperClass dag c2 c1
            if sup21
                then pure [c1]
                else pure [c1, c2]

meetBound :: (IOE :> es, Prim :> es) => ClassDAG -> TypeBound -> TypeBound -> Eff es TypeBound
meetBound dag b1 b2 = do
    let candidates = Set.toList $ Set.fromList (b1 ++ b2)
    filterM
        ( \c -> do
            -- Keep c if it is NOT a superclass of any other candidate c'
            -- i.e. for all c' != c, not (isSuperClass c c')
            -- Equivalently: not (exists c' != c s.t. isSuperClass c c')
            isRedundant <- existsM (\c' -> if c == c' then pure False else isSuperClass dag c c') candidates
            pure (not isRedundant)
        )
        candidates
  where
    existsM p xs = or <$> mapM p xs

subsumeBound ::
    (IOE :> es, Prim :> es) =>
    ClassDAG ->
    TypeBound ->
    TypeBound ->
    Eff es Bool
subsumeBound dag existing target = do
    results <- forM target $ \t -> do
        existsM (\e -> isSuperClass dag t e) existing
    pure $ and results
  where
    existsM p xs = or <$> mapM p xs
