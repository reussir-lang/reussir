{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Semi.Unification where

import Control.Monad (when, zipWithM)
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Prim.IORef.Strict (
    IORef',
    Prim,
    newIORef',
    readIORef',
    writeIORef',
 )
import Effectful.Reader.Static (Reader, ask)
import Reussir.Core2.Class (meetBound)
import Reussir.Core2.Types.Class (ClassDAG, TypeBound)
import Reussir.Core2.Types.Semi.Type (HoleID (..), Type (..))
import Reussir.Core2.Types.Semi.Unification (
    HoleState (..),
    HoleTable (..),
    UnificationState (..),
 )

introduceNewHole ::
    (Prim :> es, Reader HoleTable :> es) =>
    Maybe T.Text ->
    Maybe (Int64, Int64) ->
    TypeBound ->
    Eff es Type
introduceNewHole holeName holeSpan bound = do
    table <- ask
    holes' <- readIORef' $ holes table
    let holeID = fromIntegral $ Seq.length holes'
    holeUnification <- newIORef' $ UnSolvedUFRoot holeID bound
    let holeState = HoleState{holeName, holeSpan, holeUnification}
    writeIORef' (holes table) $ holes' Seq.|> holeState
    return $ TypeHole $ HoleID holeID

clearHoles :: (Prim :> es) => HoleTable -> Eff es ()
clearHoles table = writeIORef' (holes table) mempty

findHoleUnifState ::
    (Prim :> es, Reader HoleTable :> es) =>
    HoleID ->
    Eff es (HoleID, IORef' UnificationState)
findHoleUnifState hID@(HoleID idx) = do
    table <- ask
    holesSeq <- readIORef' $ holes table
    let holeState = Seq.index holesSeq idx
    unifState <- readIORef' (holeUnification holeState)
    case unifState of
        UnSolvedUFRoot{} -> return (hID, holeUnification holeState)
        SolvedUFRoot{} -> return (hID, holeUnification holeState)
        UFNode parentID -> do
            (rootID, rootState) <- findHoleUnifState parentID
            when (parentID /= rootID) $ do
                writeIORef' (holeUnification holeState) (UFNode rootID)
            return (rootID, rootState)

rnkOfUnifState :: UnificationState -> Int
rnkOfUnifState (UnSolvedUFRoot rnk _) = rnk
rnkOfUnifState (SolvedUFRoot rnk _) = rnk
rnkOfUnifState (UFNode _) = error "unreachable: UFNode has no rank"

unifyTwoHoles ::
    (IOE :> es, Prim :> es, Reader ClassDAG :> es, Reader HoleTable :> es) =>
    HoleID -> HoleID -> Eff es ()
unifyTwoHoles hID1 hID2 = do
    (rootID1, unifState1) <- findHoleUnifState hID1
    (rootID2, unifState2) <- findHoleUnifState hID2
    unifState1' <- readIORef' unifState1
    unifState2' <- readIORef' unifState2
    let unifRnk1 = rnkOfUnifState unifState1'
    let unifRnk2 = rnkOfUnifState unifState2'
    let ( minRnkState
            , minRnkStateRef
            , minRnk
            , maxRnkID
            , maxRnkState
            , maxRnkStateRef
            , maxRnk
            ) =
                if unifRnk1 <= unifRnk2
                    then
                        ( unifState1'
                        , unifState1
                        , unifRnk1
                        , rootID2
                        , unifState2'
                        , unifState2
                        , unifRnk2
                        )
                    else
                        ( unifState2'
                        , unifState2
                        , unifRnk2
                        , rootID1
                        , unifState1'
                        , unifState1
                        , unifRnk1
                        )
    case (minRnkState, maxRnkState) of
        (UnSolvedUFRoot _ bnds, UnSolvedUFRoot _ bnds') -> do
            let newRnk =
                    if minRnk == maxRnk
                        then minRnk + 1
                        else minRnk
            dag <- ask
            newBound <- meetBound dag bnds bnds'
            writeIORef' maxRnkStateRef (UnSolvedUFRoot newRnk newBound)
            writeIORef' minRnkStateRef (UFNode maxRnkID)
        _ -> error "unreachable: unifyTwoHoles called on solved holes"

force :: (Prim :> es, Reader HoleTable :> es) => Type -> Eff es Type
force (TypeHole holeID) = do
    (newId, unifState) <- findHoleUnifState holeID
    unifState' <- readIORef' unifState
    case unifState' of
        SolvedUFRoot rk ty -> do
            ty' <- force ty
            writeIORef' unifState (SolvedUFRoot rk ty')
            return ty'
        UnSolvedUFRoot{} -> return $ TypeHole newId
        UFNode{} ->
            error "unreachable: findHoleUnifState should have returned root"
force (TypeRecord path args) = do
    args' <- mapM force args
    return $ TypeRecord path args'
force (TypeClosure args ret) =
    TypeClosure <$> mapM force args <*> force ret
force TypeBottom = return TypeBottom
force g@(TypeGeneric _) = return g
force TypeUnit = return TypeUnit
force TypeStr = return TypeStr
force TypeBool = return TypeBool
force int@(TypeIntegral _) = return int
force fp@(TypeFP _) = return fp

-- unification
unify ::
    (IOE :> es, Prim :> es, Reader HoleTable :> es, Reader ClassDAG :> es) =>
    (Type -> TypeBound -> Eff es Bool) -> Type -> Type -> Eff es [T.Text]
unify satisfyBounds ty1 ty2 = do
    ty1' <- force ty1
    ty2' <- force ty2
    unifyForced ty1' ty2'
  where
    unifyForced (TypeHole hID1) (TypeHole hID2) = do
        unifyTwoHoles hID1 hID2
        -- TODO: may be we should detect some trivial error here. e.g.
        -- Integral and FloatingPoint bounds cannot be satisfied in the same time
        pure []
    unifyForced (TypeHole hID) ty = do
        (_, unifState) <- findHoleUnifState hID
        unifState' <- readIORef' unifState
        case unifState' of
            UnSolvedUFRoot rnk bnds -> do
                -- check if ty satisfies bounds
                -- TODO: for now, we simply check if type has Class, this is not enough
                -- and should be delayed
                isSatisfy <- satisfyBounds ty bnds
                when isSatisfy $
                    writeIORef' unifState (SolvedUFRoot rnk ty)
                if isSatisfy
                    then return []
                    else return ["Type does not satisfy bounds"]
            _ -> error "unreachable: cannot be solved or non-root here"
    unifyForced ty (TypeHole hID) = unifyForced (TypeHole hID) ty
    unifyForced (TypeRecord path1 args1) (TypeRecord path2 args2)
        | path1 == path2 && length args1 == length args2 = do
            results <- zipWithM (unify satisfyBounds) args1 args2
            return $ concat results
        | otherwise = return ["Cannot unify two record types with different path or arity"]
    unifyForced TypeBool TypeBool = return []
    unifyForced TypeStr TypeStr = return []
    unifyForced TypeUnit TypeUnit = return []
    unifyForced (TypeIntegral it1) (TypeIntegral it2) =
        if it1 == it2 then return [] else return ["Cannot unify two integer types with different kind"]
    unifyForced (TypeFP fpt1) (TypeFP fpt2) =
        if fpt1 == fpt2 then return [] else return ["Cannot unify two floating point types with different kind"]
    -- TODO: covariance/contravariance?
    unifyForced (TypeClosure args1 ret1) (TypeClosure args2 ret2)
        | length args1 == length args2 = do
            argsResults <- zipWithM (unify satisfyBounds) args1 args2
            retResult <- unify satisfyBounds ret1 ret2
            return $ concat (retResult : argsResults)
        | otherwise = return ["Cannot unify two closure types with different arity"]
    -- auto coercion from bottom
    unifyForced TypeBottom _ = return []
    unifyForced _ TypeBottom = return []
    unifyForced (TypeGeneric g1) (TypeGeneric g2) =
        if g1 == g2 then return [] else return ["Cannot unify two generic types with different name"]
    unifyForced _ _ = return ["Cannot unify two types with different kind"]
