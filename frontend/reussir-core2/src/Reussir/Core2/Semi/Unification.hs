{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Semi.Unification where

import Control.Monad (when, zipWithM)
import Data.HashSet qualified as HashSet
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Prim.IORef.Strict (
    IORef',
    Prim,
    newIORef',
    readIORef',
    writeIORef',
 )
import Effectful.Reader.Static (ask)
import Reussir.Core2.Class (isSuperClass, meetBound, subsumeBound)
import Reussir.Core2.Data (Flexivity (Flex, Rigid), GenericVar (..))
import Reussir.Core2.Data.Class (Class (..), TypeBound)
import Reussir.Core2.Data.Generic (GenericState (..))
import Reussir.Core2.Data.Semi.Type (Type (..))
import Reussir.Core2.Data.Semi.Unification (
    ErrorKind (..),
    Failure (..),
    HoleState (..),
    HoleTable (..),
    UnificationEff,
    UnificationState (..),
 )
import Reussir.Core2.Data.UniqueID (GenericID (..), HoleID (..))
import Reussir.Core2.Semi.Type (getClassesOfType)
import Reussir.Diagnostic (Report (..))
import Reussir.Diagnostic.Report (
    addBoldToText,
    defaultCodeRef,
    defaultText,
 )

newHoleTable :: (Prim :> es) => Eff es HoleTable
newHoleTable = HoleTable <$> newIORef' mempty

introduceNewHole ::
    Maybe T.Text ->
    Maybe (Int64, Int64) ->
    TypeBound ->
    UnificationEff Type
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
    HoleID ->
    UnificationEff (HoleID, IORef' UnificationState)
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
    HoleID -> HoleID -> UnificationEff ()
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

force :: Type -> UnificationEff Type
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
force (TypeRecord path args flex) = do
    args' <- mapM force args
    return $ TypeRecord path args' flex
force (TypeClosure args ret) =
    TypeClosure <$> mapM force args <*> force ret
force TypeBottom = return TypeBottom
force g@(TypeGeneric _) = return g
force TypeUnit = return TypeUnit
force TypeStr = return TypeStr
force TypeBool = return TypeBool
force int@(TypeIntegral _) = return int
force fp@(TypeFP _) = return fp
force (TypeNullable t) = TypeNullable <$> force t

getHoleState :: HoleID -> UnificationEff HoleState
getHoleState (HoleID idx) = do
    table <- ask
    holesSeq <- readIORef' $ holes table
    return $ Seq.index holesSeq idx

getGenericBound :: GenericID -> UnificationEff TypeBound
getGenericBound (GenericID gid) = do
    genericsState <- ask
    varState <- readIORef' (getStateRef genericsState)
    case Seq.lookup (fromIntegral gid) varState of
        Just var -> return $ map Class $ genericBounds var
        Nothing -> error "unreachable: getGenericBound called on non-existent generic"

exactTypeSatisfyBounds :: Type -> [Class] -> UnificationEff Bool
exactTypeSatisfyBounds ty bounds = do
    tyClassTable <- ask
    dag <- ask
    tyClasses <- getClassesOfType tyClassTable ty
    let candidates = HashSet.toList tyClasses

    let checkBound b = do
            -- Check if any candidate 'c' satisfies 'isSuperClass dag b c'
            -- isSuperClass returns Eff es Bool
            results <- mapM (isSuperClass dag b) candidates
            return $ or results

    boundChecks <- mapM checkBound bounds
    return $ and boundChecks

satisfyBounds ::
    Type -> TypeBound -> UnificationEff Bool
satisfyBounds ty bnds = do
    case ty of
        TypeHole hID -> do
            (_, unifState) <- findHoleUnifState hID
            unifState' <- readIORef' unifState
            case unifState' of
                UnSolvedUFRoot _ bnds' -> do
                    dag <- ask
                    subsumeBound dag bnds' bnds
                SolvedUFRoot _ tySolved -> do
                    forced <- force tySolved
                    satisfyBounds forced bnds
                UFNode{} -> error "unreachable: cannot be non-root here"
        TypeGeneric gID -> do
            bounds <- getGenericBound gID
            dag <- ask
            subsumeBound dag bounds bnds
        x -> exactTypeSatisfyBounds x bnds

typeHasExactBound :: Type -> Class -> UnificationEff Bool
typeHasExactBound ty bound = do
    tyClassTable <- ask
    tyClasses <- getClassesOfType tyClassTable ty
    return $ HashSet.member bound tyClasses

-- unification
unify ::
    Type -> Type -> UnificationEff (Maybe Failure)
unify ty1 ty2 = do
    ty1' <- force ty1
    ty2' <- force ty2
    unifyForced ty1' ty2'
  where
    failMismatch t1 t2 ctx =
        return $
            Just $
                Failure
                    { errorKind = URMisMatchedType t1 t2
                    , unificationContext = ctx
                    , innerFailures = []
                    }

    failMismatchNested t1 t2 ctx inner =
        return $
            Just $
                Failure
                    { errorKind = URMisMatchedType t1 t2
                    , unificationContext = ctx
                    , innerFailures = inner
                    }

    unifyForced (TypeHole hID1) (TypeHole hID2) = do
        unifyTwoHoles hID1 hID2
        -- TODO: may be we should detect some trivial error here. e.g.
        -- Integral and FloatingPoint bounds cannot be satisfied in the same time
        pure Nothing
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
                    then return Nothing
                    else do
                        holeState <- getHoleState hID
                        return $
                            Just $
                                Failure
                                    { errorKind =
                                        URMisMatchedBounds
                                            { candidateType = ty
                                            , candidateBounds = bnds
                                            , candidateHoleSpan = holeSpan holeState
                                            , candidateHoleName = holeName holeState
                                            }
                                    , unificationContext = "Type does not satisfy bounds"
                                    , innerFailures = []
                                    }
            _ -> error "unreachable: cannot be solved or non-root here"
    unifyForced t1 t2@(TypeHole _) = unifyForced t2 t1
    -- flexivity is irrelevant for unification, the type inferrence carries flexivity towards top level
    unifyForced t1@(TypeRecord path1 args1 flex1) t2@(TypeRecord path2 args2 flex2)
        | path1 == path2 && length args1 == length args2 = do
            results <- zipWithM unify args1 args2
            let flexCannotMatch = (flex1 == Flex && flex2 == Rigid) || (flex1 == Rigid && flex2 == Flex)
            let failures = catMaybes results
            if null failures
                then
                    if flexCannotMatch
                        then
                            pure $
                                Just $
                                    Failure
                                        { errorKind = URMisMatchedType t1 t2
                                        , unificationContext = "failed to unify record components"
                                        , innerFailures = []
                                        }
                        else pure Nothing
                else failMismatchNested t1 t2 "failed to unify record components" failures
        | otherwise =
            failMismatch t1 t2 $
                "failed to unify record path " <> T.pack (show path1) <> " with " <> T.pack (show path2)
    unifyForced TypeBool TypeBool = return Nothing
    unifyForced TypeStr TypeStr = return Nothing
    unifyForced TypeUnit TypeUnit = return Nothing
    unifyForced t1@(TypeIntegral it1) t2@(TypeIntegral it2) =
        if it1 == it2
            then return Nothing
            else failMismatch t1 t2 "failed to unify integral types"
    unifyForced t1@(TypeFP fpt1) t2@(TypeFP fpt2) =
        if fpt1 == fpt2
            then return Nothing
            else failMismatch t1 t2 "failed to unify floating point types"
    -- TODO: covariance/contravariance?
    unifyForced t1@(TypeClosure args1 ret1) t2@(TypeClosure args2 ret2)
        | length args1 == length args2 = do
            argsResults <- zipWithM unify args1 args2
            retResult <- unify ret1 ret2
            let failures = catMaybes (retResult : argsResults)
            if null failures
                then return Nothing
                else failMismatchNested t1 t2 "failed to unify closure types" failures
        | otherwise = failMismatch t1 t2 "failed to unify closure types with different arity"
    -- auto coercion from bottom
    unifyForced TypeBottom _ = return Nothing
    unifyForced _ TypeBottom = return Nothing
    unifyForced t1@(TypeGeneric g1) t2@(TypeGeneric g2) =
        if g1 == g2
            then return Nothing
            else
                failMismatch t1 t2 $
                    "failed to unify generic types " <> T.pack (show g1) <> " with " <> T.pack (show g2)
    unifyForced (TypeNullable t1) (TypeNullable t2) = do
        failure <- unify t1 t2
        case failure of
            Just e -> return $ Just e
            Nothing -> return Nothing
    unifyForced t1 t2 = failMismatch t1 t2 "Cannot unify two types with different kind"

errorToReport :: Failure -> FilePath -> Report
errorToReport failure path = go True failure
  where
    go isTop f =
        let headReport = renderFailure f path
            children = foldMap (go False) (innerFailures f)
         in if isTop && not (null (innerFailures f))
                then headReport <> Nested children
                else headReport <> children

    renderFailure f p =
        let contextMsg = FormattedText [defaultText (unificationContext f <> ": ")]
            kindReport = case errorKind f of
                URMisMatchedType t1 t2 ->
                    FormattedText
                        [ defaultText "Expected: "
                        , addBoldToText $ defaultText (T.pack $ show t1)
                        , defaultText ", Actual: "
                        , addBoldToText $ defaultText (T.pack $ show t2)
                        ]
                URMisMatchedBounds ty bnds hSpan name ->
                    let msg =
                            FormattedText
                                [ defaultText "Type "
                                , addBoldToText $ defaultText (T.pack $ show ty)
                                , defaultText " does not satisfy bounds "
                                , addBoldToText $ defaultText (T.pack $ show bnds)
                                ]
                        loc = case hSpan of
                            Just (s, e) ->
                                let hint = FormattedText [defaultText "The meta hole was introduced at the following place"]
                                    ref = CodeRef (defaultCodeRef p s e) (fmap defaultText name)
                                 in hint <> Nested ref
                            Nothing -> mempty
                     in msg <> loc
         in contextMsg <> kindReport
