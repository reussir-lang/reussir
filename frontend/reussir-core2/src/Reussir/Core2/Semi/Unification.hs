module Reussir.Core2.Semi.Unification where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Prim.IORef.Strict (IORef', Prim, newIORef', readIORef', writeIORef')
import Effectful.Reader.Static (Reader, ask)
import Reussir.Core2.Class (meetBound)
import Reussir.Core2.Types.Class (ClassDAG, TypeBound)
import Reussir.Core2.Types.Semi.Type (HoleID (..), Type (..))
import Reussir.Core2.Types.Semi.Unification (HoleState (..), HoleTable (..), UnificationState (..))

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

unifyTwoHoles :: (IOE :> es, Prim :> es, Reader ClassDAG :> es, Reader HoleTable :> es) => HoleID -> HoleID -> Eff es ()
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
                        (unifState1', unifState1, unifRnk1, rootID2, unifState2', unifState2, unifRnk2)
                    else
                        (unifState2', unifState2, unifRnk2, rootID1, unifState1', unifState1, unifRnk1)
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
        UFNode{} -> error "unreachable: findHoleUnifState should have returned root"
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
