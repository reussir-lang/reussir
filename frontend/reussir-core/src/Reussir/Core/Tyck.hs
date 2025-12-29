{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Tyck where

import Control.Monad (forM_, when)
import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.Function ((&))
import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (hash))
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (IORef', newIORef', readIORef', writeIORef')
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Intrinsics qualified as Intrinsic
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Core.Class (addClass, isSuperClass, meetBound, newDAG, populateDAG)
import Reussir.Core.Type qualified as Sem
import Reussir.Core.Types.Class (Class (..), ClassDAG, TypeBound)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.String (StringToken, StringUniqifier (..))
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Label (Error), Report (..))
import Reussir.Diagnostic.Report (
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path (Path), WithSpan (..))
import System.Console.ANSI.Types qualified as ANSI

strToToken :: (IOE :> es) => T.Text -> StringUniqifier -> Eff es StringToken
strToToken str (StringUniqifier table) = do
    let h = hash (XXH3 str)
    bucket <- liftIO $ H.lookup table h
    case bucket of
        Just seqs -> case Seq.elemIndexL str seqs of
            Just idx -> return (h, fromIntegral idx)
            Nothing -> do
                let newSeqs = seqs Seq.|> str
                liftIO $ H.insert table h newSeqs
                return (h, fromIntegral (Seq.length newSeqs - 1))
        Nothing -> do
            liftIO $ H.insert table h (Seq.singleton str)
            return (h, 0)

data UnificationState
    = UnSolvedUFRoot {-# UNPACK #-} !Int TypeBound
    | SolvedUFRoot !Int Sem.Type
    | UFNode {-# UNPACK #-} !Sem.HoleID

data HoleState = HoleState
    { holeName :: Maybe T.Text
    , holeSpan :: Maybe (Int64, Int64)
    , holeUnification :: IORef' UnificationState
    }

data TranslationState = TranslationState
    { currentSpan :: Maybe (Int64, Int64)
    , currentFile :: FilePath
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    , typeClassDAG :: ClassDAG
    , typeClassTable :: Sem.TypeClassTable
    , holes :: Seq.Seq HoleState
    }

-- Introduce a new hole into the translation state
introduceNewHole ::
    Maybe T.Text ->
    Maybe (Int64, Int64) ->
    TypeBound ->
    Tyck Sem.Type
introduceNewHole holeName holeSpan bound = do
    st <- State.gets holes
    let holeID = fromIntegral $ Seq.length st
    holeUnification <- newIORef' $ UnSolvedUFRoot holeID bound
    let holeState = HoleState{holeName, holeSpan, holeUnification}
    State.modify $ \s -> s{holes = st Seq.|> holeState}
    return $ Sem.TypeHole $ Sem.HoleID holeID

-- Clear all holes from the translation state, this is used for process a new
-- function
clearHoles :: Tyck ()
clearHoles = do
    State.modify $ \s -> s{holes = mempty}

-- Find unification state of a hole
findHoleUnifState :: Sem.HoleID -> Tyck (Sem.HoleID, IORef' UnificationState)
findHoleUnifState hID@(Sem.HoleID idx) = do
    holesSeq <- State.gets holes
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

-- This function list all variant on purpose to make it easier to add changes later on
force :: Sem.Type -> Tyck Sem.Type
force (Sem.TypeHole holeID) = do
    (newId, unifState) <- findHoleUnifState holeID
    unifState' <- readIORef' unifState
    case unifState' of
        SolvedUFRoot rk ty -> do
            ty' <- force ty
            writeIORef' unifState (SolvedUFRoot rk ty')
            return ty'
        UnSolvedUFRoot{} -> return $ Sem.TypeHole newId
        UFNode{} -> error "unreachable: findHoleUnifState should have returned root"
force (Sem.TypeRecord path args) = do
    args' <- mapM force args
    return $ Sem.TypeRecord path args'
force (Sem.TypeClosure args ret) =
    Sem.TypeClosure <$> mapM force args <*> force ret
force (Sem.TypeRc t cap) = Sem.TypeRc <$> force t <*> pure cap
force (Sem.TypeRef t cap) = Sem.TypeRef <$> force t <*> pure cap
force Sem.TypeBottom = return Sem.TypeBottom
force g@(Sem.TypeGeneric _) = return g
force Sem.TypeUnit = return Sem.TypeUnit
force Sem.TypeStr = return Sem.TypeStr
force Sem.TypeBool = return Sem.TypeBool
force int@(Sem.TypeIntegral _) = return int
force fp@(Sem.TypeFP _) = return fp

rnkOfUnifState :: UnificationState -> Int
rnkOfUnifState (UnSolvedUFRoot rnk _) = rnk
rnkOfUnifState (SolvedUFRoot rnk _) = rnk
rnkOfUnifState (UFNode _) = error "unreachable: UFNode has no rank"

-- This is used after force, so both holes are unsolved
unifyTwoHoles :: Sem.HoleID -> Sem.HoleID -> Tyck ()
unifyTwoHoles hID1 hID2 = do
    (rootID1, unifState1) <- findHoleUnifState hID1
    (rootID2, unifState2) <- findHoleUnifState hID2
    unifState1' <- readIORef' unifState1
    unifState2' <- readIORef' unifState2
    let unifRnk1 = rnkOfUnifState unifState1'
    let unifRnk2 = rnkOfUnifState unifState2'
    let (minRnkState, minRnkStateRef, minRnk, maxRnkID, maxRnkState, maxRnkStateRef, maxRnk) =
            if unifRnk1 <= unifRnk2
                then (unifState1', unifState1, unifRnk1, rootID2, unifState2', unifState2, unifRnk2)
                else (unifState2', unifState2, unifRnk2, rootID1, unifState1', unifState1, unifRnk1)
    case (minRnkState, maxRnkState) of
        (UnSolvedUFRoot _ bnds, UnSolvedUFRoot _ bnds') -> do
            let newRnk =
                    if minRnk == maxRnk
                        then minRnk + 1
                        else minRnk
            dag <- State.gets typeClassDAG
            newBound <- meetBound dag bnds bnds'
            writeIORef' maxRnkStateRef (UnSolvedUFRoot newRnk newBound)
            writeIORef' minRnkStateRef (UFNode maxRnkID)
        _ -> error "unreachable: unifyTwoHoles called on solved holes"

satisfyBounds :: Sem.TypeClassTable -> Sem.Type -> [Class] -> Tyck Bool
satisfyBounds tyClassTable ty bounds = do
    dag <- State.gets typeClassDAG
    tyClasses <- Sem.getClassesOfType tyClassTable ty
    let candidates = HashSet.toList tyClasses

    let checkBound b = do
            -- Check if any candidate 'c' satisfies 'isSuperClass dag b c'
            -- isSuperClass returns Eff es Bool
            results <- mapM (isSuperClass dag b) candidates
            return $ or results

    boundChecks <- mapM checkBound bounds
    return $ and boundChecks

-- unification
unify :: Sem.Type -> Sem.Type -> Tyck Bool
unify ty1 ty2 = do
    ty1' <- force ty1
    ty2' <- force ty2
    unifyForced ty1' ty2'
  where
    unifyForced (Sem.TypeHole hID1) (Sem.TypeHole hID2) = do
        unifyTwoHoles hID1 hID2
        -- TODO: may be we should detect some trivial error here. e.g.
        -- Integral and FloatingPoint bounds cannot be satisfied in the same time
        pure True
    unifyForced (Sem.TypeHole hID) ty = do
        (rootID, unifState) <- findHoleUnifState hID
        unifState' <- readIORef' unifState
        case unifState' of
            UnSolvedUFRoot rnk bnds -> do
                -- check if ty satisfies bounds
                -- TODO: for now, we simply check if type has Class, this is not enough
                -- and should be delayed
                tyClassTable <- State.gets typeClassTable
                isSatisfy <- satisfyBounds tyClassTable ty bnds
                if isSatisfy
                    then do
                        writeIORef' unifState (SolvedUFRoot rnk ty)
                        pure True
                    else pure False
            _ -> error "unreachable: cannot be solved or non-root here"

populatePrimitives :: (IOE :> es, Prim :> es) => Sem.TypeClassTable -> ClassDAG -> Eff es ()
populatePrimitives typeClassTable typeClassDAG = do
    let numClass = Class $ Path "Num" []
    let floatClass = Class $ Path "FloatingPoint" []
    let intClass = Class $ Path "Integral" []
    addClass numClass [] typeClassDAG
    addClass floatClass [numClass] typeClassDAG
    addClass intClass [numClass] typeClassDAG
    populateDAG typeClassDAG

    let fpTypes =
            [ Sem.IEEEFloat 16
            , Sem.IEEEFloat 32
            , Sem.IEEEFloat 64
            , Sem.BFloat16
            , Sem.Float8
            ]
    forM_ fpTypes $ \fp ->
        Sem.addClassToType typeClassTable (Sem.TypeFP fp) floatClass

    let intTypes =
            [ Sem.Signed 8
            , Sem.Signed 16
            , Sem.Signed 32
            , Sem.Signed 64
            , Sem.Unsigned 8
            , Sem.Unsigned 16
            , Sem.Unsigned 32
            , Sem.Unsigned 64
            ]
    forM_ intTypes $ \it ->
        Sem.addClassToType typeClassTable (Sem.TypeIntegral it) intClass

emptyTranslationState :: (IOE :> es, Prim :> es) => FilePath -> Eff es TranslationState
emptyTranslationState currentFile = do
    table <- liftIO $ H.new
    let stringUniqifier = StringUniqifier table
    typeClassDAG <- newDAG
    typeClassTable <- Sem.emptyTypeClassTable
    populatePrimitives typeClassTable typeClassDAG
    return $
        TranslationState
            { currentSpan = Nothing
            , currentFile
            , stringUniqifier
            , translationReports = []
            , typeClassDAG
            , typeClassTable
            , holes = mempty
            }

type Tyck = Eff '[IOE, Prim, State.State TranslationState] -- TODO: Define effects used in type checking

exprWithSpan :: Sem.Type -> Sem.ExprKind -> Tyck Sem.Expr
exprWithSpan exprType exprKind = do
    exprSpan <- currentSpan <$> State.get
    return $ Sem.Expr{exprKind, exprSpan, exprType}

allocateNewString :: T.Text -> Tyck StringToken
allocateNewString str = do
    uniqifier <- stringUniqifier <$> State.get
    strToToken str uniqifier

addReport :: Report -> Tyck ()
addReport report = do
    State.modify $ \st ->
        st{translationReports = report : translationReports st}

reportError :: T.Text -> Tyck ()
reportError msg = do
    st <- State.get
    let span' = currentSpan st
    let file = currentFile st
    let report = case span' of
            Just (start, end) ->
                let cr =
                        defaultCodeRef file start end
                            & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid
                    msgText =
                        defaultText msg
                            & addForegroundColorToText ANSI.Red ANSI.Vivid
                 in Labeled Error (FormattedText [defaultText "Type Error"])
                        <> Nested (annotatedCodeRef cr msgText)
            Nothing ->
                Labeled Error (FormattedText [defaultText msg])
    addReport report

inferType :: Syn.Expr -> Tyck Sem.Expr
--
--  ───────────────────
--       int <- 123
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    exprWithSpan ty $ Sem.Constant (fromIntegral value)
--
--  ─────────────────────
--       f64 <- 1.23
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    let ty = Sem.TypeFP $ Sem.IEEEFloat 64 -- TODO: we force f64 for now
    exprWithSpan ty $ Sem.Constant (realToFrac value)
--
--  ─────────────────────
--     str <- "string"
inferType (Syn.ConstExpr (Syn.ConstString value)) = do
    token <- allocateNewString value
    let ty = Sem.TypeStr
    exprWithSpan ty $ Sem.GlobalStr token
--
--  ─────────────────────
--    bool <- true/false
inferType (Syn.ConstExpr (Syn.ConstBool value)) = do
    let ty = Sem.TypeBool
    exprWithSpan ty $ Sem.Constant (if value then 1 else 0)
--     x -> bool
--  ─────────────────────
--    bool <- !x
inferType (Syn.UnaryOpExpr Syn.Not subExpr) = do
    let ty = Sem.TypeBool
    subExpr' <- checkType subExpr ty
    one <- exprWithSpan ty $ Sem.Constant 1
    let call =
            Sem.IntrinsicCall (Intrinsic.Arith Arith.Xori) [subExpr', one]
    exprWithSpan ty call
--     x -> int
--  ─────────────────────
--    int <- (-x)
inferType (Syn.UnaryOpExpr Syn.Negate subExpr) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    subExpr' <- checkType subExpr ty
    zero <- exprWithSpan ty $ Sem.Constant 0
    let callTarget = Intrinsic.Arith $ Arith.Subi Arith.iofNone
    let call = Sem.IntrinsicCall callTarget [zero, subExpr']
    exprWithSpan ty call
inferType (Syn.BinOpExpr op lhs rhs) = inferTypeBinOp op lhs rhs
inferType (Syn.SpannedExpr (WithSpan subExpr start end)) = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- inferType subExpr
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
inferType e = error $ "unimplemented inference for:\n\t" ++ show e

-- Binary operations
--          T <- x, y -> T, T = int/float
--  ────────────────────────────────────────────────── -- TODO: better bound
--    T <- (x + y) / (x - y) / (x * y) / (x / y) ...
-- Comparison operations
--          T <- x, y -> T, T = int/float
--  ────────────────────────────────────────────────── -- TODO: better bound
--    bool <- (x == y) / (x != y) / (x < y) ...
-- Short-circuit operations
--          x-> bool, y -> bool
--  ────────────────────────────────────────────────── -- TODO: better bound
--    bool <- (x || y) / (x && y)
inferTypeBinOp :: Syn.BinaryOp -> Syn.Expr -> Syn.Expr -> Tyck Sem.Expr
inferTypeBinOp Syn.And lhs rhs = do
    lhs' <- checkType lhs Sem.TypeBool
    rhs' <- checkType rhs Sem.TypeBool
    false <- exprWithSpan Sem.TypeBool $ Sem.Constant 0
    exprWithSpan Sem.TypeBool $ Sem.ScfIfExpr lhs' rhs' false
inferTypeBinOp Syn.Or lhs rhs = do
    lhs' <- checkType lhs Sem.TypeBool
    rhs' <- checkType rhs Sem.TypeBool
    true <- exprWithSpan Sem.TypeBool $ Sem.Constant 1
    exprWithSpan Sem.TypeBool $ Sem.ScfIfExpr lhs' true rhs'
inferTypeBinOp op lhs rhs = do
    lhs' <- inferType lhs
    let lhsTy = Sem.exprType lhs'
    rhs' <- checkType rhs lhsTy
    case lhsTy of
        Sem.TypeIntegral (Sem.Signed _) -> handleIntOp op lhs' rhs' lhsTy True
        Sem.TypeIntegral (Sem.Unsigned _) -> handleIntOp op lhs' rhs' lhsTy False
        Sem.TypeFP _ -> handleFPOp op lhs' rhs' lhsTy
        _ -> do
            reportError $ "Unsupported type for binary operation: " <> T.pack (show lhsTy)
            exprWithSpan Sem.TypeBottom Sem.Poison
  where
    handleIntOp op' lhs' rhs' ty isSigned = do
        let (intrinsic, resTy) = case op' of
                Syn.Add -> (Just $ Intrinsic.Arith $ Arith.Addi Arith.iofNone, ty)
                Syn.Sub -> (Just $ Intrinsic.Arith $ Arith.Subi Arith.iofNone, ty)
                Syn.Mul -> (Just $ Intrinsic.Arith $ Arith.Muli Arith.iofNone, ty)
                Syn.Div ->
                    ( Just $
                        Intrinsic.Arith $
                            if isSigned then Arith.Divsi else Arith.Divui
                    , ty
                    )
                Syn.Mod ->
                    ( Just $
                        Intrinsic.Arith $
                            if isSigned then Arith.Remsi else Arith.Remui
                    , ty
                    )
                Syn.Equ -> (Just $ Intrinsic.Arith $ Arith.Cmpi Arith.CIEq, Sem.TypeBool)
                Syn.Neq -> (Just $ Intrinsic.Arith $ Arith.Cmpi Arith.CINe, Sem.TypeBool)
                Syn.Lt ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISlt else Arith.CIUlt)
                    , Sem.TypeBool
                    )
                Syn.Gt ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISgt else Arith.CIUgt)
                    , Sem.TypeBool
                    )
                Syn.Lte ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISle else Arith.CIUle)
                    , Sem.TypeBool
                    )
                Syn.Gte ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISge else Arith.CIUge)
                    , Sem.TypeBool
                    )
                _ -> (Nothing, Sem.TypeBottom)
        case intrinsic of
            Just intr -> exprWithSpan resTy $ Sem.IntrinsicCall intr [lhs', rhs']
            Nothing -> do
                reportError "Unsupported integer binary operation"
                exprWithSpan Sem.TypeBottom Sem.Poison

    handleFPOp op' lhs' rhs' ty = do
        let (intrinsic, resTy) = case op' of
                Syn.Add -> (Just $ Intrinsic.Arith $ Arith.Addf (Arith.FastMathFlag 0), ty)
                Syn.Sub -> (Just $ Intrinsic.Arith $ Arith.Subf (Arith.FastMathFlag 0), ty)
                Syn.Mul -> (Just $ Intrinsic.Arith $ Arith.Mulf (Arith.FastMathFlag 0), ty)
                Syn.Div -> (Just $ Intrinsic.Arith $ Arith.Divf (Arith.FastMathFlag 0), ty)
                Syn.Mod -> (Just $ Intrinsic.Arith $ Arith.Remf (Arith.FastMathFlag 0), ty)
                Syn.Equ -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOeq (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Neq -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOne (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Lt -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOlt (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Gt -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOgt (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Lte -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOle (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Gte -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOge (Arith.FastMathFlag 0), Sem.TypeBool)
                _ -> (Nothing, Sem.TypeBottom)
        case intrinsic of
            Just intr -> exprWithSpan resTy $ Sem.IntrinsicCall intr [lhs', rhs']
            Nothing -> do
                reportError "Unsupported floating point binary operation"
                exprWithSpan Sem.TypeBottom Sem.Poison

checkType :: Syn.Expr -> Sem.Type -> Tyck Sem.Expr
checkType (Syn.SpannedExpr (WithSpan subExpr start end)) ty = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- checkType subExpr ty
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
checkType expr ty = do
    -- this is apparently not complete. We need to handle lambda/unification
    expr' <- inferType expr
    let exprTy = Sem.exprType expr'
    if exprTy /= ty
        then do
            reportError $
                "Type mismatch: expected "
                    <> T.pack (show ty)
                    <> ", got "
                    <> T.pack (show exprTy)
            exprWithSpan Sem.TypeBottom Sem.Poison
        else return expr'
