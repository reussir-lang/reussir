{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Tyck where

import Control.Monad (forM_, when, zipWithM)
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
import Reussir.Core.Class (addClass, isSuperClass, meetBound, newDAG, populateDAG, subsumeBound)
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
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Identifier, Path (Path), WithSpan (..), unIdentifier)
import Reussir.Parser.Types.Type qualified as Syn
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

data VarDef = VarDef
    { varName :: Identifier
    , varSpan :: Maybe (Int64, Int64)
    , varType :: Sem.Type
    }

data TranslationState = TranslationState
    { currentSpan :: Maybe (Int64, Int64)
    , currentFile :: FilePath
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    , typeClassDAG :: ClassDAG
    , typeClassTable :: Sem.TypeClassTable
    , holes :: Seq.Seq HoleState
    , variableStates :: Seq.Seq VarDef
    , variableNameMap :: H.CuckooHashTable Identifier Sem.VarID
    }

withVariable :: Identifier -> Maybe (Int64, Int64) -> Sem.Type -> (Sem.VarID -> Tyck a) -> Tyck a
withVariable varName varSpan varType cont = do
    varMap <- State.gets variableNameMap
    backup <- liftIO $ H.lookup varMap varName
    varID <- newVariable varName varSpan varType
    result <- cont varID
    case backup of
        Just oldVarID -> liftIO $ H.insert varMap varName oldVarID
        Nothing -> liftIO $ H.delete varMap varName
    pure result

newVariable :: Identifier -> Maybe (Int64, Int64) -> Sem.Type -> Tyck Sem.VarID
newVariable varName varSpan varType = do
    st <- State.gets variableStates
    let varID = Sem.VarID $ fromIntegral $ Seq.length st
    let varDef = VarDef{varName, varSpan, varType}
    State.modify $ \s ->
        s
            { variableStates = st Seq.|> varDef
            }
    nameMap <- State.gets variableNameMap
    liftIO $ H.insert nameMap varName varID
    return varID

getVarType :: Sem.VarID -> Tyck Sem.Type
getVarType (Sem.VarID idx) = do
    vars <- State.gets variableStates
    let varDef = Seq.index vars (fromIntegral idx)
    return $ varType varDef

getVarTypeViaName :: Identifier -> Tyck Sem.Type
getVarTypeViaName varName = do
    nameMap <- State.gets variableNameMap
    mVarID <- liftIO $ H.lookup nameMap varName
    case mVarID of
        Just varID -> getVarType varID
        Nothing -> do
            reportError $ "Variable not found: " <> (unIdentifier varName)
            return Sem.TypeBottom

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

exactTypeSatisfyBounds :: Sem.TypeClassTable -> Sem.Type -> [Class] -> Tyck Bool
exactTypeSatisfyBounds tyClassTable ty bounds = do
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
        (_, unifState) <- findHoleUnifState hID
        unifState' <- readIORef' unifState
        case unifState' of
            UnSolvedUFRoot rnk bnds -> do
                -- check if ty satisfies bounds
                -- TODO: for now, we simply check if type has Class, this is not enough
                -- and should be delayed
                tyClassTable <- State.gets typeClassTable
                isSatisfy <- exactTypeSatisfyBounds tyClassTable ty bnds
                when isSatisfy $
                    writeIORef' unifState (SolvedUFRoot rnk ty)
                return isSatisfy
            _ -> error "unreachable: cannot be solved or non-root here"
    unifyForced ty (Sem.TypeHole hID) = unifyForced (Sem.TypeHole hID) ty
    unifyForced (Sem.TypeRecord path1 args1) (Sem.TypeRecord path2 args2)
        | path1 == path2 && length args1 == length args2 = do
            results <- zipWithM unify args1 args2
            return $ and results
        | otherwise = return False
    unifyForced Sem.TypeBool Sem.TypeBool = return True
    unifyForced Sem.TypeStr Sem.TypeStr = return True
    unifyForced Sem.TypeUnit Sem.TypeUnit = return True
    unifyForced (Sem.TypeIntegral it1) (Sem.TypeIntegral it2) = return (it1 == it2)
    unifyForced (Sem.TypeFP fpt1) (Sem.TypeFP fpt2) = return (fpt1 == fpt2)
    -- TODO: covariance/contravariance?
    unifyForced (Sem.TypeClosure args1 ret1) (Sem.TypeClosure args2 ret2)
        | length args1 == length args2 = do
            argsResults <- zipWithM unify args1 args2
            retResult <- unify ret1 ret2
            return $ and (retResult : argsResults)
        | otherwise = return False
    unifyForced (Sem.TypeRc t1 cap1) (Sem.TypeRc t2 cap2)
        | cap1 == cap2 = unify t1 t2
        | otherwise = return False
    unifyForced (Sem.TypeRef t1 cap1) (Sem.TypeRef t2 cap2)
        | cap1 == cap2 = unify t1 t2
        | otherwise = return False
    -- auto coercion from bottom
    unifyForced Sem.TypeBottom _ = return True
    unifyForced _ Sem.TypeBottom = return True
    unifyForced (Sem.TypeGeneric g1) (Sem.TypeGeneric g2) = return (g1 == g2)
    unifyForced _ _ = return False

satisfyBounds :: Sem.Type -> TypeBound -> Tyck Bool
satisfyBounds ty bnds = do
    tyClassTable <- State.gets typeClassTable
    case ty of
        Sem.TypeHole hID -> do
            (_, unifState) <- findHoleUnifState hID
            unifState' <- readIORef' unifState
            case unifState' of
                UnSolvedUFRoot _ bnds' -> do
                    dag <- State.gets typeClassDAG
                    subsumeBound dag bnds' bnds
                SolvedUFRoot _ tySolved -> do
                    forced <- force tySolved
                    satisfyBounds forced bnds
                UFNode{} -> error "unreachable: cannot be non-root here"
        x -> exactTypeSatisfyBounds tyClassTable x bnds

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
    variableNameMap <- liftIO $ H.new
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
            , variableStates = mempty
            , variableNameMap
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

-- convert syntax type to semantic type
evalType :: Syn.Type -> Tyck Sem.Type
evalType (Syn.TypeSpanned s) = evalType (spanValue s)
-- TODO: should check if record exists or not
evalType (Syn.TypeExpr path args) = do
    args' <- mapM evalType args
    return $ Sem.TypeRecord path args'
evalType (Syn.TypeIntegral (Syn.Signed n)) = return $ Sem.TypeIntegral (Sem.Signed n)
evalType (Syn.TypeIntegral (Syn.Unsigned n)) = return $ Sem.TypeIntegral (Sem.Unsigned n)
evalType (Syn.TypeFP (Syn.IEEEFloat n)) = return $ Sem.TypeFP (Sem.IEEEFloat n)
evalType (Syn.TypeFP Syn.BFloat16) = return $ Sem.TypeFP Sem.BFloat16
evalType (Syn.TypeFP Syn.Float8) = return $ Sem.TypeFP Sem.Float8
evalType Syn.TypeBool = return Sem.TypeBool
evalType Syn.TypeStr = return Sem.TypeStr
evalType Syn.TypeUnit = return Sem.TypeUnit
evalType Syn.TypeBottom = return Sem.TypeBottom
evalType (Syn.TypeArrow t1 t2) = do
    t1' <- evalType t1
    (args, ret) <- unfoldArrow t2
    return $ Sem.TypeClosure (t1' : args) ret
  where
    unfoldArrow :: Syn.Type -> Tyck ([Sem.Type], Sem.Type)
    unfoldArrow (Syn.TypeArrow a b) = do
        a' <- evalType a
        (args, ret) <- unfoldArrow b
        return (a' : args, ret)
    unfoldArrow (Syn.TypeSpanned s) = unfoldArrow (spanValue s)
    unfoldArrow t = do
        t' <- evalType t
        return ([], t')

inferType :: Syn.Expr -> Tyck Sem.Expr
--       u fresh integral
--  ───────────────────────────
--           u <- 123
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    hole <- introduceNewHole Nothing Nothing [Class $ Path "Integral" []]
    exprWithSpan hole $ Sem.Constant (fromIntegral value)
--      u fresh fp
--  ───────────────────────────
--       u <- 1.23
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    hole <- introduceNewHole Nothing Nothing [Class $ Path "FloatingPoint" []]
    exprWithSpan hole $ Sem.Constant (realToFrac value)
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
    exprWithSpan ty $ Sem.Not subExpr'
--     u <- x, num u
--  ─────────────────────
--      u <- (-x)
inferType (Syn.UnaryOpExpr Syn.Negate subExpr) = do
    subExpr' <- inferType subExpr
    let ty = Sem.exprType subExpr'
    ty' <- force ty
    satisfyNumBound <-
        satisfyBounds
            ty'
            [Class $ Path "Num" []]
    if satisfyNumBound
        then exprWithSpan ty' $ Sem.Negate subExpr'
        else do
            reportError "Unary negation applied to non-numeric type"
            exprWithSpan Sem.TypeBottom Sem.Poison
inferType (Syn.BinOpExpr op lhs rhs) = inferTypeBinOp op lhs rhs
-- If-else operation
--           cond -> bool, T <- x, y -> T
--  ──────────────────────────────────────────────────
--             T <- if cond then x else y
inferType (Syn.If condExpr thenExpr elseExpr) = do
    condExpr' <- checkType condExpr Sem.TypeBool
    thenExpr' <- inferType thenExpr
    let ty = Sem.exprType thenExpr'
    elseExpr' <- checkType elseExpr ty
    exprWithSpan ty $ Sem.ScfIfExpr condExpr' thenExpr' elseExpr'
-- Casting operation
-- Hole (trait casting as an annotation)
--             H <- x, hole H, H -> T
--  ──────────────────────────────────────────────────
--                  T <- x as T
-- Non-Hole (type casting)
--                  T' <- x, num T'
--  ──────────────────────────────────────────────────
--                   T <- x as T
inferType (Syn.Cast targetType subExpr) = do
    targetType' <- evalType targetType
    innerExpr <- inferType subExpr
    innerTy <- force $ Sem.exprType innerExpr
    let numCast = do
            satisfyNumBound <-
                satisfyBounds
                    innerTy
                    [Class $ Path "Num" []]
            if satisfyNumBound
                then exprWithSpan targetType' $ Sem.Cast innerExpr targetType'
                else do
                    reportError "Type cast failed due to unsatisfied bounds"
                    exprWithSpan Sem.TypeBottom Sem.Poison
    case innerTy of
        Sem.TypeHole _ -> do
            unification <- unify innerTy targetType'
            if unification
                then return innerExpr
                else numCast
        _ -> numCast
-- Let-in expr
-- Untyped:
--               C |- T <- e1; C, x:T |- T' <- e2
--  ──────────────────────────────────────────────────
--                 T <- let x = e1 in e2
-- Typed (no capability annotations):
--               C |- x -> T;  C, x:T |- T' <- e2
--  ──────────────────────────────────────────────────
--                 T <- let x: T = e1 in e2
-- Notice, however, we may have capability annotations.
-- If capability annotations are present, we need to check if the type of e1 align
-- with Rc capability, if so, we directly use e1; otherwise, we insert a RcWrap operation.

inferType (Syn.LetIn varName Nothing valueExpr bodyExpr) = do
    valueExpr' <- inferType valueExpr
    let valueTy = Sem.exprType valueExpr'
    valueTy' <- force valueTy
    case mVarType of
        Just annotType -> do
            annotType' <- evalType annotType
            _ <- checkType valueExpr annotType'
            withVariable varName Nothing annotType' $ \varID -> do
                bodyExpr' <- inferType bodyExpr
                let bodyTy = Sem.exprType bodyExpr'
                exprWithSpan bodyTy $ Sem.LetIn varID valueExpr' bodyExpr'
        Nothing -> do
            withVariable varName Nothing valueTy' $ \varID -> do
                bodyExpr' <- inferType bodyExpr
                let bodyTy = Sem.exprType bodyExpr'
                exprWithSpan bodyTy $ Sem.LetIn varID valueExpr' bodyExpr'
inferType (Syn.SpannedExpr (WithSpan subExpr start end)) = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- inferType subExpr
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
inferType e = error $ "unimplemented inference for:\n\t" ++ show e

-- Binary operations
--          T <- x, y -> T, num T
--  ────────────────────────────────────────────────── -- TODO: better bound
--    T <- (x + y) / (x - y) / (x * y) / (x / y) ...
-- Comparison operations
--          T <- x, y -> T, num T
--  ────────────────────────────────────────────────── -- TODO: better bound
--    bool <- (x == y) / (x != y) / (x < y) ...
-- Short-circuit operations
--          x-> bool, y -> bool
--  ──────────────────────────────────────────────────
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
inferTypeBinOp op lhs rhs = case convertOp op of
    Left arithOp -> inferArithOp arithOp
    Right cmpOp -> inferCmpOp cmpOp
  where
    -- TODO: for now, we do not allow a == b/a != b for booleans
    convertOp :: Syn.BinaryOp -> Either Sem.ArithOp Sem.CmpOp
    convertOp Syn.Add = Left Sem.Add
    convertOp Syn.Sub = Left Sem.Sub
    convertOp Syn.Mul = Left Sem.Mul
    convertOp Syn.Div = Left Sem.Div
    convertOp Syn.Mod = Left Sem.Mod
    convertOp Syn.Lt = Right Sem.Lt
    convertOp Syn.Gt = Right Sem.Gt
    convertOp Syn.Lte = Right Sem.Lte
    convertOp Syn.Gte = Right Sem.Gte
    convertOp Syn.Equ = Right Sem.Equ
    convertOp Syn.Neq = Right Sem.Neq
    convertOp _ = error "unreachable: convertOp called on logical op"
    inferArithOp arithOp = do
        lhs' <- inferType lhs
        let lhsTy = Sem.exprType lhs'
        satisfyNumBound <-
            satisfyBounds
                lhsTy
                [Class $ Path "Num" []]
        if satisfyNumBound
            then do
                rhs' <- checkType rhs lhsTy
                exprWithSpan lhsTy $ Sem.Arith lhs' arithOp rhs'
            else do
                reportError "Binary arithmetic operation applied to non-numeric type"
                exprWithSpan Sem.TypeBottom Sem.Poison
    inferCmpOp cmpOp = do
        lhs' <- inferType lhs
        let lhsTy = Sem.exprType lhs'
        satisfyNumBound <-
            satisfyBounds
                lhsTy
                [Class $ Path "Num" []]
        if satisfyNumBound
            then do
                rhs' <- checkType rhs lhsTy
                exprWithSpan Sem.TypeBool $ Sem.Cmp lhs' cmpOp rhs'
            else do
                reportError "Comparison operation applied to non-numeric type"
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
    unification <- unify exprTy ty
    if not unification
        then do
            reportError $
                "Type mismatch: expected "
                    <> T.pack (show ty)
                    <> ", got "
                    <> T.pack (show exprTy)
            exprWithSpan Sem.TypeBottom Sem.Poison
        else return expr'
