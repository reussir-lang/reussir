{-# LANGUAGE OverloadedStrings #-}

{- |
   Bidirectional Type Checking
   ===========================

   This module implements a bidirectional type checking algorithm for Reussir.
   The inferType and the checkType functions are paired together to infer the
   expression type while translating the surface syntax into the semi-abstract
   syntax.
-}
module Reussir.Core.Semi.Tyck (
    forceAndCheckHoles,
    elimTypeHoles,
    checkFuncType,
    inferType,
    checkType,
) where

import Control.Monad (forM, unless, when, zipWithM, forM_)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (isJust)
import Effectful (MonadIO (liftIO), inject)
import Effectful.Prim.IORef.Strict (readIORef', writeIORef')
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (
    Report (..),
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Types.Lexer (
    Identifier (..),
    Path (..),
    WithSpan (WithSpan),
 )

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as State
import Reussir.Parser.Types.Capability qualified as Cap
import Reussir.Parser.Types.Expr qualified as Access (Access (..))
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Stmt qualified as Syn
import Reussir.Parser.Types.Type qualified as Syn
import System.Console.ANSI.Types qualified as ANSI

import Reussir.Core.Data.Class (Class (Class), TypeBound)
import Reussir.Core.Data.Integral (IntegralType (..))
import Reussir.Core.Data.Operator (ArithOp (..), CmpOp (..))
import Reussir.Core.Data.Semi.Context (
    GlobalSemiEff,
    LocalSemiContext (..),
    SemiContext (..),
    SemiEff,
 )
import Reussir.Core.Data.Semi.Expr (
    DTSwitchCases (..),
    DecisionTree (..),
    Expr (..),
    ExprKind (..),
 )
import Reussir.Core.Data.Semi.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core.Data.Semi.Record (
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core.Data.Semi.Type (
    Flexivity (..),
    Type (..),
 )
import Reussir.Core.Data.Semi.Unification (HoleState (..))
import Reussir.Core.Data.UniqueID (GenericID (..))
import Reussir.Core.Semi.Context (
    addErrReport,
    addErrReportMsg,
    addErrReportMsgSeq,
    evalType,
    evalTypeWithFlexivity,
    exprWithSpan,
    runUnification,
    withFreshLocalContext,
    withGenericContext,
    withMaybeSpan,
    withSpan,
    withVariable,
 )
import Reussir.Core.Semi.Function (getFunctionProto)
import Reussir.Core.Semi.PatternMatch (
    TyckCPS (TyckCPS),
    initializePMMatrix,
    translatePMToDT,
 )
import Reussir.Core.Semi.Projection (projectType, resolveProjection)
import Reussir.Core.Semi.Type (substituteGenericMap)
import Reussir.Core.Semi.Unification (
    errorToReport,
    force,
    getGenericBound,
    getHoleState,
    introduceNewHole,
    satisfyBounds,
    unify,
 )
import Reussir.Core.Semi.Variable (lookupVar, newVariable)
import Reussir.Core.String (allocateStrToken)

introduceNewHoleInContext :: TypeBound -> SemiEff Type
introduceNewHoleInContext bounds = do
    currentSpan <- State.gets currentSpan
    runUnification $ introduceNewHole Nothing currentSpan bounds

{- | Best effort attempt to force evaluate a pending type hole to a concrete type.
Report an error if the any type hole cannot be resolved.
-}
forceAndCheckHoles :: Type -> SemiEff Type
forceAndCheckHoles ty = do
    ty' <- runUnification $ force ty
    case ty' of
        TypeHole holeID -> do
            holeState <- runUnification $ getHoleState holeID
            currentSpan' <- State.gets currentSpan
            file <- State.gets currentFile

            let outerReport =
                    case currentSpan' of
                        Just (start, end) ->
                            let cr =
                                    defaultCodeRef file start end
                                        & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid
                                msgText =
                                    defaultText "Unsolved type hole"
                                        & addForegroundColorToText ANSI.Red ANSI.Vivid
                             in Labeled Error (FormattedText [defaultText "Elaboration Error"])
                                    <> Nested (annotatedCodeRef cr msgText)
                        Nothing ->
                            Labeled Error (FormattedText [defaultText "Unsolved type hole"])

            let holeReport =
                    case holeSpan holeState of
                        Just (hStart, hEnd) ->
                            let cr = defaultCodeRef file hStart hEnd
                                msgText = defaultText "Hole introduced here"
                             in Nested (annotatedCodeRef cr msgText)
                        Nothing -> mempty

            inject $ addErrReport (outerReport <> holeReport)
            return ty'
        TypeRecord path args flex -> do
            args' <- mapM forceAndCheckHoles args
            return $ TypeRecord path args' flex
        TypeClosure args ret -> do
            args' <- mapM forceAndCheckHoles args
            ret' <- forceAndCheckHoles ret
            return $ TypeClosure args' ret'
        _ -> return ty'

-- | eliminate type holes from the expression
elimTypeHoles :: Expr -> SemiEff Expr
elimTypeHoles expr = withMaybeSpan (exprSpan expr) $ do
    ty' <- forceAndCheckHoles (exprType expr)
    kind' <- case exprKind expr of
        GlobalStr s -> return $ GlobalStr s
        Constant c -> return $ Constant c
        Negate e -> Negate <$> elimTypeHoles e
        Not e -> Not <$> elimTypeHoles e
        Arith e1 op e2 -> Arith <$> elimTypeHoles e1 <*> pure op <*> elimTypeHoles e2
        Cmp e1 op e2 -> Cmp <$> elimTypeHoles e1 <*> pure op <*> elimTypeHoles e2
        Cast e t -> do
            e' <- elimTypeHoles e
            t' <- forceAndCheckHoles t
            return $ Cast e' t'
        ScfIfExpr e1 e2 e3 -> ScfIfExpr <$> elimTypeHoles e1 <*> elimTypeHoles e2 <*> elimTypeHoles e3
        Var v -> return $ Var v
        Proj e idx -> Proj <$> elimTypeHoles e <*> pure idx
        Let span' varID name val -> do
            val' <- elimTypeHoles val
            return $ Let span' varID name val'
        FuncCall target tyArgs args regional -> do
            tyArgs' <- mapM forceAndCheckHoles tyArgs
            args' <- mapM elimTypeHoles args
            return $ FuncCall target tyArgs' args' regional
        CompoundCall path tyArgs args -> do
            tyArgs' <- mapM forceAndCheckHoles tyArgs
            args' <- mapM elimTypeHoles args
            return $ CompoundCall path tyArgs' args'
        VariantCall path tyArgs variant arg -> do
            tyArgs' <- mapM forceAndCheckHoles tyArgs
            arg' <- elimTypeHoles arg
            return $ VariantCall path tyArgs' variant arg'
        Poison -> return Poison
        NullableCall e -> NullableCall <$> mapM elimTypeHoles e
        RegionRun e -> RegionRun <$> elimTypeHoles e
        Assign e idx e' -> Assign <$> elimTypeHoles e <*> pure idx <*> elimTypeHoles e'
        IntrinsicCall path args -> IntrinsicCall path <$> mapM elimTypeHoles args
        Sequence subexprs -> Sequence <$> mapM elimTypeHoles subexprs
        Match val dt -> Match <$> elimTypeHoles val <*> elimTypeHolesDT dt
    return $ expr{exprType = ty', exprKind = kind'}

elimTypeHolesDT :: DecisionTree -> SemiEff DecisionTree
elimTypeHolesDT DTUncovered = return DTUncovered
elimTypeHolesDT DTUnreachable = return DTUnreachable
elimTypeHolesDT (DTLeaf body bindings) = do
    body' <- elimTypeHoles body
    return $ DTLeaf body' bindings
elimTypeHolesDT (DTGuard bindings guard trueBr falseBr) = do
    guard' <- elimTypeHoles guard
    trueBr' <- elimTypeHolesDT trueBr
    falseBr' <- elimTypeHolesDT falseBr
    return $ DTGuard bindings guard' trueBr' falseBr'
elimTypeHolesDT (DTSwitch ref cases) = DTSwitch ref <$> elimTypeHolesCases cases

elimTypeHolesCases :: DTSwitchCases -> SemiEff DTSwitchCases
elimTypeHolesCases (DTSwitchInt m def) =
    DTSwitchInt <$> mapM elimTypeHolesDT m <*> elimTypeHolesDT def
elimTypeHolesCases (DTSwitchBool t f) =
    DTSwitchBool <$> elimTypeHolesDT t <*> elimTypeHolesDT f
elimTypeHolesCases (DTSwitchCtor cases def) =
    DTSwitchCtor <$> mapM elimTypeHolesDT cases <*> elimTypeHolesDT def
elimTypeHolesCases (DTSwitchString m def) =
    DTSwitchString <$> mapM elimTypeHolesDT m <*> elimTypeHolesDT def
elimTypeHolesCases (DTSwitchNullable j n) =
    DTSwitchNullable <$> elimTypeHolesDT j <*> elimTypeHolesDT n

-- | Infer the type of an expression that is inside a region
inferWithRegion :: Syn.Expr -> SemiEff Expr
inferWithRegion bodyExpr = do
    L.logTrace_ "infer region expression"
    alreadyInsideRegion <- State.gets insideRegion
    when alreadyInsideRegion $ do
        addErrReportMsg "Cannot create nested region"
    State.modify $ \s -> s{insideRegion = True}
    result <- inferType bodyExpr
    State.modify $ \s -> s{insideRegion = alreadyInsideRegion}
    freeze result
  where
    freeze :: Expr -> SemiEff Expr
    freeze expr = case exprType expr of
        -- forward poison the avoid complexity
        TypeBottom -> exprWithSpan TypeBottom Poison
        TypeRecord path tyArgs flex -> case flex of
            Irrelevant -> exprWithSpan (TypeRecord path tyArgs flex) $ RegionRun expr
            _ -> exprWithSpan (TypeRecord path tyArgs Rigid) $ RegionRun expr
        _ -> exprWithSpan (exprType expr) $ RegionRun expr

-- | Type check a function by injecting its generics and parameters into the context
checkFuncType :: Syn.Function -> GlobalSemiEff Expr
checkFuncType func = withFreshLocalContext $ do
    L.logTrace_ $ "Tyck: checking function " <> unIdentifier (Syn.funcName func)
    let name = Syn.funcName func
    let path = Path name [] -- TODO: handle module prefix later on
    funcTable <- State.gets functions
    varTable <- State.gets varTable
    liftIO (H.lookup (functionProtos funcTable) path) >>= \case
        Nothing -> do
            addErrReportMsg $ "Function not found in table: " <> unIdentifier name
            exprWithSpan TypeBottom Poison
        Just proto -> do
            let generics = funcGenerics proto
            L.logTrace_ $
                "Tyck: entering function generic context (generics="
                    <> T.pack (show (length generics))
                    <> ")"
            State.modify $ \s -> s{insideRegion = Syn.funcIsRegional func}
            withGenericContext generics $ do
                let params = funcParams proto
                withParams params varTable $ do
                    case Syn.funcBody func of
                        Just body -> do
                            L.logTrace_ $ "Tyck: checking function body for " <> unIdentifier name
                            wellTyped <- checkType body (funcReturnType proto) >>= elimTypeHoles
                            writeIORef' (funcBody proto) (Just wellTyped)
                            return wellTyped
                        Nothing -> exprWithSpan (funcReturnType proto) (Poison)
  where
    withParams [] _ action = action
    withParams ((pName, ty) : ps) varTable action = do
        _ <- newVariable pName Nothing ty varTable -- no need to rollback
        withParams ps varTable action

-- Helper function for convert type args into semantic counterparts.
-- For placeholder type args (represented as Nothing), we always introduce a
-- new meta hole.
tyArgOrMetaHole :: Maybe Syn.Type -> TypeBound -> SemiEff Type
tyArgOrMetaHole (Just ty) bounds = do
    ty' <- evalType ty
    satisfied <- runUnification $ satisfyBounds ty' bounds
    unless satisfied $ do
        addErrReportMsg "Type argument does not satisfy the required bounds"
    pure ty'
tyArgOrMetaHole Nothing bounds = introduceNewHoleInContext bounds

inferType :: Syn.Expr -> SemiEff Expr
--            U is fresh in Г
--  ───────────────────────────────────
--        Г, U <: Integral |- U <- 123
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    hole <- introduceNewHoleInContext [Class $ Path "Integral" []]
    exprWithSpan hole $ Constant (fromIntegral value)
--            U is fresh in Г
--  ───────────────────────────────────────
--       U <: FloatingPoint |- U <- 1.23
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    hole <- introduceNewHoleInContext [Class $ Path "FloatingPoint" []]
    exprWithSpan hole $ Constant (realToFrac value)
--
--  ─────────────────────
--     str <- "string"
inferType (Syn.ConstExpr (Syn.ConstString value)) = do
    uniquifier <- State.gets stringUniqifier
    token <- allocateStrToken value uniquifier
    let ty = TypeStr
    exprWithSpan ty $ GlobalStr token
--
--  ─────────────────────
--    bool <- true/false
inferType (Syn.ConstExpr (Syn.ConstBool value)) = do
    let ty = TypeBool
    exprWithSpan ty $ Constant (if value then 1 else 0)

--     x -> bool
--  ─────────────────────
--    bool <- !x
inferType (Syn.UnaryOpExpr Syn.Not subExpr) = do
    let ty = TypeBool
    subExpr' <- checkType subExpr ty
    exprWithSpan ty $ Not subExpr'
--     Г, Num U |- U <- x
--  ──────────────────────────
--    Г, Num U |- U <- (-x)
inferType (Syn.UnaryOpExpr Syn.Negate subExpr) = do
    subExpr' <- inferType subExpr
    let ty = exprType subExpr'
    ty' <- runUnification $ force ty
    satisfyNumBound <-
        runUnification $
            satisfyBounds
                ty'
                [Class $ Path "Num" []]
    if satisfyNumBound
        then exprWithSpan ty' $ Negate subExpr'
        else do
            addErrReportMsg "Unary negation applied to non-numeric type"
            exprWithSpan TypeBottom Poison
inferType (Syn.BinOpExpr op lhs rhs) = inferTypeBinOp op lhs rhs
-- If-else operation
--        Г, T |- cond -> bool, T <- x, y -> T
--  ──────────────────────────────────────────────────
--          Г, T |- T <- if cond then x else y
inferType (Syn.If condExpr thenExpr elseExpr) = do
    condExpr' <- checkType condExpr TypeBool
    thenExpr' <- inferType thenExpr
    let ty = exprType thenExpr'
    elseExpr' <- checkType elseExpr ty
    exprWithSpan ty $ ScfIfExpr condExpr' thenExpr' elseExpr'

-- Casting operation
-- If inner expression is a hole with exactly type bound Num.
-- And T is a concrete primitive type
-- Instead of unification, we treat the casting as a type annotation to the
-- expression. Notice that we can only do this if the hole is exactly annotated with Num,
-- since the exact semantic of casting will require knowing the type and we are
-- conservative in this case.
--   Г, H : Hole, H := Num |- H <- x        T <: Num
--  ──────────────────────────────────────────────────────────
--          Г, H : Hole, H := Num, H <=> T |-  T <- x as T
-- Otherwise, we perform numeric casting
--       Г, T' <: Num |- T' <- x       T <: Num
--  ──────────────────────────────────────────────────
--                   T <- x as T
inferType (Syn.Cast targetType subExpr) = do
    targetType' <- evalType targetType
    innerExpr <- inferType subExpr
    innerTy <- runUnification $ force $ exprType innerExpr
    targetTypeSatisfyNumBound <-
        runUnification $ satisfyBounds targetType' [Class $ Path "Num" []]
    if targetTypeSatisfyNumBound
        then do
            innerTyHasExactNumBound <-
                runUnification $ satisfyBounds innerTy [Class $ Path "Num" []]
            case innerTy of
                TypeHole _ | innerTyHasExactNumBound -> do
                    unification <- runUnification $ unify innerTy targetType'
                    if isJust unification
                        then return innerExpr
                        else error "Unification should have succeeded"
                _ -> numCast innerTy innerExpr targetType'
        else do
            addErrReportMsg "Cannot cast to non-numeric type"
            exprWithSpan TypeBottom Poison
  where
    numCast innerTy innerExpr targetType' = do
        satisfyNumBound <-
            runUnification $
                satisfyBounds
                    innerTy
                    [Class $ Path "Num" []]
        if satisfyNumBound
            then exprWithSpan targetType' $ Cast innerExpr targetType'
            else do
                addErrReportMsg "Type cast failed due to unsatisfied bounds"
                exprWithSpan TypeBottom Poison
-- Let-in expr
-- Untyped:
--               Г |- T <- e1; Г, x:T |- T' <- e2
--  ──────────────────────────────────────────────────
--                 T <- let x = e1 in e2
-- Typed:
--               Г |- x -> T;  Г, x:T |- T' <- e2
--  ──────────────────────────────────────────────────
--                 T <- let x: T = e1 in e2
inferType (Syn.Let{}) = do
    addErrReportMsg "Let expression in a non sequence environment is not supported"
    exprWithSpan TypeBottom Poison

-- Regional expression:
--  no Region in Г          Г + region, T | T <- e
--  ──────────────────────────────────────────────────
--     Г, T, T' = Freeze T |- T' <- region e
inferType (Syn.RegionalExpr bodyExpr) = inferWithRegion bodyExpr
-- Variable:
--
--  ──────────────────────
--     C, x:T |- T <- x
inferType (Syn.Var varName) = do
    let baseName = pathBasename varName
    case pathSegments varName of
        [] -> do
            varTable <- State.gets varTable
            lookupVar baseName varTable >>= \case
                Just (varId, varType) -> exprWithSpan varType $ Var varId
                Nothing -> do
                    addErrReportMsg "Variable not found"
                    exprWithSpan TypeBottom Poison
        _ -> error "unimplemented yet"

-- Project:
--     C, record R, I : T, I in R |- R <- e
--  ────────────────────────────────────────
--     C, record R, I : T, I in R |- T <- e.I
-- Access a field of a record.
inferType (Syn.AccessChain baseExpr accesses) = do
    baseExpr' <- inferType baseExpr
    let baseTy = exprType baseExpr'
    baseTy' <- runUnification $ force baseTy
    (projectedTy, indices) <- V.foldM' resolveAccess (baseTy', mempty) accesses
    exprWithSpan projectedTy $ Proj baseExpr' (UV.fromList $ reverse indices)
  where
    resolveAccess (currentTy, acc) access = do
        (idx, fieldTy) <- resolveProjection currentTy access
        return (fieldTy, idx : acc)
-- Function call:
--            Г |- f : (T1, T2, ..., Tn) -> T, Г |- ei : Ti
--  ────────────────────────────────────────────────────────────────────────────
--               Г |- T <- f(e1, e2, ..., en)
--
--            Г + region |- f : [regional] (T1, T2, ..., Tn) -> T
--            Г + region |- ei : Ti
--  ─────────────────────────────────────────────────────────────────────────────────────
--               Г |- T <- f(e1, e2, ..., en)
inferType (Syn.FuncCallExpr call) = inferTypeForCallExpr call
-- Nullable::Null constructor call:
-- Special handling for Nullable::Null to produce NullableCall Nothing
--  ─────────────────────────────────────────────────────
--    C |- Nullable[T] <- Nullable::Null[T]()
inferType
    ( Syn.CtorCallExpr
            Syn.CtorCall
                { Syn.ctorArgs = []
                , Syn.ctorName = Path "Null" ["Nullable"]
                , Syn.ctorTyArgs = ctorTyArgs
                }
        ) = do
        L.logTrace_ "Tyck: infer Nullable::Null ctor call"
        ty' <- case ctorTyArgs of
            [Just ty] -> do
                ty' <- evalType ty
                return ty'
            [Nothing] -> introduceNewHoleInContext [] -- TODO: locate the exact span of the type arg
            [] -> introduceNewHoleInContext []
            _ -> do
                addErrReportMsg "Nullable::Null expects exactly 1 type argument"
                return TypeBottom
        let nullableTy = TypeNullable ty'
        exprWithSpan nullableTy $ NullableCall Nothing
-- Nullable::NonNull constructor call:
-- Special handling for Nullable::NonNull to produce NullableCall (Just e)
--  ─────────────────────────────────────────────────────
--    C |- Nullable[T] <- Nullable::NonNull[T](e)
inferType
    ( Syn.CtorCallExpr
            Syn.CtorCall
                { Syn.ctorArgs = [(_, innerExpr)]
                , Syn.ctorName = Path "NonNull" ["Nullable"]
                , Syn.ctorTyArgs = ctorTyArgs
                }
        ) = do
        L.logTrace_ "Tyck: infer Nullable::NonNull ctor call"
        expectedTy <- case ctorTyArgs of
            [Just ty] -> evalType ty
            [Nothing] -> introduceNewHoleInContext []
            [] -> introduceNewHoleInContext []
            _ -> do
                addErrReportMsg "Nullable::NonNull expects exactly 1 type argument"
                return TypeBottom
        innerExpr' <- checkType innerExpr expectedTy
        let nullableTy = TypeNullable expectedTy
        exprWithSpan nullableTy $ NullableCall (Just innerExpr')
inferType (Syn.CtorCallExpr call) = inferTypeForNormalCtorCall call
inferType (Syn.Assign dst field src) = do
    -- Infer dst and check it is a record with flex annotation
    dst' <- inferType dst
    let dstTy = exprType dst'
    dstTy' <- runUnification $ force dstTy
    case dstTy' of
        TypeRecord _ _ Flex -> do
            -- Resolve field and check it is mutable
            (field', rawFieldTy, isMutable) <- resolveSingleAccessForAssign dstTy' field
            unless isMutable $ do
                addErrReportMsg "Cannot assign to immutable field"
            -- The projected type for mutable fields is Nullable<rawFieldTy>
            let projectedTy = TypeNullable rawFieldTy
            src' <- checkType src projectedTy
            exprWithSpan TypeUnit $ Assign dst' field' src'
        TypeRecord _ _ _ -> do
            addErrReportMsg "Assignment target must be a flex record"
            exprWithSpan TypeBottom Poison
        TypeBottom -> exprWithSpan TypeBottom Poison
        _ -> do
            addErrReportMsg "Assignment target must be a record type"
            exprWithSpan TypeBottom Poison
inferType (Syn.ExprSeq exprs) = do
    inferTypeForSequence exprs []
inferType (Syn.Match scrutinee patterns) = do
    scrutinee' <- inferType scrutinee
    let matrix = initializePMMatrix patterns (exprType scrutinee')
    let cps = TyckCPS inferType checkType withVariable
    decisionTree <- translatePMToDT cps matrix

    let leafTypes = collectLeafExprTypes decisionTree
    matchType <- case leafTypes of
        [] -> return TypeUnit
        (t : ts) -> do
            forM_ ts $ \t' -> do
                failure <- runUnification $ unify t t'
                case failure of
                    Just f -> do
                        filePath <- State.gets currentFile
                        addErrReportMsgSeq "Type mismatch in match branches" (Just $ errorToReport f filePath)
                    Nothing -> return ()
            runUnification $ force t

    exprWithSpan matchType $ Match scrutinee' decisionTree
-- Advance the span in the context and continue on inner expression
inferType (Syn.SpannedExpr (WithSpan expr start end)) =
    withSpan (start, end) $ inferType expr
inferType _ = error "Not implemented"

collectLeafExprTypes :: DecisionTree -> [Type]
collectLeafExprTypes DTUncovered = []
collectLeafExprTypes DTUnreachable = []
collectLeafExprTypes (DTLeaf body _) = [exprType body]
collectLeafExprTypes (DTGuard _ _ trueBr falseBr) =
    collectLeafExprTypes trueBr ++ collectLeafExprTypes falseBr
collectLeafExprTypes (DTSwitch _ cases) = collectLeafExprTypesCases cases

collectLeafExprTypesCases :: DTSwitchCases -> [Type]
collectLeafExprTypesCases (DTSwitchInt m def) =
    concatMap collectLeafExprTypes (IntMap.elems m) ++ collectLeafExprTypes def
collectLeafExprTypesCases (DTSwitchBool t f) =
    collectLeafExprTypes t ++ collectLeafExprTypes f
collectLeafExprTypesCases (DTSwitchCtor cases def) =
    concatMap collectLeafExprTypes (V.toList cases) ++ collectLeafExprTypes def
collectLeafExprTypesCases (DTSwitchString m def) =
    concatMap (collectLeafExprTypes . snd) (HashMap.toList m) ++ collectLeafExprTypes def
collectLeafExprTypesCases (DTSwitchNullable j n) =
    collectLeafExprTypes j ++ collectLeafExprTypes n

checkType :: Syn.Expr -> Type -> SemiEff Expr
checkType (Syn.SpannedExpr (WithSpan expr start end)) ty =
    withSpan (start, end) $ checkType expr ty
checkType expr ty = do
    L.logTrace_ $ "Tyck: checkType expected=" <> T.pack (show ty)
    -- this is apparently not complete. We need to handle lambda/unification
    expr' <- inferType expr
    L.logTrace_ $ "Tyck: inferred type=" <> T.pack (show $ exprType expr')
    exprTy <- runUnification $ force $ exprType expr'
    unification <- runUnification $ unify exprTy ty
    case unification of
        Just err -> do
            filePath <- State.gets currentFile
            let report = errorToReport err filePath
            addErrReportMsgSeq "Failed to unify types" (Just report)
            exprWithSpan TypeBottom Poison
        Nothing -> return expr'

-- | Helper function to infer the type of an intrinsic function call
inferTypeForIntrinsicCall :: Syn.FuncCall -> SemiEff (Maybe Expr)
inferTypeForIntrinsicCall
    Syn.FuncCall
        { Syn.funcCallName = path
        , Syn.funcCallTyArgs = tyArgs
        , Syn.funcCallArgs = argExprs
        } = do
        case path of
            Path name ["core", "intrinsic", "math"] -> do
                let floatUnary =
                        [ "absf"
                        , "acos"
                        , "acosh"
                        , "asin"
                        , "asinh"
                        , "atan"
                        , "atanh"
                        , "cbrt"
                        , "ceil"
                        , "cos"
                        , "cosh"
                        , "erf"
                        , "erfc"
                        , "exp"
                        , "exp2"
                        , "expm1"
                        , "floor"
                        , "log10"
                        , "log1p"
                        , "log2"
                        , "round"
                        , "roundeven"
                        , "rsqrt"
                        , "sin"
                        , "sinh"
                        , "sqrt"
                        , "tan"
                        , "tanh"
                        , "trunc"
                        ]
                let checks = ["isfinite", "isinf", "isnan", "isnormal"]
                let floatBinary = ["atan2", "copysign", "powf"]

                let isUnary = name `elem` floatUnary
                let isCheck = name `elem` checks
                let isBinary = name `elem` floatBinary
                let isFMA = name == "fma"
                let isFPowi = name == "fpowi"

                if isUnary || isCheck || isBinary || isFMA || isFPowi
                    then do
                        -- Check arguments count
                        -- Unary/Check: 1 arg + flag
                        -- Binary/FPowi: 2 args + flag
                        -- FMA: 3 args + flag
                        let expectedArgs =
                                if isUnary || isCheck
                                    then 2
                                    else
                                        if isBinary || isFPowi
                                            then 3
                                            else 4 -- FMA
                        if length argExprs /= expectedArgs
                            then do
                                addErrReportMsg $
                                    "Intrinsic function "
                                        <> T.pack (show path)
                                        <> " expects "
                                        <> T.pack (show expectedArgs)
                                        <> " arguments, but got "
                                        <> T.pack (show (length argExprs))
                                Just <$> exprWithSpan TypeBottom Poison
                            else do
                                -- Infer float type T
                                floatTy <- case tyArgs of
                                    [Just ty] -> evalType ty
                                    [Nothing] -> introduceNewHoleInContext [Class $ Path "FloatingPoint" []]
                                    [] -> introduceNewHoleInContext [Class $ Path "FloatingPoint" []]
                                    _ -> do
                                        addErrReportMsg "Intrinsic function expects exactly 0 or 1 type argument"
                                        return TypeBottom

                                -- Ensure T is FloatingPoint
                                satisfyFloatBound <-
                                    runUnification $
                                        satisfyBounds
                                            floatTy
                                            [Class $ Path "FloatingPoint" []]

                                unless satisfyFloatBound $ do
                                    addErrReportMsg "Intrinsic function type argument must be a floating point type"

                                -- Check arguments
                                let argsToCheck = init argExprs
                                let flagArg = last argExprs

                                args' <-
                                    if isFPowi
                                        then case argsToCheck of
                                            [base, exponentExpr] -> do
                                                base' <- checkType base floatTy
                                                -- exp must be i32
                                                let i32 = TypeIntegral (Signed 32)
                                                exp' <- checkType exponentExpr i32
                                                return [base', exp']
                                            _ -> error "unreachable: args count checked"
                                        else mapM (`checkType` floatTy) argsToCheck

                                -- Check flag
                                let i32 = TypeIntegral (Signed 32)
                                flagArg' <- checkType flagArg i32
                                case exprKind flagArg' of
                                    Constant _ -> return ()
                                    _ -> addErrReportMsg "Intrinsic flag must be a constant literal"

                                let retTy = if isCheck then TypeBool else floatTy

                                -- Construct IntrinsicCall. Type arguments are not stored explicitly;
                                -- type information is carried by the (already checked) arguments and return type.
                                let finalArgs = args' ++ [flagArg']

                                Just
                                    <$> exprWithSpan
                                        retTy
                                        IntrinsicCall
                                            { intrinsicCallTarget = path
                                            , intrinsicCallArgs = finalArgs
                                            }
                    else return Nothing
            _ -> return Nothing

-- | Helper function to infer the type of a function call expression
inferTypeForCallExpr :: Syn.FuncCall -> SemiEff Expr
inferTypeForCallExpr call = do
    inferTypeForIntrinsicCall call >>= \case
        Just expr -> return expr
        Nothing -> inferTypeForNormalCall call

inferTypeForNormalCall :: Syn.FuncCall -> SemiEff Expr
inferTypeForNormalCall
    Syn.FuncCall
        { Syn.funcCallName = path
        , Syn.funcCallTyArgs = tyArgs
        , Syn.funcCallArgs = argExprs
        } = do
        L.logTrace_ $ "Tyck: infer func call " <> T.pack (show path)
        -- Lookup function
        functionTable <- State.gets functions
        getFunctionProto path functionTable >>= \case
            Just proto -> do
                let numGenerics = length (funcGenerics proto)
                -- Allow empty type argument list as syntax sugar for all holes
                let paddedTyArgs =
                        if null tyArgs && numGenerics > 0
                            then replicate numGenerics Nothing
                            else tyArgs
                insideRegion' <- State.gets insideRegion
                when (not insideRegion' && funcIsRegional proto) $ do
                    addErrReportMsg "Cannot call regional function outside of region"
                checkTypeArgsNum numGenerics paddedTyArgs $ do
                    bounds <-
                        mapM (\(_, gid) -> runUnification $ getGenericBound gid) (funcGenerics proto)
                    tyArgs' <- zipWithM tyArgOrMetaHole paddedTyArgs bounds
                    L.logTrace_ $
                        "Tyck: instantiated call generics for "
                            <> T.pack (show path)
                            <> ": "
                            <> T.pack (show tyArgs')
                    let genericMap = functionGenericMap proto tyArgs'
                    let instantiatedArgTypes = map (\(_, ty) -> substituteGenericMap ty genericMap) (funcParams proto)
                    let expectedNumParams = length instantiatedArgTypes
                    checkArgsNum expectedNumParams $ do
                        argExprs' <- zipWithM checkType argExprs instantiatedArgTypes
                        let instantiatedRetType = substituteGenericMap (funcReturnType proto) genericMap
                        exprWithSpan instantiatedRetType $
                            FuncCall
                                { funcCallTarget = path
                                , funcCallArgs = argExprs'
                                , funcCallTyArgs = tyArgs'
                                , funcCallRegional = funcIsRegional proto
                                }
            Nothing -> do
                addErrReportMsg $ "Function not found: " <> T.pack (show path)
                exprWithSpan TypeBottom Poison
      where
        checkArgsNum :: Int -> SemiEff Expr -> SemiEff Expr
        checkArgsNum expectedNum argAction = do
            if expectedNum /= length argExprs
                then do
                    addErrReportMsg $
                        "Function "
                            <> T.pack (show path)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " arguments, but got "
                            <> T.pack (show (length argExprs))
                    exprWithSpan TypeBottom Poison
                else argAction
        checkTypeArgsNum :: Int -> [Maybe Syn.Type] -> SemiEff Expr -> SemiEff Expr
        checkTypeArgsNum expectedNum actualTyArgs paramAction = do
            if expectedNum /= length actualTyArgs
                then do
                    addErrReportMsg $
                        "Function "
                            <> T.pack (show path)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " type arguments, but got "
                            <> T.pack (show (length actualTyArgs))
                    exprWithSpan TypeBottom Poison
                else paramAction
        functionGenericMap :: FunctionProto -> [Type] -> IntMap.IntMap Type
        functionGenericMap proto assignedTypes =
            let genericIDs = map (\(_, GenericID gid) -> fromIntegral gid) (funcGenerics proto)
             in IntMap.fromList $ zip genericIDs assignedTypes

-- Binary operations
--          Г, Num U |- U <- x, y -> U
--  ─────────────────────────────────────────────────────────────────
--    Г, Num U |- U <- (x + y) / (x - y) / (x * y) / (x / y) ...
-- Comparison operations
--          Г, Num U |- U <- x, y -> U
--  ─────────────────────────────────────────────────────────────────
--          Г, Num U |- bool <- (x < y) / (x > y) / (x <= y) / (x >= y)
-- Short-circuit operations
--          Г, Bool U |- U <- x, y -> U
--  ──────────────────────────────────────────────────
--          bool <- (x || y) / (x && y)
inferTypeBinOp :: Syn.BinaryOp -> Syn.Expr -> Syn.Expr -> SemiEff Expr
inferTypeBinOp Syn.And lhs rhs = do
    lhs' <- checkType lhs TypeBool
    rhs' <- checkType rhs TypeBool
    false <- exprWithSpan TypeBool $ Constant 0
    exprWithSpan TypeBool $ ScfIfExpr lhs' rhs' false
inferTypeBinOp Syn.Or lhs rhs = do
    lhs' <- checkType lhs TypeBool
    rhs' <- checkType rhs TypeBool
    true <- exprWithSpan TypeBool $ Constant 1
    exprWithSpan TypeBool $ ScfIfExpr lhs' true rhs'
inferTypeBinOp op lhs rhs = case convertOp op of
    Left arithOp -> inferArithOp arithOp
    Right cmpOp -> inferCmpOp cmpOp
  where
    -- TODO: for now, we do not allow a == b/a != b for booleans
    convertOp :: Syn.BinaryOp -> Either ArithOp CmpOp
    convertOp Syn.Add = Left Add
    convertOp Syn.Sub = Left Sub
    convertOp Syn.Mul = Left Mul
    convertOp Syn.Div = Left Div
    convertOp Syn.Mod = Left Mod
    convertOp Syn.Lt = Right Lt
    convertOp Syn.Gt = Right Gt
    convertOp Syn.Lte = Right Lte
    convertOp Syn.Gte = Right Gte
    convertOp Syn.Equ = Right Equ
    convertOp Syn.Neq = Right Neq
    convertOp _ = error "unreachable: convertOp called on logical op"
    inferArithOp arithOp = do
        lhs' <- inferType lhs
        let lhsTy = exprType lhs'
        satisfyNumBound <-
            runUnification $
                satisfyBounds
                    lhsTy
                    [Class $ Path "Num" []]
        if satisfyNumBound
            then do
                rhs' <- checkType rhs lhsTy
                exprWithSpan lhsTy $ Arith lhs' arithOp rhs'
            else do
                addErrReportMsg $
                    "Binary arithmetic operation applied to non-numeric type: " <> T.show lhsTy
                exprWithSpan TypeBottom Poison
    inferCmpOp cmpOp = do
        lhs' <- inferType lhs
        let lhsTy = exprType lhs'
        satisfyNumBound <-
            runUnification $
                satisfyBounds
                    lhsTy
                    [Class $ Path "Num" []]
        if satisfyNumBound
            then do
                rhs' <- checkType rhs lhsTy
                exprWithSpan TypeBool $ Cmp lhs' cmpOp rhs'
            else do
                addErrReportMsg "Comparison operation applied to non-numeric type"
                exprWithSpan TypeBottom Poison

{- | Resolve a single field access for assignment. Returns (index, rawFieldType, isMutable).
The rawFieldType is the field type before any nullable wrapping.
-}
resolveSingleAccessForAssign ::
    Type -> Access.Access -> SemiEff (Int, Type, Bool)
resolveSingleAccessForAssign currentTy access = do
    case currentTy of
        TypeRecord path args _ -> do
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords path) >>= \case
                Just record -> do
                    let subst =
                            IntMap.fromList $
                                zip (map (\(_, GenericID gid) -> fromIntegral gid) $ recordTyParams record) args
                    fieldsMaybe <- readIORef' (recordFields record)
                    case (fieldsMaybe, access) of
                        (Just (Named fields), Access.Named name) -> do
                            case V.findIndex (\(WithSpan (n, _, _) _ _) -> n == name) fields of
                                Just idx -> do
                                    let WithSpan (_, fieldTy, isMutable) _ _ = fields `V.unsafeIndex` idx
                                    let rawFieldTy = substituteGenericMap fieldTy subst
                                    return (idx, rawFieldTy, isMutable)
                                Nothing -> do
                                    addErrReportMsg $ "Field not found: " <> unIdentifier name
                                    return (-1, TypeBottom, False)
                        (Just (Unnamed fields), Access.Unnamed idx) -> do
                            let idxInt = fromIntegral idx
                            case fields V.!? idxInt of
                                Just (WithSpan (fieldTy, isMutable) _ _) -> do
                                    let rawFieldTy = substituteGenericMap fieldTy subst
                                    return (idxInt, rawFieldTy, isMutable)
                                Nothing -> do
                                    addErrReportMsg $ "Field index out of bounds: " <> T.pack (show idx)
                                    return (-1, TypeBottom, False)
                        (Nothing, _) -> do
                            addErrReportMsg "Record fields not populated"
                            return (-1, TypeBottom, False)
                        _ -> do
                            addErrReportMsg "Invalid access type for record kind"
                            return (-1, TypeBottom, False)
                Nothing -> do
                    addErrReportMsg $ "Unknown record: " <> T.pack (show path)
                    return (-1, TypeBottom, False)
        _ -> do
            addErrReportMsg "Accessing field of non-record type"
            return (-1, TypeBottom, False)

{- | Extract the identifier from a simple variable expression.
Returns Just identifier if the expression is a simple variable (Var with no path segments),
otherwise returns Nothing.
-}
extractSimpleVarName :: Syn.Expr -> Maybe Identifier
extractSimpleVarName (Syn.Var (Path name [])) = Just name
extractSimpleVarName (Syn.SpannedExpr (WithSpan e _ _)) = extractSimpleVarName e
extractSimpleVarName _ = Nothing

-- | Helper function to infer the type of a normal constructor call
inferTypeForNormalCtorCall :: Syn.CtorCall -> SemiEff Expr
inferTypeForNormalCtorCall
    Syn.CtorCall
        { Syn.ctorArgs = ctorArgs
        , Syn.ctorName = ctorCallTarget
        , Syn.ctorTyArgs = ctorTyArgs
        } = do
        L.logTrace_ $ "Tyck: infer ctor call " <> T.pack (show ctorCallTarget)
        records <- State.gets knownRecords
        liftIO (H.lookup records ctorCallTarget) >>= \case
            Nothing -> do
                addErrReportMsg $ "Record not found: " <> T.pack (show ctorCallTarget)
                exprWithSpan TypeBottom Poison
            Just record -> do
                let numGenerics = length (recordTyParams record)
                -- Allow empty type argument list as syntax sugar for all holes
                let paddedTyArgs =
                        if null ctorTyArgs && numGenerics > 0
                            then replicate numGenerics Nothing
                            else ctorTyArgs
                bounds <-
                    mapM (\(_, gid) -> runUnification $ getGenericBound gid) (recordTyParams record)
                tyArgs' <- zipWithM tyArgOrMetaHole paddedTyArgs bounds
                L.logTrace_ $
                    "Tyck: instantiated ctor generics for "
                        <> T.pack (show ctorCallTarget)
                        <> ": "
                        <> T.pack (show tyArgs')
                checkTypeArgsNum numGenerics paddedTyArgs $ do
                    let genericMap = recordGenericMap record tyArgs'

                    fieldsMaybe <- readIORef' (recordFields record)
                    (fields, variantInfo) <- case (recordKind record, fieldsMaybe) of
                        (StructKind, Just (Named fs)) ->
                            return
                                (V.toList $ V.map (\(WithSpan (n, t, f) _ _) -> (Just n, t, f)) fs, Nothing)
                        (StructKind, Just (Unnamed fs)) ->
                            return
                                (V.toList $ V.map (\(WithSpan (t, f) _ _) -> (Nothing, t, f)) fs, Nothing)
                        (EnumVariant parentPath idx, Just (Unnamed fs)) ->
                            return
                                ( V.toList $ V.map (\(WithSpan (t, f) _ _) -> (Nothing, t, f)) fs
                                , Just (parentPath, idx)
                                )
                        (EnumVariant _ _, Just (Named _)) -> do
                            -- This should be unreachable based on current translation logic
                            addErrReportMsg "Enum variant cannot have named fields yet"
                            return ([], Nothing)
                        (EnumKind, _) -> do
                            addErrReportMsg "Cannot instantiate Enum directly"
                            return ([], Nothing)
                        (_, Nothing) -> do
                            addErrReportMsg "Record fields not populated"
                            return ([], Nothing)
                        _ -> do
                            addErrReportMsg "Invalid record kind/fields combination"
                            return ([], Nothing)

                    let expectedNumParams = length fields
                    checkArgsNum expectedNumParams $ do
                        -- Expand shorthand args: for positional args that are simple Var expressions
                        -- matching a record field name and in-scope variable, treat as named args.
                        -- E.g., `{ val }` becomes `{ val: val }` if `val` is in scope and matches a field.
                        let fieldNames = [n | (Just n, _, _) <- fields]
                        varTable <- State.gets varTable
                        expandedCtorArgs <- forM ctorArgs $ \(mArgName, argExpr) -> case mArgName of
                            Just _ -> return (mArgName, argExpr) -- Already named, keep as-is
                            Nothing -> case extractSimpleVarName argExpr of
                                Just varName | varName `elem` fieldNames -> do
                                    -- Check if the variable exists in scope
                                    lookupVar varName varTable >>= \case
                                        Just _ -> return (Just varName, argExpr) -- Expand shorthand
                                        Nothing -> return (Nothing, argExpr) -- Not in scope, keep positional
                                _ -> return (Nothing, argExpr) -- Not a simple var or doesn't match field, keep positional

                        -- Reconstruct an ordered list of syntactic expressions in the same order as required in the record
                        orderedArgsExprs <-
                            if isJust variantInfo || case fieldsMaybe of Just (Unnamed _) -> True; _ -> False
                                then return $ map (Just . snd) expandedCtorArgs
                                else do
                                    let hasNamedArgs = any (isJust . fst) expandedCtorArgs
                                    if hasNamedArgs
                                        then
                                            mapM
                                                ( \(mName, _, _) -> case mName of
                                                    Just name -> case find (\(aName, _) -> aName == Just name) expandedCtorArgs of
                                                        Just (_, expr) -> return $ Just expr
                                                        Nothing -> do
                                                            addErrReportMsg $ "Missing argument for field: " <> unIdentifier name
                                                            pure Nothing
                                                    Nothing -> error "Named field has no name?"
                                                )
                                                fields
                                        else return $ map (Just . snd) expandedCtorArgs
                        argExprs' <-
                            zipWithM
                                ( \expr (_, ty, flag) -> do
                                    let ty' = substituteGenericMap ty genericMap
                                    expectedTy <- projectType flag ty'
                                    case expr of
                                        Just expr' -> checkType expr' expectedTy
                                        Nothing -> do
                                            exprWithSpan TypeBottom Poison
                                )
                                orderedArgsExprs
                                fields
                        insideRegion' <- State.gets insideRegion
                        let recordIsRegional = recordDefaultCap record == Cap.Regional
                        flexivity <-
                            if recordIsRegional
                                then do
                                    unless insideRegion' $
                                        addErrReportMsg "Cannot instantiate regional record outside of a region"
                                    return Flex
                                else return Irrelevant
                        let recordTy = case variantInfo of
                                Just (parent, _) -> TypeRecord parent tyArgs' flexivity
                                Nothing -> TypeRecord ctorCallTarget tyArgs' flexivity
                        case variantInfo of
                            Just (parentPath, idx) -> do
                                let innerTy = TypeRecord ctorCallTarget tyArgs' Irrelevant
                                innerExpr <-
                                    exprWithSpan innerTy $
                                        CompoundCall
                                            { compoundCallTarget = ctorCallTarget
                                            , compoundCallTyArgs = tyArgs'
                                            , compoundCallArgs = argExprs'
                                            }
                                exprWithSpan recordTy $
                                    VariantCall
                                        { variantCallTarget = parentPath
                                        , variantCallTyArgs = tyArgs'
                                        , variantCallVariant = idx
                                        , variantCallArg = innerExpr
                                        }
                            Nothing ->
                                exprWithSpan recordTy $
                                    CompoundCall
                                        { compoundCallTarget = ctorCallTarget
                                        , compoundCallTyArgs = tyArgs'
                                        , compoundCallArgs = argExprs'
                                        }
      where
        recordGenericMap :: Record -> [Type] -> IntMap.IntMap Type
        recordGenericMap record assignedTypes =
            let genericIDs = map (\(_, GenericID gid) -> fromIntegral gid) (recordTyParams record)
             in IntMap.fromList $ zip genericIDs assignedTypes
        checkArgsNum :: Int -> SemiEff Expr -> SemiEff Expr
        checkArgsNum expectedNum argAction = do
            if expectedNum /= length ctorArgs
                then do
                    addErrReportMsg $
                        "Record "
                            <> T.pack (show ctorCallTarget)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " arguments, but got "
                            <> T.pack (show (length ctorArgs))
                    exprWithSpan TypeBottom Poison
                else argAction
        checkTypeArgsNum :: Int -> [Maybe Syn.Type] -> SemiEff Expr -> SemiEff Expr
        checkTypeArgsNum expectedNum actualTyArgs paramAction = do
            if expectedNum /= length actualTyArgs
                then do
                    addErrReportMsg $
                        "Record "
                            <> T.pack (show ctorCallTarget)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " type arguments, but got "
                            <> T.pack (show (length actualTyArgs))
                    exprWithSpan TypeBottom Poison
                else paramAction

inferTypeForSequence :: [Syn.Expr] -> [Expr] -> SemiEff Expr
inferTypeForSequence [] exprs = do
    case exprs of
        [] -> exprWithSpan TypeBottom Poison
        subexprs@(x : _) -> do
            let exprTy = exprType x
            exprWithSpan exprTy $ Sequence (reverse subexprs)
inferTypeForSequence (Syn.SpannedExpr (WithSpan expr start end) : es) exprs = do
    withSpan (start, end) $ inferTypeForSequence (expr : es) exprs
inferTypeForSequence
    ((Syn.Let (WithSpan varName vnsStart vnsEnd) Nothing valueExpr) : es)
    exprs = do
        valueExpr' <- inferType valueExpr
        let valueTy = exprType valueExpr'
        valueTy' <- runUnification $ force valueTy
        withVariable varName (Just (vnsStart, vnsEnd)) valueTy' $ \varID -> do
            expr <-
                exprWithSpan TypeUnit $
                    Let
                        { letVarID = varID
                        , letVarName = varName
                        , letVarExpr = valueExpr'
                        , letVarSpan = Just (vnsStart, vnsEnd)
                        }
            inferTypeForSequence es $ expr : exprs
inferTypeForSequence
    ((Syn.Let (WithSpan varName vnsStart vnsEnd) (Just (ty, flex)) valueExpr) : es)
    exprs = do
        annotatedType' <- evalTypeWithFlexivity ty flex
        valueExpr' <- checkType valueExpr annotatedType'
        let valueTy = exprType valueExpr'
        valueTy' <- runUnification $ force valueTy
        withVariable varName (Just (vnsStart, vnsEnd)) valueTy' $ \varID -> do
            expr <-
                exprWithSpan TypeUnit $
                    Let
                        { letVarID = varID
                        , letVarName = varName
                        , letVarExpr = valueExpr'
                        , letVarSpan = Just (vnsStart, vnsEnd)
                        }
            inferTypeForSequence es $ expr : exprs
inferTypeForSequence (e : es) exprs = do
    e' <- inferType e
    inferTypeForSequence es $ e' : exprs
