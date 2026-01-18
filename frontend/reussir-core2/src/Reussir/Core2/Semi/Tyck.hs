{-# LANGUAGE OverloadedStrings #-}

{- |
   Bidirectional Type Checking
   ===========================

   This module implements a bidirectional type checking algorithm for Reussir.
   The inferType and the checkType functions are paired together to infer the
   expression type while translating the surface syntax into the semi-abstract
   syntax.
-}
module Reussir.Core2.Semi.Tyck (
    forceAndCheckHoles,
    elimTypeHoles,
) where

import Data.Function ((&))
import Effectful.State.Static.Local qualified as State
import System.Console.ANSI.Types qualified as ANSI

import Control.Monad (unless, when)
import Data.HashTable.IO qualified as H
import Data.Maybe
import Data.Text qualified as T
import Effectful (MonadIO (liftIO))
import Effectful.Log qualified as L
import Effectful.Prim.IORef.Strict (writeIORef')
import Reussir.Core2.Data.Class (Class (Class), TypeBound)
import Reussir.Core2.Data.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core2.Data.Operator (ArithOp (..), CmpOp (..))
import Reussir.Core2.Data.Semi (
    LocalSemiContext (..),
    SemiContext (..),
    SemiEff,
    Type (..),
 )
import Reussir.Core2.Data.Semi.Expr (Expr (..), ExprKind (..))
import Reussir.Core2.Data.Semi.Unification (HoleState (..))
import Reussir.Core2.Semi.Context (
    addErrReport,
    addErrReportMsg,
    evalType,
    evalTypeWithFlexivity,
    exprWithSpan,
    refreshLocalContext,
    runUnification,
    withGenericContext,
    withMaybeSpan,
    withSpan,
    withVariable,
 )
import Reussir.Core2.Semi.Unification (force, getHoleState, introduceNewHole, satisfyBounds, unify)
import Reussir.Core2.Semi.Variable (lookupVar)
import Reussir.Core2.String (allocateStrToken)
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (
    Report (..),
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (WithSpan))
import Reussir.Parser.Types.Stmt qualified as Syn
import Reussir.Parser.Types.Type qualified as Syn

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
                             in Labeled Error (FormattedText [defaultText "Type Error"])
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

            addErrReport (outerReport <> holeReport)
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
        ProjChain e idxs -> ProjChain <$> elimTypeHoles e <*> pure idxs
        Let span' varID name val body -> do
            val' <- elimTypeHoles val
            body' <- elimTypeHoles body
            return $ Let span' varID name val' body'
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
    return $ expr{exprType = ty', exprKind = kind'}

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
        TypeRecord{} -> undefined -- FIME: fill this
        _ -> exprWithSpan (exprType expr) $ RegionRun expr

-- | Type check a function by injecting its generics and parameters into the context
checkFuncType :: Syn.Function -> SemiEff Expr
checkFuncType func = do
    L.logTrace_ $ "Tyck: checking function " <> unIdentifier (Syn.funcName func)
    refreshLocalContext
    let name = Syn.funcName func
    let path = Path name [] -- TODO: handle module prefix later on
    funcTable <- State.gets functions
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
                withParams params $ do
                    case Syn.funcBody func of
                        Just body -> do
                            L.logTrace_ $ "Tyck: checking function body for " <> unIdentifier name
                            wellTyped <- checkType body (funcReturnType proto) >>= elimTypeHoles
                            writeIORef' (funcBody proto) (Just wellTyped)
                            return wellTyped
                        Nothing -> exprWithSpan (funcReturnType proto) (Poison)
  where
    withParams [] action = action
    withParams ((pName, ty) : ps) action =
        withVariable pName Nothing ty $ \_ -> withParams ps action

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
tyArgOrMetaHole Nothing bounds =
    runUnification $
        introduceNewHole Nothing Nothing bounds

{- | Helper to find a named field in a list of fields.
Returns the index and the type of the field if found.
-}
findFieldBy :: (x -> Bool) -> [x] -> Maybe (Int, x)
findFieldBy key fields = go 0 fields
  where
    go _ [] = Nothing
    go i (x : rest)
        | key x = Just (i, x)
        | otherwise = go (i + 1) rest

-- | Helper to safely index into a list.
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n
    | n < 0 = Nothing
    | otherwise = safeIndex xs (n - 1)

inferType :: Syn.Expr -> SemiEff Expr
--            U is fresh in Г
--  ───────────────────────────────────
--        Г, U <: Integral |- U <- 123
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    hole <- runUnification $ introduceNewHole Nothing Nothing [Class $ Path "Integral" []]
    exprWithSpan hole $ Constant (fromIntegral value)
--            U is fresh in Г
--  ───────────────────────────────────────
--       U <: FloatingPoint |- U <- 1.23
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    hole <- runUnification $ introduceNewHole Nothing Nothing [Class $ Path "FloatingPoint" []]
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
inferType (Syn.LetIn (WithSpan varName vnsStart vnsEnd) Nothing valueExpr bodyExpr) = do
    valueExpr' <- inferType valueExpr
    let valueTy = exprType valueExpr'
    valueTy' <- runUnification $ force valueTy
    withVariable varName (Just (vnsStart, vnsEnd)) valueTy' $ \varID -> do
        bodyExpr' <- inferType bodyExpr
        let bodyTy = exprType bodyExpr'
        exprWithSpan bodyTy $
            Let
                { letVarID = varID
                , letVarName = varName
                , letVarExpr = valueExpr'
                , letBodyExpr = bodyExpr'
                , letVarSpan = Just (vnsStart, vnsEnd)
                }
inferType (Syn.LetIn (WithSpan varName vnsStart vnsEnd) (Just (ty, flex)) valueExpr bodyExpr) = do
    annotatedType' <- evalTypeWithFlexivity ty flex
    valueExpr' <- checkType valueExpr annotatedType'
    withVariable varName (Just (vnsStart, vnsEnd)) annotatedType' $ \varID -> do
        bodyExpr' <- inferType bodyExpr
        let bodyTy = exprType bodyExpr'
        exprWithSpan bodyTy $
            Let
                { letVarID = varID
                , letVarName = varName
                , letVarExpr = valueExpr'
                , letBodyExpr = bodyExpr'
                , letVarSpan = Just (vnsStart, vnsEnd)
                }
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
-- Advance the span in the context and continue on inner expression
inferType (Syn.SpannedExpr (WithSpan expr start end)) =
    withSpan (start, end) $ inferType expr
inferType _ = error "Not implemented"

checkType :: Syn.Expr -> Type -> SemiEff Expr
checkType (Syn.SpannedExpr (WithSpan expr start end)) ty =
    withSpan (start, end) $ checkType expr ty
checkType _ _ = error "Not implemented"

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
                addErrReportMsg $ "Binary arithmetic operation applied to non-numeric type: " <> T.show lhsTy
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
