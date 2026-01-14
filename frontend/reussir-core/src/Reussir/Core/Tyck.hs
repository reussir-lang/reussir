{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Tyck where

import Control.Monad (foldM, unless, when, zipWithM)
import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex, find)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Effectful (liftIO)
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as State
import Reussir.Bridge qualified as B

import Reussir.Core.Translation
import Reussir.Core.Types.Class (Class (..), TypeBound)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.GenericID (GenericID (..))
import Reussir.Core.Types.Record qualified as Sem
import Reussir.Core.Types.Translation
import Reussir.Core.Types.Type qualified as Sem

import Effectful.Prim.IORef.Strict (writeIORef')
import Reussir.Core.Function (getFunctionProto)
import Reussir.Core.Types.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Parser.Types.Capability qualified as Cap
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path (..), WithSpan (..), pathBasename, pathSegments, unIdentifier)
import Reussir.Parser.Types.Stmt qualified as Stmt
import Reussir.Parser.Types.Type qualified as Syn

inferWithRegion :: Syn.Expr -> Tyck Sem.Expr
inferWithRegion bodyExpr = do
    L.logTrace_ "infer region expression"
    alreadyInsideRegion <- State.gets insideRegion
    when alreadyInsideRegion $ do
        reportError "Cannot create nested region"
    State.modify $ \s -> s{insideRegion = True}
    result <- inferType bodyExpr
    State.modify $ \s -> s{insideRegion = alreadyInsideRegion}
    freeze result
  where
    freeze :: Sem.Expr -> Tyck Sem.Expr
    freeze expr = case Sem.exprType expr of
        Sem.TypeRc inner Cap.Flex -> do
            let rigidRc = Sem.TypeRc inner Cap.Rigid
            exprWithSpan rigidRc $ Sem.RunRegion expr
        Sem.TypeBottom -> exprWithSpan Sem.TypeBottom Sem.Poison
        Sem.TypeRecord{} -> do
            reportError "Currently, only managed objects can be returned from regions"
            exprWithSpan Sem.TypeBottom Sem.Poison
        _ -> exprWithSpan (Sem.exprType expr) $ Sem.RunRegion expr

checkFuncType :: Stmt.Function -> Tyck Sem.Expr
checkFuncType func = do
    L.logTrace_ $ "Tyck: checking function " <> unIdentifier (Stmt.funcName func)
    clearLocals
    clearHoles
    let name = Stmt.funcName func
    let path = Path name []
    funcTable <- State.gets functions
    liftIO (H.lookup (functionProtos funcTable) path) >>= \case
        Nothing -> do
            reportError $ "Function not found in table: " <> unIdentifier name
            exprWithSpan Sem.TypeBottom Sem.Poison
        Just proto -> do
            let generics = funcGenerics proto
            L.logTrace_ $
                "Tyck: entering function generic context (generics="
                    <> T.pack (show (length generics))
                    <> ")"
            State.modify $ \s -> s{insideRegion = Stmt.funcIsRegional func, exprCounter = 0}
            withGenericContext generics $ do
                let params = funcParams proto
                withParams params $ do
                    case Stmt.funcBody func of
                        Just body -> do
                            L.logTrace_ $ "Tyck: checking function body for " <> unIdentifier name
                            wellTyped <- checkType body (funcReturnType proto) >>= wellTypedExpr
                            writeIORef' (funcBody proto) (Just wellTyped)
                            return wellTyped
                        Nothing -> exprWithSpan (funcReturnType proto) (Sem.Poison)
  where
    withParams [] action = action
    withParams ((pName, ty) : ps) action =
        withVariable pName Nothing ty $ \_ -> withParams ps action

-- Helper function for convert type args into semantic counterparts
tyArgOrMetaHole :: Maybe Syn.Type -> TypeBound -> Tyck Sem.Type
tyArgOrMetaHole (Just ty) bounds = do
    ty' <- evalType ty
    satisfied <- satisfyBounds ty' bounds
    unless satisfied $ do
        reportError "Type argument does not satisfy the required bounds"
    pure ty'
tyArgOrMetaHole Nothing bounds = introduceNewHole Nothing Nothing bounds

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
inferType (Syn.LetIn (WithSpan varName vnsStart vnsEnd) Nothing valueExpr bodyExpr) = do
    valueExpr' <- inferType valueExpr
    let valueTy = Sem.exprType valueExpr'
    valueTy' <- force valueTy
    withVariable varName (Just (vnsStart, vnsEnd)) valueTy' $ \varID -> do
        bodyExpr' <- inferType bodyExpr
        let bodyTy = Sem.exprType bodyExpr'
        exprWithSpan bodyTy $
            Sem.Let
                { Sem.letVarID = varID
                , Sem.letVarName = varName
                , Sem.letVarExpr = valueExpr'
                , Sem.letBodyExpr = bodyExpr'
                , Sem.letVarSpan = Just (vnsStart, vnsEnd)
                }
inferType (Syn.LetIn (WithSpan varName vnsStart vnsEnd) (Just (ty, flex)) valueExpr bodyExpr) = do
    annotatedType' <- evalTypeWithFlexivity ty flex
    valueExpr' <- checkType valueExpr annotatedType'
    withVariable varName (Just (vnsStart, vnsEnd)) annotatedType' $ \varID -> do
        bodyExpr' <- inferType bodyExpr
        let bodyTy = Sem.exprType bodyExpr'
        exprWithSpan bodyTy $
            Sem.Let
                { Sem.letVarID = varID
                , Sem.letVarName = varName
                , Sem.letVarExpr = valueExpr'
                , Sem.letBodyExpr = bodyExpr'
                , Sem.letVarSpan = Just (vnsStart, vnsEnd)
                }
-- Regional expression:
--  no Region in C, C + region | T <- e
--  ─────────────────────────────────────
--     C |- freeze T <- region e
inferType (Syn.RegionalExpr bodyExpr) = inferWithRegion bodyExpr
-- Variable:
--
--  ──────────────────────
--     C, x:T |- T <- x
inferType (Syn.Var varName) = do
    let baseName = pathBasename varName
    case pathSegments varName of
        [] -> do
            (varId, varType) <- lookupVar baseName
            exprWithSpan varType $ Sem.Var varId
        _ -> error "unimplemented yet"
-- Project:
--     C, record R, I : T, I in R |- R <- e
--  ────────────────────────────────────────
--     C, record R, I : T, I in R |- T <- e.I
-- Access a field of a record
-- Notabily, a record may be inferred as a raw type or a rc wrapped type
-- in the latter case, the record itself is the immediate inner type of the rc.
inferType (Syn.AccessChain baseExpr projs) = do
    baseExpr' <- inferType baseExpr
    let baseTy = Sem.exprType baseExpr'
    baseTy' <- force baseTy

    (finalTy, indices) <- foldM resolveAccess (baseTy', []) projs

    exprWithSpan finalTy $ Sem.ProjChain baseExpr' (reverse indices)
  where
    resolveAccess (currentTy, indices) access = do
        currentTy' <- force currentTy
        let innerTy = case currentTy' of
                Sem.TypeRc t _ -> t
                t -> t

        case innerTy of
            Sem.TypeRecord path args -> do
                knownRecords <- State.gets knownRecords
                liftIO (H.lookup knownRecords path) >>= \case
                    Just record -> do
                        let subst = IntMap.fromList $ zip (map (\(_, GenericID gid) -> fromIntegral gid) $ Sem.recordTyParams record) args
                        case (Sem.recordFields record, access) of
                            (Sem.Named fields, Syn.Named name) -> do
                                case elemIndex name (map (\(n, _, _) -> n) fields) of
                                    Just idx -> do
                                        let (_, fieldTy, _) = fields !! idx
                                        let fieldTy' = substituteTypeParams fieldTy subst
                                        return (fieldTy', idx : indices)
                                    Nothing -> do
                                        reportError $ "Field not found: " <> unIdentifier name
                                        return (Sem.TypeBottom, indices)
                            (Sem.Unnamed fields, Syn.Unnamed idx) -> do
                                let idxInt = fromIntegral idx
                                if idxInt >= 0 && idxInt < length fields
                                    then do
                                        let (fieldTy, _) = fields !! idxInt
                                        let fieldTy' = substituteTypeParams fieldTy subst
                                        return (fieldTy', idxInt : indices)
                                    else do
                                        reportError $ "Field index out of bounds: " <> T.pack (show idx)
                                        return (Sem.TypeBottom, indices)
                            _ -> do
                                reportError "Invalid access type for record kind"
                                return (Sem.TypeBottom, indices)
                    Nothing -> do
                        reportError $ "Unknown record: " <> T.pack (show path)
                        return (Sem.TypeBottom, indices)
            _ -> do
                reportError "Accessing field of non-record type"
                return (Sem.TypeBottom, indices)
-- Function call:
--            C |- f : (T1, T2, ..., Tn) -> T, C |- ei : Ti
--  ────────────────────────────────────────────────────────────────────────────
--               C |- T <- f(e1, e2, ..., en)
--
--            C + region |- f : [regional] (T1, T2, ..., Tn) -> T
--            C + region |- ei : Ti
--  ─────────────────────────────────────────────────────────────────────────────────────
--               C |- T <- f(e1, e2, ..., en)
inferType (Syn.FuncCallExpr (Syn.FuncCall{Syn.funcCallName = path, Syn.funcCallTyArgs = tyArgs, Syn.funcCallArgs = argExprs})) = do
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
                reportError "Cannot call regional function outside of region"
            checkTypeArgsNum numGenerics paddedTyArgs $ do
                bounds <- mapM (\(_, gid) -> getGenericBound gid) (funcGenerics proto)
                tyArgs' <- zipWithM tyArgOrMetaHole paddedTyArgs bounds
                logTraceWhen $
                    "Tyck: instantiated call generics for "
                        <> T.pack (show path)
                        <> ": "
                        <> T.pack (show tyArgs')
                let genericMap = functionGenericMap proto tyArgs'
                let instantiatedArgTypes = map (\(_, ty) -> substituteTypeParams ty genericMap) (funcParams proto)
                let expectedNumParams = length instantiatedArgTypes
                checkArgsNum expectedNumParams $ do
                    argExprs' <- zipWithM checkType argExprs instantiatedArgTypes
                    -- Check for intrinsic flags
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
                            let hasFlag =
                                    (name `elem` floatUnary)
                                        || (name `elem` checks)
                                        || (name `elem` floatBinary)
                                        || name == "fma"
                                        || name == "fpowi"
                            if hasFlag
                                then do
                                    let flagArg = last argExprs'
                                    case Sem.exprKind flagArg of
                                        Sem.Constant _ -> return ()
                                        _ -> reportError "Intrinsic flag must be a constant literal"
                                else return ()
                        _ -> return ()
                    let instantiatedRetType = substituteTypeParams (funcReturnType proto) genericMap
                    exprWithSpan instantiatedRetType $
                        Sem.FuncCall
                            { Sem.funcCallTarget = path
                            , Sem.funcCallArgs = argExprs'
                            , Sem.funcCallTyArgs = tyArgs'
                            , Sem.funcCallRegional = funcIsRegional proto
                            }
        Nothing -> do
            reportError $ "Function not found: " <> T.pack (show path)
            exprWithSpan Sem.TypeBottom Sem.Poison
  where
    checkArgsNum :: Int -> Tyck Sem.Expr -> Tyck Sem.Expr
    checkArgsNum expectedNum argAction = do
        if expectedNum /= length argExprs
            then do
                reportError $
                    "Function "
                        <> T.pack (show path)
                        <> " expects "
                        <> T.pack (show expectedNum)
                        <> " arguments, but got "
                        <> T.pack (show (length argExprs))
                exprWithSpan Sem.TypeBottom Sem.Poison
            else argAction
    checkTypeArgsNum :: Int -> [Maybe Syn.Type] -> Tyck Sem.Expr -> Tyck Sem.Expr
    checkTypeArgsNum expectedNum actualTyArgs paramAction = do
        if expectedNum /= length actualTyArgs
            then do
                reportError $
                    "Function "
                        <> T.pack (show path)
                        <> " expects "
                        <> T.pack (show expectedNum)
                        <> " type arguments, but got "
                        <> T.pack (show (length actualTyArgs))
                exprWithSpan Sem.TypeBottom Sem.Poison
            else paramAction
    functionGenericMap :: FunctionProto -> [Sem.Type] -> IntMap.IntMap Sem.Type
    functionGenericMap proto assignedTypes =
        let genericIDs = map (\(_, GenericID gid) -> fromIntegral gid) (funcGenerics proto)
         in IntMap.fromList $ zip genericIDs assignedTypes
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
        let ptrLikeBound = [Class $ Path "PtrLike" []]
        -- We need to resolve the type argument for T
        tyArgs' <- case ctorTyArgs of
            [Just ty] -> do
                ty' <- evalType ty
                satisfied <- satisfyBounds ty' ptrLikeBound
                unless satisfied $ do
                    reportError "Nullable::Null type argument does not satisfy PtrLike bound"
                return [ty']
            [Nothing] -> do
                hole <- introduceNewHole Nothing Nothing ptrLikeBound
                return [hole]
            [] -> do
                hole <- introduceNewHole Nothing Nothing ptrLikeBound
                return [hole]
            _ -> do
                reportError "Nullable::Null expects exactly 1 type argument"
                return []
        let nullableTy = Sem.TypeRecord (Path "Nullable" []) tyArgs'
        exprWithSpan nullableTy $ Sem.NullableCall Nothing
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
        let ptrLikeBound = [Class $ Path "PtrLike" []]
        -- First infer the type of the inner expression
        innerExpr' <- inferType innerExpr
        let innerTy = Sem.exprType innerExpr'
        -- Handle type argument: it may be explicit, a hole, or inferred from innerTy
        tyArgs' <- case ctorTyArgs of
            [Just ty] -> do
                ty' <- evalType ty
                satisfied <- satisfyBounds ty' ptrLikeBound
                unless satisfied $ do
                    reportError "Nullable::NonNull type argument does not satisfy PtrLike bound"
                -- Unify the expected type with the inner type
                unification <- unify innerTy ty'
                if not unification
                    then do
                        reportError "Type mismatch in Nullable::NonNull"
                        return [Sem.TypeBottom]
                    else return [ty']
            [Nothing] -> do
                -- Infer from inner expression type, but check bounds
                satisfied <- satisfyBounds innerTy ptrLikeBound
                unless satisfied $ do
                    reportError "Nullable::NonNull type argument does not satisfy PtrLike bound"
                return [innerTy]
            [] -> do
                -- Infer from inner expression type, but check bounds
                satisfied <- satisfyBounds innerTy ptrLikeBound
                unless satisfied $ do
                    reportError "Nullable::NonNull type argument does not satisfy PtrLike bound"
                return [innerTy]
            _ -> do
                reportError "Nullable::NonNull expects exactly 1 type argument"
                return []
        let nullableTy = Sem.TypeRecord (Path "Nullable" []) tyArgs'
        exprWithSpan nullableTy $ Sem.NullableCall (Just innerExpr')
-- Ctor call:
--   C |- record R, default-cap R = value, R : (T1, T2, ..., Tn), C |- ei -> Ti
--  ────────────────────────────────────────────────────────────────────────────
--               C |- R <- R(e1, e2, ..., en)
-- Similarly for shared capability, we have
--                   .....
-- ────────────────────────────────────────────
--    C |- Rc[shared] R <- R(e1, e2, ..., en)
-- For now, we do not support regional capability.
inferType
    ( Syn.CtorCallExpr
            Syn.CtorCall
                { Syn.ctorArgs = ctorArgs
                , Syn.ctorName = ctorCallTarget
                , Syn.ctorTyArgs = ctorTyArgs
                }
        ) = do
        L.logTrace_ $ "Tyck: infer ctor call " <> T.pack (show ctorCallTarget)
        records <- State.gets knownRecords
        liftIO (H.lookup records ctorCallTarget) >>= \case
            Nothing -> do
                reportError $ "Record not found: " <> T.pack (show ctorCallTarget)
                exprWithSpan Sem.TypeBottom Sem.Poison
            Just record -> do
                let numGenerics = length (Sem.recordTyParams record)
                -- Allow empty type argument list as syntax sugar for all holes
                let paddedTyArgs =
                        if null ctorTyArgs && numGenerics > 0
                            then replicate numGenerics Nothing
                            else ctorTyArgs
                bounds <- mapM (\(_, gid) -> getGenericBound gid) (Sem.recordTyParams record)
                tyArgs' <- zipWithM tyArgOrMetaHole paddedTyArgs bounds
                logTraceWhen $
                    "Tyck: instantiated ctor generics for "
                        <> T.pack (show ctorCallTarget)
                        <> ": "
                        <> T.pack (show tyArgs')
                checkTypeArgsNum numGenerics paddedTyArgs $ do
                    let genericMap = recordGenericMap record tyArgs'

                    (fields, variantInfo) <- case (Sem.recordKind record, Sem.recordFields record) of
                        (Sem.StructKind, Sem.Named fs) -> return (map (\(n, t, f) -> (Just n, t, f)) fs, Nothing)
                        (Sem.StructKind, Sem.Unnamed fs) -> return (map (\(t, f) -> (Nothing, t, f)) fs, Nothing)
                        (Sem.EnumVariant parentPath idx, Sem.Unnamed fs) -> return (map (\(t, f) -> (Nothing, t, f)) fs, Just (parentPath, idx))
                        (Sem.EnumVariant _ _, Sem.Named _) -> do
                            -- This should be unreachable based on current translation logic
                            reportError "Enum variant cannot have named fields yet"
                            return ([], Nothing)
                        (Sem.EnumKind, _) -> do
                            reportError "Cannot instantiate Enum directly"
                            return ([], Nothing)
                        _ -> do
                            reportError "Invalid record kind/fields combination"
                            return ([], Nothing)

                    let expectedNumParams = length fields
                    checkArgsNum expectedNumParams $ do
                        -- Reconstruct an ordered list of syntatic expressions in the same order as required in the record
                        orderedArgsExprs <-
                            if isJust variantInfo || case Sem.recordFields record of Sem.Unnamed _ -> True; _ -> False
                                then return $ map (Just . snd) ctorArgs
                                else do
                                    let hasNamedArgs = any (isJust . fst) ctorArgs
                                    if hasNamedArgs
                                        then
                                            mapM
                                                ( \(mName, _, _) -> case mName of
                                                    Just name -> case find (\(aName, _) -> aName == Just name) ctorArgs of
                                                        Just (_, expr) -> return $ Just expr
                                                        Nothing -> do
                                                            reportError $ "Missing argument for field: " <> unIdentifier name
                                                            pure Nothing
                                                    Nothing -> error "Named field has no name?"
                                                )
                                                fields
                                        else return $ map (Just . snd) ctorArgs
                        argExprs' <-
                            zipWithM
                                ( \expr (_, ty, flag) -> do
                                    let ty' = substituteTypeParams ty genericMap
                                    let expectedTy = case (flag, ty') of
                                            (True, Sem.TypeRc innerTy Cap.Regional) ->
                                                Sem.TypeRc innerTy Cap.Flex
                                            (True, _) -> error "internal error: should not reach here"
                                            (False, _) -> ty'
                                    case expr of
                                        Just expr' -> checkType expr' expectedTy
                                        Nothing -> do
                                            exprWithSpan Sem.TypeBottom Sem.Poison
                                )
                                orderedArgsExprs
                                fields

                        let recordTy = case variantInfo of
                                Just (parent, _) -> Sem.TypeRecord parent tyArgs'
                                Nothing -> Sem.TypeRecord ctorCallTarget tyArgs'

                        ctorCall <- case variantInfo of
                            Just (parentPath, idx) -> do
                                let innerTy = Sem.TypeRecord ctorCallTarget tyArgs'
                                innerExpr <-
                                    exprWithSpan innerTy $
                                        Sem.CompoundCall
                                            { Sem.compoundCallTarget = ctorCallTarget
                                            , Sem.compoundCallTyArgs = tyArgs'
                                            , Sem.compoundCallArgs = argExprs'
                                            }
                                pure $
                                    Sem.VariantCall
                                        { Sem.variantCallTarget = parentPath
                                        , Sem.variantCallTyArgs = tyArgs'
                                        , Sem.variantCallVariant = idx
                                        , Sem.variantCallArg = innerExpr
                                        }
                            Nothing ->
                                pure
                                    Sem.CompoundCall
                                        { Sem.compoundCallTarget = ctorCallTarget
                                        , Sem.compoundCallTyArgs = tyArgs'
                                        , Sem.compoundCallArgs = argExprs'
                                        }

                        defaultCap <- case variantInfo of
                            Nothing -> return $ case Sem.recordDefaultCap record of
                                Cap.Regional -> Cap.Flex
                                cap -> cap
                            Just (parentPath, _) -> do
                                liftIO (H.lookup records parentPath) >>= \case
                                    Just parentRecord -> return $ case Sem.recordDefaultCap parentRecord of
                                        Cap.Regional -> Cap.Flex
                                        cap -> cap
                                    Nothing -> do
                                        reportError $ "Parent record not found: " <> T.pack (show parentPath)
                                        return Cap.Value

                        when (defaultCap == Cap.Flex) $ do
                            insideRegion' <- State.gets insideRegion
                            unless insideRegion' $ do
                                reportError "Flex capability not allowed outside of regional scope"

                        if defaultCap == Cap.Value
                            then exprWithSpan recordTy ctorCall
                            else do
                                innerExpr <- exprWithSpan recordTy ctorCall
                                exprWithSpan (Sem.TypeRc recordTy defaultCap) (Sem.RcWrap innerExpr defaultCap)
      where
        recordGenericMap :: Sem.Record -> [Sem.Type] -> IntMap.IntMap Sem.Type
        recordGenericMap record assignedTypes =
            let genericIDs = map (\(_, GenericID gid) -> fromIntegral gid) (Sem.recordTyParams record)
             in IntMap.fromList $ zip genericIDs assignedTypes
        checkArgsNum :: Int -> Tyck Sem.Expr -> Tyck Sem.Expr
        checkArgsNum expectedNum argAction = do
            if expectedNum /= length ctorArgs
                then do
                    reportError $
                        "Record "
                            <> T.pack (show ctorCallTarget)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " arguments, but got "
                            <> T.pack (show (length ctorArgs))
                    exprWithSpan Sem.TypeBottom Sem.Poison
                else argAction
        checkTypeArgsNum :: Int -> [Maybe Syn.Type] -> Tyck Sem.Expr -> Tyck Sem.Expr
        checkTypeArgsNum expectedNum actualTyArgs paramAction = do
            if expectedNum /= length actualTyArgs
                then do
                    reportError $
                        "Record "
                            <> T.pack (show ctorCallTarget)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " type arguments, but got "
                            <> T.pack (show (length actualTyArgs))
                    exprWithSpan Sem.TypeBottom Sem.Poison
                else paramAction
inferType (Syn.SpannedExpr (WithSpan subExpr start end)) =
    withSpan (start, end) $ inferType subExpr
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
                reportError $ "Binary arithmetic operation applied to non-numeric type: " <> T.show lhsTy
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
checkType (Syn.SpannedExpr (WithSpan subExpr start end)) ty = withSpan (start, end) $ checkType subExpr ty
checkType expr ty = do
    L.logTrace_ $ "Tyck: checkType expected=" <> T.pack (show ty)
    -- this is apparently not complete. We need to handle lambda/unification
    expr' <- inferType expr
    L.logTrace_ $ "Tyck: inferred type=" <> T.pack (show $ Sem.exprType expr')
    exprTy <- force $ Sem.exprType expr'
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

logTraceWhen :: T.Text -> Tyck ()
logTraceWhen msg = do
    lvl <- State.gets translationLogLevel
    when (lvl == B.LogDebug || lvl == B.LogTrace) $ L.logTrace_ msg
