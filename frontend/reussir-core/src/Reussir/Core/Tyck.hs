{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Tyck where

import Control.Monad (foldM, unless, zipWithM)
import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex, find)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Effectful (liftIO)
import Effectful.State.Static.Local qualified as State

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

checkFuncType :: Stmt.Function -> Tyck Sem.Expr
checkFuncType func = do
    let name = Stmt.funcName func
    let path = Path name []
    funcTable <- State.gets functions
    mProto <- liftIO $ H.lookup (functionProtos funcTable) path
    case mProto of
        Nothing -> do
            reportError $ "Function not found in table: " <> unIdentifier name
            exprWithSpan Sem.TypeBottom Sem.Poison
        Just proto -> do
            let generics = funcGenerics proto
            withGenericContext generics $ do
                let params = funcParams proto
                withParams params $ do
                    -- this is so sketchy....
                    case Stmt.funcBody func of
                        Just body -> do
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
    annotatedType' <- if flex then evalTypeWithFlexivity ty True else evalType ty
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
                mRecord <- liftIO $ H.lookup knownRecords path
                case mRecord of
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
--  ──────────────────────────────────────────────────────────────────────
--               C |- T <- f(e1, e2, ..., en)
inferType (Syn.FuncCallExpr (Syn.FuncCall{Syn.funcCallName = path, Syn.funcCallTyArgs = tyArgs, Syn.funcCallArgs = argExprs})) = do
    -- Lookup function
    functionTable <- State.gets functions
    callee <- getFunctionProto path functionTable
    case callee of
        Just proto -> do
            let numGenerics = length (funcGenerics proto)
            checkTypeArgsNum numGenerics $ do
                bounds <- mapM (\(_, gid) -> getGenericBound gid) (funcGenerics proto)
                tyArgs' <- zipWithM tyArgOrMetaHole tyArgs bounds
                let genericMap = functionGenericMap proto tyArgs'
                let instantiatedArgTypes = map (\(_, ty) -> substituteTypeParams ty genericMap) (funcParams proto)
                let expectedNumParams = length instantiatedArgTypes
                checkArgsNum expectedNumParams $ do
                    argExprs' <- zipWithM checkType argExprs instantiatedArgTypes
                    let instantiatedRetType = substituteTypeParams (funcReturnType proto) genericMap
                    exprWithSpan instantiatedRetType $
                        Sem.FuncCall
                            { Sem.funcCallTarget = path
                            , Sem.funcCallArgs = argExprs'
                            , Sem.funcCallTyArgs = tyArgs'
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
    checkTypeArgsNum :: Int -> Tyck Sem.Expr -> Tyck Sem.Expr
    checkTypeArgsNum expectedNum paramAction = do
        if expectedNum /= length tyArgs
            then do
                reportError $
                    "Function "
                        <> T.pack (show path)
                        <> " expects "
                        <> T.pack (show expectedNum)
                        <> " type arguments, but got "
                        <> T.pack (show (length tyArgs))
                exprWithSpan Sem.TypeBottom Sem.Poison
            else paramAction
    functionGenericMap :: FunctionProto -> [Sem.Type] -> IntMap.IntMap Sem.Type
    functionGenericMap proto assignedTypes =
        let genericIDs = map (\(_, GenericID gid) -> fromIntegral gid) (funcGenerics proto)
         in IntMap.fromList $ zip genericIDs assignedTypes
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
                , Syn.ctorVariant = ctorVariant
                }
        ) = do
        records <- State.gets knownRecords
        mRecord <- liftIO $ H.lookup records ctorCallTarget
        case mRecord of
            Nothing -> do
                reportError $ "Record not found: " <> T.pack (show ctorCallTarget)
                exprWithSpan Sem.TypeBottom Sem.Poison
            Just record -> do
                bounds <- mapM (\(_, gid) -> getGenericBound gid) (Sem.recordTyParams record)
                tyArgs' <- zipWithM tyArgOrMetaHole ctorTyArgs bounds
                let numGenerics = length (Sem.recordTyParams record)
                checkTypeArgsNum numGenerics $ do
                    let genericMap = recordGenericMap record tyArgs'
                    -- Find variant index if target variant is specified
                    -- Check if the ctor call style fulfills the variant/compound style in the same time
                    (fields, variantIdx) <- case (Sem.recordFields record, ctorVariant) of
                        (Sem.Named fs, Nothing) -> return (map (\(n, t, f) -> (Just n, t, f)) fs, Nothing)
                        (Sem.Unnamed fs, Nothing) -> return (map (\(t, f) -> (Nothing, t, f)) fs, Nothing)
                        (Sem.Variants vs, Just vName) -> case elemIndex vName (map fst vs) of
                            Just idx -> let (_, ts) = vs !! idx in return (map (\t -> (Nothing, t, False)) ts, Just idx)
                            Nothing -> do
                                reportError $ "Variant not found: " <> unIdentifier vName
                                return ([], Nothing)
                        (Sem.Variants _, Nothing) -> do
                            reportError "Expected variant for enum type"
                            return ([], Nothing)
                        (_, Just _) -> do
                            reportError "Unexpected variant for non-enum type"
                            return ([], Nothing)

                    let expectedNumParams = length fields
                    checkArgsNum expectedNumParams $ do
                        -- Reconstruct an ordered list of syntatic expressions in the same order as required in the record
                        orderedArgsExprs <-
                            if isJust ctorVariant || case Sem.recordFields record of Sem.Unnamed _ -> True; _ -> False
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

                        let recordTy = Sem.TypeRecord ctorCallTarget tyArgs'
                        let ctorCall =
                                Sem.CtorCall
                                    { Sem.ctorCallTarget = ctorCallTarget
                                    , Sem.ctorCallTyArgs = tyArgs'
                                    , Sem.ctorCallVariant = variantIdx
                                    , Sem.ctorCallArgs = argExprs'
                                    }

                        let defaultCap = Sem.recordDefaultCap record

                        if defaultCap == Cap.Regional
                            then error "Regional capability not supported yet"
                            else
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
        checkTypeArgsNum :: Int -> Tyck Sem.Expr -> Tyck Sem.Expr
        checkTypeArgsNum expectedNum paramAction = do
            if expectedNum /= length ctorTyArgs
                then do
                    reportError $
                        "Record "
                            <> T.pack (show ctorCallTarget)
                            <> " expects "
                            <> T.pack (show expectedNum)
                            <> " type arguments, but got "
                            <> T.pack (show (length ctorTyArgs))
                    exprWithSpan Sem.TypeBottom Sem.Poison
                else paramAction
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
checkType (Syn.SpannedExpr (WithSpan subExpr start end)) ty = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- checkType subExpr ty
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
checkType expr ty = do
    -- this is apparently not complete. We need to handle lambda/unification
    expr' <- inferType expr
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
