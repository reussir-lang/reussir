{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Expr where

import Control.Monad (forM_, when)
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific, toBoundedInteger)
import Effectful (inject)
import Reussir.Codegen.Context.Symbol (Symbol, verifiedSymbol)
import Reussir.Parser.Types.Lexer (Identifier (unIdentifier), Path (..))

import Data.IntMap.Strict qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Effectful.Log qualified as L
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as IR
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Codegen.Intrinsics.Math qualified as Math
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Codegen.Value qualified as IR

import Reussir.Core.Data.Lowering.Context (
    ExprResult,
    LocalLoweringContext (..),
    LoweringContext (recordInstances, ownershipAnnotations),
    LoweringEff,
 )
import Reussir.Core.Data.Ownership qualified as Own
import Reussir.Core.Data.UniqueID (ExprID, VarID (..))
import Reussir.Core.Lowering.Context (
    addIRInstr,
    materializeCurrentBlock,
    nextValue,
    tyValOrICE,
    withLocationMetaData,
    withLocationSpan,
    withVar,
 )
import Reussir.Core.Lowering.Debug (typeAsDbgType)
import Reussir.Core.Lowering.Type (convertType, mkRefType)
import Reussir.Core.String (mangleStringToken)

import Reussir.Core.Data.FP qualified as FP
import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Lowering.DecisionTree qualified as DT
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Integral qualified as Int
import Reussir.Core.Data.Operator qualified as Sem
import Reussir.Core.Uitls.HashTable qualified as H

createConstant :: IR.Type -> Scientific -> LoweringEff IR.TypedValue
createConstant ty val = do
    value' <- nextValue
    let instr = IR.ICall $ IR.IntrinsicCall (IR.Arith (Arith.Constant val)) [] [(value', ty)]
    addIRInstr instr
    pure (value', ty)

lowerExpr :: Full.Expr -> LoweringEff ExprResult
lowerExpr expr = do
    let eid = Full.exprID expr
    let kind = Full.exprKind expr
    let ty = Full.exprType expr
    let span' = Full.exprSpan expr
    -- Emit before-annotations
    emitOwnershipBefore eid
    -- Lower the expression
    result <- case span' of
        Nothing -> lowerExprInBlock kind ty
        Just s -> withLocationSpan s $ lowerExprInBlock kind ty
    -- Emit after-annotations
    emitOwnershipAfter eid result
    pure result

-- | Emit ownership operations that should happen before an expression
emitOwnershipBefore :: ExprID -> LoweringEff ()
emitOwnershipBefore eid = do
    annotations <- Reader.asks ownershipAnnotations
    case Own.lookupAnnotation eid annotations of
        Nothing -> pure ()
        Just action -> mapM_ emitOwnershipOp (Own.oaBefore action)

-- | Emit ownership operations that should happen after an expression
emitOwnershipAfter :: ExprID -> ExprResult -> LoweringEff ()
emitOwnershipAfter eid result = do
    annotations <- Reader.asks ownershipAnnotations
    case Own.lookupAnnotation eid annotations of
        Nothing -> pure ()
        Just action -> do
            forM_ (Own.oaAfter action) $ \op -> case op of
                Own.OInc -> case result of
                    Just val -> addIRInstr (IR.RcInc val)
                    Nothing -> pure ()
                Own.ODec -> case result of
                    Just val -> addIRInstr (IR.RcDec val)
                    Nothing -> pure ()
                Own.ODecVar (VarID vid) -> do
                    varMap' <- State.gets @LocalLoweringContext varMap
                    case IntMap.lookup (fromIntegral vid) varMap' of
                        Just val -> addIRInstr (IR.RcDec val)
                        Nothing -> pure ()
                Own.OIncVar (VarID vid) -> do
                    varMap' <- State.gets @LocalLoweringContext varMap
                    case IntMap.lookup (fromIntegral vid) varMap' of
                        Just val -> addIRInstr (IR.RcInc val)
                        Nothing -> pure ()

-- | Emit a single ownership operation (for before-ops that don't reference result)
emitOwnershipOp :: Own.OwnershipOp -> LoweringEff ()
emitOwnershipOp Own.OInc = pure () -- Inc before doesn't make sense without value
emitOwnershipOp Own.ODec = pure () -- Dec before doesn't make sense without value
emitOwnershipOp (Own.ODecVar (VarID vid)) = do
    varMap' <- State.gets @LocalLoweringContext varMap
    case IntMap.lookup (fromIntegral vid) varMap' of
        Just val -> addIRInstr (IR.RcDec val)
        Nothing -> pure ()
emitOwnershipOp (Own.OIncVar (VarID vid)) = do
    varMap' <- State.gets @LocalLoweringContext varMap
    case IntMap.lookup (fromIntegral vid) varMap' of
        Just val -> addIRInstr (IR.RcInc val)
        Nothing -> pure ()

-- lower expression as a block with given block arguments and finalizer
lowerExprAsBlock ::
    Full.Expr ->
    [IR.TypedValue] ->
    (ExprResult -> LoweringEff ()) ->
    LoweringEff IR.Block
lowerExprAsBlock expr blkArgs finalizer = do
    backupBlock <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    lastVal <- lowerExpr expr
    finalizer lastVal
    res <- materializeCurrentBlock blkArgs
    when (null $ IR.blkBody res) $
        L.logAttention_ $
            "Lowered empty block: "
                <> T.show res
                <> " from:\n\
                   \t"
                <> T.show expr
    State.modify $ \s -> s{currentBlock = backupBlock}
    pure res

lowerExprInBlock ::
    Full.ExprKind -> Full.Type -> LoweringEff ExprResult
lowerExprInBlock (Full.GlobalStr token) ty = do
    ty' <- inject $ convertType ty
    let sym = verifiedSymbol $ mangleStringToken token
    value' <- nextValue
    let instr = IR.StrLiteral sym (value', ty')
    addIRInstr instr
    pure $ Just (value', ty')
lowerExprInBlock (Full.Constant value) ty = do
    ty' <- inject $ convertType ty
    Just <$> createConstant ty' value
lowerExprInBlock (Full.Not inner) ty = do
    (innerValue, _) <- tyValOrICE <$> lowerExpr inner
    ty' <- inject $ convertType ty
    (one, _) <- createConstant ty' 1
    value' <- nextValue
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    (IR.Arith Arith.Xori)
                    [(innerValue, ty'), (one, ty')]
                    [(value', ty')]
    addIRInstr call
    pure $ Just (value', ty')
lowerExprInBlock (Full.Negate innerExpr) ty = do
    (innerValue, _) <- tyValOrICE <$> lowerExpr innerExpr
    ty' <- inject $ convertType ty
    if IRType.isFloatType ty'
        then do
            value' <- nextValue
            let call =
                    IR.ICall $
                        IR.IntrinsicCall
                            (IR.Arith $ Arith.Negf $ Arith.FastMathFlag 0)
                            [(innerValue, ty')]
                            [(value', ty')]
            addIRInstr call
            pure $ Just (value', ty')
        else do
            (zero, _) <- createConstant ty' 0
            value' <- nextValue
            let call =
                    IR.ICall $
                        IR.IntrinsicCall
                            (IR.Arith $ Arith.Subi $ Arith.iofNone)
                            [(zero, ty'), (innerValue, ty')]
                            [(value', ty')]
            addIRInstr call
            pure $ Just (value', ty')
lowerExprInBlock (Full.Arith lhs op rhs) ty = do
    (lhsVal, _) <- tyValOrICE <$> lowerExpr lhs
    (rhsVal, _) <- tyValOrICE <$> lowerExpr rhs
    ty' <- inject $ convertType ty
    let intrinsic = case op of
            Sem.Add -> case ty of
                Full.TypeFP _ -> IR.Arith $ Arith.Addf (Arith.FastMathFlag 0)
                Full.TypeIntegral _ -> IR.Arith $ Arith.Addi Arith.iofNone
                _ -> error "Unsupported type for Add"
            Sem.Sub -> case ty of
                Full.TypeFP _ -> IR.Arith $ Arith.Subf (Arith.FastMathFlag 0)
                Full.TypeIntegral _ -> IR.Arith $ Arith.Subi Arith.iofNone
                _ -> error "Unsupported type for Sub"
            Sem.Mul -> case ty of
                Full.TypeFP _ -> IR.Arith $ Arith.Mulf (Arith.FastMathFlag 0)
                Full.TypeIntegral _ -> IR.Arith $ Arith.Muli Arith.iofNone
                _ -> error "Unsupported type for Mul"
            Sem.Div -> case ty of
                Full.TypeFP _ -> IR.Arith $ Arith.Divf (Arith.FastMathFlag 0)
                Full.TypeIntegral (Int.Signed _) -> IR.Arith Arith.Divsi
                Full.TypeIntegral (Int.Unsigned _) -> IR.Arith Arith.Divui
                _ -> error "Unsupported type for Div"
            Sem.Mod -> case ty of
                Full.TypeFP _ -> IR.Arith $ Arith.Remf (Arith.FastMathFlag 0)
                Full.TypeIntegral (Int.Signed _) -> IR.Arith Arith.Remsi
                Full.TypeIntegral (Int.Unsigned _) -> IR.Arith Arith.Remui
                _ -> error "Unsupported type for Mod"
    resVal <- nextValue
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    intrinsic
                    [(lhsVal, ty'), (rhsVal, ty')]
                    [(resVal, ty')]
    addIRInstr call
    pure $ Just (resVal, ty')
lowerExprInBlock (Full.Cmp lhs op rhs) ty = do
    (lhsVal, _) <- tyValOrICE <$> lowerExpr lhs
    (rhsVal, _) <- tyValOrICE <$> lowerExpr rhs
    let lhsTy = Full.exprType lhs
    lhsIRTy <- inject $ convertType lhsTy

    let intrinsic = case lhsTy of
            Full.TypeFP _ -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpf Arith.CFOlt (Arith.FastMathFlag 0)
                Sem.Gt -> IR.Arith $ Arith.Cmpf Arith.CFOgt (Arith.FastMathFlag 0)
                Sem.Lte -> IR.Arith $ Arith.Cmpf Arith.CFOle (Arith.FastMathFlag 0)
                Sem.Gte -> IR.Arith $ Arith.Cmpf Arith.CFOge (Arith.FastMathFlag 0)
                Sem.Equ -> IR.Arith $ Arith.Cmpf Arith.CFOeq (Arith.FastMathFlag 0)
                Sem.Neq -> IR.Arith $ Arith.Cmpf Arith.CFOne (Arith.FastMathFlag 0)
            Full.TypeIntegral (Int.Signed _) -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpi Arith.CISlt
                Sem.Gt -> IR.Arith $ Arith.Cmpi Arith.CISgt
                Sem.Lte -> IR.Arith $ Arith.Cmpi Arith.CISle
                Sem.Gte -> IR.Arith $ Arith.Cmpi Arith.CISge
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
            Full.TypeIntegral (Int.Unsigned _) -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpi Arith.CIUlt
                Sem.Gt -> IR.Arith $ Arith.Cmpi Arith.CIUgt
                Sem.Lte -> IR.Arith $ Arith.Cmpi Arith.CIUle
                Sem.Gte -> IR.Arith $ Arith.Cmpi Arith.CIUge
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
            Full.TypeBool -> case op of
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
                _ -> error "Ordered comparison on Bool"
            _ -> error $ "Unsupported type for Cmp " ++ show lhsIRTy

    resVal <- nextValue
    ty' <- inject $ convertType ty
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    intrinsic
                    [(lhsVal, lhsIRTy), (rhsVal, lhsIRTy)]
                    [(resVal, ty')]
    addIRInstr call
    pure $ Just (resVal, ty')
lowerExprInBlock (Full.Cast innerExpr targetTy) _ = do
    (innerVal, innerIRTy) <- tyValOrICE <$> lowerExpr innerExpr
    let innerSemTy = Full.exprType innerExpr
    targetIRTy <- inject $ convertType targetTy

    if innerIRTy == targetIRTy
        then pure $ Just (innerVal, targetIRTy)
        else case (innerSemTy, targetTy) of
            (Full.TypeIntegral _, Full.TypeBool) -> do
                (zero, _) <- createConstant innerIRTy 0
                resVal <- nextValue
                let call =
                        IR.ICall $
                            IR.IntrinsicCall
                                (IR.Arith $ Arith.Cmpi Arith.CINe)
                                [(innerVal, innerIRTy), (zero, innerIRTy)]
                                [(resVal, targetIRTy)]
                addIRInstr call
                pure $ Just (resVal, targetIRTy)
            _ -> do
                let intrinsic = case (innerSemTy, targetTy) of
                        (Full.TypeIntegral (Int.Signed w1), Full.TypeIntegral (Int.Signed w2))
                            | w1 < w2 -> IR.Arith Arith.Extsi
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Full.TypeIntegral (Int.Unsigned w1), Full.TypeIntegral (Int.Unsigned w2))
                            | w1 < w2 -> IR.Arith Arith.Extui
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Full.TypeIntegral (Int.Signed w1), Full.TypeIntegral (Int.Unsigned w2))
                            | w1 < w2 -> IR.Arith Arith.Extsi
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Full.TypeIntegral (Int.Unsigned w1), Full.TypeIntegral (Int.Signed w2))
                            | w1 < w2 -> IR.Arith Arith.Extui
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Full.TypeFP f1, Full.TypeFP f2) ->
                            let w1 = case f1 of FP.IEEEFloat w -> w; FP.BFloat16 -> 16; FP.Float8 -> 8
                                w2 = case f2 of FP.IEEEFloat w -> w; FP.BFloat16 -> 16; FP.Float8 -> 8
                             in if w1 < w2
                                    then IR.Arith $ Arith.Extf (Arith.FastMathFlag 0)
                                    else IR.Arith $ Arith.Truncf Nothing (Arith.FastMathFlag 0)
                        (Full.TypeIntegral (Int.Signed _), Full.TypeFP _) -> IR.Arith Arith.Sitofp
                        (Full.TypeIntegral (Int.Unsigned _), Full.TypeFP _) -> IR.Arith Arith.Uitofp
                        (Full.TypeFP _, Full.TypeIntegral (Int.Signed _)) -> IR.Arith Arith.Fptosi
                        (Full.TypeFP _, Full.TypeIntegral (Int.Unsigned _)) -> IR.Arith Arith.Fptoui
                        (Full.TypeBool, Full.TypeIntegral _) -> IR.Arith Arith.Extui
                        _ ->
                            error $ "Unsupported cast from " ++ show innerSemTy ++ " to " ++ show targetTy

                resVal <- nextValue
                let call =
                        IR.ICall $
                            IR.IntrinsicCall intrinsic [(innerVal, innerIRTy)] [(resVal, targetIRTy)]
                addIRInstr call
                pure $ Just (resVal, targetIRTy)
lowerExprInBlock
    ( Full.Let
            {
            }
        )
    _ = error "Should not lower Let as a standalone expression"
lowerExprInBlock (Full.Sequence subexprs) _ = lowerSequenceExprInBlock subexprs Nothing
lowerExprInBlock (Full.Var (VarID varID)) _ = do
    varMap' <- State.gets @LocalLoweringContext varMap
    case IntMap.lookup (fromIntegral varID) varMap' of
        Nothing -> error $ "Variable not found in lowering: " ++ show varID
        Just val -> pure $ Just val
lowerExprInBlock (Full.ScfIfExpr condExpr thenExpr elseExpr) ty = do
    (condVal, irType) <- tyValOrICE <$> lowerExpr condExpr
    returnTy <- inject $ convertType ty
    thenBlock <- lowerExprAsBlock thenExpr [] $ \thenVal -> do
        let retInstr = IR.Yield IR.YieldScf $ thenVal
        addIRInstr retInstr

    elseBlock <- lowerExprAsBlock elseExpr [] $ \elseVal -> do
        let retInstr = IR.Yield IR.YieldScf $ elseVal
        addIRInstr retInstr

    resultVal <- nextValue
    let ifInstr =
            IR.IfThenElse
                (condVal, irType)
                thenBlock
                (Just elseBlock)
                $ Just (resultVal, returnTy)
    addIRInstr ifInstr
    pure $ Just (resultVal, returnTy)
lowerExprInBlock Full.Poison ty = do
    ty' <- inject $ convertType ty
    value <- nextValue
    let intrinsicCall = IR.IntrinsicCall IR.UBPoison [] [(value, ty')]
    let instr = IR.ICall intrinsicCall
    addIRInstr instr
    pure $ Just (value, ty')
lowerExprInBlock (Full.IntrinsicCall path args) ty =
    Just <$> lowerIntrinsicCallInBlock path args ty
lowerExprInBlock (Full.FuncCall callee args regional) ty = do
    -- if a function is regional, its first argument is the region handle
    handle <-
        if regional
            then maybeToList <$> State.gets regionHandle
            else pure []
    typedArgs <- mapM (fmap tyValOrICE . lowerExpr) args
    retTy <- inject $ convertType ty
    ret <- case ty of
        Full.TypeUnit -> pure Nothing
        _ -> do
            retVal <- nextValue
            pure $ Just (retVal, retTy)
    let instr = IR.FCall $ IR.FuncCall callee (handle <> typedArgs) ret
    addIRInstr instr
    pure ret
-- Compound CtorCall
lowerExprInBlock (Full.CompoundCall args) ty = do
    typedArgs <- mapM (fmap tyValOrICE . lowerExpr) args
    retTy <- inject $ convertType ty
    retVal <- nextValue
    let instr = IR.CompoundCreate typedArgs (retVal, retTy)
    addIRInstr instr
    pure $ Just (retVal, retTy)
lowerExprInBlock (Full.VariantCall variant arg) ty = do
    typedArg <- fmap tyValOrICE $ lowerExpr arg
    retTy <- inject $ convertType ty
    retVal <- nextValue
    let instr = IR.VariantCreate (fromIntegral variant) typedArg (retVal, retTy)
    addIRInstr instr
    pure $ Just (retVal, retTy)
lowerExprInBlock (Full.RegionRun bodyExpr) ty = do
    -- We assume region run returns a value, so we need a result type
    -- The region expression itself (bodyExpr) should have the same return type
    regionTy <- inject $ convertType ty
    regionVal <- nextValue

    handleValue <- nextValue
    let handle = (handleValue, IR.TypeRegion)
    State.modify $ \s -> s{regionHandle = Just handle}

    bodyBlock <- lowerExprAsBlock bodyExpr [handle] $ \bodyRes -> do
        addIRInstr (IR.Yield IR.YieldRegion bodyRes)

    State.modify $ \s -> s{regionHandle = Nothing}
    let instr = IR.RegionRun bodyBlock $ Just (regionVal, regionTy)
    addIRInstr instr
    pure $ Just (regionVal, regionTy)

-- Projection chain handling
lowerExprInBlock (Full.Proj baseExpr indices) _ = do
    baseVal <- tyValOrICE <$> lowerExpr baseExpr
    projVal <- UV.foldM' handleProjection baseVal indices
    Just <$> loadIfRef projVal
lowerExprInBlock (Full.RcWrap innerExpr) ty@(Full.TypeRc _ cap) = do
    innerVal <- tyValOrICE <$> lowerExpr innerExpr
    regionHandle <- case cap of
        IR.Flex -> State.gets regionHandle
        _ -> pure Nothing
    retVal <- nextValue
    retTy <- inject $ convertType ty
    let instr = IR.RcCreate innerVal regionHandle (retVal, retTy)
    addIRInstr instr
    pure $ Just (retVal, retTy)
lowerExprInBlock (Full.NullableCall maybeExpr) ty = do
    retVal <- nextValue
    retTy <- inject $ convertType ty
    maybeExpr' <- mapM (fmap tyValOrICE . lowerExpr) maybeExpr
    let instr = IR.NullableCreate maybeExpr' (retVal, retTy)
    addIRInstr instr
    pure $ Just (retVal, retTy)
lowerExprInBlock (Full.Assign dst idx src) _ = do
    dst'@(_, dstTy) <- tyValOrICE <$> lowerExpr dst
    src'@(_, srcTy) <- tyValOrICE <$> lowerExpr src
    case dstTy of
        IR.TypeRc (IR.Rc recTy@(IR.TypeExpr _) atm IR.Flex) -> do
            let flexRecordRef = IR.TypeRef (IR.Ref recTy atm IR.Flex)
            let fieldRef = IR.TypeRef (IR.Ref srcTy atm IR.Field)
            recRefVal <- nextValue
            let loweredRecRef = (recRefVal, flexRecordRef)
            let borrowOp = IR.RcBorrow dst' loweredRecRef
            addIRInstr borrowOp
            fieldRefVal <- nextValue
            let loweredFieldRef = (fieldRefVal, fieldRef)
            let projOp = IR.RefProject loweredRecRef (fromIntegral idx) loweredFieldRef
            addIRInstr projOp
            let storeOp = IR.RefStore loweredFieldRef src'
            addIRInstr storeOp
            pure Nothing
        _ -> error $ "assign to non-flex rc types: " ++ show dstTy
lowerExprInBlock (Full.Match scrutinee dt) ty =
    DT.lowerMatch scrutinee dt ty
lowerExprInBlock kind ty =
    error $
        "Detailed lowerExprInBlock implementation missing for "
            ++ show kind
            ++ " : "
            ++ show ty

lowerIntrinsicCallInBlock ::
    Path -> [Full.Expr] -> Full.Type -> LoweringEff IR.TypedValue
lowerIntrinsicCallInBlock (Path name ["core", "intrinsic", "math"]) args ty = do
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

    let (valArgs, fmf) =
            if name `elem` floatUnary
                || name `elem` checks
                || name == "fma"
                || name == "fpowi"
                || name `elem` floatBinary
                then
                    let (vals, flag) = (init args, last args)
                        val = case Full.exprKind flag of
                            Full.Constant c -> case toBoundedInteger c of
                                Just i -> Arith.FastMathFlag (fromIntegral (i :: Int))
                                Nothing -> Arith.FastMathFlag 0
                            _ -> Arith.FastMathFlag 0
                     in (vals, val)
                else (args, Arith.FastMathFlag 0)

    typedArgs <- mapM (fmap tyValOrICE . lowerExpr) valArgs

    resTy <- inject $ convertType ty
    resVal <- nextValue
    let typedRes = (resVal, resTy)

    let mnemonic = case unIdentifier name of
            "absf" -> Math.Absf fmf
            "acos" -> Math.Acos fmf
            "acosh" -> Math.Acosh fmf
            "asin" -> Math.Asin fmf
            "asinh" -> Math.Asinh fmf
            "atan" -> Math.Atan fmf
            "atanh" -> Math.Atanh fmf
            "cbrt" -> Math.Cbrt fmf
            "ceil" -> Math.Ceil fmf
            "cos" -> Math.Cos fmf
            "cosh" -> Math.Cosh fmf
            "erf" -> Math.Erf fmf
            "erfc" -> Math.Erfc fmf
            "exp" -> Math.Exp fmf
            "exp2" -> Math.Exp2 fmf
            "expm1" -> Math.Expm1 fmf
            "floor" -> Math.Floor fmf
            "isfinite" -> Math.Isfinite fmf
            "isinf" -> Math.Isinf fmf
            "isnan" -> Math.Isnan fmf
            "isnormal" -> Math.Isnormal fmf
            "log10" -> Math.Log10 fmf
            "log1p" -> Math.Log1p fmf
            "log2" -> Math.Log2 fmf
            "round" -> Math.Round fmf
            "roundeven" -> Math.Roundeven fmf
            "rsqrt" -> Math.Rsqrt fmf
            "sin" -> Math.Sin fmf
            "sinh" -> Math.Sinh fmf
            "sqrt" -> Math.Sqrt fmf
            "tan" -> Math.Tan fmf
            "tanh" -> Math.Tanh fmf
            "trunc" -> Math.Trunc fmf
            "atan2" -> Math.Atan2 fmf
            "copysign" -> Math.Copysign fmf
            "powf" -> Math.Powf fmf
            "fma" -> Math.Fma fmf
            "fpowi" -> Math.Fpowi fmf
            "absi" -> Math.Absi
            "ctlz" -> Math.Ctlz
            "ctpop" -> Math.Ctpop
            "cttz" -> Math.Cttz
            "ipowi" -> Math.Ipowi
            _ -> error $ "Unknown math intrinsic: " <> show name

    let instr = IR.ICall $ IR.IntrinsicCall (IR.Math mnemonic) typedArgs [typedRes]
    addIRInstr instr
    pure (resVal, resTy)
lowerIntrinsicCallInBlock path _ _ = error $ "Not implemented: " <> show path

loadIfRef :: IR.TypedValue -> LoweringEff IR.TypedValue
loadIfRef ref@(_, valTy) = do
    case valTy of
        IR.TypeRef (IR.Ref innerTy _atm _cap) -> do
            resVal <- nextValue
            let instr = IR.RefLoad ref (resVal, innerTy)
            addIRInstr instr
            pure (resVal, innerTy)
        _ -> pure ref

handleProjection :: IR.TypedValue -> Int -> LoweringEff IR.TypedValue
handleProjection base@(_, valTy) index = do
    case valTy of
        IR.TypeRef (IR.Ref (IR.TypeExpr sym) atm cap) -> do
            resVal <- nextValue
            projTy <- projectedType cap atm sym index
            let projRef = mkRefType projTy cap
            let instr = IR.RefProject base (fromIntegral index) (resVal, projRef)
            addIRInstr instr
            pure (resVal, projRef)
        IR.TypeRef (IR.Ref (IR.TypeRc (IR.Rc innerTy _ _)) atm' cap') -> do
            resVal <- nextValue
            let res = (resVal, IR.TypeRc (IR.Rc innerTy atm' cap'))
            let instr = IR.RefLoad base res
            addIRInstr instr
            handleProjection res index
        IR.TypeRc (IR.Rc innerTy _atm cap) -> do
            let borrowedRef = mkRefType innerTy cap
            resVal <- nextValue
            let instr = IR.RcBorrow base (resVal, borrowedRef)
            addIRInstr instr
            handleProjection (resVal, borrowedRef) index
        IR.TypeExpr sym -> do
            projTy <- projectedType IRType.Unspecified IRType.NonAtomic sym index
            resVal <- nextValue
            let instr = IR.RecordExtract base (fromIntegral index) (resVal, projTy)
            addIRInstr instr
            pure (resVal, projTy)
        _ -> error $ "projection not supported for base type " <> show valTy
  where
    projectedType ::
        IR.Capability -> IR.Atomicity -> Symbol -> Int -> LoweringEff IR.Type
    projectedType cap atm sym idx = do
        table <- Reader.asks recordInstances
        record <- H.lookup table sym
        case record of
            Just record'
                | Full.StructKind <- Full.recordKind record'
                , Full.Components fs <- Full.recordFields record' -> do
                    let (_, ty, flag) = fs V.! idx
                    ty' <- inject $ convertType ty
                    case ty of
                        Full.TypeRecord fieldSym -> do
                            fieldRecord <- H.lookup table fieldSym
                            case fieldRecord of
                                Just fr -> case Full.recordDefaultCap fr of
                                    IRType.Value -> pure ty'
                                    IRType.Regional | flag -> pure $ IR.TypeNullable (IR.TypeRc $ IR.Rc ty' atm cap)
                                    IRType.Regional -> pure $ IR.TypeRc $ IR.Rc ty' atm IRType.Rigid
                                    IRType.Shared -> pure $ IR.TypeRc $ IR.Rc ty' atm IRType.Shared
                                    other -> error $ "projectedType: unsupported capability " <> show other
                                Nothing -> error $ "projectedType: record not found " <> show fieldSym
                        _ -> pure ty'
            _ -> error $ "invalid record for projection: " <> show sym

lowerSequenceExprInBlock :: [Full.Expr] -> ExprResult -> LoweringEff ExprResult
lowerSequenceExprInBlock [] res = pure res
lowerSequenceExprInBlock (e : es) _ =
    case Full.exprKind e of
        Full.Let
            { Full.letVarID = varID
            , Full.letVarExpr = varExpr
            , Full.letVarName = name
            , Full.letVarSpan = varSpan
            } -> do
                -- Emit before-annotations for the Let wrapper expression
                emitOwnershipBefore (Full.exprID e)
                let makeDbgTy action = case varSpan of
                        Nothing -> action
                        Just (start, end) -> do
                            dbgTy <- typeAsDbgType (Full.exprType varExpr)
                            let meta = (\d -> IR.DBGLocalVar d (unIdentifier name)) <$> dbgTy
                            case meta of
                                Nothing -> action
                                Just m ->
                                    withLocationSpan (start, end) $
                                        withLocationMetaData m action
                varValue <- makeDbgTy $ tyValOrICE <$> lowerExpr varExpr
                -- Emit after-annotations for the Let wrapper expression
                emitOwnershipAfter (Full.exprID e) (Just varValue)
                withVar varID varValue $ lowerSequenceExprInBlock es Nothing
        _ -> do
            res' <- lowerExpr e
            lowerSequenceExprInBlock es res'
