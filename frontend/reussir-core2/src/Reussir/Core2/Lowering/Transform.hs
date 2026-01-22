{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Lowering.Transform where

import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (maybeToList)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Sequence qualified as Seq
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Effectful (inject, liftIO)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as IR
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Codegen.Intrinsics.Math qualified as Math
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Codegen.Value qualified as IR
import Reussir.Core2.Data.FP qualified as FP
import Reussir.Core2.Data.Full.Expr qualified as Full
import Reussir.Core2.Data.Full.Record qualified as Full
import Reussir.Core2.Data.Full.Type qualified as Full
import Reussir.Core2.Data.Integral qualified as Int
import Reussir.Core2.Data.Lowering.Context (LocalLoweringContext (..), LoweringContext (recordInstances), LoweringEff)
import Reussir.Core2.Data.Operator qualified as Sem
import Reussir.Core2.Data.UniqueID (VarID (..))
import Reussir.Core2.Lowering.Context (addIRInstr, materializeCurrentBlock, nextValue, withLocationMetaData, withLocationSpan, withVar)
import Reussir.Core2.Lowering.Debug (typeAsDbgType)
import Reussir.Core2.Lowering.Type (convertType, mkRefType)
import Reussir.Parser.Types.Lexer (Identifier (unIdentifier), Path (..))

createConstant :: IR.Type -> Scientific -> LoweringEff IR.TypedValue
createConstant ty val = do
    value' <- nextValue
    let instr = IR.ICall $ IR.IntrinsicCall (IR.Arith (Arith.Constant val)) [] [(value', ty)]
    addIRInstr instr
    pure (value', ty)

lowerExpr :: Full.Expr -> LoweringEff IR.TypedValue
lowerExpr (Full.Expr kind Nothing ty _) = lowerExprInBlock kind ty
lowerExpr (Full.Expr kind (Just span') ty _) =
    withLocationSpan span' $ lowerExprInBlock kind ty

-- lower expression as a block with given block arguments and finalizer
lowerExprAsBlock ::
    Full.Expr -> [IR.TypedValue] -> (IR.TypedValue -> LoweringEff ()) -> LoweringEff IR.Block
lowerExprAsBlock expr blkArgs finalizer = do
    backupBlock <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    lastVal <- lowerExpr expr
    finalizer lastVal
    res <- materializeCurrentBlock blkArgs
    State.modify $ \s -> s{currentBlock = backupBlock}
    pure res

lowerExprInBlock ::
    Full.ExprKind -> Full.Type -> LoweringEff IR.TypedValue
lowerExprInBlock (Full.Constant value) ty = do
    ty' <- inject $ convertType ty
    createConstant ty' value
lowerExprInBlock (Full.Not inner) ty = do
    (innerValue, _) <- lowerExpr inner
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
    pure (value', ty')
lowerExprInBlock (Full.Negate innerExpr) ty = do
    (innerValue, _) <- lowerExpr innerExpr
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
            pure (value', ty')
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
            pure (value', ty')
lowerExprInBlock (Full.Arith lhs op rhs) ty = do
    (lhsVal, _) <- lowerExpr lhs
    (rhsVal, _) <- lowerExpr rhs
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
    pure (resVal, ty')
lowerExprInBlock (Full.Cmp lhs op rhs) ty = do
    (lhsVal, _) <- lowerExpr lhs
    (rhsVal, _) <- lowerExpr rhs
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
    pure (resVal, ty')
lowerExprInBlock (Full.Cast innerExpr targetTy) _ = do
    (innerVal, innerIRTy) <- lowerExpr innerExpr
    let innerSemTy = Full.exprType innerExpr
    targetIRTy <- inject $ convertType targetTy

    if innerIRTy == targetIRTy
        then pure (innerVal, targetIRTy)
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
                pure (resVal, targetIRTy)
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
                pure (resVal, targetIRTy)
lowerExprInBlock
    ( Full.Let
            { Full.letVarID = varID
            , Full.letVarExpr = varExpr
            , Full.letBodyExpr = bodyExpr
            , Full.letVarName = name
            , Full.letVarSpan = varSpan
            }
        )
    _ = do
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
        varValue <- makeDbgTy $ lowerExpr varExpr
        withVar varID varValue $
            lowerExpr bodyExpr
lowerExprInBlock (Full.Var (VarID varID)) _ = do
    varMap' <- State.gets @LocalLoweringContext varMap
    case IntMap.lookup (fromIntegral varID) varMap' of
        Nothing -> error $ "Variable not found in lowering: " ++ show varID
        Just val -> pure val
lowerExprInBlock (Full.ScfIfExpr condExpr thenExpr elseExpr) ty = do
    (condVal, irType) <- lowerExpr condExpr
    returnTy <- inject $ convertType ty
    thenBlock <- lowerExprAsBlock thenExpr [] $ \(thenVal, _) -> do
        let retInstr = IR.Yield IR.YieldScf $ Just (thenVal, returnTy)
        addIRInstr retInstr

    elseBlock <- lowerExprAsBlock elseExpr [] $ \(elseVal, _) -> do
        let retInstr = IR.Yield IR.YieldScf $ Just (elseVal, returnTy)
        addIRInstr retInstr

    resultVal <- nextValue
    let ifInstr =
            IR.IfThenElse
                (condVal, irType)
                thenBlock
                (Just elseBlock)
                $ Just (resultVal, returnTy)
    addIRInstr ifInstr
    pure (resultVal, returnTy)
lowerExprInBlock Full.Poison ty = do
    ty' <- inject $ convertType ty
    value <- nextValue
    let intrinsicCall = IR.IntrinsicCall IR.UBPoison [] [(value, ty')]
    let instr = IR.ICall intrinsicCall
    addIRInstr instr
    pure (value, ty')
lowerExprInBlock (Full.IntrinsicCall path args) ty = lowerIntrinsicCallInBlock path args ty
lowerExprInBlock (Full.FuncCall callee args regional) ty = do
    -- if a function is regional, its first argument is the region handle
    handle <-
        if regional
            then maybeToList <$> State.gets regionHandle
            else pure []
    typedArgs <- mapM lowerExpr args
    retTy <- inject $ convertType ty
    retVal <- nextValue
    let instr = IR.FCall $ IR.FuncCall callee (handle <> typedArgs) $ Just (retVal, retTy)
    addIRInstr instr
    pure (retVal, retTy)
-- Compound CtorCall
lowerExprInBlock (Full.CompoundCall args) ty = do
    typedArgs <- mapM lowerExpr args
    retTy <- inject $ convertType ty
    retVal <- nextValue
    let instr = IR.CompoundCreate typedArgs (retVal, retTy)
    addIRInstr instr
    pure (retVal, retTy)
lowerExprInBlock (Full.VariantCall variant arg) ty = do
    typedArg <- lowerExpr arg
    retTy <- inject $ convertType ty
    retVal <- nextValue
    let instr = IR.VariantCreate (fromIntegral variant) typedArg (retVal, retTy)
    addIRInstr instr
    pure (retVal, retTy)
lowerExprInBlock (Full.RegionRun bodyExpr) ty = do
    -- We assume region run returns a value, so we need a result type
    -- The region expression itself (bodyExpr) should have the same return type
    regionTy <- inject $ convertType ty
    regionVal <- nextValue

    handleValue <- nextValue
    let handle = (handleValue, IR.TypeRegion)
    State.modify $ \s -> s{regionHandle = Just handle}

    bodyBlock <- lowerExprAsBlock bodyExpr [handle] $ \(bodyVal, bodyTy) -> do
        addIRInstr (IR.Yield IR.YieldRegion $ Just (bodyVal, bodyTy))

    State.modify $ \s -> s{regionHandle = Nothing}
    let instr = IR.RegionRun bodyBlock $ Just (regionVal, regionTy)
    addIRInstr instr
    pure (regionVal, regionTy)

-- Projection chain handling
lowerExprInBlock (Full.Proj baseExpr indices) _ = do
    baseVal <- lowerExpr baseExpr
    projVal <- UV.foldM' handleProjection baseVal indices
    loadIfRef projVal
lowerExprInBlock (Full.RcWrap innerExpr) ty@(Full.TypeRc _ cap) = do
    innerVal <- lowerExpr innerExpr
    regionHandle <- case cap of
        IR.Flex -> State.gets regionHandle
        _ -> pure Nothing
    retVal <- nextValue
    retTy <- inject $ convertType ty
    let instr = IR.RcCreate innerVal regionHandle (retVal, retTy)
    addIRInstr instr
    pure (retVal, retTy)
lowerExprInBlock kind ty =
    error $
        "Detailed lowerExprInBlock implementation missing for "
            ++ show kind
            ++ " : "
            ++ show ty

lowerIntrinsicCallInBlock :: Path -> [Full.Expr] -> Full.Type -> LoweringEff IR.TypedValue
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
            if name `elem` floatUnary || name `elem` checks || name == "fma" || name == "fpowi" || name `elem` floatBinary
                then
                    let (vals, flag) = (init args, last args)
                        val = case Full.exprKind flag of
                            Full.Constant c -> case toBoundedInteger c of
                                Just i -> Arith.FastMathFlag (fromIntegral (i :: Int))
                                Nothing -> Arith.FastMathFlag 0
                            _ -> Arith.FastMathFlag 0
                     in (vals, val)
                else (args, Arith.FastMathFlag 0)

    typedArgs <- mapM lowerExpr valArgs

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
        IR.TypeRef (IR.Ref innerTy _atm cap) -> do
            resVal <- nextValue
            let innerRef = mkRefType innerTy cap
            let instr = IR.RefLoad ref (resVal, innerRef)
            addIRInstr instr
            pure (resVal, innerRef)
        _ -> pure ref

handleProjection :: IR.TypedValue -> Int -> LoweringEff IR.TypedValue
handleProjection base@(_, valTy) index = do
    case valTy of
        IR.TypeRef (IR.Ref innerTy _atm cap) -> do
            resVal <- nextValue
            let innerRef = mkRefType innerTy cap
            let instr = IR.RefProject base (fromIntegral index) (resVal, innerRef)
            addIRInstr instr
            pure (resVal, innerRef)
        IR.TypeRc (IR.Rc innerTy _atm cap) -> do
            let borrowedRef = mkRefType innerTy cap
            resVal <- nextValue
            let instr = IR.RcBorrow base (resVal, borrowedRef)
            addIRInstr instr
            handleProjection (resVal, borrowedRef) index
        IR.TypeExpr sym -> do
            projTy <- projectedType sym index
            resVal <- nextValue
            let instr = IR.RecordExtract base (fromIntegral index) (resVal, projTy)
            addIRInstr instr
            pure (resVal, projTy)
        _ -> error "projection not supported"
  where
    projectedType :: Symbol -> Int -> LoweringEff IR.Type
    projectedType sym idx = do
        table <- Reader.asks recordInstances
        record <- liftIO $ H.lookup table sym
        case record of
            Just record'
                | Full.StructKind <- Full.recordKind record'
                , Full.Components fs <- Full.recordFields record' -> do
                    let (_, ty, flag) = fs V.! idx
                    ty' <- inject $ convertType ty
                    if flag
                        then pure $ IR.TypeNullable ty'
                        else pure ty'
            _ -> error $ "invalid record for projection: " <> show sym
