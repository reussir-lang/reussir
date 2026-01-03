{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering where

import Data.Foldable (Foldable (..))
import Data.Int (Int64, Int8)
import Data.IntMap.Strict qualified as IntMap
import Data.Scientific (Scientific)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as IR
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Codegen.Value (Value (Value))
import Reussir.Codegen.Value qualified as IR
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Lowering (Lowering, LoweringState (currentBlock, moduleFile, srcRepository, valueCounter, varMap))
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic.Repository (lookupRepositoryAsRange)
import Reussir.Parser.Types.Lexer (Identifier (unIdentifier), Path (..))

-- TODO: currently not mangled at all
manglePath :: Path -> T.Text
manglePath (Path name components) =
    T.intercalate "$$" (map unIdentifier components ++ [unIdentifier name])

-- TODO: appearantly not correct, need to develop a mangle scheme
mangleRecord :: Path -> [Sem.Type] -> IR.Symbol
mangleRecord path tyArgs =
    let base = manglePath path
        args = T.intercalate "$" (map T.show tyArgs)
     in IR.verifiedSymbol $
            if T.null args
                then base
                else base <> "$" <> args <> "$"

convertType :: Sem.Type -> Lowering IR.Type
convertType Sem.TypeBool = pure $ IR.TypePrim IR.PrimBool
convertType Sem.TypeUnit = pure $ IR.TypePrim IR.PrimUnit
convertType (Sem.TypeIntegral (Sem.Signed w)) = convertIntegral w
convertType (Sem.TypeIntegral (Sem.Unsigned w)) = convertIntegral w
convertType (Sem.TypeFP (Sem.IEEEFloat w)) = convertFloat w
convertType (Sem.TypeFP Sem.BFloat16) = pure $ IR.TypePrim (IR.PrimFloat IR.PrimBFloat16)
convertType (Sem.TypeFP Sem.Float8) = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat8)
convertType (Sem.TypeClosure args ret) = do
    args' <- mapM convertType args
    ret' <- convertType ret
    pure $ IR.TypeClosure $ IR.Closure args' ret'
convertType (Sem.TypeRecord path tyArgs) = do
    let symbol = mangleRecord path tyArgs
    pure $ IR.TypeExpr symbol
convertType _ = error "Not yet implemented"

-- TODO: fix i128, it is not parsed in frontend anyway
convertIntegral :: Int8 -> Lowering IR.Type
convertIntegral 8 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt8)
convertIntegral 16 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt16)
convertIntegral 32 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt32)
convertIntegral 64 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt64)
convertIntegral w = error $ "Unsupported integer width: " ++ show w

-- TODO: fix f128, it is not parsed in frontend anyway
convertFloat :: Int8 -> Lowering IR.Type
convertFloat 16 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat16)
convertFloat 32 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat32)
convertFloat 64 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat64)
convertFloat w = error $ "Unsupported float width: " ++ show w

-- span to localtion
withLocation :: IR.Instr -> Maybe (Int64, Int64) -> Lowering IR.Instr
withLocation instr Nothing = pure instr
withLocation instr (Just (start, end)) = do
    modPath <- State.gets moduleFile
    case modPath of
        Nothing -> pure instr
        Just path -> do
            repo <- State.gets srcRepository
            (a, b, c, d) <- case lookupRepositoryAsRange repo (path, start, end) of
                Nothing -> error "Failed to lookup source location"
                Just r -> pure r
            pure $ flip IR.WithLoc instr $ IR.FileLineColRange (T.pack path) a b c d

addIRInstr :: IR.Instr -> Maybe (Int64, Int64) -> Lowering ()
addIRInstr instr mSpan = do
    instr' <- withLocation instr mSpan
    State.modify $ \s -> s{currentBlock = currentBlock s Seq.|> instr'}

nextValue :: Lowering IR.Value
nextValue = do
    next <- State.gets valueCounter
    State.modify $ \s -> s{valueCounter = next + 1}
    pure $ Value $ fromIntegral next

createConstant :: IR.Type -> Scientific -> Maybe (Int64, Int64) -> Lowering IR.Value
createConstant ty val mSpan = do
    value' <- nextValue
    let instr = IR.ICall $ IR.IntrinsicCall (IR.Arith (Arith.Constant val)) [] [(value', ty)]
    addIRInstr instr mSpan
    pure value'

lowerExpr :: Sem.Expr -> Lowering IR.Value
lowerExpr (Sem.Expr kind span' ty) = lowerExprInBlock kind ty span'

-- lower expression as a block with given block arguments and finalizer
lowerExprAsBlock ::
    Sem.Expr -> [IR.TypedValue] -> (IR.Value -> Lowering ()) -> Lowering IR.Block
lowerExprAsBlock expr blkArgs finalizer = do
    backupBlock <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    lastVal <- lowerExpr expr
    finalizer lastVal
    res <- materializeCurrentBlock blkArgs
    State.modify $ \s -> s{currentBlock = backupBlock}
    pure res

materializeCurrentBlock :: [IR.TypedValue] -> Lowering IR.Block
materializeCurrentBlock blkArgs = do
    blockInstrs <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    pure $ IR.Block blkArgs (toList blockInstrs)

withVar :: Sem.VarID -> IR.TypedValue -> Lowering a -> Lowering a
withVar (Sem.VarID vid) val action = do
    backup <- State.gets varMap
    State.modify $ \s -> s{varMap = IntMap.insert (fromIntegral vid) val (varMap s)}
    res <- action
    State.modify $ \s -> s{varMap = backup}
    pure res

-- TODO: span information is not being used to generate debug info
lowerExprInBlock :: Sem.ExprKind -> Sem.Type -> Maybe (Int64, Int64) -> Lowering IR.Value
lowerExprInBlock (Sem.Constant value) ty exprSpan = do
    let arithConstant = Arith.Constant value
    irType <- convertType ty
    value' <- nextValue
    let call = IR.ICall $ IR.IntrinsicCall (IR.Arith arithConstant) [] [(value', irType)]
    addIRInstr call exprSpan
    pure value'
lowerExprInBlock (Sem.Not inner) ty exprSpan = do
    innerValue <- lowerExpr inner
    irType <- convertType ty
    one <- createConstant irType 1 exprSpan
    value' <- nextValue
    let call = IR.ICall $ IR.IntrinsicCall (IR.Arith Arith.Xori) [(innerValue, irType), (one, irType)] [(value', irType)]
    addIRInstr call exprSpan
    pure value'
lowerExprInBlock (Sem.Negate (Sem.Expr innerKind innerSpan innerTy)) ty exprSpan = do
    innerValue <- lowerExprInBlock innerKind innerTy innerSpan
    irType <- convertType ty
    if IRType.isFloatType irType
        then do
            value' <- nextValue
            let call = IR.ICall $ IR.IntrinsicCall (IR.Arith $ Arith.Negf $ Arith.FastMathFlag 0) [(innerValue, irType)] [(value', irType)]
            addIRInstr call exprSpan
            pure value'
        else do
            zero <- createConstant irType 0 exprSpan
            value' <- nextValue
            let call = IR.ICall $ IR.IntrinsicCall (IR.Arith $ Arith.Subi $ Arith.iofNone) [(zero, irType), (innerValue, irType)] [(value', irType)]
            addIRInstr call exprSpan
            pure value'
lowerExprInBlock (Sem.Arith lhs op rhs) ty exprSpan = do
    lhsVal <- lowerExpr lhs
    rhsVal <- lowerExpr rhs
    irType <- convertType ty
    let intrinsic = case op of
            Sem.Add -> case ty of
                Sem.TypeFP _ -> IR.Arith $ Arith.Addf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Addi Arith.iofNone
                _ -> error "Unsupported type for Add"
            Sem.Sub -> case ty of
                Sem.TypeFP _ -> IR.Arith $ Arith.Subf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Subi Arith.iofNone
                _ -> error "Unsupported type for Sub"
            Sem.Mul -> case ty of
                Sem.TypeFP _ -> IR.Arith $ Arith.Mulf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Muli Arith.iofNone
                _ -> error "Unsupported type for Mul"
            Sem.Div -> case ty of
                Sem.TypeFP _ -> IR.Arith $ Arith.Divf (Arith.FastMathFlag 0)
                Sem.TypeIntegral (Sem.Signed _) -> IR.Arith Arith.Divsi
                Sem.TypeIntegral (Sem.Unsigned _) -> IR.Arith Arith.Divui
                _ -> error "Unsupported type for Div"
            Sem.Mod -> case ty of
                Sem.TypeFP _ -> IR.Arith $ Arith.Remf (Arith.FastMathFlag 0)
                Sem.TypeIntegral (Sem.Signed _) -> IR.Arith Arith.Remsi
                Sem.TypeIntegral (Sem.Unsigned _) -> IR.Arith Arith.Remui
                _ -> error "Unsupported type for Mod"
    resVal <- nextValue
    let call = IR.ICall $ IR.IntrinsicCall intrinsic [(lhsVal, irType), (rhsVal, irType)] [(resVal, irType)]
    addIRInstr call exprSpan
    pure resVal
lowerExprInBlock (Sem.Cmp lhs op rhs) ty exprSpan = do
    lhsVal <- lowerExpr lhs
    rhsVal <- lowerExpr rhs
    let lhsTy = Sem.exprType lhs
    lhsIRTy <- convertType lhsTy

    let intrinsic = case lhsTy of
            Sem.TypeFP _ -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpf Arith.CFOlt (Arith.FastMathFlag 0)
                Sem.Gt -> IR.Arith $ Arith.Cmpf Arith.CFOgt (Arith.FastMathFlag 0)
                Sem.Lte -> IR.Arith $ Arith.Cmpf Arith.CFOle (Arith.FastMathFlag 0)
                Sem.Gte -> IR.Arith $ Arith.Cmpf Arith.CFOge (Arith.FastMathFlag 0)
                Sem.Equ -> IR.Arith $ Arith.Cmpf Arith.CFOeq (Arith.FastMathFlag 0)
                Sem.Neq -> IR.Arith $ Arith.Cmpf Arith.CFOne (Arith.FastMathFlag 0)
            Sem.TypeIntegral (Sem.Signed _) -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpi Arith.CISlt
                Sem.Gt -> IR.Arith $ Arith.Cmpi Arith.CISgt
                Sem.Lte -> IR.Arith $ Arith.Cmpi Arith.CISle
                Sem.Gte -> IR.Arith $ Arith.Cmpi Arith.CISge
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
            Sem.TypeIntegral (Sem.Unsigned _) -> case op of
                Sem.Lt -> IR.Arith $ Arith.Cmpi Arith.CIUlt
                Sem.Gt -> IR.Arith $ Arith.Cmpi Arith.CIUgt
                Sem.Lte -> IR.Arith $ Arith.Cmpi Arith.CIUle
                Sem.Gte -> IR.Arith $ Arith.Cmpi Arith.CIUge
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
            Sem.TypeBool -> case op of
                Sem.Equ -> IR.Arith $ Arith.Cmpi Arith.CIEq
                Sem.Neq -> IR.Arith $ Arith.Cmpi Arith.CINe
                _ -> error "Ordered comparison on Bool"
            _ -> error "Unsupported type for Cmp"

    resVal <- nextValue
    irType <- convertType ty
    let call = IR.ICall $ IR.IntrinsicCall intrinsic [(lhsVal, lhsIRTy), (rhsVal, lhsIRTy)] [(resVal, irType)]
    addIRInstr call exprSpan
    pure resVal
lowerExprInBlock (Sem.Cast innerExpr targetTy) _ exprSpan = do
    innerVal <- lowerExpr innerExpr
    let innerSemTy = Sem.exprType innerExpr
    innerIRTy <- convertType innerSemTy
    targetIRTy <- convertType targetTy

    if innerIRTy == targetIRTy
        then pure innerVal
        else case (innerSemTy, targetTy) of
            (Sem.TypeIntegral _, Sem.TypeBool) -> do
                zero <- createConstant innerIRTy 0 exprSpan
                resVal <- nextValue
                let call = IR.ICall $ IR.IntrinsicCall (IR.Arith $ Arith.Cmpi Arith.CINe) [(innerVal, innerIRTy), (zero, innerIRTy)] [(resVal, targetIRTy)]
                addIRInstr call exprSpan
                pure resVal
            _ -> do
                let intrinsic = case (innerSemTy, targetTy) of
                        (Sem.TypeIntegral (Sem.Signed w1), Sem.TypeIntegral (Sem.Signed w2))
                            | w1 < w2 -> IR.Arith Arith.Extsi
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Sem.TypeIntegral (Sem.Unsigned w1), Sem.TypeIntegral (Sem.Unsigned w2))
                            | w1 < w2 -> IR.Arith Arith.Extui
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Sem.TypeIntegral (Sem.Signed w1), Sem.TypeIntegral (Sem.Unsigned w2))
                            | w1 < w2 -> IR.Arith Arith.Extsi
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Sem.TypeIntegral (Sem.Unsigned w1), Sem.TypeIntegral (Sem.Signed w2))
                            | w1 < w2 -> IR.Arith Arith.Extui
                            | w1 > w2 -> IR.Arith $ Arith.Trunci Arith.iofNone
                        (Sem.TypeFP f1, Sem.TypeFP f2) ->
                            let w1 = case f1 of Sem.IEEEFloat w -> w; Sem.BFloat16 -> 16; Sem.Float8 -> 8
                                w2 = case f2 of Sem.IEEEFloat w -> w; Sem.BFloat16 -> 16; Sem.Float8 -> 8
                             in if w1 < w2
                                    then IR.Arith $ Arith.Extf (Arith.FastMathFlag 0)
                                    else IR.Arith $ Arith.Truncf Nothing (Arith.FastMathFlag 0)
                        (Sem.TypeIntegral (Sem.Signed _), Sem.TypeFP _) -> IR.Arith Arith.Sitofp
                        (Sem.TypeIntegral (Sem.Unsigned _), Sem.TypeFP _) -> IR.Arith Arith.Uitofp
                        (Sem.TypeFP _, Sem.TypeIntegral (Sem.Signed _)) -> IR.Arith Arith.Fptosi
                        (Sem.TypeFP _, Sem.TypeIntegral (Sem.Unsigned _)) -> IR.Arith Arith.Fptoui
                        (Sem.TypeBool, Sem.TypeIntegral _) -> IR.Arith Arith.Extui
                        _ -> error $ "Unsupported cast from " ++ show innerSemTy ++ " to " ++ show targetTy

                resVal <- nextValue
                let call = IR.ICall $ IR.IntrinsicCall intrinsic [(innerVal, innerIRTy)] [(resVal, targetIRTy)]
                addIRInstr call exprSpan
                pure resVal
lowerExprInBlock
    ( Sem.Let
            { Sem.letVarID = varID
            , Sem.letVarExpr = varExpr
            , Sem.letBodyExpr = bodyExpr
            }
        )
    _
    _ = do
        varValue <- lowerExpr varExpr
        varIRType <- convertType (Sem.exprType varExpr)
        withVar varID (varValue, varIRType) $ lowerExpr bodyExpr
lowerExprInBlock (Sem.Var varID) _ _ = do
    varMap' <- State.gets varMap
    case IntMap.lookup (fromIntegral $ Sem.unVarID varID) varMap' of
        Nothing -> error $ "Variable not found in lowering: " ++ show varID
        Just (val, _) -> pure val
lowerExprInBlock (Sem.ScfIfExpr condExpr thenExpr elseExpr) ty exprSpan = do
    condVal <- lowerExpr condExpr
    irType <- convertType (Sem.exprType condExpr) -- should be prim-bool (i1)
    returnTy <- convertType ty
    thenBlock <- lowerExprAsBlock thenExpr [] $ \thenVal -> do
        let retInstr = IR.Yield IR.YieldScf $ Just (thenVal, returnTy)
        addIRInstr retInstr exprSpan

    elseBlock <- lowerExprAsBlock elseExpr [] $ \elseVal -> do
        let retInstr = IR.Yield IR.YieldScf $ Just (elseVal, returnTy)
        addIRInstr retInstr exprSpan

    resultVal <- nextValue
    let ifInstr =
            IR.IfThenElse
                (condVal, irType)
                thenBlock
                (Just elseBlock)
                $ Just (resultVal, returnTy)
    addIRInstr ifInstr exprSpan
    pure resultVal
lowerExprInBlock _ _ _ = error "Not yet implemented"
