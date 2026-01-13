{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Data.Foldable (Foldable (..))
import Data.HashTable.IO qualified as H
import Data.Int (Int16, Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes, maybeToList)
import Data.Scientific (Scientific)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Log qualified as L
import Effectful.Prim.IORef.Strict (readIORef')
import Effectful.State.Static.Local qualified as State
import GHC.Stack (HasCallStack)
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Context.Symbol qualified as IR
import Reussir.Codegen.Global qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as IR
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Codegen.Location (DBGMetaInfo (DBGFuncArg, DBGLocalVar))
import Reussir.Codegen.Location qualified as DBG
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type (Capability)
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Codegen.Type.Record qualified as IRRecord
import Reussir.Codegen.Value (Value (Value))
import Reussir.Codegen.Value qualified as IR
import Reussir.Core.Generic (GenericSolution)
import Reussir.Core.Mangle (mangleABIName)
import Reussir.Core.Type (substituteGeneric)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Function (FunctionProto (..), functionProtos)
import Reussir.Core.Types.Function qualified as Sem
import Reussir.Core.Types.GenericID (GenericID (GenericID))
import Reussir.Core.Types.Lowering (
    GenericAssignment,
    Lowering,
    LoweringSpan (..),
    LoweringState (..),
    genericAssignment,
 )
import Reussir.Core.Types.Record (
    Record (recordFields),
    RecordFields (Named, Unnamed),
    recordTyParams,
 )
import Reussir.Core.Types.Record qualified as Sem
import Reussir.Core.Types.String (
    StringToken,
    StringUniqifier (StringUniqifier),
 )
import Reussir.Core.Types.Translation (
    TranslationState (knownRecords, stringUniqifier),
    functions,
 )
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic.Repository (Repository, lookupRepositoryAsRange)
import Reussir.Parser.Types.Capability qualified as SemCap
import Reussir.Parser.Types.Lexer (Identifier (unIdentifier), Path (..))
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, takeFileName)

typeAsDbgType :: Sem.Type -> Lowering (Maybe IR.DBGType)
typeAsDbgType ty = do
    ty' <- canonicalType ty
    case ty' of
        Sem.TypeIntegral (Sem.Signed w) -> pure $ do
            prim <- convertIntegralToPrim w
            pure $ IR.Signed prim (T.pack $ "i" ++ show w)
        Sem.TypeIntegral (Sem.Unsigned w) -> pure $ do
            prim <- convertIntegralToPrim w
            pure $ IR.Unsigned prim (T.pack $ "u" ++ show w)
        Sem.TypeFP fpt -> pure $ do
            prim <- convertFloatToPrim fpt
            pure $ IR.FP prim (T.pack $ show fpt)
        Sem.TypeRecord path args -> do
            recordTable <- State.gets (knownRecords . translationState)
            record <- liftIO $ H.lookup recordTable path
            case record of
                Nothing -> pure Nothing
                Just rec -> do
                    let symbol = IR.verifiedSymbol $ mangleABIName (Sem.TypeRecord path args)
                    let generics = Sem.recordTyParams rec
                        gids = map (\(_, GenericID gid) -> fromIntegral gid) generics
                        assignment = IntMap.fromList $ zip gids args

                        subst t =
                            substituteGeneric
                                t
                                (\(GenericID gid') -> IntMap.lookup (fromIntegral gid') assignment)

                        processField (name, fieldTy) = do
                            let fieldTy' = subst fieldTy
                            mDbgTy <- typeAsDbgType fieldTy'
                            pure $ fmap (\d -> (name, d)) mDbgTy

                    fields <- case Sem.recordFields rec of
                        Sem.Named fs -> do
                            let mapped = map (\(id', t, _) -> (unIdentifier id', t)) fs
                            mapM processField mapped
                        Sem.Unnamed fs -> do
                            let mapped = zipWith (\i (t, _) -> (T.show i, t)) [0 :: Int ..] fs
                            mapM processField mapped
                        Sem.Variants vs -> do
                            let mapVariant (id', tys) = case tys of
                                    [] -> (unIdentifier id', Sem.TypeUnit)
                                    [t] -> (unIdentifier id', t)
                                    _ -> (unIdentifier id', Sem.TypeBottom)
                            let mapped = map mapVariant vs
                            mapM processField mapped

                    if any isNothing fields
                        then pure Nothing
                        else
                            pure $
                                Just $
                                    DBG.Record
                                        { DBG.dbgRecordName = unmangledPath path
                                        , DBG.dbgRecordFields = catMaybes fields
                                        , DBG.dbgRecordRep = symbol
                                        , DBG.dbgRecordIsVariant = case Sem.recordKind rec of
                                            Sem.EnumKind -> True
                                            _ -> False
                                        }
        _ -> pure Nothing
  where
    convertIntegralToPrim :: Int16 -> Maybe IRType.PrimitiveInt
    convertIntegralToPrim 8 = Just IRType.PrimInt8
    convertIntegralToPrim 16 = Just IRType.PrimInt16
    convertIntegralToPrim 32 = Just IRType.PrimInt32
    convertIntegralToPrim 64 = Just IRType.PrimInt64
    convertIntegralToPrim 128 = Just IRType.PrimInt128
    convertIntegralToPrim _ = Nothing

    convertFloatToPrim :: Sem.FloatingPointType -> Maybe IRType.PrimitiveFloat
    convertFloatToPrim (Sem.IEEEFloat 16) = Just IRType.PrimFloat16
    convertFloatToPrim (Sem.IEEEFloat 32) = Just IRType.PrimFloat32
    convertFloatToPrim (Sem.IEEEFloat 64) = Just IRType.PrimFloat64
    convertFloatToPrim Sem.BFloat16 = Just IRType.PrimBFloat16
    convertFloatToPrim Sem.Float8 = Just IRType.PrimFloat8
    convertFloatToPrim _ = Nothing

    unmangledPath :: Path -> T.Text
    unmangledPath (Path name components) =
        T.intercalate "::" (map unIdentifier components ++ [unIdentifier name])

    isNothing Nothing = True
    isNothing _ = False

createLoweringState ::
    (IOE :> es) =>
    FilePath -> Repository -> IR.Module -> TranslationState -> Eff es LoweringState
createLoweringState moduleFile repo mod' transState = do
    (dir, base) <- liftIO $ do
        result <- try @SomeException $ canonicalizePath moduleFile
        case result of
            Left _ -> pure ("<unknown>", "<unknown>")
            Right path -> pure (T.pack $ takeDirectory path, T.pack $ takeFileName path)
    pure
        LoweringState
            { currentBlock = Seq.empty
            , moduleBasename = base
            , moduleDirectory = dir
            , srcRepository = repo
            , valueCounter = 0
            , varMap = IntMap.empty
            , translationState = transState
            , genericAssignment = IntMap.empty
            , currentModule = mod'
            , moduleFullPath = moduleFile
            , regionHandle = Nothing
            }

mangleSymbol :: Path -> [Sem.Type] -> Lowering IR.Symbol
mangleSymbol path tyArgs = do
    instantiatedArgs <- mapM canonicalType tyArgs
    return $ IR.verifiedSymbol $ mangleABIName (Sem.TypeRecord path instantiatedArgs)

convertType :: (HasCallStack) => Sem.Type -> Lowering IR.Type
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
    symbol <- mangleSymbol path tyArgs
    pure $ IR.TypeExpr symbol
convertType (Sem.TypeGeneric (GenericID gid)) = do
    State.gets (IntMap.lookup (fromIntegral gid) . genericAssignment) >>= \case
        Just ty' -> convertType ty'
        Nothing -> error $ "Unresolved generic type: T" ++ show gid
convertType (Sem.TypeRc ty cap) = do
    inner <- convertType ty
    pure $
        IR.TypeRc
            IR.Rc
                { IR.rcBoxInner = inner
                , IR.rcBoxCapability = case cap of
                    SemCap.Rigid -> IR.Rigid
                    SemCap.Flex -> IR.Flex
                    SemCap.Shared -> IR.Shared
                    _ -> error $ "Invalid capability: " ++ show cap
                , IR.rcBoxAtomicity = IR.NonAtomic -- TODO: implement atomicity
                }
convertType unknownTy = error $ "Not yet implemented for type: " ++ show unknownTy

-- TODO: fix i128, it is not parsed in frontend anyway
convertIntegral :: Int16 -> Lowering IR.Type
convertIntegral 8 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt8)
convertIntegral 16 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt16)
convertIntegral 32 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt32)
convertIntegral 64 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt64)
convertIntegral 128 = pure $ IR.TypePrim (IR.PrimInt IR.PrimInt128)
convertIntegral w = error $ "Unsupported integer width: " ++ show w

-- TODO: fix f128, it is not parsed in frontend anyway
convertFloat :: Int16 -> Lowering IR.Type
convertFloat 16 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat16)
convertFloat 32 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat32)
convertFloat 64 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat64)
convertFloat 128 = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat128)
convertFloat w = error $ "Unsupported float width: " ++ show w

lookupLocation :: (Int64, Int64) -> Lowering (Maybe IR.Location)
lookupLocation (start, end) = do
    base <- State.gets moduleBasename
    path <- State.gets moduleFullPath
    repo <- State.gets srcRepository
    case lookupRepositoryAsRange repo (path, start, end) of
        Nothing ->
            error $
                "Failed to lookup source location for path "
                    ++ show path
                    ++ " in byte range "
                    ++ show start
                    ++ "-"
                    ++ show end
        Just (a, b, c, d) -> pure $ Just $ IR.FileLineColRange base a b c d

-- span to location
withLocation :: IR.Instr -> LoweringSpan -> Lowering IR.Instr
withLocation instr NoSpan = pure instr
withLocation instr (LineSpan locSpan) = do
    loc <- lookupLocation locSpan
    case loc of
        Nothing -> pure instr
        Just l -> pure $ IR.WithLoc l instr
withLocation instr (FusedSpan locSpan dbgMeta) = do
    loc <- lookupLocation locSpan
    case loc of
        Nothing -> pure instr
        Just l -> pure $ IR.WithLoc (IR.FusedLoc (Just dbgMeta) [l]) instr

addIRInstr :: IR.Instr -> LoweringSpan -> Lowering ()
addIRInstr instr mSpan = do
    instr' <- withLocation instr mSpan
    State.modify $ \s -> s{currentBlock = currentBlock s Seq.|> instr'}

nextValue :: Lowering IR.Value
nextValue = do
    next <- State.gets valueCounter
    State.modify $ \s -> s{valueCounter = next + 1}
    pure $ Value $ fromIntegral next

createConstant :: IR.Type -> Scientific -> LoweringSpan -> Lowering IR.Value
createConstant ty val mSpan = do
    value' <- nextValue
    let instr = IR.ICall $ IR.IntrinsicCall (IR.Arith (Arith.Constant val)) [] [(value', ty)]
    addIRInstr instr mSpan
    pure value'

-- TODO: handle atomicity
mkRefType :: IR.Type -> Capability -> IR.Type
mkRefType ty cap = IR.TypeRef $ IR.Ref ty IR.NonAtomic cap

lowerExpr :: Sem.Expr -> Lowering IR.Value
lowerExpr (Sem.Expr kind Nothing ty _) = lowerExprInBlock kind ty NoSpan
lowerExpr (Sem.Expr kind (Just span') ty _) = lowerExprInBlock kind ty (LineSpan span')

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

lowerRegionalExpr :: Sem.Expr -> IR.Type -> LoweringSpan -> Lowering IR.Value
lowerRegionalExpr bodyExpr regionTy regionSpan = do
    regionVal <- nextValue
    bodyTy <- convertType $ Sem.exprType bodyExpr
    handleValue <- nextValue
    let handle = (handleValue, IR.TypeRegion)
    State.modify $ \s -> s{regionHandle = Just handle}
    bodyBlock <- lowerExprAsBlock bodyExpr [handle] $ \bodyVal -> do
        addIRInstr (IR.Yield IR.YieldRegion $ Just (bodyVal, bodyTy)) regionSpan
    State.modify $ \s -> s{regionHandle = Nothing}
    let instr = IR.RegionRun bodyBlock $ Just (regionVal, regionTy)
    addIRInstr instr regionSpan
    pure regionVal

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

canonicalType :: Sem.Type -> Lowering Sem.Type
canonicalType ty = do
    assignment <- State.gets genericAssignment
    return $
        substituteGeneric
            ty
            (\(GenericID gid') -> IntMap.lookup (fromIntegral gid') assignment)

-- TODO: span information is not being used to generate debug info
lowerExprInBlock ::
    Sem.ExprKind -> Sem.Type -> LoweringSpan -> Lowering IR.Value
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
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    (IR.Arith Arith.Xori)
                    [(innerValue, irType), (one, irType)]
                    [(value', irType)]
    addIRInstr call exprSpan
    pure value'
lowerExprInBlock (Sem.Negate innerExpr) ty exprSpan = do
    innerValue <- lowerExpr innerExpr
    irType <- convertType ty
    if IRType.isFloatType irType
        then do
            value' <- nextValue
            let call =
                    IR.ICall $
                        IR.IntrinsicCall
                            (IR.Arith $ Arith.Negf $ Arith.FastMathFlag 0)
                            [(innerValue, irType)]
                            [(value', irType)]
            addIRInstr call exprSpan
            pure value'
        else do
            zero <- createConstant irType 0 exprSpan
            value' <- nextValue
            let call =
                    IR.ICall $
                        IR.IntrinsicCall
                            (IR.Arith $ Arith.Subi $ Arith.iofNone)
                            [(zero, irType), (innerValue, irType)]
                            [(value', irType)]
            addIRInstr call exprSpan
            pure value'
lowerExprInBlock (Sem.Arith lhs op rhs) ty exprSpan = do
    lhsVal <- lowerExpr lhs
    rhsVal <- lowerExpr rhs
    ty' <- canonicalType ty
    irType <- convertType ty'
    let intrinsic = case op of
            Sem.Add -> case ty' of
                Sem.TypeFP _ -> IR.Arith $ Arith.Addf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Addi Arith.iofNone
                _ -> error "Unsupported type for Add"
            Sem.Sub -> case ty' of
                Sem.TypeFP _ -> IR.Arith $ Arith.Subf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Subi Arith.iofNone
                _ -> error "Unsupported type for Sub"
            Sem.Mul -> case ty' of
                Sem.TypeFP _ -> IR.Arith $ Arith.Mulf (Arith.FastMathFlag 0)
                Sem.TypeIntegral _ -> IR.Arith $ Arith.Muli Arith.iofNone
                _ -> error "Unsupported type for Mul"
            Sem.Div -> case ty' of
                Sem.TypeFP _ -> IR.Arith $ Arith.Divf (Arith.FastMathFlag 0)
                Sem.TypeIntegral (Sem.Signed _) -> IR.Arith Arith.Divsi
                Sem.TypeIntegral (Sem.Unsigned _) -> IR.Arith Arith.Divui
                _ -> error "Unsupported type for Div"
            Sem.Mod -> case ty' of
                Sem.TypeFP _ -> IR.Arith $ Arith.Remf (Arith.FastMathFlag 0)
                Sem.TypeIntegral (Sem.Signed _) -> IR.Arith Arith.Remsi
                Sem.TypeIntegral (Sem.Unsigned _) -> IR.Arith Arith.Remui
                _ -> error "Unsupported type for Mod"
    resVal <- nextValue
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    intrinsic
                    [(lhsVal, irType), (rhsVal, irType)]
                    [(resVal, irType)]
    addIRInstr call exprSpan
    pure resVal
lowerExprInBlock (Sem.Cmp lhs op rhs) ty exprSpan = do
    lhsVal <- lowerExpr lhs
    rhsVal <- lowerExpr rhs
    lhsTy <- canonicalType (Sem.exprType lhs)
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
            _ -> error $ "Unsupported type for Cmp " ++ show lhsIRTy

    resVal <- nextValue
    irType <- convertType ty
    let call =
            IR.ICall $
                IR.IntrinsicCall
                    intrinsic
                    [(lhsVal, lhsIRTy), (rhsVal, lhsIRTy)]
                    [(resVal, irType)]
    addIRInstr call exprSpan
    pure resVal
lowerExprInBlock (Sem.Cast innerExpr targetTy) _ exprSpan = do
    innerVal <- lowerExpr innerExpr
    innerSemTy <- canonicalType $ Sem.exprType innerExpr
    innerIRTy <- convertType innerSemTy
    targetIRTy <- convertType targetTy
    targetTy' <- canonicalType targetTy

    if innerIRTy == targetIRTy
        then pure innerVal
        else case (innerSemTy, targetTy') of
            (Sem.TypeIntegral _, Sem.TypeBool) -> do
                zero <- createConstant innerIRTy 0 exprSpan
                resVal <- nextValue
                let call =
                        IR.ICall $
                            IR.IntrinsicCall
                                (IR.Arith $ Arith.Cmpi Arith.CINe)
                                [(innerVal, innerIRTy), (zero, innerIRTy)]
                                [(resVal, targetIRTy)]
                addIRInstr call exprSpan
                pure resVal
            _ -> do
                let intrinsic = case (innerSemTy, targetTy') of
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
                        _ ->
                            error $ "Unsupported cast from " ++ show innerSemTy ++ " to " ++ show targetTy

                resVal <- nextValue
                let call =
                        IR.ICall $
                            IR.IntrinsicCall intrinsic [(innerVal, innerIRTy)] [(resVal, targetIRTy)]
                addIRInstr call exprSpan
                pure resVal
lowerExprInBlock
    ( Sem.Let
            { Sem.letVarID = varID
            , Sem.letVarExpr = varExpr
            , Sem.letBodyExpr = bodyExpr
            , Sem.letVarName = name
            }
        )
    _
    exprSpan = do
        varSpan' <- case Sem.exprSpan varExpr of
            Nothing -> pure NoSpan
            Just span' ->
                maybe NoSpan (\dbgTy -> FusedSpan span' $ DBGLocalVar dbgTy (unIdentifier name))
                    <$> typeAsDbgType (Sem.exprType varExpr)
        varValue <-
            lowerExprInBlock (Sem.exprKind varExpr) (Sem.exprType varExpr) varSpan'
        varIRType <- convertType (Sem.exprType varExpr)
        withVar varID (varValue, varIRType) $
            lowerExprInBlock (Sem.exprKind bodyExpr) (Sem.exprType bodyExpr) exprSpan
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
lowerExprInBlock Sem.Poison ty exprSpan = do
    ty' <- convertType ty
    value <- nextValue
    let intrinsicCall = IR.IntrinsicCall IR.UBPoison [] [(value, ty')]
    let instr = IR.ICall intrinsicCall
    addIRInstr instr exprSpan
    pure value
-- TODO: erase unit and allow zero return funcall
lowerExprInBlock (Sem.FuncCall callee tyArgs args regional) ty exprSpan = do
    -- if a function is regional, its first argument is the region handle
    handle <-
        if regional
            then maybeToList <$> State.gets regionHandle
            else pure []
    calleeSymbol <- mangleSymbol callee tyArgs
    argVals <- mapM lowerExpr args
    argTys <- mapM (convertType . Sem.exprType) args
    let typedArgs = zip argVals argTys
    retTy <- convertType ty
    retVal <- nextValue
    let instr = IR.FCall $ IR.FuncCall calleeSymbol (handle <> typedArgs) $ Just (retVal, retTy)
    addIRInstr instr exprSpan
    pure retVal
-- Compound CtorCall
-- No need to handle ty args in this level as the ty already encode enough information
lowerExprInBlock (Sem.CtorCall _ _ Nothing args) ty exprSpan = do
    argVals <- mapM lowerExpr args
    argTys <- mapM (convertType . Sem.exprType) args
    let typedArgs = zip argVals argTys
    retTy <- convertType ty
    retVal <- nextValue
    let instr = IR.CompoundCreate typedArgs (retVal, retTy)
    addIRInstr instr exprSpan
    pure retVal
-- Variant CtorCall: TODO, need to reword the translation from upper level
lowerExprInBlock (Sem.CtorCall _ _ (Just _) _) _ _ =
    error "TODO: need to rework the codegen representation"
-- Projection
-- TODO: handle chain of projection
lowerExprInBlock (Sem.ProjChain _ []) _ _ =
    error "empty project chain?"
lowerExprInBlock (Sem.ProjChain baseExpr [index]) _ exprSpan = do
    baseExpr' <- lowerExpr baseExpr
    let baseExprTy = Sem.exprType baseExpr
    baseExprTy' <- convertType baseExprTy
    let refTy = mkRefType baseExprTy' IR.Unspecified
    spilledVal <- nextValue
    let spilledInstr = IR.RefSpill (baseExpr', baseExprTy') (spilledVal, refTy)
    addIRInstr spilledInstr exprSpan
    case baseExprTy of
        Sem.TypeRecord recordName tyArgs -> do
            recordTable <- State.gets (knownRecords . translationState)
            record <- liftIO $ H.lookup recordTable recordName
            case record of
                Nothing -> error $ "Record type not found during lowering: " ++ show recordName
                Just record' -> do
                    let generics = recordTyParams record'
                    let gids = map (\(_, GenericID gid) -> fromIntegral gid) generics
                    let newAssignment = IntMap.fromList $ zip gids tyArgs

                    oldAssignment <- State.gets genericAssignment
                    let combinedAssignment = IntMap.union newAssignment oldAssignment
                    State.modify $ \s -> s{genericAssignment = combinedAssignment}

                    let fieldTy = lookupFieldType $ recordFields record'
                    fieldTy' <- convertType fieldTy

                    State.modify $ \s -> s{genericAssignment = oldAssignment}

                    let fieldRefTy = mkRefType fieldTy' IR.Unspecified
                    projVal <- nextValue
                    let projInstr = IR.RefProject (spilledVal, refTy) (fromIntegral index) (projVal, fieldRefTy)
                    addIRInstr projInstr exprSpan
                    loadVal <- nextValue
                    let loadInstr = IR.RefLoad (projVal, fieldRefTy) (loadVal, fieldTy')
                    addIRInstr loadInstr exprSpan
                    pure loadVal
        _ -> error "non-value record projection is not handled yet"
  where
    lookupFieldType :: RecordFields -> Sem.Type
    lookupFieldType (Named fields) =
        let (_, ty', _) = fields !! (fromIntegral index) in ty'
    lookupFieldType (Unnamed fields) =
        fst $ fields !! (fromIntegral index)
    lookupFieldType _ = error "cannot project out of variant"
lowerExprInBlock (Sem.RunRegion bodyExpr) regionTy exprSpan = do
    regionTy' <- convertType regionTy
    lowerRegionalExpr bodyExpr regionTy' exprSpan
lowerExprInBlock (Sem.RcWrap bodyExpr cap) ty exprSpan = do
    handle <- State.gets regionHandle
    inner <- lowerExpr bodyExpr
    innerTy <- convertType (Sem.exprType bodyExpr)
    ty' <- convertType ty
    let handle' = case cap of
            SemCap.Flex -> handle
            _ -> Nothing
    resultVal <- nextValue
    let instr = IR.RcCreate (inner, innerTy) handle' (resultVal, ty')
    addIRInstr instr exprSpan
    pure resultVal
lowerExprInBlock kind _ _ = error $ "Not yet implemented for kind: " ++ show kind

-- Translate a function into backend function under generic assignment
-- 1. set generic assignment to the lowering state
-- 2. create function name with converted types using manglePathWithTyArgs
-- 3. if the function has no body, this is an external declaration
--    it should have available-externally linkage, default llvm visibility
--    and private mlir visibility
-- 4. otherwise, the function is defined in this module. For now we always set
--    weak_odr linkage, default llvm visibility and public mlir visibility
-- 5. translate the function span via repository lookup
-- 6. If the function body is provided, lower the body into a block:
--    - the block arguments are function params
--    - introduce function params as variables in the lowering state
--    - lower the function body expression
--    - finalize the block with a return instruction
translateFunction ::
    Path ->
    FunctionProto ->
    (Int64, Int64) ->
    GenericAssignment ->
    Lowering IR.Function
translateFunction path proto locSpan assignment = do
    L.logTrace_ $
        "Lowering: translateFunction "
            <> T.pack (show path)
            <> " (assignment="
            <> T.pack (show (IntMap.size assignment))
            <> ")"
    State.modify $ \s -> s{genericAssignment = assignment, valueCounter = 0}
    let generics = Sem.funcGenerics proto
    let tyArgs =
            map
                ( \(_, GenericID gid) ->
                    case IntMap.lookup (fromIntegral gid) assignment of
                        Just ty -> ty
                        Nothing -> error $ "Generic ID not found in assignment: " ++ show gid
                )
                generics
    symbol <- mangleSymbol path tyArgs

    mBody <- readIORef' (Sem.funcBody proto)
    L.logTrace_ $
        "Lowering: function body present="
            <> case mBody of
                Nothing -> "false"
                Just _ -> "true"
    let (linkage, llvmVis, mlirVis) = case mBody of
            Nothing -> (IR.LnkAvailableExternally, IR.LLVMVisDefault, IR.MLIRVisPrivate)
            Just _ -> (IR.LnkWeakODR, IR.LLVMVisDefault, IR.MLIRVisPublic)

    loc <- lookupLocation locSpan
    loc' <- forM loc $ \span' -> do
        let convertGenerics [] = pure (Just [])
            convertGenerics (t : ts) = do
                mTy <- typeAsDbgType t
                case mTy of
                    Nothing -> pure Nothing
                    Just ty -> do
                        rest <- convertGenerics ts
                        pure $ (ty :) <$> rest

        mAssignedGenerics <- convertGenerics tyArgs
        let unmangledPath :: Path -> T.Text
            unmangledPath (Path name components) =
                T.intercalate "::" (map unIdentifier components ++ [unIdentifier name])
        case mAssignedGenerics of
            Nothing -> pure span'
            Just assignedGenerics -> do
                let functionMeta =
                        IR.DBGFunction
                            { DBG.dbgFuncRawName = unmangledPath path
                            , DBG.dbgFuncTyParams = assignedGenerics
                            }
                return $ IR.FusedLoc (Just functionMeta) [span']
    retTy <- convertType (Sem.funcReturnType proto)

    (bodyBlock, args) <- case mBody of
        Nothing -> pure (Nothing, [])
        Just bodyExpr -> do
            regionParam <-
                if funcIsRegional proto
                    then do
                        handle <- nextValue
                        pure $ Just (handle, IR.TypeRegion)
                    else pure Nothing
            let params = Sem.funcParams proto
            paramValues <-
                mapM
                    ( \(_, ty) -> do
                        irTy <- convertType ty
                        val <- nextValue
                        pure (val, irTy)
                    )
                    params
            let varMapUpdates = zip [0 ..] paramValues
            State.modify $ \s -> s{varMap = IntMap.fromList varMapUpdates, regionHandle = regionParam}
            let argValues = maybeToList regionParam <> paramValues
            block <- lowerExprAsBlock bodyExpr argValues $ \retVal -> do
                let retInstr = IR.Return (Just (retVal, retTy))
                addIRInstr retInstr $ case Sem.exprSpan bodyExpr of
                    Just span' -> LineSpan span'
                    Nothing -> NoSpan

            pure (Just block, argValues)

    -- Generate debug info for function parameters
    let params = Sem.funcParams proto
    dbgArgs <- case mBody of
        Nothing -> pure []
        Just _ -> do
            dbgParams <- forM (zip [1 ..] params) $ \(idx, (name, ty)) -> do
                mDbgTy <- typeAsDbgType ty
                pure $ fmap (\dbgTy -> DBGFuncArg dbgTy (unIdentifier name) idx) mDbgTy
            pure $ catMaybes dbgParams

    pure $
        IR.Function
            { IR.funcLinkage = linkage
            , IR.funcLLVMVisibility = llvmVis
            , IR.funcMLIRVisibility = mlirVis
            , IR.funcBody = bodyBlock
            , IR.funcArgs = args
            , IR.funcDbgArgs = dbgArgs
            , IR.funcLoc = loc'
            , IR.funcResult = retTy
            , IR.funcSymbol = symbol
            }

convertCapability :: SemCap.Capability -> IR.Capability
convertCapability SemCap.Unspecified = IRType.Unspecified
convertCapability SemCap.Shared = IRType.Shared
convertCapability SemCap.Value = IRType.Value
convertCapability SemCap.Flex = IRType.Flex
convertCapability SemCap.Rigid = IRType.Rigid
convertCapability SemCap.Field = IRType.Field
convertCapability SemCap.Regional = IRType.Regional

-- Translate a record into backend record instance under generic assignment
-- 1. set generic assignment to the lowering state
-- 2. translate type parameters and use path together with type params to get the symbol
-- 3. convert record fields to name erased record fields in IR type system
translateRecord ::
    Path -> Sem.Record -> GenericAssignment -> Lowering IR.RecordInstance
translateRecord path record assignment = do
    L.logTrace_ $
        "Lowering: translateRecord "
            <> T.pack (show path)
            <> " (assignment="
            <> T.pack (show (IntMap.size assignment))
            <> ")"
    State.modify $ \s -> s{genericAssignment = assignment}

    let generics = Sem.recordTyParams record
    let tyArgs =
            map
                ( \(_, GenericID gid) ->
                    case IntMap.lookup (fromIntegral gid) assignment of
                        Just ty -> ty
                        Nothing -> error $ "Generic ID not found in assignment: " ++ show gid
                )
                generics
    symbol <- mangleSymbol path tyArgs

    let semKind = Sem.recordKind record
    let irKind = case semKind of
            Sem.StructKind -> IRRecord.Compound
            Sem.EnumKind -> IRRecord.Variant

    irFields <- case (semKind, Sem.recordFields record) of
        (Sem.StructKind, Sem.Named fields) ->
            mapM
                ( \(_, ty, mutable) -> do
                    irTy <- convertType ty
                    pure $ IRRecord.RecordField irTy mutable
                )
                fields
        (Sem.StructKind, Sem.Unnamed fields) ->
            mapM
                ( \(ty, mutable) -> do
                    irTy <- convertType ty
                    pure $ IRRecord.RecordField irTy mutable
                )
                fields
        (Sem.EnumKind, Sem.Variants variants) ->
            mapM
                ( \(_, tys) -> do
                    irTy <- case tys of
                        [] -> pure $ IR.TypePrim IR.PrimUnit
                        [ty] -> convertType ty
                        _ -> error "Multiple fields in variant not supported yet"
                    pure $ IRRecord.RecordField irTy False -- Variants are immutable?
                )
                variants
        _ -> error "Mismatched record kind and fields"

    let irRecord =
            IRRecord.Record
                { IRRecord.defaultCapability = convertCapability (Sem.recordDefaultCap record)
                , IRRecord.fields = irFields
                , IRRecord.kind = irKind
                }

    pure $ IR.RecordInstance (symbol, irRecord)

mangleStrToken :: StringToken -> IR.Symbol
mangleStrToken (x, y) = verifiedSymbol $ T.concat ["str$$", T.show x, "$$", T.show y]

translateModule :: GenericSolution -> Lowering ()
translateModule gSln = do
    L.logTrace_ "Lowering: translateModule start"
    -- Add function per instantiation
    functionTable <- State.gets (functionProtos . functions . translationState)
    functionList <- liftIO $ H.toList functionTable
    L.logTrace_ $
        "Lowering: functions to translate=" <> T.pack (show (length functionList))
    forM_ functionList $ \(path, proto) -> do
        if null (funcGenerics proto)
            then do
                let span' = case Sem.funcSpan proto of
                        Just s -> s
                        Nothing -> (0, 0)
                func <- translateFunction path proto span' IntMap.empty
                mod' <- State.gets currentModule
                let updatedMod = mod'{IR.moduleFunctions = func : IR.moduleFunctions mod'}
                State.modify $ \s -> s{currentModule = updatedMod}
            else do
                assignments <- forM (funcGenerics proto) $ \(_, gid) -> do
                    liftIO (H.lookup gSln gid) >>= \case
                        Just assignment -> return assignment
                        Nothing -> error $ "Generic solution not found for generic ID: " ++ show gid
                let gids = map (\(_, GenericID gid) -> fromIntegral gid) (funcGenerics proto)
                let crossProd = sequence assignments
                forM_ crossProd $ \assignment -> do
                    let localAssignment = IntMap.fromList $ zip gids assignment
                    let span' = case Sem.funcSpan proto of
                            Just s -> s
                            Nothing -> (0, 0)
                    func <- translateFunction path proto span' localAssignment
                    mod' <- State.gets currentModule
                    let updatedMod = mod'{IR.moduleFunctions = func : IR.moduleFunctions mod'}
                    State.modify $ \s -> s{currentModule = updatedMod}
    recordTable <- State.gets (knownRecords . translationState)
    recordList <- liftIO $ H.toList recordTable
    L.logTrace_ $
        "Lowering: records to translate=" <> T.pack (show (length recordList))
    -- Add record instance per instantiation
    forM_ recordList $ \(path, record) -> do
        if null (recordTyParams record)
            then do
                recInst <- translateRecord path record IntMap.empty
                mod' <- State.gets currentModule
                let updatedMod = mod'{IR.recordInstances = recInst : IR.recordInstances mod'}
                State.modify $ \s -> s{currentModule = updatedMod}
            else do
                assignments <- forM (recordTyParams record) $ \(_, gid) -> do
                    liftIO (H.lookup gSln gid) >>= \case
                        Just assignment -> return assignment
                        Nothing -> error $ "Generic solution not found for generic ID: " ++ show gid
                let gids = map (\(_, GenericID gid) -> fromIntegral gid) (recordTyParams record)
                let crossProd = sequence assignments
                forM_ crossProd $ \assignment -> do
                    let localAssignment = IntMap.fromList $ zip gids assignment
                    recInst <- translateRecord path record localAssignment
                    mod' <- State.gets currentModule
                    let updatedMod = mod'{IR.recordInstances = recInst : IR.recordInstances mod'}
                    State.modify $ \s -> s{currentModule = updatedMod}
    -- Add string token
    StringUniqifier storage <- State.gets (stringUniqifier . translationState)
    strList <- liftIO $ H.toList storage
    L.logTrace_ $ "Lowering: string buckets=" <> T.pack (show (length strList))
    forM_ strList $ \(fstComponent, strSeq) ->
        forM_ (zip [0 ..] (toList strSeq)) $ \(sndComponent, strVal) -> do
            let symbol = mangleStrToken (fstComponent, sndComponent)
            mod' <- State.gets currentModule
            let global = IR.GlobalString symbol strVal
            let updatedMod = mod'{IR.globals = global : IR.globals mod'}
            State.modify $ \s -> s{currentModule = updatedMod}
