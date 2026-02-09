{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.DecisionTree (lowerMatch) where

import Data.Int (Int64)
import Effectful (inject)

import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as IR
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Codegen.Value qualified as IR

import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Integral qualified as Int
import Reussir.Core.Data.Lowering.Context (
    ExprResult,
    LocalLoweringContext (..),
    LoweringContext (recordInstances),
    LoweringEff,
 )
import Reussir.Core.Data.UniqueID (VarID (..))
import Reussir.Core.Lowering.Context (
    addIRInstr,
    materializeCurrentBlock,
    nextValue,
    tyValOrICE,
    withVar,
 )
import {-# SOURCE #-} Reussir.Core.Lowering.Expr (lowerExpr, loadIfRef)
import Reussir.Core.Lowering.Type (convertType, mkRefType)
import Reussir.Core.Uitls.HashTable qualified as H

-- | Context for decision tree lowering.
-- Maps PatternVarRef paths to (Current Value, Current Type, Optional Original Value).
newtype DTContext = DTContext
    { dtRefMap :: Map.Map (Seq.Seq Int) (IR.TypedValue, Full.Type, Maybe (IR.TypedValue, Full.Type))
    }

-- | Entry point: lower a match expression.
lowerMatch :: Full.Expr -> Full.DecisionTree -> Full.Type -> LoweringEff ExprResult
lowerMatch scrutinee dt resultTy = do
    scrutVal <- tyValOrICE <$> lowerExpr scrutinee
    let scrutFullTy = Full.exprType scrutinee
    let rootPath = Seq.singleton 0
    let ctx = DTContext{dtRefMap = Map.singleton rootPath (scrutVal, scrutFullTy, Nothing)}
    lowerDecisionTree ctx dt resultTy

-- | Recursively lower a decision tree node.
lowerDecisionTree :: DTContext -> Full.DecisionTree -> Full.Type -> LoweringEff ExprResult
lowerDecisionTree _ Full.DTUncovered ty = do
    -- Panic for uncovered cases, then produce poison
    addIRInstr $ IR.Panic "match is not exhaustive"
    ty' <- inject $ convertType ty
    value <- nextValue
    addIRInstr $ IR.ICall $ IR.IntrinsicCall IR.UBPoison [] [(value, ty')]
    pure $ Just (value, ty')
lowerDecisionTree _ Full.DTUnreachable ty = do
    -- Unreachable: just produce poison
    ty' <- inject $ convertType ty
    value <- nextValue
    addIRInstr $ IR.ICall $ IR.IntrinsicCall IR.UBPoison [] [(value, ty')]
    pure $ Just (value, ty')
lowerDecisionTree ctx (Full.DTLeaf body bindings) _ = do
    withPatternBindings ctx bindings $ lowerExpr body
lowerDecisionTree ctx (Full.DTGuard bindings guardExpr trueDT falseDT) ty = do
    withPatternBindings ctx bindings $ do
        (condVal, condTy) <- tyValOrICE <$> lowerExpr guardExpr
        returnTy <- inject $ convertType ty
        thenBlock <- lowerDTAsBlock ctx trueDT ty []
        elseBlock <- lowerDTAsBlock ctx falseDT ty []
        resultVal <- nextValue
        let ifInstr =
                IR.IfThenElse
                    (condVal, condTy)
                    thenBlock
                    (Just elseBlock)
                    $ Just (resultVal, returnTy)
        addIRInstr ifInstr
        pure $ Just (resultVal, returnTy)
lowerDecisionTree ctx (Full.DTSwitch varRef cases) ty = do
    lowerDTSwitch ctx varRef cases ty

-- | Lower a decision tree into a block with specified yield kind.
lowerDTAsBlockWith ::
    IR.YieldKind -> DTContext -> Full.DecisionTree -> Full.Type -> [IR.TypedValue] -> LoweringEff IR.Block
lowerDTAsBlockWith yieldKind ctx dt ty blkArgs = do
    backupBlock <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    res <- lowerDecisionTree ctx dt ty
    addIRInstr $ IR.Yield yieldKind res
    block <- materializeCurrentBlock blkArgs
    State.modify $ \s -> s{currentBlock = backupBlock}
    pure block

-- | Lower a decision tree into a block (for use inside scf.if, scf.index_switch)
lowerDTAsBlock ::
    DTContext -> Full.DecisionTree -> Full.Type -> [IR.TypedValue] -> LoweringEff IR.Block
lowerDTAsBlock = lowerDTAsBlockWith IR.YieldScf

-- | Resolve a PatternVarRef to an IR value.
-- Find the longest matching prefix in the context, then emit RefProject
-- for any remaining suffix.
resolvePatternVarRef :: DTContext -> Full.PatternVarRef -> LoweringEff IR.TypedValue
resolvePatternVarRef ctx (Full.PatternVarRef path) = do
    let (baseVal, baseTy, remainingIndices) = findLongestPrefix (dtRefMap ctx) path
    foldProjections baseVal baseTy remainingIndices
  where
    findLongestPrefix ::
        Map.Map (Seq.Seq Int) (IR.TypedValue, Full.Type, Maybe (IR.TypedValue, Full.Type)) ->
        Seq.Seq Int ->
        (IR.TypedValue, Full.Type, [Int])
    findLongestPrefix refMap fullPath =
        go (Seq.length fullPath) []
      where
        go 0 acc =
            case Map.lookup Seq.empty refMap of
                Just (val, fty, orig) ->
                    if null acc
                        then case orig of
                            Just (origVal, origTy) -> (origVal, origTy, [])
                            Nothing -> (val, fty, [])
                        else (val, fty, acc)
                Nothing -> error "resolvePatternVarRef: root not in context"
        go n acc =
            let prefix = Seq.take n fullPath
             in case Map.lookup prefix refMap of
                    Just (val, fty, orig) ->
                        if null acc
                            then case orig of
                                Just (origVal, origTy) -> (origVal, origTy, [])
                                Nothing -> (val, fty, [])
                            else (val, fty, acc)
                    Nothing ->
                        let idx = Seq.index fullPath (n - 1)
                         in go (n - 1) (idx : acc)

    foldProjections :: IR.TypedValue -> Full.Type -> [Int] -> LoweringEff IR.TypedValue
    foldProjections val _ [] = pure val
    foldProjections val fty (idx : rest) = do
        (val', fieldTy) <- projectOneField val fty idx
        foldProjections val' fieldTy rest

    projectOneField :: IR.TypedValue -> Full.Type -> Int -> LoweringEff (IR.TypedValue, Full.Type)
    projectOneField base@(_, irTy) fty idx = do
        case irTy of
            IR.TypeRef (IR.Ref (IR.TypeExpr sym) atm cap) -> do
                -- Project a ref to a record field
                table <- Reader.asks recordInstances
                record <- H.lookup table sym
                case record of
                    Just r
                        | Full.Components fs <- Full.recordFields r -> do
                            let (_, fieldFullTy, fieldFlag) = fs V.! idx
                            fieldIRTy <- inject $ convertType fieldFullTy
                            actualFieldTy <- case fieldFullTy of
                                Full.TypeRecord fieldSym -> do
                                    fieldRec <- H.lookup table fieldSym
                                    case fieldRec of
                                        Just fr -> case Full.recordDefaultCap fr of
                                            IRType.Value -> pure fieldIRTy
                                            IRType.Regional | fieldFlag -> pure $ IR.TypeNullable (IR.TypeRc $ IR.Rc fieldIRTy atm cap)
                                            IRType.Regional -> pure $ IR.TypeRc $ IR.Rc fieldIRTy atm IRType.Rigid
                                            IRType.Shared -> pure $ IR.TypeRc $ IR.Rc fieldIRTy atm IRType.Shared
                                            other -> error $ "projectOneField: unsupported capability " <> show other
                                        Nothing -> error $ "projectOneField: record not found " <> show fieldSym
                                _ -> pure fieldIRTy
                            let projRef = mkRefType actualFieldTy cap
                            resVal <- nextValue
                            addIRInstr $ IR.RefProject base (fromIntegral idx) (resVal, projRef)
                            pure ((resVal, projRef), fieldFullTy)
                    _ -> error $ "projectOneField: invalid record for " <> show sym
            IR.TypeRc (IR.Rc innerTy _atm cap) -> do
                -- Borrow the Rc first, then project
                let borrowedRef = mkRefType innerTy cap
                borrowVal <- nextValue
                addIRInstr $ IR.RcBorrow base (borrowVal, borrowedRef)
                projectOneField (borrowVal, borrowedRef) fty idx
            IR.TypeRef (IR.Ref (IR.TypeRc (IR.Rc innerTy atm' cap')) _atm _cap) -> do
                -- Load the Rc from ref, then project
                loadVal <- nextValue
                let loadTy = IR.TypeRc (IR.Rc innerTy atm' cap')
                addIRInstr $ IR.RefLoad base (loadVal, loadTy)
                projectOneField (loadVal, loadTy) fty idx
            _ -> error $ "projectOneField: unsupported base type " <> show irTy

-- | Ensure a value is a ref for dispatch operations.
ensureRef :: IR.TypedValue -> Full.Type -> LoweringEff IR.TypedValue
ensureRef val@(_, irTy) _fty = do
    case irTy of
        IR.TypeRef (IR.Ref (IR.TypeRc (IR.Rc innerTy atm cap)) _refAtm _refCap) -> do
            -- Ref to Rc: load the Rc, then borrow it
            loadVal <- nextValue
            let rcTy = IR.TypeRc (IR.Rc innerTy atm cap)
            addIRInstr $ IR.RefLoad val (loadVal, rcTy)
            let borrowedRef = mkRefType innerTy cap
            borrowVal <- nextValue
            addIRInstr $ IR.RcBorrow (loadVal, rcTy) (borrowVal, borrowedRef)
            pure (borrowVal, borrowedRef)
        IR.TypeRef _ -> pure val
        IR.TypeRc (IR.Rc innerTy _atm cap) -> do
            let borrowedRef = mkRefType innerTy cap
            resVal <- nextValue
            addIRInstr $ IR.RcBorrow val (resVal, borrowedRef)
            pure (resVal, borrowedRef)
        _ -> do
            -- Value type: spill to stack
            let spillRef = mkRefType irTy IRType.Unspecified
            resVal <- nextValue
            addIRInstr $ IR.RefSpill val (resVal, spillRef)
            pure (resVal, spillRef)

-- | Bind pattern variables, resolve their refs, load if needed, then run action.
withPatternBindings :: DTContext -> IntMap.IntMap Full.PatternVarRef -> LoweringEff a -> LoweringEff a
withPatternBindings ctx bindings action = do
    let bindingsList = IntMap.toAscList bindings
    go bindingsList
  where
    go [] = action
    go ((varId, pvRef) : rest) = do
        refVal <- resolvePatternVarRef ctx pvRef
        loaded <- loadIfRef refVal
        withVar (VarID (fromIntegral varId)) loaded $ go rest

-- | Lower a DTSwitch node.
lowerDTSwitch ::
    DTContext -> Full.PatternVarRef -> Full.DTSwitchCases -> Full.Type -> LoweringEff ExprResult
lowerDTSwitch ctx varRef (Full.DTSwitchBool trueDT falseDT) ty = do
    scrutRef <- resolvePatternVarRef ctx varRef
    scrutVal <- loadIfRef scrutRef
    returnTy <- inject $ convertType ty
    thenBlock <- lowerDTAsBlock ctx trueDT ty []
    elseBlock <- lowerDTAsBlock ctx falseDT ty []
    resultVal <- nextValue
    let ifInstr =
            IR.IfThenElse
                scrutVal
                thenBlock
                (Just elseBlock)
                $ Just (resultVal, returnTy)
    addIRInstr ifInstr
    pure $ Just (resultVal, returnTy)
lowerDTSwitch ctx varRef (Full.DTSwitchCtor ctorCases) ty = do
    lowerCtorSwitch ctx varRef ctorCases ty
lowerDTSwitch ctx varRef (Full.DTSwitchNullable justDT nothingDT) ty = do
    lowerNullableSwitch ctx varRef justDT nothingDT ty
lowerDTSwitch ctx varRef (Full.DTSwitchInt intMap defaultDT) ty = do
    lowerIntSwitch ctx varRef intMap defaultDT ty
lowerDTSwitch ctx varRef (Full.DTSwitchString strMap defaultDT) ty = do
    lowerStringSwitch ctx varRef strMap defaultDT ty

-- | Lower a ctor switch using VariantDispatch.
lowerCtorSwitch ::
    DTContext ->
    Full.PatternVarRef ->
    V.Vector Full.DecisionTree ->
    Full.Type ->
    LoweringEff ExprResult
lowerCtorSwitch ctx varRef ctorCases ty = do
    let path = Full.unPatternVarRef varRef
    scrutRef <- resolvePatternVarRef ctx varRef
    -- Determine the scrutinee's Full.Type to look up variant info
    scrutFullTy <- lookupTypeAtPath ctx path
    refVal <- ensureRef scrutRef scrutFullTy
    let (_, refIRTy) = refVal
    -- Get the Ref's capability
    let cap = case refIRTy of
            IR.TypeRef (IR.Ref _ _ c) -> c
            _ -> error "lowerCtorSwitch: expected ref type"
    -- Look up the enum's variant symbols (unwrapping Rc if needed)
    table <- Reader.asks recordInstances
    let recordSym = case scrutFullTy of
            Full.TypeRecord sym -> sym
            Full.TypeRc (Full.TypeRecord sym) _ -> sym
            _ -> error $ "lowerCtorSwitch: scrutinee not a record type " <> show scrutFullTy
    variantSyms <- do
        record <- H.lookup table recordSym
        case record of
            Just r | Full.Variants vs <- Full.recordFields r -> pure vs
            _ -> error $ "lowerCtorSwitch: not an enum record " <> show recordSym
    returnTy <- inject $ convertType ty
    -- Build VariantDispData: one arm per variant
    arms <- V.imapM (buildCtorArm path cap returnTy scrutRef scrutFullTy) (V.zip variantSyms ctorCases)
    resultVal <- nextValue
    let dispInstr =
            IR.VariantDispatch
                refVal
                (IR.VariantDispData $ V.toList arms)
                $ Just (resultVal, returnTy)
    addIRInstr dispInstr
    pure $ Just (resultVal, returnTy)
  where
    buildCtorArm ::
        Seq.Seq Int ->
        IR.Capability ->
        IR.Type ->
        IR.TypedValue ->
        Full.Type ->
        Int ->
        (IR.Symbol, Full.DecisionTree) ->
        LoweringEff ([Int64], IR.Block)
    buildCtorArm dispPath cap _returnTy origVal origTy idx (varSym, dt) = do
        -- Block arg: ref to the variant's inner type
        let variantInnerTy = IR.TypeExpr varSym
        let variantRef = mkRefType variantInnerTy cap
        argVal <- nextValue
        let blockArg = (argVal, variantRef)
        -- Update context: rebind the dispatched path to the variant ref
        let variantFullTy = Full.TypeRecord varSym
        let ctx' =
                ctx
                    { dtRefMap =
                        Map.insert
                            dispPath
                            (blockArg, variantFullTy, Just (origVal, origTy))
                            (dtRefMap ctx)
                    }
        -- Build block (VariantDispatch uses reussir.scf.yield)
        backupBlock <- State.gets currentBlock
        State.modify $ \s -> s{currentBlock = Seq.empty}
        res <- lowerDecisionTree ctx' dt ty
        addIRInstr $ IR.Yield IR.YieldReussirScf res
        block <- materializeCurrentBlock [blockArg]
        State.modify $ \s -> s{currentBlock = backupBlock}
        pure ([fromIntegral idx], block)

-- | Lower a nullable switch using NullableDispatch.
lowerNullableSwitch ::
    DTContext ->
    Full.PatternVarRef ->
    Full.DecisionTree ->
    Full.DecisionTree ->
    Full.Type ->
    LoweringEff ExprResult
lowerNullableSwitch ctx varRef justDT nothingDT ty = do
    let path = Full.unPatternVarRef varRef
    scrutRef <- resolvePatternVarRef ctx varRef
    scrutFullTy <- lookupTypeAtPath ctx path
    scrutLoaded <- loadIfRef scrutRef
    returnTy <- inject $ convertType ty
    -- Determine inner type for the nonnull block arg
    let innerFullTy = case scrutFullTy of
            Full.TypeNullable inner -> inner
            _ -> error $ "lowerNullableSwitch: not a nullable type " <> show scrutFullTy
    innerIRTy <- inject $ convertType innerFullTy
    -- Nonnull arm: block arg is the inner value
    argVal <- nextValue
    let blockArg = (argVal, innerIRTy)
    let ctx' =
            ctx
                { dtRefMap =
                    Map.insert
                        path
                        (blockArg, innerFullTy, Just (scrutRef, scrutFullTy))
                        (dtRefMap ctx)
                }
    backupBlock <- State.gets currentBlock
    -- Nonnull block (NullableDispatch uses reussir.scf.yield)
    State.modify $ \s -> s{currentBlock = Seq.empty}
    nonnullRes <- lowerDecisionTree ctx' justDT ty
    addIRInstr $ IR.Yield IR.YieldReussirScf nonnullRes
    nonnullBlock <- materializeCurrentBlock [blockArg]
    -- Null block
    State.modify $ \s -> s{currentBlock = Seq.empty}
    nullRes <- lowerDecisionTree ctx nothingDT ty
    addIRInstr $ IR.Yield IR.YieldReussirScf nullRes
    nullBlock <- materializeCurrentBlock []
    State.modify $ \s -> s{currentBlock = backupBlock}
    resultVal <- nextValue
    let dispInstr =
            IR.NullableDispatch
                scrutLoaded
                nonnullBlock
                nullBlock
                $ Just (resultVal, returnTy)
    addIRInstr dispInstr
    pure $ Just (resultVal, returnTy)

-- | Lower an int switch using IndexCast + IndexSwitch.
lowerIntSwitch ::
    DTContext ->
    Full.PatternVarRef ->
    IntMap.IntMap Full.DecisionTree ->
    Full.DecisionTree ->
    Full.Type ->
    LoweringEff ExprResult
lowerIntSwitch ctx varRef intMap defaultDT ty = do
    let path = Full.unPatternVarRef varRef
    scrutRef <- resolvePatternVarRef ctx varRef
    scrutVal <- loadIfRef scrutRef
    scrutFullTy <- lookupTypeAtPath ctx path
    returnTy <- inject $ convertType ty
    -- Cast to index type
    let castIntrinsic = case scrutFullTy of
            Full.TypeIntegral (Int.Signed _) -> Arith.IndexCast
            Full.TypeIntegral (Int.Unsigned _) -> Arith.IndexCastui
            _ -> error $ "lowerIntSwitch: not an integral type " <> show scrutFullTy
    let indexTy = IR.TypePrim (IR.PrimInt IR.PrimIndex)
    indexVal <- nextValue
    addIRInstr $
        IR.ICall $
            IR.IntrinsicCall
                (IR.Arith castIntrinsic)
                [scrutVal]
                [(indexVal, indexTy)]
    -- Build case blocks
    let casesList = IntMap.toAscList intMap
    caseBlocks <- mapM (buildIntCase returnTy) casesList
    defaultBlock <- lowerDTAsBlock ctx defaultDT ty []
    resultVal <- nextValue
    let switchInstr =
            IR.IndexSwitch
                (indexVal, indexTy)
                caseBlocks
                defaultBlock
                $ Just (resultVal, returnTy)
    addIRInstr switchInstr
    pure $ Just (resultVal, returnTy)
  where
    buildIntCase :: IR.Type -> (IntMap.Key, Full.DecisionTree) -> LoweringEff (Int64, IR.Block)
    buildIntCase _returnTy (key, dt) = do
        block <- lowerDTAsBlock ctx dt ty []
        pure (fromIntegral key, block)

-- | Lower a string switch using StrCast + StrSelect + IfThenElse + IndexSwitch.
lowerStringSwitch ::
    DTContext ->
    Full.PatternVarRef ->
    HashMap.HashMap (XXH3 T.Text) Full.DecisionTree ->
    Full.DecisionTree ->
    Full.Type ->
    LoweringEff ExprResult
lowerStringSwitch ctx varRef strMap defaultDT ty = do
    scrutRef <- resolvePatternVarRef ctx varRef
    scrutVal <- loadIfRef scrutRef
    returnTy <- inject $ convertType ty
    -- Cast global string to local
    let localStrTy = IR.TypeStr IR.Local
    localVal <- nextValue
    addIRInstr $ IR.StrCast scrutVal (localVal, localStrTy)
    -- Extract patterns and build ordered list
    let entries = HashMap.toList strMap
    let (patterns, dts) = unzip [(txt, dt) | (XXH3 txt, dt) <- entries]
    -- StrSelect: find matching pattern index
    let indexTy = IR.TypePrim (IR.PrimInt IR.PrimIndex)
    let boolTy = IR.TypePrim IR.PrimBool
    idxVal <- nextValue
    foundVal <- nextValue
    addIRInstr $
        IR.StrSelect
            (localVal, localStrTy)
            patterns
            (idxVal, indexTy)
            (foundVal, boolTy)
    -- IfThenElse on found: if found, IndexSwitch on index; else default
    -- Build case blocks for each pattern
    caseBlocks <- mapWithIndex buildStrCase dts
    -- Build the IndexSwitch as the "then" branch
    -- We need a default for IndexSwitch too (unreachable)
    unreachableBlock <- lowerDTAsBlock ctx Full.DTUnreachable ty []
    -- Then block: IndexSwitch
    backupBlock <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    switchResultVal <- nextValue
    addIRInstr $
        IR.IndexSwitch
            (idxVal, indexTy)
            caseBlocks
            unreachableBlock
            $ Just (switchResultVal, returnTy)
    addIRInstr $ IR.Yield IR.YieldScf $ Just (switchResultVal, returnTy)
    thenBlock <- materializeCurrentBlock []
    -- Else block: default
    elseBlock <- lowerDTAsBlock ctx defaultDT ty []
    State.modify $ \s -> s{currentBlock = backupBlock}
    resultVal <- nextValue
    let ifInstr =
            IR.IfThenElse
                (foundVal, boolTy)
                thenBlock
                (Just elseBlock)
                $ Just (resultVal, returnTy)
    addIRInstr ifInstr
    pure $ Just (resultVal, returnTy)
  where
    buildStrCase :: Int -> Full.DecisionTree -> LoweringEff (Int64, IR.Block)
    buildStrCase idx dt = do
        block <- lowerDTAsBlock ctx dt ty []
        pure (fromIntegral idx, block)

    mapWithIndex :: (Monad m) => (Int -> a -> m b) -> [a] -> m [b]
    mapWithIndex f xs = mapM (uncurry f) (zip [0 ..] xs)

-- | Look up the Full.Type at a given path, traversing the record table as needed.
lookupTypeAtPath :: DTContext -> Seq.Seq Int -> LoweringEff Full.Type
lookupTypeAtPath ctx path = do
    let (_, baseTy, remaining) = findPrefix (dtRefMap ctx) path
    resolveFieldType baseTy remaining
  where
    findPrefix ::
        Map.Map (Seq.Seq Int) (IR.TypedValue, Full.Type, Maybe (IR.TypedValue, Full.Type)) ->
        Seq.Seq Int ->
        (IR.TypedValue, Full.Type, [Int])
    findPrefix refMap fullPath = go (Seq.length fullPath) []
      where
        go 0 acc =
            case Map.lookup Seq.empty refMap of
                Just (val, fty, _) -> (val, fty, acc)
                Nothing -> error "lookupTypeAtPath: root not in context"
        go n acc =
            let prefix = Seq.take n fullPath
             in case Map.lookup prefix refMap of
                    Just (val, fty, _) -> (val, fty, acc)
                    Nothing ->
                        let idx = Seq.index fullPath (n - 1)
                         in go (n - 1) (idx : acc)

    resolveFieldType :: Full.Type -> [Int] -> LoweringEff Full.Type
    resolveFieldType ty [] = pure ty
    resolveFieldType ty (idx : rest) = do
        table <- Reader.asks recordInstances
        let sym = case ty of
                Full.TypeRecord s -> s
                Full.TypeRc (Full.TypeRecord s) _ -> s
                _ -> error $ "lookupTypeAtPath: cannot project into " <> show ty
        record <- H.lookup table sym
        case record of
            Just r | Full.Components fs <- Full.recordFields r -> do
                let (_, fieldTy, _) = fs V.! idx
                resolveFieldType fieldTy rest
            _ -> error $ "lookupTypeAtPath: invalid record " <> show sym
