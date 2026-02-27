{-# LANGUAGE OverloadedStrings #-}
-- Emission instances for IR data types (Linkage, LLVMVisibility) live here
-- alongside their codegen functions, following the same pattern as Type.Emission.
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.IR.Emission (
    instrCodegen,
    functionCodegen,
) where

import Control.Monad (unless, when, (>=>))
import Data.Foldable (forM_, for_)
import Data.Int (Int64)
import Data.String (IsString (fromString))
import Effectful.Log (logAttention_)

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB

import Reussir.Codegen.Context (
    Emission (emit),
    emitIndentation,
    emitLine,
    incIndentation,
 )
import Reussir.Codegen.Context.Codegen (
    Codegen,
    getNewBlockId,
    incIndentationBy,
    withLocation,
    withoutLocation,
 )
import Reussir.Codegen.Context.Emission (
    emitBuilder,
    emitBuilderLine,
    emitBuilderLineM,
    emitLocIfPresent,
    intercalate,
 )
import Reussir.Codegen.Context.Symbol (Symbol, symbolBuilder)
import Reussir.Codegen.IR.Data
import Reussir.Codegen.Intrinsics (intrinsicCallCodegen)
import Reussir.Codegen.Type.Data (isBoolType, isVoidType)
import Reussir.Codegen.Value (TypedValue)

fmtTypedValue :: TypedValue -> Codegen TB.Builder
fmtTypedValue (val, ty) = do
    val' <- emit val
    ty' <- emit ty
    return $ val' <> " : " <> ty'

-- | Common pattern for operations: res = operation (input) : resType
emitUnaryOp :: TB.Builder -> TypedValue -> TypedValue -> Codegen ()
emitUnaryOp opName inputVal (resVal, resTy) = emitLine $ do
    inputVal' <- fmtTypedValue inputVal
    resVal' <- emit resVal
    resTy' <- emit resTy
    emitBuilder $
        resVal' <> " = " <> opName <> " (" <> inputVal' <> ") : " <> resTy'

blockCodegen :: Bool -> Block -> Codegen ()
blockCodegen printArgs blk = do
    emitBuilder "{\n"
    incIndentation $ do
        when printArgs $ do
            blkId <- getNewBlockId
            emitIndentation
            emitBuilder $ "^bb" <> TB.fromDec blkId
            unless (null (blkArgs blk)) $ do
                argList <- mapM fmtTypedValue (blkArgs blk)
                emitBuilder $ "(" <> intercalate ", " argList <> ")"
            emitBuilder ":\n"
        let innerIndent = if printArgs then 1 else 0
        incIndentationBy innerIndent $ forM_ (blkBody blk) instrCodegen
    emitIndentation
    emitBuilder "}"

funcCallCodegen :: FuncCall -> Codegen ()
funcCallCodegen (FuncCall target args result) = emitLine $ do
    argList <- mapM (emit . fst) args
    tyList <- mapM (emit . snd) args
    for_ result $ emit . fst >=> emitBuilder . (<> " = ")
    let target' = symbolBuilder target
    emitBuilder $
        "func.call @\"" <> target' <> "\"(" <> intercalate ", " argList <> ")"
    emitBuilder $ " : (" <> intercalate ", " tyList <> ")"
    retTy <- maybe mempty (emit . snd) result
    emitBuilder $ " -> (" <> retTy <> ")"

nullableDispCodegen ::
    TypedValue -> Block -> Block -> Maybe TypedValue -> Codegen ()
nullableDispCodegen nullDispVal nullDispNonnull nullDispNull nullDispRes = do
    emitIndentation
    for_ nullDispRes $ emit . fst >=> emitBuilder . (<> " = ")
    nullDispVal' <- fmtTypedValue nullDispVal
    emitBuilder $ "reussir.nullable.dispatch (" <> nullDispVal' <> ")"
    for_ nullDispRes $ emit . snd >=> emitBuilder . (" -> " <>)
    emitBuilder " {\n"
    unless (isSingleton (blkArgs nullDispNonnull)) $
        logAttention_ "nonnull region must have exactly one argument"
    unless (null (blkArgs nullDispNull)) $
        logAttention_ "null region must have no arguments"
    incIndentation $ withoutLocation $ do
        emitIndentation
        emitBuilder "nonnull -> "
        blockCodegen True nullDispNonnull
        emitBuilder "\n"
        emitIndentation
        emitBuilder "null -> "
        blockCodegen True nullDispNull
        emitBuilder "\n"
    emitBuilder "}"
    emitLocIfPresent
    emitBuilder "\n"
  where
    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _ = False

rcCreateCodegen :: TypedValue -> Maybe TypedValue -> TypedValue -> Codegen ()
rcCreateCodegen rcCreateVal rcCreateRegion (resVal, resTy) = emitLine $ do
    rcCreateVal' <- fmtTypedValue rcCreateVal
    resVal' <- emit resVal
    resTy' <- emit resTy
    emitBuilder $ resVal' <> " = reussir.rc.create value(" <> rcCreateVal' <> ") "
    for_ rcCreateRegion $
        fmtTypedValue >=> \x -> emitBuilder $ "region(" <> x <> ") "
    emitBuilder $ ": " <> resTy'

rcFreezeCodegen :: TypedValue -> TypedValue -> Codegen ()
rcFreezeCodegen = emitUnaryOp "reussir.rc.freeze"

rcBorrowCodegen :: TypedValue -> TypedValue -> Codegen ()
rcBorrowCodegen = emitUnaryOp "reussir.rc.borrow"

rcIsUniqueCodegen :: TypedValue -> TypedValue -> Codegen ()
rcIsUniqueCodegen rcIsUniqueVal res@(_, resTy) = do
    unless (isBoolType resTy) $ logAttention_ "non-boolean rc.is_unique result"
    emitUnaryOp "reussir.rc.is_unique" rcIsUniqueVal res

panicCodegen :: T.Text -> Codegen ()
panicCodegen message = emitBuilderLine $ "reussir.panic " <> fromString (show message)

returnCodegen :: Maybe TypedValue -> Codegen ()
returnCodegen result = emitLine $ do
    emitBuilder "func.return"
    for_ result $ fmtTypedValue >=> emitBuilder . (" " <>)

nullableCheckCodegen :: TypedValue -> TypedValue -> Codegen ()
nullableCheckCodegen nullChkVal res@(_, resTy) = do
    unless (isBoolType resTy) $ logAttention_ "non-boolean nullable.check result"
    emitUnaryOp "reussir.nullable.check" nullChkVal res

nullableCreateCodegen :: Maybe TypedValue -> TypedValue -> Codegen ()
nullableCreateCodegen nullCreateVal (resVal, resTy) = emitBuilderLineM $ do
    nullCreateVal' <-
        maybe mempty (fmap (\x -> " (" <> x <> ")") <$> fmtTypedValue) nullCreateVal
    nullCreateResVal <- emit resVal
    nullCreateresTy <- emit resTy
    return $
        nullCreateResVal
            <> " = reussir.nullable.create"
            <> nullCreateVal'
            <> " : "
            <> nullCreateresTy

rcIncCodegen :: TypedValue -> Codegen ()
rcIncCodegen rcIncVal = emitBuilderLineM $ do
    rcIncVal' <- fmtTypedValue rcIncVal
    return $ "reussir.rc.inc (" <> rcIncVal' <> ")"

rcDecCodegen :: TypedValue -> Codegen ()
rcDecCodegen rcDecVal = emitBuilderLineM $ do
    rcDecVal' <- fmtTypedValue rcDecVal
    return $ "reussir.rc.dec (" <> rcDecVal' <> ")"

compoundCreateCodegen :: [TypedValue] -> TypedValue -> Codegen ()
compoundCreateCodegen fields (resVal, resTy) = emitBuilderLineM $ do
    fieldVals <- mapM (emit . fst) fields
    fieldTys <- mapM (emit . snd) fields
    resVal' <- emit resVal
    resTy' <- emit resTy
    return $
        if null fieldVals
            then resVal' <> " = reussir.record.compound : " <> resTy'
            else
                resVal'
                    <> " = reussir.record.compound ("
                    <> intercalate ", " fieldVals
                    <> " : "
                    <> intercalate ", " fieldTys
                    <> ") : "
                    <> resTy'

variantCreateCodegen :: Int64 -> TypedValue -> TypedValue -> Codegen ()
variantCreateCodegen tag value (resVal, resTy) = emitBuilderLineM $ do
    value' <- fmtTypedValue value
    resVal' <- emit resVal
    resTy' <- emit resTy
    return $
        resVal'
            <> " = reussir.record.variant ["
            <> TB.fromDec tag
            <> "] ("
            <> value'
            <> ") : "
            <> resTy'

refProjectCodegen :: TypedValue -> Int64 -> TypedValue -> Codegen ()
refProjectCodegen val field (resVal, resTy) = emitBuilderLineM $ do
    val' <- fmtTypedValue val
    resVal' <- emit resVal
    resTy' <- emit resTy
    return $
        resVal'
            <> " = reussir.ref.project ("
            <> val'
            <> ") ["
            <> TB.fromDec field
            <> "] : "
            <> resTy'

refSpillCodegen :: TypedValue -> TypedValue -> Codegen ()
refSpillCodegen = emitUnaryOp "reussir.ref.spilled"

refLoadCodegen :: TypedValue -> TypedValue -> Codegen ()
refLoadCodegen = emitUnaryOp "reussir.ref.load"

refStoreCodegen :: TypedValue -> TypedValue -> Codegen ()
refStoreCodegen target value = emitBuilderLineM $ do
    target' <- fmtTypedValue target
    value' <- fmtTypedValue value
    return $ "reussir.ref.store (" <> target' <> ") (" <> value' <> ")"

refDropCodegen :: TypedValue -> Codegen ()
refDropCodegen val = emitBuilderLineM $ do
    val' <- fmtTypedValue val
    return $ "reussir.ref.drop (" <> val' <> ")"

refAcquireCodegen :: TypedValue -> Codegen ()
refAcquireCodegen val = emitBuilderLineM $ do
    val' <- fmtTypedValue val
    return $ "reussir.ref.acquire (" <> val' <> ")"

regionRunCodegen :: Block -> Maybe TypedValue -> Codegen ()
regionRunCodegen regionRunBody regionRunRes = do
    emitIndentation
    for_ regionRunRes $ emit . fst >=> emitBuilder . (<> " = ")
    emitBuilder "reussir.region.run "
    for_ regionRunRes $ emit . snd >=> emitBuilder . (" -> " <>)
    withoutLocation $ blockCodegen True regionRunBody
    emitLocIfPresent
    emitBuilder "\n"

yieldCodegen :: YieldKind -> Maybe TypedValue -> Codegen ()
yieldCodegen kind result = emitBuilderLineM $ do
    result' <- maybe mempty (fmap (" " <>) . fmtTypedValue) result
    return $ opName <> result'
  where
    opName = case kind of
        YieldClosure -> "reussir.closure.yield"
        YieldRegion -> "reussir.region.yield"
        YieldScf -> "scf.yield"
        YieldReussirScf -> "reussir.scf.yield"

closureCreateCodegen :: Block -> TypedValue -> Codegen ()
closureCreateCodegen body (resVal, resTy) = do
    resVal' <- emit resVal
    resTy' <- emit resTy
    emitIndentation
    emitBuilder $ resVal' <> " = reussir.closure.create -> " <> resTy' <> " {\n"
    incIndentation $ withoutLocation $ do
        emitIndentation
        emitBuilder "body"
        blockCodegen True body
        emitBuilder "\n"
    emitBuilder "}"
    emitLocIfPresent
    emitBuilder "\n"

closureApplyCodegen :: TypedValue -> TypedValue -> TypedValue -> Codegen ()
closureApplyCodegen target arg (resVal, resTy) = emitBuilderLineM $ do
    target' <- fmtTypedValue target
    arg' <- fmtTypedValue arg
    resVal' <- emit resVal
    resTy' <- emit resTy
    return $
        resVal'
            <> " = reussir.closure.apply ("
            <> arg'
            <> ") to ("
            <> target'
            <> ") : "
            <> resTy'

closureEvalCodegen :: TypedValue -> Maybe TypedValue -> Codegen ()
closureEvalCodegen target result = emitLine $ do
    target' <- fmtTypedValue target
    for_ result $ emit . fst >=> emitBuilder . (<> " = ")
    emitBuilder $ "reussir.closure.eval (" <> target' <> ")"
    for_ result $ emit . snd >=> emitBuilder . (" : " <>)

closureUniqifyCodegen :: TypedValue -> TypedValue -> Codegen ()
closureUniqifyCodegen = emitUnaryOp "reussir.closure.uniqify"

recordExtractCodegen :: TypedValue -> Int64 -> TypedValue -> Codegen ()
recordExtractCodegen val fieldIdx (resVal, resTy) = emitBuilderLineM $ do
    val' <- fmtTypedValue val
    resVal' <- emit resVal
    resTy' <- emit resTy
    return $
        resVal'
            <> " = reussir.record.extract ("
            <> val'
            <> ") ["
            <> TB.fromDec fieldIdx
            <> "] : "
            <> resTy'

ifThenElseCodegen ::
    TypedValue -> Block -> Maybe Block -> Maybe TypedValue -> Codegen ()
ifThenElseCodegen (condVal, _) thenBlock elseBlock result = do
    emitIndentation
    for_ result $ emit . fst >=> emitBuilder . (<> " = ")
    condVal' <- emit condVal
    emitBuilder $ "scf.if " <> condVal'
    for_ result $ emit . snd >=> emitBuilder . (" -> " <>) . (<> " ")
    withoutLocation $ blockCodegen False thenBlock
    for_ elseBlock $ \blk -> withoutLocation $ do
        emitBuilder " else "
        blockCodegen False blk
    emitLocIfPresent
    emitBuilder "\n"

variantDispCaseCodegen :: [Int64] -> Block -> Codegen ()
variantDispCaseCodegen tagSets body = do
    emitIndentation
    emitBuilder $ "[" <> intercalate ", " (map TB.fromDec tagSets) <> "] ->"
    withoutLocation $ blockCodegen True body
    emitBuilder "\n"

variantDispCodegen ::
    TypedValue -> VariantDispData -> Maybe TypedValue -> Codegen ()
variantDispCodegen val (VariantDispData cases) result = do
    emitIndentation
    for_ result $ emit . fst >=> emitBuilder . (<> " = ")
    val' <- fmtTypedValue val
    emitBuilder $ "reussir.record.dispatch (" <> val' <> ")"
    for_ result $ emit . snd >=> emitBuilder . (" -> " <>)
    emitBuilder " {\n"
    incIndentation $ forM_ cases $ uncurry variantDispCaseCodegen
    emitBuilderLine "}"

{- | Generate scf.index_switch operation
Syntax:
  [%res =] scf.index_switch %val [-> type]
  case 0 {
    ...
  }
  default {
    ...
  }
-}
indexSwitchCodegen ::
    TypedValue -> [(Int64, Block)] -> Block -> Maybe TypedValue -> Codegen ()
indexSwitchCodegen (switchVal, _) cases defaultBlock result = do
    emitIndentation
    for_ result $ emit . fst >=> emitBuilder . (<> " = ")
    switchVal' <- emit switchVal
    emitBuilder $ "scf.index_switch " <> switchVal'
    for_ result $ emit . snd >=> emitBuilder . (" -> " <>) . (<> " ")
    emitBuilder "\n"
    incIndentation $ withoutLocation $ do
        forM_ cases $ \(caseVal, blk) -> do
            emitIndentation
            emitBuilder $ "case " <> TB.fromDec caseVal <> " "
            blockCodegen False blk
            emitBuilder "\n"
        emitIndentation
        emitBuilder "default "
        blockCodegen False defaultBlock
        emitBuilder "\n"
    emitLocIfPresent

{- | Generate reussir.str.select operation
Syntax: %idx, %found = reussir.str.select (%str) ["foo", "bar"] : (!reussir.str<local>) -> (index, i1)
-}
strSelectCodegen ::
    TypedValue -> [T.Text] -> TypedValue -> TypedValue -> Codegen ()
strSelectCodegen inputVal patterns (idxResVal, idxResTy) (foundResVal, foundResTy) = emitLine $ do
    inputVal' <- fmtTypedValue inputVal
    inputTy' <- emit (snd inputVal)
    idxResVal' <- emit idxResVal
    idxResTy' <- emit idxResTy
    foundResVal' <- emit foundResVal
    foundResTy' <- emit foundResTy
    let patternStrs = intercalate ", " (map (\p -> "\"" <> TB.fromText p <> "\"") patterns)
    emitBuilder $
        idxResVal'
            <> ", "
            <> foundResVal'
            <> " = reussir.str.select ("
            <> inputVal'
            <> ") ["
            <> patternStrs
            <> "] : ("
            <> inputTy'
            <> ") -> ("
            <> idxResTy'
            <> ", "
            <> foundResTy'
            <> ")"

strCastCodegen :: TypedValue -> TypedValue -> Codegen ()
strCastCodegen = emitUnaryOp "reussir.str.cast"

{- | Generate reussir.str.literal operation
Syntax: %res = reussir.str.literal @sym : !reussir.str<global>
-}
strLiteralCodegen :: Symbol -> TypedValue -> Codegen ()
strLiteralCodegen sym (resVal, resTy) = emitBuilderLineM $ do
    resVal' <- emit resVal
    resTy' <- emit resTy
    sym' <- emit sym
    pure $ resVal' <> " = reussir.str.literal @" <> sym' <> " : " <> resTy'

instrCodegen :: Instr -> Codegen ()
instrCodegen (ICall intrinsic) = intrinsicCallCodegen intrinsic
instrCodegen (FCall funcCall) = funcCallCodegen funcCall
instrCodegen (Panic message) = panicCodegen message
instrCodegen (Return result) = returnCodegen result
instrCodegen (NullableCheck nullChkVal nullChkRes) = nullableCheckCodegen nullChkVal nullChkRes
instrCodegen (NullableCreate nullCreateVal nullCreateRes) = nullableCreateCodegen nullCreateVal nullCreateRes
instrCodegen (NullableDispatch val nnul nul res) = nullableDispCodegen val nnul nul res
instrCodegen (RcInc rcIncVal) = rcIncCodegen rcIncVal
instrCodegen (RcDec rcDecVal) = rcDecCodegen rcDecVal
instrCodegen (RcCreate i r o) = rcCreateCodegen i r o
instrCodegen (RcFreeze i o) = rcFreezeCodegen i o
instrCodegen (RcBorrow i o) = rcBorrowCodegen i o
instrCodegen (RcIsUnique i o) = rcIsUniqueCodegen i o
instrCodegen (CompoundCreate fields res) = compoundCreateCodegen fields res
instrCodegen (VariantCreate tag value res) = variantCreateCodegen tag value res
instrCodegen (VariantDispatch val cases res) = variantDispCodegen val cases res
instrCodegen (RefProject val field res) = refProjectCodegen val field res
instrCodegen (RefSpill val res) = refSpillCodegen val res
instrCodegen (RefLoad val res) = refLoadCodegen val res
instrCodegen (RefStore target value) = refStoreCodegen target value
instrCodegen (RefDrop val) = refDropCodegen val
instrCodegen (RefAcquire val) = refAcquireCodegen val
instrCodegen (RegionRun body res) = regionRunCodegen body res
instrCodegen (Yield kind result) = yieldCodegen kind result
instrCodegen (ClosureCreate body res) = closureCreateCodegen body res
instrCodegen (ClosureApply target arg res) = closureApplyCodegen target arg res
instrCodegen (ClosureEval target res) = closureEvalCodegen target res
instrCodegen (ClosureUniqify target res) = closureUniqifyCodegen target res
instrCodegen (IfThenElse cond thenBlock elseBlock res) = ifThenElseCodegen cond thenBlock elseBlock res
instrCodegen (IndexSwitch val cases def res) = indexSwitchCodegen val cases def res
instrCodegen (StrSelect val pats idxRes foundRes) = strSelectCodegen val pats idxRes foundRes
instrCodegen (StrCast val res) = strCastCodegen val res
instrCodegen (WithLoc loc instr) = withLocation loc (instrCodegen instr)
instrCodegen (RecordExtract val field res) = recordExtractCodegen val field res
instrCodegen (StrLiteral sym res) = strLiteralCodegen sym res

wrapLinkage :: TB.Builder -> Codegen TB.Builder
wrapLinkage builder = pure $ "#llvm.linkage<" <> builder <> ">"

instance Emission Linkage where
    emit LnkInternal = wrapLinkage "internal"
    emit LnkPrivate = wrapLinkage "private"
    emit LnkWeakODR = wrapLinkage "weak_odr"
    emit LnkExternal = wrapLinkage "external"
    emit LnkLinkOnce = wrapLinkage "linkonce"
    emit LnkLinkOnceODR = wrapLinkage "linkonce_odr"
    emit LnkAvailableExternally = wrapLinkage "available_externally"
    emit LnkCommon = wrapLinkage "common"
    emit LnkAppending = wrapLinkage "appending"
    emit LnkWeak = wrapLinkage "weak"
    emit LnkExternWeak = wrapLinkage "extern_weak"

instance Emission LLVMVisibility where
    emit LLVMVisDefault = pure "default"
    emit LLVMVisHidden = pure "hidden"
    emit LLVMVisProtected = pure "protected"

functionCodegen :: Function -> Codegen ()
functionCodegen function = do
    linkage <- emit (funcLinkage function)
    visibility <- emit (funcLLVMVisibility function)
    let mlirVis = if funcMLIRVisibility function == MLIRVisPrivate then " private " else " "
    let symbol = symbolBuilder (funcSymbol function)
    args <- mapM fmtTypedValue (funcArgs function)
    emitIndentation
    emitBuilder $ "func.func" <> mlirVis <> "@\"" <> symbol <> "\""
    emitBuilder $ "(" <> intercalate ", " args <> ")"
    let result = funcResult function
    unless (isVoidType result) $ do
        result' <- emit result
        emitBuilder $ " -> " <> result'
    -- Emit attributes including debug func args if present
    dbgArgsAttr <- case funcDbgArgs function of
        [] -> pure ""
        dbgArgs -> do
            dbgArgsEmitted <- mapM emit dbgArgs
            pure $
                ", \"reussir.dbg_func_args\" = [" <> intercalate ", " dbgArgsEmitted <> "]"
    emitBuilder $
        " attributes { llvm.linkage = "
            <> linkage
            <> ", llvm.visibility = \""
            <> visibility
            <> "\""
            <> dbgArgsAttr
            <> " }"
    for_ (funcBody function) $ \body -> emitBuilder " " >> blockCodegen False body
    for_ (funcLoc function) $ \loc -> withLocation loc emitLocIfPresent
    emitBuilder "\n"
