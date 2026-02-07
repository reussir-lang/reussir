{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.IR (
    Instr (..),
    FuncCall (..),
    Block (..),
    VariantDispData (..),
    Function (..),
    Linkage (..),
    LLVMVisibility (..),
    MLIRVisibility (..),
    YieldKind (..),
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
import Reussir.Codegen.Intrinsics (IntrinsicCall, intrinsicCallCodegen)
import Reussir.Codegen.Location (DBGMetaInfo, Location)
import Reussir.Codegen.Type.Data (isBoolType, isVoidType)
import Reussir.Codegen.Value (TypedValue)

import Reussir.Codegen.Type.Data qualified as TT

{- | A function call instruction.
Unlike intrinsic calls, function calls cannot have multiple results.
-}
data FuncCall = FuncCall
    { target :: Symbol
    , args :: [TypedValue]
    , results :: Maybe TypedValue
    }
    deriving (Show)

data Block = Block
    { blkArgs :: [TypedValue]
    , blkBody :: [Instr]
    }
    deriving (Show)

data YieldKind = YieldClosure | YieldRegion | YieldScf
    deriving (Show)

newtype VariantDispData = VariantDispData [([Int64], Block)]
    deriving (Show)

{- |
  High-level representation of Reussir operations for code generation.

  This instruction set provides a comprehensive abstraction over the lower-level MLIR
  operations defined in ReussirOps.td. It includes:

  **Core Operations:**
    - Intrinsic calls: Low-level arithmetic, math, and bitwise operations
    - Function calls: User-defined function invocation
    - Panic: Error handling and abort

  **Nullable Operations:**
    - nullable.check: Check if a nullable value is null
    - nullable.create: Create a nullable value (with or without inner value)
    - nullable.dispatch: Structured control flow for nullable handling (high-level scf)

  **Reference Counting Operations:**
    - rc.inc: Increment reference count
    - rc.create: Create an RC pointer from a value (with optional token/region)
    - rc.freeze: Freeze an RC pointer (intermediate placeholder)
    - rc.borrow: Borrow an RC pointer to get a reference
    - rc.is_unique: Check if an RC pointer has unique ownership

  **Record Operations:**
    - record.compound: Create a compound record from field values
    - record.variant: Create a variant record with a tag and value
    - record.dispatch: Structured control flow for variant dispatching (high-level scf)

  **Reference Operations:**
    - ref.project: Project a reference (GEP-like operation)
    - ref.spill: Spill a value to stack allocation
    - ref.load: Load a value from a reference
    - ref.store: Store a value to a reference

  **Region Operations:**
    - region.run: Execute a region (with optional yield)
    - region.yield: Yield a value from a region

  **Closure Operations:**
    - closure.create: Create a closure (inlined or outlined)
    - closure.yield: Yield from a closure body
    - closure.apply: Apply an argument to a closure (partial application)
    - closure.eval: Evaluate a fully-applied closure
    - closure.uniqify: Ensure unique ownership (high-level scf)

  **Exclusions:**
  Many low-level placeholder operations are intentionally excluded from this high-level
  abstraction, including:
    - token.*: Token allocation, deallocation, and reinterpretation
    - rc.dec, rc.fetch_dec, rc.reinterpret: Low-level RC management
    - nullable.coerce, record.coerce: Type coercion placeholders
    - record.tag: Tag extraction (handled implicitly in variant operations)
    - ref.drop: Destructor calls (handled by backend)
    - region.create, region.cleanup: Region lifecycle management
    - closure.clone: Low-level cloning (handled via vtable)

  Additionally, Tokens, vtables, and other ABI-level constructs are not included
  in this high-level IR as they will be automatically handled during backend
  lowering passes. The backend manages:
    - Memory allocation/deallocation through tokens
    - VTable generation for closures and flex/rigid types
    - Layout calculations and alignments
    - Clone/drop function insertion
    - Scanner instruction generation for GC

  This abstraction allows code generation to focus on the semantic intent of
  operations rather than low-level memory management details.
-}
data Instr
    = ICall IntrinsicCall
    | -- | User-defined function call
      FCall FuncCall
    | -- | Panic with error message
      Panic T.Text
    | -- | Return from function with optional value
      Return (Maybe TypedValue)
    | -- | reussir.nullable.check: Check if a nullable token is null
      NullableCheck {nullChkVal :: TypedValue, nullChkRes :: TypedValue}
    | -- | reussir.nullable.create: Create a nullable token (with or without inner pointer)
      NullableCreate {nullCreateVal :: Maybe TypedValue, nullCreateRes :: TypedValue}
    | -- | reussir.nullable.dispatch: High-level structured control flow for nullable handling
      NullableDispatch
        { nullDispVal :: TypedValue
        , nullDispNonnull :: Block
        , nullDispNull :: Block
        , nullDispRes :: Maybe TypedValue
        }
    | -- | reussir.rc.inc: Increment the reference count of a RC pointer
      RcInc TypedValue
    | -- | reussir.rc.dec: Decrement the reference count of a RC pointer
      RcDec TypedValue
    | -- | reussir.rc.create: Create a RC pointer from a value (with optional token/region)
      RcCreate
        { rcCreateVal :: TypedValue
        , rcCreateRegion :: Maybe TypedValue
        , rcCreateRes :: TypedValue
        }
    | -- | reussir.rc.freeze: Freeze an RC pointer into a reference (intermediate placeholder)
      RcFreeze
        { rcFreezeVal :: TypedValue
        , rcFreezeRes :: TypedValue
        }
    | -- | reussir.rc.borrow: Borrow a RC pointer to get an access reference
      RcBorrow
        { rcBorrowVal :: TypedValue
        , rcBorrowRes :: TypedValue
        }
    | -- | reussir.rc.is_unique: Check if a RC pointer is unique
      RcIsUnique
        { rcIsUniqueVal :: TypedValue
        , rcIsUniqueRes :: TypedValue
        }
    | -- | reussir.record.compound: Create a compound record from field values
      CompoundCreate
        { compoundCreateFields :: [TypedValue]
        , compoundCreateRes :: TypedValue
        }
    | -- | reussir.record.extract: Extract a field from a compound record
      RecordExtract
        { recordExtractVal :: TypedValue
        , recordExtractFieldIdx :: Int64
        , recordExtractRes :: TypedValue
        }
    | -- | reussir.record.variant: Create a variant record with a tag and value
      VariantCreate
        { variantCreateTag :: Int64
        , variantCreateValue :: TypedValue
        , variantCreateRes :: TypedValue
        }
    | -- | reussir.record.dispatch: High-level structured control flow for variant dispatching
      VariantDispatch
        { variantDispVal :: TypedValue
        , variantDispData :: VariantDispData
        , variantDispRes :: Maybe TypedValue
        }
    | -- | reussir.ref.project: Project a reference (GEP-like operation without self indexing)
      RefProject
        { refProjectVal :: TypedValue
        , refProjectField :: Int64
        , refProjectRes :: TypedValue
        }
    | -- | reussir.ref.spill: Spill a value to stack allocation for cross-region/cross-function passing
      RefSpill
        { refSpillVal :: TypedValue
        , refSpillRes :: TypedValue
        }
    | -- | reussir.ref.load: Load a value from a reference
      RefLoad
        { refLoadVal :: TypedValue
        , refLoadRes :: TypedValue
        }
    | -- | reussir.ref.store: Store a value to a reference (target must have field capability)
      RefStore
        { refStoreTarget :: TypedValue
        , refStoreVal :: TypedValue
        }
    | {- | reussir.region.run: Execute a region (inner region accepts !reussir.region argument)

      Syntax: [%res =] reussir.region.run [-> result_type] {
          ^bb0(%region : !reussir.region):
            ...
            reussir.region.yield [%val]
        }
      Constraints: inner region must accept one !reussir.region argument
             can optionally yield a flex value which becomes rigid after execution
             cannot be nested in the same function
      -}
      RegionRun
        { regionRunBody :: Block
        , regionRunRes :: Maybe TypedValue
        }
    | {- | Yield operations for different contexts

      reussir.region.yield - Yield from a region
      Syntax: reussir.region.yield [%value : !reussir.rc<T flex>]
      Constraints: must be inside region.run
               value is optional and must be !reussir.rc<T flex> if present

      reussir.closure.yield - Yield from a closure body
      Syntax: reussir.closure.yield [%value : T]
      Constraints: must be inside closure.create
               value type must match closure return type
               value is optional only if closure has no return type

      scf.yield - Yield from high-level SCF operations
      Syntax: reussir.scf.yield [%value : T]
      Constraints: must be inside nullable.dispatch or record.dispatch
               value type must match parent operation's result type
               value is optional only if parent has no result
      -}
      Yield YieldKind (Maybe TypedValue)
    | {- | reussir.closure.create: Create a closure (inlined with body or outlined with vtable)

      Syntax: %closure = reussir.closure.create [token(%t : !reussir.token<...>)] [vtable(@symbol)] {
          ^bb0(%arg1 : T1, %arg2 : T2, ...):
            ...
            reussir.closure.yield [%result : R]
        }
      Constraints: closure is always wrapped in !reussir.rc pointer
             inlined form: body region required, no vtable attribute
             outlined form: vtable attribute required, no body region
             token layout must match RcBox with ClosureHeader and Payload
      -}
      ClosureCreate
        { closureCreateBody :: Block
        , closureCreateRes :: TypedValue
        }
    | {- | reussir.closure.apply: Apply an argument to a closure (partial application)

      Syntax: %applied = reussir.closure.apply (%arg : T) to (%closure : !reussir.closure<(T, ...) -> R>)
          : !reussir.closure<(...) -> R>
      Constraints: closure must have at least one unapplied argument
             arg type must match the next expected argument type
             result closure has one fewer argument
      -}
      ClosureApply
        { closureApplyTarget :: TypedValue
        , closureApplyArg :: TypedValue
        , closureApplyRes :: TypedValue
        }
    | {- | reussir.closure.eval: Evaluate a fully-applied closure

      Syntax: [%result =] reussir.closure.eval (%closure : !reussir.closure<() -> R>) [: R]
      Constraints: closure must be fully applied (no remaining arguments)
             consumes one reference to the closure
             result type must match closure return type
      -}
      ClosureEval
        { closureEvalTarget :: TypedValue
        , closureEvalRes :: Maybe TypedValue
        }
    | {- | reussir.closure.uniqify: Ensure unique closure ownership (high-level SCF)

      Syntax: %unique = reussir.closure.uniqify (%closure : !reussir.closure<T>) : !reussir.closure<T>
      Constraints: expands to rc.is_unique guarded scf.if-else
             clones closure if not unique (refcount > 1)
             returns original closure if unique
      -}
      ClosureUniqify
        { closureUniqifyTarget :: TypedValue
        , closureUniqifyRes :: TypedValue
        }
    | {- | If-then-else operation

      Example:
      ```mlir
      %x, %y = scf.if %b -> (f32, f32) {
        %x_true = ...
        %y_true = ...
        scf.yield %x_true, %y_true : f32, f32
      } else {
        %x_false = ...
        %y_false = ...
        scf.yield %x_false, %y_false : f32, f32
      }
      ```
      -}
      IfThenElse
        { ifThenElseCond :: TypedValue
        , ifThenElseThen :: Block
        , ifThenElseElse :: Maybe Block
        , ifThenElseRes :: Maybe TypedValue
        }
    | -- | Attach location information to an instruction
      WithLoc Location Instr
    | -- | reussir.str.literal: Create a reference to a global string literal
      StrLiteral
        { strLitSym :: Symbol
        , strLitRes :: TypedValue
        }
    deriving (Show)

data Linkage
    = LnkPrivate
    | LnkInternal
    | LnkAvailableExternally
    | LnkLinkOnce
    | LnkWeak
    | LnkCommon
    | LnkAppending
    | LnkExternWeak
    | LnkLinkOnceODR
    | LnkWeakODR
    | LnkExternal
    deriving (Show)

data LLVMVisibility
    = LLVMVisDefault
    | LLVMVisHidden
    | LLVMVisProtected
    deriving (Show)

data MLIRVisibility
    = MLIRVisPublic
    | MLIRVisPrivate
    deriving (Show, Eq)

data Function = Function
    { funcLinkage :: Linkage
    , funcLLVMVisibility :: LLVMVisibility
    , funcMLIRVisibility :: MLIRVisibility
    , funcBody :: Maybe Block
    , funcArgs :: [TypedValue]
    , funcDbgArgs :: [DBGMetaInfo] -- Debug info for function arguments
    , funcLoc :: Maybe Location
    , funcResult :: TT.Type
    , funcSymbol :: Symbol
    }
    deriving (Show)

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
            <> target'
            <> ") ("
            <> arg'
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
    for_ result $ emit . fst >=> emitBuilder . (" -> " <>)
    emitBuilder " {\n"
    incIndentation $ forM_ cases $ uncurry variantDispCaseCodegen
    emitBuilder "}"

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
instrCodegen (RegionRun body res) = regionRunCodegen body res
instrCodegen (Yield kind result) = yieldCodegen kind result
instrCodegen (ClosureCreate body res) = closureCreateCodegen body res
instrCodegen (ClosureApply target arg res) = closureApplyCodegen target arg res
instrCodegen (ClosureEval target res) = closureEvalCodegen target res
instrCodegen (ClosureUniqify target res) = closureUniqifyCodegen target res
instrCodegen (IfThenElse cond thenBlock elseBlock res) = ifThenElseCodegen cond thenBlock elseBlock res
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
