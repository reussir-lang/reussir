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
) where

import Control.Monad (unless, when)
import Data.Foldable (forM_)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Codegen.Context (Emission (emit), Path, emitIndentation, emitLine, incIndentation)
import Reussir.Codegen.Context.Codegen (Codegen, getNewBlockId, incIndentationBy, withLocation)
import Reussir.Codegen.Context.Emission (emitBuilder, intercalate)
import Reussir.Codegen.Intrinsics (IntrinsicCall, intrinsicCallCodegen)
import Reussir.Codegen.Location (Location)
import Reussir.Codegen.Type.Data qualified as TT
import Reussir.Codegen.Value (TypedValue)

{- | A function call instruction.
Unlike intrinsic calls, function calls cannot have multiple results.
-}
data FuncCall = FuncCall
    { target :: Path
    , args :: [TypedValue]
    , results :: TypedValue
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
        { compoundCreateExpr :: TT.Expr
        , compoundCreateFields :: [TypedValue]
        , compoundCreateRes :: TypedValue
        }
    | -- | reussir.record.variant: Create a variant record with a tag and value
      VariantCreate
        { variantCreateExpr :: TT.Expr
        , variantCreateTag :: Int64
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
    | -- | reussir.region.run: Execute a region (inner region accepts !reussir.region argument)
      RegionRun
        { regionRunBody :: Block
        , regionRunRes :: Maybe TypedValue
        }
    | -- | generic yield operation for regions and closures
      Yield YieldKind (Maybe TypedValue)
    | -- | reussir.closure.create: Create a closure (inlined with body or outlined with vtable)
      ClosureCreate
        { closureCreateBody :: Block
        , closureCreateRes :: TypedValue
        }
    | -- | reussir.closure.apply: Apply an argument to a closure (partial application)
      ClosureApply
        { closureApplyTarget :: TypedValue
        , closureApplyArg :: TypedValue
        , closureApplyRes :: TypedValue
        }
    | -- | reussir.closure.eval: Evaluate a fully-applied closure
      ClosureEval
        { closureEvalTarget :: TypedValue
        , closureEvalRes :: TypedValue
        }
    | -- | reussir.closure.uniqify: Ensure unique closure ownership
      ClosureUniqify
        { closureUniqifyTarget :: TypedValue
        , closureUniqifyRes :: TypedValue
        }
    | -- | If-then-else operation
      IfThenElse
        { ifThenElseCond :: TypedValue
        , ifThenElseThen :: Block
        , ifThenElseElse :: Maybe Block
        , ifThenElseRes :: Maybe TypedValue
        }
    | -- | Attach location information to an instruction
      WithLoc Location Instr
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
    | MLIRvisProtected
    deriving (Show)

data Function = Function
    { funcLinkage :: Linkage
    , funcLLVMVisibility :: LLVMVisibility
    , funcMLIRVisibility :: MLIRVisibility
    , funcBody :: Maybe Block
    , funcArgs :: [TypedValue]
    , funcLoc :: Maybe Location
    , result :: TT.Type
    }
    deriving (Show)

fmtTypedValue :: TypedValue -> Codegen TB.Builder
fmtTypedValue (val, ty) = do
    val' <- emit val
    ty' <- emit ty
    return $ val' <> " : " <> ty'

blockCodegen :: Bool -> Block -> Codegen ()
blockCodegen printArgs blk = incIndentation $ do
    emitLine $ emitBuilder "{"
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
    emitLine $ emitBuilder "}"

instrCodegen :: Instr -> Codegen ()
instrCodegen (Panic message) = emitLine $ emitBuilder $ "reussir.panic " <> TB.fromText message
instrCodegen (ICall intrinsic) = intrinsicCallCodegen intrinsic
instrCodegen (WithLoc loc instr) = withLocation loc (instrCodegen instr)
instrCodegen _ = error "Not implemented"
