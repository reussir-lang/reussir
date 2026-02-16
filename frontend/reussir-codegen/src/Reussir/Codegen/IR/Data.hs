module Reussir.Codegen.IR.Data (
    Instr (..),
    FuncCall (..),
    Block (..),
    VariantDispData (..),
    Function (..),
    Linkage (..),
    LLVMVisibility (..),
    MLIRVisibility (..),
    YieldKind (..),
) where

import Data.Int (Int64)

import Data.Text qualified as T

import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Intrinsics (IntrinsicCall)
import Reussir.Codegen.Location (DBGMetaInfo, Location)
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

data YieldKind = YieldClosure | YieldRegion | YieldScf | YieldReussirScf
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
    - ref.drop: Also available as high-level RefDrop instruction for value struct destruction
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
    | -- | reussir.ref.drop: Drop (destruct) the element behind a reference in place
      RefDrop
        { refDropVal :: TypedValue
        }
    | -- | reussir.ref.acquire: Acquire ownership of the element behind a reference in place
      RefAcquire
        { refAcquireVal :: TypedValue
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
    | -- | scf.index_switch: Switch on an index value with cases and default
      IndexSwitch
        { indexSwitchVal :: TypedValue -- index value to switch on
        , indexSwitchCases :: [(Int64, Block)] -- (case_value, block) pairs
        , indexSwitchDefault :: Block -- default block
        , indexSwitchRes :: Maybe TypedValue -- optional result
        }
    | -- | reussir.str.select: Select first matched string pattern
      StrSelect
        { strSelectVal :: TypedValue -- input string (!reussir.str<local>)
        , strSelectPatterns :: [T.Text] -- pattern strings to match
        , strSelectIdxRes :: TypedValue -- result: selected index (index type)
        , strSelectFoundRes :: TypedValue -- result: match found (i1 type)
        }
    | -- | reussir.str.cast: Cast a global string to a local string
      StrCast
        { strCastVal :: TypedValue -- input (!reussir.str<global>)
        , strCastRes :: TypedValue -- result (!reussir.str<local>)
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
