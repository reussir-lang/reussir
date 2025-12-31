{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Type.Data (
    PrimitiveFloat (..),
    PrimitiveInt (..),
    Primitive (..),
    Atomicity (..),
    Capability (..),
    Type (..),
    Rc (..),
    Ref (..),
    Closure (..),
    Tensor (..),
    isRefType,
    isBoolType,
    isVoidType,
    isIntegralType,
    isFloatType,
)
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Reussir.Codegen.Context.Symbol (Symbol)

{- | Integer primitive types for MLIR code generation.

=== MLIR Signless Integer Model ===

MLIR operates on /signless/ integers, meaning integer types (i8, i16, i32, i64, i128)
do not have inherent signedness. Instead, signedness is determined by the
/operations/ performed on these values. This design allows MLIR to represent
both signed and unsigned semantics through the same type system, with the
actual signedness semantics defined by the specific arithmetic operations used.

=== Sign Management Through Arithmetic Intrinsics ===

The signedness semantics are encoded in the intrinsic call operations:

[Basic Arithmetic Operations (Signless)]
These operations work on the raw bit patterns without sign interpretation:
  * 'Addi' - Addition: performs two's complement addition (same result for
    signed and unsigned when no overflow occurs)
  * 'Subi' - Subtraction: performs two's complement subtraction
  * 'Muli' - Multiplication: performs two's complement multiplication
  * 'Andi', 'Ori', 'Xori' - Bitwise operations: inherently signless

[Signed Operations]
These operations interpret values as two's complement signed integers:
  * 'Divsi' - Signed division (rounds toward zero, e.g., -5 / 2 = -2)
  * 'Remsi' - Signed remainder (sign matches dividend, e.g., -5 % 2 = -1)
  * 'Ceildivsi' - Signed ceiling division (rounds toward +∞)
  * 'Floordivsi' - Signed floor division (rounds toward -∞)
  * 'Maxsi', 'Minsi' - Signed maximum/minimum
  * 'Extsi' - Signed extension (sign-extends the most significant bit)
  * 'Trunci' - Truncation (preserves lower bits, sign-agnostic)
  * 'Cmpi' with 'CISgt', 'CISge', 'CISlt', 'CISle' - Signed comparisons
  * 'Fptosi' - Float-to-signed-integer conversion

[Unsigned Operations]
These operations interpret values as unsigned integers:
  * 'Divui' - Unsigned division (always non-negative result)
  * 'Remui' - Unsigned remainder (always non-negative result)
  * 'Ceildivui' - Unsigned ceiling division
  * 'Maxui' - Unsigned maximum
  * 'Extui' - Unsigned extension (zero-extends, fills upper bits with 0)
  * 'Cmpi' with 'CIUgt', 'CIUge', 'CIUlt', 'CIUle' - Unsigned comparisons
  * 'Fptoui' - Float-to-unsigned-integer conversion
  * 'Uitofp' - Unsigned-integer-to-float conversion

[Extended Precision Operations]
Operations that produce wider results:
  * 'AdduiExtended', 'MulsiExtended', 'MuluiExtended' - Extended precision
    arithmetic for cases where intermediate results exceed the operand width

=== Examples ===

Consider an i8 value with bit pattern 0xFF (255 unsigned, -1 signed):

* Signed interpretation:
  - 'Extsi' extends to i16: 0xFFFF (-1 signed)
  - 'Divsi' by 2: 0xFF / 2 = -1 (signed division rounds toward zero)
  - 'Cmpi CISgt' with 0: false (signed comparison: -1 > 0 is false)

* Unsigned interpretation:
  - 'Extui' extends to i16: 0x00FF (255 unsigned)
  - 'Divui' by 2: 0xFF / 2 = 127 (unsigned division)
  - 'Cmpi CIUgt' with 0: true (unsigned comparison: 255 > 0 is true)

=== Type Conversion and Casting ===

* 'Bitcast' - Reinterprets bits without conversion (signless)
* 'IndexCast', 'IndexCastui' - Conversions to/from index types
  (index types are typically signless machine-word-sized integers)

=== Overflow Flags ===

The 'IntOFFlag' parameter on operations like 'Addi', 'Subi', 'Muli', 'Trunci'
allows specifying overflow behavior flags (e.g., 'nsw' for "no signed wrap",
'nuw' for "no unsigned wrap"), which enable optimizations by asserting that
the operation does not wrap in the specified signedness interpretation.

=== Why Signless Integers? ===

This design provides several benefits:
1. /Flexibility/: The same value can be treated as signed or unsigned
   depending on context
2. /Optimization opportunities/: The compiler can choose the most efficient
   representation and operations
3. /Explicit semantics/: Signedness is explicit in operations rather than
   implicit in types, making code generation more precise
4. /LLVM compatibility/: LLVM also uses signless integers, making MLIR-to-LLVM
   translation straightforward

=== Implementation Note ===

When generating MLIR code, the codegen layer must choose the appropriate
operation variant ('Divsi' vs 'Divui', 'Extsi' vs 'Extui', etc.) based on
the source language's type semantics. The 'PrimitiveInt' type itself does
not encode signedness; this must be tracked separately in the type system
or inferred from the operations used.
-}
data PrimitiveInt = PrimInt8 | PrimInt16 | PrimInt32 | PrimInt64 | PrimInt128 | PrimIndex
    deriving (Eq, Show, Hashable, Generic)

data PrimitiveFloat = PrimFloat8 | PrimFloat16 | PrimBFloat16 | PrimFloat32 | PrimFloat64 | PrimFloat128
    deriving (Eq, Show, Hashable, Generic)

data Primitive
    = PrimInt PrimitiveInt
    | PrimFloat PrimitiveFloat
    | PrimBool
    | PrimUnit
    deriving (Eq, Show, Hashable, Generic)

data Rc = Rc {rcBoxInner :: Type, rcBoxAtomicity :: Atomicity, rcBoxCapability :: Capability}
    deriving (Eq, Show, Hashable, Generic)
data Ref = Ref {refInner :: Type, refAtomicity :: Atomicity, refCapability :: Capability}
    deriving (Eq, Show, Hashable, Generic)
data Closure = Closure {closureArgs :: [Type], closureReturnType :: Type}
    deriving (Eq, Show, Hashable, Generic)
data Tensor = Tensor {tensorEleTy :: Type, tensorDimensions :: [Int]}
    deriving (Eq, Show, Hashable, Generic)

data Type
    = TypePrim Primitive
    | TypeTensor Tensor
    | TypeClosure Closure
    | TypeRc Rc
    | TypeRef Ref
    | TypeExpr Symbol
    | TypeNullable Type
    | TypeRegion -- Region handle
    deriving (Eq, Show, Hashable, Generic)

data Atomicity = Atomic | NonAtomic
    deriving (Eq, Show, Hashable, Generic)

data Capability = Unspecified | Shared | Value | Flex | Rigid | Field | Regional
    deriving (Eq, Show, Hashable, Generic)

isBoolType :: Type -> Bool
isBoolType (TypePrim PrimBool) = True
isBoolType _ = False

isRefType :: Type -> Bool
isRefType (TypeRef _) = True
isRefType _ = False

isVoidType :: Type -> Bool
isVoidType (TypePrim PrimUnit) = True
isVoidType _ = False

isIntegralType :: Type -> Bool
isIntegralType (TypePrim (PrimInt _)) = True
isIntegralType _ = False

isFloatType :: Type -> Bool
isFloatType (TypePrim (PrimFloat _)) = True
isFloatType _ = False
