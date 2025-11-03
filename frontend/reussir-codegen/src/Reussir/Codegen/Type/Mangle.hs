{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Type.Mangle (mangleType, mangleTypeWithPrefix) where

import Data.Interned (unintern)
import Data.Interned.Text (InternedText)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context.Path (Path (..))
import Reussir.Codegen.Type.Data (
    Atomicity (..),
    Capability (..),
    Closure (..),
    Expr (..),
    Primitive (..),
    PrimitiveFloat (..),
    PrimitiveInt (..),
    Rc (..),
    Ref (..),
    Tensor (..),
    Type (..),
 )

-- ================================================================
-- Type Mangling Implementation
-- ================================================================
--
-- This module implements type name mangling following the Itanium C++ ABI
-- convention (also known as name mangling or name decoration).
--
-- Formal Syntax:
-- <name> ::= <encoding>
-- <encoding> ::= <name> [<bare-function-type>] <type>+
-- <name> ::= [<nested-name>] | [<unscoped-name>]
-- <nested-name> ::= N [<source-name>] [<bare-function-type>] <type>+ E
-- <type> ::= <builtin-type> | <class-enum-type> | <function-type> | ...
--
-- We adapt this for Reussir types with some modifications.

-- ================================================================
-- Path Mangling
-- ================================================================
--
-- Formal Syntax:
-- <source-name> ::= <positive length number> <identifier>
-- <nested-name> ::= N [<source-name>] <type>* E
--
-- For a path "A::B::C", we mangle it as: N <source-name-A> <source-name-B> <source-name-C> E
-- Where each <source-name> is: <length> <identifier>

{- | Mangle a single path segment (identifier).
  Format: <length><identifier>
  Example: "My" -> "2My", "Path" -> "4Path"
-}
manglePathSegment :: InternedText -> TB.Builder
manglePathSegment text = TB.fromString (show (T.length text')) <> TB.fromString (T.unpack text')
  where
    text' = T.fromStrict (unintern text)

{- | Mangle all segments of a path.
  Format: <segment1><segment2>...
  Each segment follows <source-name> format: <length><identifier>
-}
manglePathSegments :: Path -> TB.Builder
manglePathSegments (Path segments) = foldMap manglePathSegment segments

-- ================================================================
-- Primitive Type Mangling
-- ================================================================
--
-- Formal Syntax:
-- <builtin-type> ::= i | v | b | ...
--   Where: i = int, v = void, b = bool, etc.
--
-- For Reussir, we use a different convention:
-- We always treat primitives as identifiers: <length><type-name>
-- This makes them more readable and avoids conflicts.

{- | Mangle primitive integer types.
  Format: <length><type-name>
  Examples:
    PrimInt8    -> "2i8"
    PrimInt16   -> "3i16"
    PrimInt32   -> "3i32"
    PrimInt64   -> "3i64"
    PrimInt128  -> "4i128"
    PrimIndex   -> "5index"
-}
manglePrimInt :: PrimitiveInt -> TB.Builder
manglePrimInt PrimInt8 = "2i8"
manglePrimInt PrimInt16 = "3i16"
manglePrimInt PrimInt32 = "3i32"
manglePrimInt PrimInt64 = "3i64"
manglePrimInt PrimInt128 = "4i128"
manglePrimInt PrimIndex = "5index"

{- | Mangle primitive float types.
  Format: <length><type-name>
  Examples:
    PrimFloat8    -> "2f8"
    PrimFloat16   -> "3f16"
    PrimBFloat16  -> "4bf16"
    PrimFloat32   -> "3f32"
    PrimFloat64   -> "3f64"
    PrimFloat128  -> "4f128"
-}
manglePrimFloat :: PrimitiveFloat -> TB.Builder
manglePrimFloat PrimFloat8 = "2f8"
manglePrimFloat PrimBFloat16 = "4bf16"
manglePrimFloat PrimFloat16 = "3f16"
manglePrimFloat PrimFloat32 = "3f32"
manglePrimFloat PrimFloat64 = "3f64"
manglePrimFloat PrimFloat128 = "4f128"

{- | Mangle primitive types (int, float, bool, unit).
  Format:
    - Integers/Floats: <length><type-name>
    - Bool: "b"
    - Unit: "v" (void)
  Examples:
    PrimInt (PrimInt32) -> "3i32"
    PrimFloat (PrimFloat64) -> "3f64"
    PrimBool -> "b"
    PrimUnit -> "v"
-}
manglePrimitive :: Primitive -> TB.Builder
manglePrimitive (PrimInt i) = manglePrimInt i
manglePrimitive (PrimFloat f) = manglePrimFloat f
manglePrimitive PrimBool = "b"
manglePrimitive PrimUnit = "v"

-- ================================================================
-- Tensor/Array Type Mangling
-- ================================================================
--
-- Formal Syntax (adapted from C++ array mangling):
-- <array-type> ::= A [<dimension>] _ <element-type>
--
-- For Reussir, we wrap arrays in a Tensor type:
-- Format: <type-name>I <array-mangling> E
--   Where: <type-name> = "6Tensor"
--          <array-mangling> = A[<dim1>_][<dim2>_]... <element-type>
--
-- Examples:
--   Tensor<i32[3]>           -> "6TensorIA3_3i32E"
--   Tensor<f64[10][20]>      -> "6TensorIA10_A20_3f64E"
--   Tensor<bool[]>           -> "6TensorIA_bE"

{- | Mangle tensor/array types.
  Format: "6Tensor" I [A<dim1>_][A<dim2>_]... <element-type> E
  For empty dimensions, use: "6Tensor" I A_ <element-type> E
-}
mangleTensor :: Tensor -> TB.Builder
mangleTensor (Tensor eleTy dimensions) =
    "6Tensor"
        <> "I"
        <> (if null dimensions then "A_" else foldMap (\d -> "A" <> TB.fromString (show d) <> "_") dimensions)
        <> mangleType eleTy
        <> "E"

-- ================================================================
-- Rc (Reference Counted) Type Mangling
-- ================================================================
--
-- Format: <rc-type-name> I <inner-type> E
--   Where <rc-type-name> depends on atomicity and capability:
--     - Rc              -> "2Rc"
--     - AtomicRc        -> "8AtomicRc"
--     - FlexRc          -> "6FlexRc"
--     - RigidRc         -> "7RigidRc"
--     - AtomicFlexRc    -> "12AtomicFlexRc"
--     - AtomicRigidRc   -> "13AtomicRigidRc"
--
-- Examples:
--   Rc<i32>              -> "2RcI3i32E"
--   AtomicRc<i64>        -> "8AtomicRcI3i64E"
--   FlexRc<f32>          -> "6FlexRcI3f32E"
--   AtomicFlexRc<bool>   -> "12AtomicFlexRcIbE"

{- | Mangle reference-counted (Rc) types.
  Format: <rc-name> I <inner-type> E
-}
mangleRc :: Rc -> TB.Builder
mangleRc (Rc eleTy atom cap) = rcName <> "I" <> mangleType eleTy <> "E"
  where
    rcName = case (cap, atom) of
        (Flex, NonAtomic) -> "6FlexRc"
        (Rigid, NonAtomic) -> "7RigidRc"
        (Flex, Atomic) -> "12AtomicFlexRc"
        (Rigid, Atomic) -> "13AtomicRigidRc"
        (_, Atomic) -> "8AtomicRc"
        _ -> "2Rc"

-- ================================================================
-- Ref (Reference) Type Mangling
-- ================================================================
--
-- Format: <ref-type-name> I <inner-type> E
--   Where <ref-type-name> depends on atomicity and capability:
--     - Ref              -> "3Ref"
--     - AtomicRef        -> "9AtomicRef"
--     - FlexRef          -> "7FlexRef"
--     - RigidRef         -> "8RigidRef"
--     - AtomicFlexRef    -> "13AtomicFlexRef"
--     - AtomicRigidRef   -> "14AtomicRigidRef"
--
-- Examples:
--   Ref<i32>              -> "3RefI3i32E"
--   AtomicRef<i64>        -> "9AtomicRefI3i64E"
--   FlexRef<f32>          -> "7FlexRefI3f32E"

{- | Mangle reference (Ref) types.
  Format: <ref-name> I <inner-type> E
-}
mangleRef :: Ref -> TB.Builder
mangleRef (Ref eleTy atom cap) = refName <> "I" <> mangleType eleTy <> "E"
  where
    refName = case (cap, atom) of
        (Flex, NonAtomic) -> "7FlexRef"
        (Rigid, NonAtomic) -> "8RigidRef"
        (Flex, Atomic) -> "13AtomicFlexRef"
        (Rigid, Atomic) -> "14AtomicRigidRef"
        (_, Atomic) -> "9AtomicRef"
        _ -> "3Ref"

-- ================================================================
-- Closure/Function Type Mangling
-- ================================================================
--
-- Following the C++ std::function mangling convention:
-- Formal Syntax:
--   <function-type> ::= F <type>+ E
--   std::function mangling: N St 8function I F <arg-types> <return-type> E E
--
-- For Reussir Closure:
-- Format: "7Closure" I F <return-type> <arg-types> E E
--   Where empty arg list is represented as "v" (void)
--
-- Examples:
--   Closure() -> i32        -> "7ClosureIF3i32vEE"
--   Closure(i32) -> i64     -> "7ClosureIF3i643i32EE"
--   Closure(i32,f64) -> bool -> "7ClosureIFb3i323f64EE"

{- | Mangle closure/function types.
  Format: "7Closure" I F <return-type> [<arg1> <arg2> ... | v] E E
  Note: Return type comes first (after F), then arguments.
        Empty arguments are represented as "v".
-}
mangleClosure :: Closure -> TB.Builder
mangleClosure (Closure args ret) = "7Closure" <> "I" <> "F" <> mangleType ret <> mangleArgs args <> "EE"
  where
    -- \| Mangle function arguments.
    --   Empty list -> "v" (void)
    --   Non-empty -> concatenation of mangled argument types
    mangleArgs :: [Type] -> TB.Builder
    mangleArgs [] = "v"
    mangleArgs xs = foldMap mangleType xs

-- ================================================================
-- Expr (Path-based) Type Mangling
-- ================================================================
--
-- Formal Syntax (adapted from C++ nested-name):
--   <nested-name> ::= N [<source-name>] [<bare-function-type>] <type>* E
--
-- For Reussir Expr:
-- Format: N <path-segments> [I <type-args> E] E
--   Where:
--     - <path-segments> are mangled path segments
--     - If no type args: N <path-segments> E
--     - If type args exist: N <path-segments> I <arg1> <arg2> ... E E
--
-- Examples:
--   Expr(My::Path)                      -> "N2My4PathE"
--   Expr(My::Path<i32>)                 -> "N2My4PathI3i32EE"
--   Expr(Long::Namespace::Type<i32,f64>) -> "N4Long9Namespace4TypeI3i323f64EE"

{- | Mangle expression types (path-based types with optional type arguments).
  Format: N <path-segments> [I <type-args> E] E
  If no type arguments, format is: N <path-segments> E
  If type arguments exist, format is: N <path-segments> I <type-args> E E
-}
mangleExpr :: Expr -> TB.Builder
mangleExpr (Expr path args) =
    "N" <> manglePathSegments path <> (if null args then "" else "I" <> foldMap mangleType args <> "E") <> "E"

-- ================================================================
-- Nullable Type Mangling
-- ================================================================
--
-- Format: "8Nullable" I <inner-type> E
--   Where <inner-type> is the mangled type of the inner pointer
--
-- Examples:
--   Nullable<i32> -> "8NullableI3i32E"
--   Nullable<f64> -> "8NullableI3f64E"

{- | Mangle nullable types.
  Format: "8Nullable" I <inner-type> E
-}
mangleNullable :: Type -> TB.Builder
mangleNullable ty = "8Nullable" <> "I" <> mangleType ty <> "E"

-- ================================================================
-- Type Mangling Dispatcher
-- ================================================================

{- | Mangle a type according to its kind.
  Dispatches to the appropriate mangle function based on the type constructor.
-}
mangleType :: Type -> TB.Builder
mangleType (TypePrim p) = manglePrimitive p
mangleType (TypeTensor t) = mangleTensor t
mangleType (TypeRc rc) = mangleRc rc
mangleType (TypeRef ref) = mangleRef ref
mangleType (TypeClosure c) = mangleClosure c
mangleType (TypeExpr e) = mangleExpr e
mangleType (TypeNullable ty) = mangleNullable ty
mangleType TypeRegion = "6Region"

-- ================================================================
-- Prefix for External Linkage
-- ================================================================
--
-- Following the Itanium ABI convention for external linkage names:
-- Format: _Z <mangled-name>
--
-- The "_Z" prefix indicates that the following is a mangled name
-- following the Itanium C++ ABI.

{- | Mangle a type with the external linkage prefix "_Z".
  Format: "_Z" <mangled-type>
  Example: mangleTypeWithPrefix (TypePrim (PrimInt PrimInt32))
           -> "_Z3i32"
-}
mangleTypeWithPrefix :: Type -> TB.Builder
mangleTypeWithPrefix = ("_Z" <>) . mangleType
