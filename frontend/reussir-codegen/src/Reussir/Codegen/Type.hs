{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Type
  ( PrimitiveFloat (..),
    PrimitiveInt (..),
    Primitive (..),
    Atomicity (..),
    Capability (..),
    Type (..),
  )
where

import Reussir.Codegen.Context (Emission (emit), Path)

data PrimitiveInt = PrimInt8 | PrimInt16 | PrimInt32 | PrimInt64 | PrimInt128 | PrimIndex
  deriving (Eq, Show)

data PrimitiveFloat = PrimFloat8 | PrimFloat16 | PrimBFloat16 | PrimFloat32 | PrimFloat64 | PrimFloat128
  deriving (Eq, Show)

data Primitive
  = PrimInt PrimitiveInt
  | PrimFloat PrimitiveFloat
  | PrimBool
  | PrimUnit
  deriving (Eq, Show)

data Type
  = TypePrim Primitive
  | TypeTensor Type [Int]
  | TypeClosure [Type] Type
  | TypeRc
      { rcInner :: Type,
        rcAtomicity :: Atomicity,
        rcCapability :: Capability
      }
  | TypeRef
      { refInner :: Type,
        refAtomicity :: Atomicity,
        refCapability :: Capability
      }
  | TypeExpr
      { tyExprPath :: Path,
        tyExprArgs :: [Type]
      }
  | TypeNullable Type
  deriving (Eq, Show)

instance Emission PrimitiveInt where
  emit PrimInt8 = pure "i8"
  emit PrimInt16 = pure "i16"
  emit PrimInt32 = pure "i32"
  emit PrimInt64 = pure "i64"
  emit PrimInt128 = pure "i128"
  emit PrimIndex = pure "index"

instance Emission PrimitiveFloat where
  emit PrimFloat8 = pure "f8"
  emit PrimFloat16 = pure "f16"
  emit PrimBFloat16 = pure "bf16"
  emit PrimFloat32 = pure "f32"
  emit PrimFloat64 = pure "f64"
  emit PrimFloat128 = pure "f128"

instance Emission Primitive where
  emit (PrimInt bits) = emit bits
  emit (PrimFloat pft) = emit pft
  emit PrimBool = pure "i1"
  emit PrimUnit = pure "none"

instance Emission Type where
  emit (TypePrim prim) = emit prim
  emit _ = error "Emission for non-primitive types not implemented yet"

data Atomicity = Atomic | NonAtomic
  deriving (Eq, Show)

data Capability = Unspecified | Shared | Value | Flex | Rigid
  deriving (Eq, Show)

type RecordField = (Type, Capability)

data Record = Record
  { defaultCapability :: Capability,
    fields :: [(String, RecordField)]
  }
  deriving (Eq, Show)