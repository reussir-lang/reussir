{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Type.Data
  ( PrimitiveFloat (..),
    PrimitiveInt (..),
    Primitive (..),
    Atomicity (..),
    Capability (..),
    Type (..),
  )
where

import Reussir.Codegen.Context (Path)

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

data Atomicity = Atomic | NonAtomic
  deriving (Eq, Show)

data Capability = Unspecified | Shared | Value | Flex | Rigid
  deriving (Eq, Show)

