{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Type.Data
  ( PrimitiveFloat (..),
    PrimitiveInt (..),
    Primitive (..),
    Atomicity (..),
    Capability (..),
    Type (..),
    Rc (..),
    Ref (..),
    Expr (..),
    Closure (..),
    Tensor (..),
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

data Rc = Rc { rcBoxInner :: Type, rcBoxAtomicity :: Atomicity, rcBoxCapability :: Capability }
  deriving (Eq, Show)
data Ref = Ref { refInner :: Type, refAtomicity :: Atomicity, refCapability :: Capability }
  deriving (Eq, Show)
data Expr = Expr { exprPath :: Path, exprArgs :: [Type] }
  deriving (Eq, Show)
data Closure = Closure { closureArgs :: [Type], closureReturnType :: Type }
  deriving (Eq, Show)
data Tensor = Tensor { tensorEleTy :: Type, tensorDimensions :: [Int] }
  deriving (Eq, Show)

data Type
  = TypePrim Primitive
  | TypeTensor Tensor
  | TypeClosure Closure
  | TypeRc Rc
  | TypeRef Ref
  | TypeExpr Expr
  deriving (Eq, Show)

data Atomicity = Atomic | NonAtomic
  deriving (Eq, Show)

data Capability = Unspecified | Shared | Value | Flex | Rigid
  deriving (Eq, Show)

