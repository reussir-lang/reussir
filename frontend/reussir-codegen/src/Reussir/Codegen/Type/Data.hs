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
    Expr (..),
    Closure (..),
    Tensor (..),
    isRefType,
    isBoolType,
)
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Reussir.Codegen.Context.Path (Path)

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
data Expr = Expr {exprPath :: Path, exprArgs :: [Type]}
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
    | TypeExpr Expr
    | TypeNullable Type
    | TypeRegion -- Region handle
    deriving (Eq, Show, Hashable, Generic)

data Atomicity = Atomic | NonAtomic
    deriving (Eq, Show, Hashable, Generic)

data Capability = Unspecified | Shared | Value | Flex | Rigid | Field
    deriving (Eq, Show, Hashable, Generic)

isBoolType :: Type -> Bool
isBoolType (TypePrim PrimBool) = True
isBoolType _ = False

isRefType :: Type -> Bool
isRefType (TypeRef _) = True
isRefType _ = False
