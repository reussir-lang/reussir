module Reussir.Codegen.Type(
    PrimInt(..),
    PrimFloat(..),
    Primitive(..),
    Type(..)
) where

data PrimInt = PrimInt8 | PrimInt16 | PrimInt32 | PrimInt64 | PrimInt128
    deriving (Eq, Show)

data PrimFloat = PrimFloat8 | PrimFloat16 | PrimBFloat16 | PrimFloat32 | PrimFloat64 | PrimFloat128
    deriving (Eq, Show)

data Primitive
    = PrimInt PrimInt
    | PrimFloat PrimFloat
    | PrimBool
    | PrimUnit
    deriving (Eq, Show)

data Type
    = TypePrimitive Primitive
    | TypePointer Type
    | TypeTensor Type [Int]
    | TypeFunction [Type] Type
    deriving (Eq, Show)
