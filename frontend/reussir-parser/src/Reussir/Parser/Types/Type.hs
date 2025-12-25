-- | Module for type representations in the Reussir parser.
module Reussir.Parser.Types.Type where

import Data.Int (Int8)
import Data.List (intercalate)
import Reussir.Parser.Types.Lexer (Path (..), WithSpan (..))

{- | Represents integral types with a specific bit width.
Integral types can be either signed or unsigned.
-}
data IntegralType
    = -- | Signed integer with specified bit width (e.g., i8, i16, i32, i64)
      Signed {-# UNPACK #-} !Int8
    | -- | Unsigned integer with specified bit width (e.g., u8, u16, u32, u64)
      Unsigned {-# UNPACK #-} !Int8
    deriving (Eq)

instance Show IntegralType where
    show (Signed bits) = "i" ++ show bits
    show (Unsigned bits) = "u" ++ show bits

{- | Represents floating-point types.
Supports IEEE floating-point formats as well as specialized formats like BFloat16 and Float8.
-}
data FloatingPointType
    = -- | IEEE floating-point with specified bit width (e.g., f16, f32, f64)
      IEEEFloat !Int8
    | -- | Brain Floating Point format (16-bit) used in ML applications
      BFloat16
    | -- | 8-bit floating-point format (specification pending)
      Float8
    deriving (Eq)

instance Show FloatingPointType where
    show (IEEEFloat bits) = "f" ++ show bits
    show BFloat16 = "bfloat16"
    show Float8 = "float8"

{- | Represents a type in the Reussir type system.
Types can be primitives (integral, floating-point, bool, string, unit)
or user-defined types with optional type parameters.
-}
data Type
    = -- | User-defined type with a path and optional type parameters
      TypeExpr Path [Type]
    | -- | Integral type (signed or unsigned)
      TypeIntegral IntegralType
    | -- | Floating-point type
      TypeFP FloatingPointType
    | -- | Boolean type
      TypeBool
    | -- | String type
      TypeStr
    | -- | Unit type (similar to void)
      TypeUnit
    | -- | Arrow type
      TypeArrow Type Type
    | -- | Spanned
      TypeSpanned (WithSpan Type)

instance Eq Type where
    (TypeExpr p1 a1) == (TypeExpr p2 a2) = p1 == p2 && a1 == a2
    (TypeIntegral i1) == (TypeIntegral i2) = i1 == i2
    (TypeFP f1) == (TypeFP f2) = f1 == f2
    TypeBool == TypeBool = True
    TypeStr == TypeStr = True
    TypeUnit == TypeUnit = True
    (TypeArrow a1 b1) == (TypeArrow a2 b2) = a1 == a2 && b1 == b2
    (TypeSpanned w1) == t2 = spanValue w1 == t2
    t1 == (TypeSpanned w2) = t1 == spanValue w2
    _ == _ = False

instance Show Type where
    show (TypeExpr path []) = show path
    show (TypeExpr path args) =
        show path
            ++ "<"
            ++ intercalate ", " (map show args)
            ++ ">"
    show (TypeIntegral it) = show it
    show (TypeFP fpt) = show fpt
    show TypeBool = "bool"
    show TypeStr = "str"
    show TypeUnit = "unit"
    show (TypeArrow a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (TypeSpanned ws) = show ws
