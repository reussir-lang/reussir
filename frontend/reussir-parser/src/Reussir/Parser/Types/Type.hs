-- | Module for type representations in the Reussir parser.
module Reussir.Parser.Types.Type where

import Data.Int (Int16)
import Reussir.Parser.Types.Lexer (Path (..), WithSpan (..))

{- | Represents integral types with a specific bit width.
Integral types can be either signed or unsigned.
-}
data IntegralType
    = -- | Signed integer with specified bit width (e.g., i8, i16, i32, i64)
      Signed {-# UNPACK #-} !Int16
    | -- | Unsigned integer with specified bit width (e.g., u8, u16, u32, u64)
      Unsigned {-# UNPACK #-} !Int16
    deriving (Eq)

instance Show IntegralType where
    showsPrec _ (Signed bits) = showString "i" . shows bits
    showsPrec _ (Unsigned bits) = showString "u" . shows bits

{- | Represents floating-point types.
Supports IEEE floating-point formats as well as specialized formats like BFloat16 and Float8.
-}
data FloatingPointType
    = -- | IEEE floating-point with specified bit width (e.g., f16, f32, f64)
      IEEEFloat !Int16
    | -- | Brain Floating Point format (16-bit) used in ML applications
      BFloat16
    | -- | 8-bit floating-point format (specification pending)
      Float8
    deriving (Eq)

instance Show FloatingPointType where
    showsPrec _ (IEEEFloat bits) = showString "f" . shows bits
    showsPrec _ BFloat16 = showString "bfloat16"
    showsPrec _ Float8 = showString "float8"

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
    | -- | Bottom type
      TypeBottom
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
    showsPrec _ (TypeExpr path []) = shows path
    showsPrec p (TypeExpr path args) =
        shows path
            . showString "<"
            . showArgs args
            . showString ">"
      where
        showArgs [] = id
        showArgs [x] = shows x
        showArgs (x : xs) = shows x . showString ", " . showArgs xs
    showsPrec _ (TypeIntegral it) = shows it
    showsPrec _ (TypeFP fpt) = shows fpt
    showsPrec _ TypeBool = showString "bool"
    showsPrec _ TypeStr = showString "str"
    showsPrec _ TypeUnit = showString "unit"
    showsPrec _ (TypeArrow a b) =
        showParen True $
            shows a . showString " -> " . shows b
    showsPrec p (TypeSpanned ws) = showsPrec p (spanValue ws)
    showsPrec _ TypeBottom = showString "⊥"
