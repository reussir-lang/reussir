-- | Module for type representations in the Reussir parser.
module Reussir.Parser.Types.Type where

import Data.Int (Int8)
import Data.List (intercalate)
import Reussir.Parser.Types.Lexer (Path (..))

-- | Represents integral types with a specific bit width.
-- Integral types can be either signed or unsigned.
data IntegralType
    = Signed {-# UNPACK #-} !Int8   -- ^ Signed integer with specified bit width (e.g., i8, i16, i32, i64)
    | Unsigned {-# UNPACK #-} !Int8 -- ^ Unsigned integer with specified bit width (e.g., u8, u16, u32, u64)

instance Show IntegralType where
    show (Signed bits) = "i" ++ show bits
    show (Unsigned bits) = "u" ++ show bits

-- | Represents floating-point types.
-- Supports IEEE floating-point formats as well as specialized formats like BFloat16 and Float8.
data FloatingPointType
    = IEEEFloat !Int8 -- ^ IEEE floating-point with specified bit width (e.g., f16, f32, f64)
    | BFloat16        -- ^ Brain Floating Point format (16-bit) used in ML applications
    | Float8          -- ^ 8-bit floating-point format (specification pending)

instance Show FloatingPointType where
    show (IEEEFloat bits) = "f" ++ show bits
    show BFloat16 = "bfloat16"
    show Float8 = "float8"

-- | Represents a type in the Reussir type system.
-- Types can be primitives (integral, floating-point, bool, string, unit)
-- or user-defined types with optional type parameters.
data Type
    = TypeExpr Path [Type]        -- ^ User-defined type with a path and optional type parameters
    | TypeIntegral IntegralType   -- ^ Integral type (signed or unsigned)
    | TypeFP FloatingPointType    -- ^ Floating-point type
    | TypeBool                    -- ^ Boolean type
    | TypeStr                     -- ^ String type
    | TypeUnit                    -- ^ Unit type (similar to void)

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
