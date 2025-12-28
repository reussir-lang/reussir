module Reussir.Core.Types.Type where

import Data.Int (Int8)
import Data.List (intercalate)
import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Path (..))

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

{- | Represents a local hole identifier in the Reussir type system.
Holes are placeholders for types to be inferred later.
-}
newtype HoleID = HoleID {holeIDValue :: Int}
    deriving (Eq, Ord)

instance Show HoleID where
    show (HoleID val) = "?" ++ show val

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
    | -- | Closure type
      TypeClosure [Type] Type
    | -- | Generic Variable (meta from interface, awaiting for instantiation)
      TypeGeneric GenericID
    | -- | Local hole (meta local to function, to be solved during type inference)
      TypeHole HoleID
    | -- | Rc type (boxed object)
      TypeRc Type Capability
    | -- | Reference type (projected)
      TypeRef Type Capability
    deriving (Eq)

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
    show (TypeClosure args ret) =
        "(" ++ intercalate ", " (map show args) ++ ") -> " ++ show ret
    show (TypeRc t cap) = "Rc<" ++ show t ++ ", " ++ show cap ++ ">"
    show (TypeGeneric generic) = show generic
    show (TypeHole hole) = show hole
    show (TypeRef t cap) = "&" ++ show cap ++ " " ++ show t
