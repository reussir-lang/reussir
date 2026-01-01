module Reussir.Core.Types.Type where

import Data.HashSet (HashSet)
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.Int (Int8)
import Data.List (intercalate)
import Reussir.Core.Types.Class
import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path (..))
import Reussir.Parser.Types.Stmt (Visibility)

{- | Represents integral types with a specific bit width.
Integral types can be either signed or unsigned.
-}
data IntegralType
    = -- | Signed integer with specified bit width (e.g., i8, i16, i32, i64)
      Signed {-# UNPACK #-} !Int8
    | -- | Unsigned integer with specified bit width (e.g., u8, u16, u32, u64)
      Unsigned {-# UNPACK #-} !Int8
    deriving (Eq)

instance Hashable IntegralType where
    hashWithSalt salt (Signed bits) =
        salt `hashWithSalt` True `hashWithSalt` bits
    hashWithSalt salt (Unsigned bits) =
        salt `hashWithSalt` False `hashWithSalt` bits

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

instance Hashable FloatingPointType where
    hashWithSalt salt (IEEEFloat bits) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` bits
    hashWithSalt salt BFloat16 =
        salt `hashWithSalt` (1 :: Int)
    hashWithSalt salt Float8 =
        salt `hashWithSalt` (2 :: Int)

{- | Represents a local hole identifier in the Reussir type system.
Holes are placeholders for types to be inferred later.
-}
newtype HoleID = HoleID {holeIDValue :: Int}
    deriving (Eq, Ord)

instance Hashable HoleID where
    hashWithSalt salt (HoleID val) = hashWithSalt salt val

instance Show HoleID where
    show (HoleID val) = "?" ++ show val

{- | Represents a type in the Reussir type system.
Types can be primitives (integral, floating-point, bool, string, unit)
or user-defined types with optional type parameters.
-}
data Type
    = -- | User-defined type with a path and optional type parameters
      TypeRecord Path [Type]
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
    | -- | Bottom type
      TypeBottom
    deriving (Eq)

instance Hashable Type where
    hashWithSalt salt (TypeRecord path args) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` path `hashWithSalt` args
    hashWithSalt salt (TypeIntegral it) =
        salt `hashWithSalt` (1 :: Int) `hashWithSalt` it
    hashWithSalt salt (TypeFP fpt) =
        salt `hashWithSalt` (2 :: Int) `hashWithSalt` fpt
    hashWithSalt salt TypeBool =
        salt `hashWithSalt` (3 :: Int)
    hashWithSalt salt TypeStr =
        salt `hashWithSalt` (4 :: Int)
    hashWithSalt salt TypeUnit =
        salt `hashWithSalt` (5 :: Int)
    hashWithSalt salt (TypeClosure args ret) =
        salt `hashWithSalt` (6 :: Int) `hashWithSalt` args `hashWithSalt` ret
    hashWithSalt salt (TypeGeneric generic) =
        salt `hashWithSalt` (7 :: Int) `hashWithSalt` generic
    hashWithSalt salt (TypeHole hole) =
        salt `hashWithSalt` (8 :: Int) `hashWithSalt` hole
    hashWithSalt salt (TypeRc t cap) =
        salt `hashWithSalt` (9 :: Int) `hashWithSalt` t `hashWithSalt` cap
    hashWithSalt salt (TypeRef t cap) =
        salt `hashWithSalt` (10 :: Int) `hashWithSalt` t `hashWithSalt` cap
    hashWithSalt salt TypeBottom =
        salt `hashWithSalt` (11 :: Int)

instance Show Type where
    show (TypeRecord path []) = show path
    show (TypeRecord path args) =
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
    show TypeBottom = "⊥"

newtype TypeClassTable = TypeClassTable
    { unTypeClassTable :: H.CuckooHashTable Type (HashSet Class)
    }
    deriving (Show)

type FieldFlag = Bool

data RecordFields
    = Named [(Identifier, Type, FieldFlag)]
    | Unnamed [(Type, FieldFlag)]
    | Variants [(Identifier, [Type])]
    deriving (Show, Eq)

data RecordKind = StructKind | EnumKind deriving (Show, Eq)

data Record = Record
    { recordName :: Identifier
    , recordTyParams :: [(Identifier, GenericID)]
    , recordFields :: RecordFields
    , recordKind :: RecordKind
    , recordVisibility :: Visibility
    , recordDefaultCap :: Capability
    }
    deriving (Show, Eq)
