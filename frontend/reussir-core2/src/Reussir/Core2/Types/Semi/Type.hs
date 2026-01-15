module Reussir.Core2.Types.Semi.Type where

import Data.HashSet (HashSet)
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.List (intercalate)
import Reussir.Core2.Types.Class
import Reussir.Core2.Types.FP
import Reussir.Core2.Types.GenericID (GenericID)
import Reussir.Core2.Types.Integral
import Reussir.Parser.Types.Lexer (Path (..))

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
    hashWithSalt salt TypeBottom =
        salt `hashWithSalt` (9 :: Int)

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
    show (TypeGeneric generic) = show generic
    show (TypeHole hole) = show hole
    show TypeBottom = "⊥"

newtype TypeClassTable = TypeClassTable
    { unTypeClassTable :: H.CuckooHashTable Type (HashSet Class)
    }
    deriving (Show)
