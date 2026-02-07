module Reussir.Core.Data.Semi.Type where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.List (intercalate)
import Reussir.Parser.Types.Lexer (Path (..))

import Data.HashTable.IO qualified as H

import Reussir.Core.Data.Class
import Reussir.Core.Data.FP
import Reussir.Core.Data.Integral
import Reussir.Core.Data.UniqueID (GenericID, HoleID)

data Flexivity = Irrelevant | Regional | Flex | Rigid
    deriving (Eq, Ord)

instance Hashable Flexivity where
    hashWithSalt salt Irrelevant = salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt Regional = salt `hashWithSalt` (1 :: Int)
    hashWithSalt salt Flex = salt `hashWithSalt` (2 :: Int)
    hashWithSalt salt Rigid = salt `hashWithSalt` (3 :: Int)

instance Show Flexivity where
    show Irrelevant = ""
    show Regional = "[regional]"
    show Flex = "[flex]"
    show Rigid = "[rigid]"

{- | Represents a type in the Reussir type system.
Data can be primitives (integral, floating-point, bool, string, unit)
or user-defined types with optional type parameters.
-}
data Type
    = -- | User-defined type with a path and optional type parameters
      TypeRecord
        { tyRecPath :: Path
        , tyRecParams :: [Type]
        , tyRecFlex :: Flexivity
        }
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
    | -- | Type of a nullable region
      TypeNullable Type
    deriving (Eq)

instance Hashable Type where
    hashWithSalt salt (TypeRecord path args flex) =
        salt
            `hashWithSalt` (0 :: Int)
            `hashWithSalt` path
            `hashWithSalt` args
            `hashWithSalt` flex
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
    hashWithSalt salt (TypeNullable t) =
        salt `hashWithSalt` (10 :: Int) `hashWithSalt` t

instance Show Type where
    show (TypeRecord path [] flex) = show flex <> " " <> show path
    show (TypeRecord path args flex) =
        show flex <> " " <> show path
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
    show (TypeNullable t) = "Nullable<" <> show t <> ">"

newtype TypeClassTable = TypeClassTable
    { unTypeClassTable :: H.CuckooHashTable Type (HashSet Class)
    }
    deriving (Show)
