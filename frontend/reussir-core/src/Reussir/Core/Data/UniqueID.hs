module Reussir.Core.Data.UniqueID where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int64)

newtype GenericID = GenericID {genericIDValue :: Int64}
    deriving (Eq, Ord, Hashable)

instance Show GenericID where
    show (GenericID val) = "@" ++ show val

newtype VarID = VarID {unVarID :: Int}
    deriving (Eq, Ord, Hashable)

instance Show VarID where
    show (VarID val) = "#" ++ show val

newtype ExprID = ExprID {unExprID :: Int}
    deriving (Eq, Ord, Hashable)

instance Show ExprID where
    show (ExprID val) = "ε" ++ show val

{- | Represents a local hole identifier in the Reussir type system.
Holes are placeholders for types to be inferred later.
-}
newtype HoleID = HoleID {holeIDValue :: Int}
    deriving (Eq, Ord)

instance Hashable HoleID where
    hashWithSalt salt (HoleID val) = hashWithSalt salt val

instance Show HoleID where
    show (HoleID val) = "?" ++ show val
