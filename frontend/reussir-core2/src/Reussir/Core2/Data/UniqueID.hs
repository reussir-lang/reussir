module Reussir.Core2.Data.UniqueID where

import Data.Hashable (Hashable)
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
