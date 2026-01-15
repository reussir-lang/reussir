module Reussir.Core2.Types.GenericID where

import Data.Hashable (Hashable)
import Data.Int (Int64)

newtype GenericID = GenericID {genericIDValue :: Int64}
    deriving (Eq, Ord, Hashable)

instance Show GenericID where
    show (GenericID val) = "@" ++ show val
