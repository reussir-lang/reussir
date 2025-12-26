module Reussir.Core.Types.MetaID where

import Data.Hashable (Hashable)
import Data.Int (Int64)

newtype MetaID = MetaID {metaIDValue :: Int64}
    deriving (Eq, Ord, Hashable)

instance Show MetaID where
    show (MetaID val) = "?" ++ show val
