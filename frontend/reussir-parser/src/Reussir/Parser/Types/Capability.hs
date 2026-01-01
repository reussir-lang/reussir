module Reussir.Parser.Types.Capability where

import Data.Hashable (Hashable (hashWithSalt))

data Capability
    = Unspecified
    | Shared
    | Value
    | Flex
    | Rigid
    | Field
    | Regional
    deriving (Show, Eq)

instance Hashable Capability where
    hashWithSalt salt Unspecified = hashWithSalt salt (0 :: Int)
    hashWithSalt salt Shared = hashWithSalt salt (1 :: Int)
    hashWithSalt salt Value = hashWithSalt salt (2 :: Int)
    hashWithSalt salt Flex = hashWithSalt salt (3 :: Int)
    hashWithSalt salt Rigid = hashWithSalt salt (4 :: Int)
    hashWithSalt salt Field = hashWithSalt salt (5 :: Int)
    hashWithSalt salt Regional = hashWithSalt salt (6 :: Int)
