module Reussir.Core.Types.Class where

import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import Reussir.Parser.Types.Lexer (Path)

newtype Class = Class Path
    deriving (Show, Eq, Ord)

instance Hashable Class where
    hashWithSalt salt (Class path) = hashWithSalt salt path

type ChainID = Int64
type ChainPos = Int64

data ClassNode = ClassNode
    { chain :: (ChainID, ChainPos)
    , parents :: [Class]
    }
    deriving (Show, Eq)

data ClassDAG = ClassDAG
    { chainHead :: H.CuckooHashTable Int64 Class
    , classes :: H.CuckooHashTable Class ClassNode
    }
    deriving (Show)
