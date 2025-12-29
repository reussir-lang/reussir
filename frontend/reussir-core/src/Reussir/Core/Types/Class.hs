module Reussir.Core.Types.Class where

import Data.Array (Array)
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Parser.Types.Lexer (Path)

newtype Class = Class Path
    deriving (Show, Eq, Ord)

instance Hashable Class where
    hashWithSalt salt (Class path) = hashWithSalt salt path

type ChainID = Int64
type ChainPos = Int64

data ClassNode = ClassNode
    { chain :: (ChainID, ChainPos)
    , parents :: [Int]
    }
    deriving (Show, Eq)

data ClassDAG = ClassDAG
    { nameMap :: H.CuckooHashTable Class Int
    , idMap :: H.CuckooHashTable Int Class
    , nodeMap :: H.CuckooHashTable Int ClassNode
    , chainHeadMap :: H.CuckooHashTable ChainID Int
    , counter :: IORef' Int
    , finalizedGraph :: IORef' (Maybe (Array Int ClassNode))
    }

instance Show ClassDAG where
    show _ = "<ClassDAG>"

type TypeBound = [Class]
