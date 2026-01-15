module Reussir.Core2.Types.String where

import Data.Digest.XXHash.FFI (XXH3)
import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Word (Word64)

newtype StringToken = StringToken (Word64, Word64, Word64, Word64)
    deriving (Show, Eq)

newtype StringUniqifier = StringUniqifier
    { stringStorage :: H.CuckooHashTable (XXH3 T.Text) StringToken
    }
    deriving (Show)
