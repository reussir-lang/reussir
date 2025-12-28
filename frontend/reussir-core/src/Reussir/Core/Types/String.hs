module Reussir.Core.Types.String where

import Data.HashTable.IO qualified as H
import Data.Sequence qualified as Seq
import Data.Text qualified as T

type StringToken = (Int, Int)

newtype StringUniqifier = StringUniqifier
    { stringStorage :: H.CuckooHashTable Int (Seq.Seq T.Text)
    }
    deriving (Show)
