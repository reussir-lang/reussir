module Reussir.Core.Types.Generic where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Lexer (Path)

data GenericVar = GenericVar
    { genericName :: T.Text
    , genericSpan :: Maybe (Int64, Int64)
    , genericLinks :: H.CuckooHashTable GenericID (Maybe Type)
    , genericBounds :: [Path]
    }

data GenericState = GenericState
    { getStateRef :: IORef' (Seq.Seq GenericVar)
    , concreteFlow :: H.CuckooHashTable GenericID [Type]
    }
