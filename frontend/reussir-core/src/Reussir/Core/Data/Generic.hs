module Reussir.Core.Data.Generic where

import Data.Int (Int64)
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.HashTable.IO qualified as H
import Data.Sequence qualified as Seq

import Reussir.Core.Data.Semi.Type (Type)
import Reussir.Core.Data.UniqueID (GenericID)

data GenericVar = GenericVar
    { genericName :: Identifier
    , genericSpan :: Maybe (Int64, Int64)
    , genericLinks :: H.CuckooHashTable GenericID (Maybe Type)
    , genericBounds :: [Path]
    }

data GenericState = GenericState
    { getStateRef :: IORef' (Seq.Seq GenericVar)
    , concreteFlow :: H.CuckooHashTable GenericID [Type]
    }

type GenericSolution = H.CuckooHashTable GenericID [Type]
