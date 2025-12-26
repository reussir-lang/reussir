module Reussir.Core.Types.Meta where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Core.Types.MetaID (MetaID)
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Lexer (Path)

data MetaVar = MetaVar
    { metaName :: T.Text
    , metaSpan :: Maybe (Int64, Int64)
    , metaLinks :: H.CuckooHashTable MetaID (Maybe Type)
    , metaBounds :: [Path]
    }

newtype MetaState = MetaState {getStateRef :: IORef' (Seq.Seq MetaVar)}
