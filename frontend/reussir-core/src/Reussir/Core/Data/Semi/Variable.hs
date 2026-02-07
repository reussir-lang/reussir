module Reussir.Core.Data.Semi.Variable where

import Data.Int (Int64)
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Parser.Types.Lexer

import Data.HashTable.IO qualified as H
import Data.Sequence qualified as Seq

import Reussir.Core.Data.Semi.Type (Type)
import Reussir.Core.Data.UniqueID (VarID)

data VarDef = VarDef
    { varName :: Identifier
    , varSpan :: Maybe (Int64, Int64)
    , varType :: Type
    }

data VarTable = VarTable
    { localBindings :: H.CuckooHashTable Identifier VarID
    , uniqueBindings :: IORef' (Seq.Seq VarDef)
    }

data ChangeLog = ChangeLog
    { prevLocal :: (Identifier, Maybe VarID)
    , prevUnique :: Seq.Seq VarDef
    }
