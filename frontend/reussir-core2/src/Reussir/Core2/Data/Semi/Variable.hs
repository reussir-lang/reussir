module Reussir.Core2.Data.Semi.Variable where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Core2.Data.Semi.Type (Type)
import Reussir.Core2.Data.UniqueID (VarID)
import Reussir.Parser.Types.Lexer

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
