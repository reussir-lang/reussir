module Reussir.Core.Data.Full.Context where

import Data.Int (Int64)
import Effectful (Eff, IOE)
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (State)
import Reussir.Core.Data.Full.Error (Error)
import Reussir.Core.Data.Full.Function (FunctionTable)
import Reussir.Core.Data.Full.Record (FullRecordTable, SemiRecordTable)
import Reussir.Core.Data.Full.Type (GenericMap)
import Reussir.Core.Data.String (StringUniqifier)

data FullContext = FullContext
    { ctxFunctions :: FunctionTable
    , ctxRecords :: FullRecordTable
    , ctxSemiRecords :: SemiRecordTable
    , ctxErrors :: [Error]
    , ctxStringUniqifier :: StringUniqifier
    , ctxFilePath :: FilePath
    }

data LocalFullContext = LocalFullContext
    { currentSpan :: Maybe (Int64, Int64)
    , genericMap :: GenericMap
    , exprCounter :: Int
    }

type FullEff = Eff '[IOE, Prim, Log, State FullContext, State LocalFullContext]
type GlobalFullEff = Eff '[IOE, Prim, Log, State FullContext]
