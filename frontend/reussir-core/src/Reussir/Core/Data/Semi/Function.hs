module Reussir.Core.Data.Semi.Function where

import Data.HashTable.IO qualified as H
import Effectful.Prim.IORef.Strict (IORef')
import GHC.Int (Int64)
import Reussir.Core.Data.Semi.Expr qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Data.UniqueID (GenericID)
import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Stmt (Visibility)

{- | Function prototype in the Semi Phase. At this point memory management except
| for flexivity annotation is irrelevant.
-}
data FunctionProto = FunctionProto
    { funcVisibility :: Visibility
    , funcName :: Identifier
    , funcGenerics :: [(Identifier, GenericID)]
    , funcParams :: [(Identifier, Semi.Type)]
    , funcReturnType :: Semi.Type
    , funcIsRegional :: Bool
    , funcBody :: IORef' (Maybe Semi.Expr)
    , funcSpan :: Maybe (Int64, Int64)
    }

newtype FunctionTable = FunctionTable
    { functionProtos :: H.CuckooHashTable Path FunctionProto
    }
