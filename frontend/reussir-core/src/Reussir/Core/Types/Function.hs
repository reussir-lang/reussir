module Reussir.Core.Types.Function where

import Data.HashTable.IO qualified as H
import Effectful.Prim.IORef.Strict (IORef')
import GHC.Int (Int64)
import Reussir.Core.Types (GenericID)
import Reussir.Core.Types.Expr (Expr)
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Stmt (Visibility)

data FunctionProto = FunctionProto
    { funcVisibility :: Visibility
    , funcName :: Identifier
    , funcGenerics :: [(Identifier, GenericID)]
    , funcParams :: [(Identifier, Type)]
    , funcReturnType :: Type
    , funcIsRegional :: Bool
    , funcBody :: IORef' (Maybe Expr)
    , funcSpan :: Maybe (Int64, Int64)
    }

newtype FunctionTable = FunctionTable
    { functionProtos :: H.CuckooHashTable Path FunctionProto
    }
