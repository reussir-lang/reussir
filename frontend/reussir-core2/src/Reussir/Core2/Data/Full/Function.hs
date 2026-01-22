module Reussir.Core2.Data.Full.Function where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Core2.Data.Full.Expr qualified as Full
import Reussir.Core2.Data.Full.Type qualified as Full
import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Stmt (Visibility)

data Function = Function
    { funcVisibility :: Visibility
    , funcName :: Symbol
    , funcRawPath :: Path
    , funcInstantiatedTyArgs :: [Full.Type]
    , funcParams :: [(Identifier, Full.Type)]
    , funcReturnType :: Full.Type
    , funcIsRegional :: Bool
    , funcBody :: Maybe Full.Expr
    , funcSpan :: Maybe (Int64, Int64)
    }

type FunctionTable = H.CuckooHashTable Symbol Function
