module Reussir.Parser.Types.Stmt where

import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Expr (Expr)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan)
import Reussir.Parser.Types.Type (Type)

data Visibility = Public | Private deriving (Show, Eq)

data RecordFields
    = Named [(Identifier, Type, Capability)]
    | Unnamed [(Type, Capability)]
    | Variants [(Identifier, [Type])]
    deriving (Show, Eq)

data RecordKind = StructKind | EnumKind deriving (Show, Eq)

data Record = Record
    { recordName :: Identifier
    , recordTyParams :: [Identifier]
    , recordFields :: RecordFields
    , recordKind :: RecordKind
    , recordVisibility :: Visibility
    , recordDefaultCap :: Capability
    }
    deriving (Show, Eq)

data Function = Function
    { funcVisibility :: Visibility
    , funcName :: Identifier
    , funcGenerics :: [Identifier]
    , funcParams :: [(Identifier, Type, Capability)]
    , funcReturnType :: Maybe (Type, Capability)
    , funcIsRegional :: Bool
    , funcBody :: Maybe Expr
    }
    deriving (Show, Eq)

data Stmt
    = FunctionStmt Function
    | RecordStmt Record
    | SpannedStmt (WithSpan Stmt)
    deriving (Show, Eq)
