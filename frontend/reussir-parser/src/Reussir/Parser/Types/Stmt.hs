module Reussir.Parser.Types.Stmt where

import Reussir.Parser.Types.Expr (Expr)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan)
import Reussir.Parser.Types.Type (Type)

data Visibility = Public | Private deriving (Show, Eq)

data RecordFields
    = Named [(Identifier, Type, Capability)]
    | Unnamed [(Type, Capability)]
    | Variants [(Identifier, [Type])]
    deriving (Show, Eq)

data Capability
    = Unspecified
    | Shared
    | Value
    | Flex
    | Rigid
    | Field
    deriving (Show, Eq)

data RecordKind = StructKind | EnumKind deriving (Show, Eq)

data Record = Record
    { recordName :: Identifier
    , recordTyParams :: [Identifier]
    , recordFields :: RecordFields
    , recordKind :: RecordKind
    , recordVisibility :: Visibility
    }
    deriving (Show, Eq)

data Stmt
    = Function Visibility Identifier [Identifier] [(Identifier, Type)] (Maybe Type) Expr
    | RecordStmt Record
    | SpannedStmt (WithSpan Stmt)
    deriving (Show, Eq)
