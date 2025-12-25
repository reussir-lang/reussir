module Reussir.Parser.Types.Stmt where

import Reussir.Parser.Types.Expr (Expr)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan)
import Reussir.Parser.Types.Type (Type)

data Visibility = Public | Private deriving (Show)

data Stmt
    = Function Visibility Identifier [Identifier] [(Identifier, Type)] (Maybe Type) Expr
    | Struct Visibility Identifier [Type]
    | Enum Visibility Identifier [Identifier] [(Identifier, [Type])]
    | SpannedStmt (WithSpan Stmt)
    deriving (Show)
