module Reussir.Parser.Types.Stmt where

import Reussir.Parser.Types.Expr (Expr, Typename)
import Reussir.Parser.Types.Type (Identifier)

data Visibility = Public | Private deriving (Show)

data Stmt
    = Function Visibility Identifier [Identifier] [(Identifier, Typename)] (Maybe Typename) Expr
    | Struct Visibility Identifier [Typename]
    | Enum Visibility Identifier [Identifier] [(Identifier, [Typename])]
    deriving (Show)
