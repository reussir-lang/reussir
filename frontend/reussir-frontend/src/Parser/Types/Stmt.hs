module Parser.Types.Stmt where

import Parser.Types.Expr

data Visibility = Public | Private deriving Show

data Stmt 
    = Function Visibility Identifier [Identifier] [(Identifier, Typename)] (Maybe Typename) Expr
    | Struct Visibility Identifier [Typename]
    | Enum Visibility Identifier [Identifier] [(Identifier, [Typename])]
    deriving Show
