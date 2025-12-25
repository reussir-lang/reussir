module Reussir.Parser.Types.Expr where

import Data.List
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Reussir.Parser.Types.Lexer (Identifier (..), Path, WithSpan)
import Reussir.Parser.Types.Type (Type)

data Pattern = Pattern Identifier Identifier [Identifier]

instance Show Pattern where
    show (Pattern ns name es) = show ns ++ "::" ++ show name ++ "(" ++ intercalate ", " (map show es) ++ ")"

data Constant
    = ConstInt Int
    | ConstDouble Scientific
    | ConstString T.Text
    | ConstBool Bool
    deriving (Show)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Gt
    | Lte
    | Gte
    | Equ
    | Neq
    | And
    | Or
    deriving (Show)
data UnaryOp = Negate | Not deriving (Show)

data Expr
    = ConstExpr Constant
    | BinOpExpr BinaryOp Expr Expr
    | UnaryOpExpr UnaryOp Expr
    | If Expr Expr Expr
    | Cast Type Expr
    | LetIn Identifier Expr Expr
    | FuncCall Path [Expr]
    | Lambda Identifier Type Expr
    | Match Expr [(Pattern, Expr)]
    | Var Path
    | SpannedExpr (WithSpan Expr)
    deriving (Show)
