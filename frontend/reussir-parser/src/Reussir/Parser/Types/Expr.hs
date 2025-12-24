module Reussir.Parser.Types.Expr where

import Data.List
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Reussir.Parser.Types.Lexer (Identifier (..), Path)

data Typename = Typename Identifier [Typename] | Arr Typename Typename

instance Show Typename where
    show (Typename name []) = '@' : T.unpack (unIdentifier name)
    show (Typename name args) =
        '@'
            : T.unpack (unIdentifier name)
            ++ "<"
            ++ intercalate ", " (map show args)
            ++ ">"
    show (Arr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

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
    | Cast Typename Expr
    | LetIn Identifier Expr Expr
    | FuncCall Path [Expr]
    | Lambda Identifier Typename Expr
    | Match Expr [(Pattern, Expr)]
    | Var Path
    deriving (Show)
