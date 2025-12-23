module Reussir.Parser.Types.Expr where

import Data.List

newtype Identifier = Identifier {unIdentifier :: String}

instance Show Identifier where
    show (Identifier name) = '$' : name

data Typename = Typename String [Typename] | Arr Typename Typename

instance Show Typename where
    show (Typename name []) = '@' : name
    show (Typename name args) = '@' : name ++ "<" ++ intercalate ", " (map show args) ++ ">"
    show (Arr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

data Pattern = Pattern Identifier Identifier [Identifier]

instance Show Pattern where
    show (Pattern ns name es) = show ns ++ "::" ++ show name ++ "(" ++ intercalate ", " (map show es) ++ ")"

data Constant
    = ConstInt Int
    | ConstID Identifier
    | ConstDouble Double
    | ConstString String
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
    | FuncCall Identifier [Expr]
    | Lambda Identifier Typename Expr
    | Match Expr [(Pattern, Expr)]
    deriving (Show)
