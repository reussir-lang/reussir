module Parser.Types.Expr where

newtype Identifier = Identifier String deriving Show

data Constant
    = ConstInt Int
    | ConstID Identifier
    | ConstDouble Double
    | ConstString String
    | ConstBool Bool
    deriving Show

data BinaryOp = Add | Sub | Mul | Div deriving Show
data UnaryOp  = Negate deriving Show

data Expr
    = ConstExpr Constant
    | BinOpExpr BinaryOp Expr Expr
    | UnaryOpExpr UnaryOp Expr
    deriving Show
