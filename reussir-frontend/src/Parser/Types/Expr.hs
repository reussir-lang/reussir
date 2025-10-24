module Parser.Types.Expr where

data Constant
    = ConstInt Int
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
