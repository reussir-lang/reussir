module Parser.Types.Expr where

data Constant
    = ConstInt Int
    | ConstDouble Double
    | ConstString String
    | ConstBool Bool

data BinaryOp = Add | Sub | Mul | Div
data UnaryOp  = Negate

data Expr
    = ConstExpr Constant
    | BinOpExpr BinaryOp Expr Expr
    | UnaryOpExpr UnaryOp Expr
