module Parser.Types.Expr where

newtype Identifier = Identifier String deriving Show
newtype Typename = Typename String deriving Show

data Constant
    = ConstInt Int
    | ConstID Identifier
    | ConstDouble Double
    | ConstString String
    | ConstBool Bool
    deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Equ | Neq 
              | And | Or deriving Show
data UnaryOp  = Negate | Not deriving Show

data Expr
    = ConstExpr Constant
    | BinOpExpr BinaryOp Expr Expr
    | UnaryOpExpr UnaryOp Expr
    | If Expr Expr Expr
    | Cast Typename Expr
    | LetIn Identifier Expr Expr
    deriving Show
