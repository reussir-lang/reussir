module Reussir.Core.Types.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Core.Types.String (StringToken)
import Reussir.Core.Types.Type (Type)

data ArithOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Show, Eq)

data CmpOp = Lt | Gt | Lte | Gte | Equ | Neq deriving (Show, Eq)

data ExprKind
    = GlobalStr StringToken
    | Constant Scientific
    | Negate Expr
    | Not Expr
    | Arith Expr ArithOp Expr
    | Cmp Expr CmpOp Expr
    | Cast Expr Type
    | ScfIfExpr Expr Expr Expr
    | Poison
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    }
    deriving (Show, Eq)
