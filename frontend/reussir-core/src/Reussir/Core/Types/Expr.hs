module Reussir.Core.Types.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Core.Types.String (StringToken)
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan)

data ArithOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Show, Eq)

data CmpOp = Lt | Gt | Lte | Gte | Equ | Neq deriving (Show, Eq)

newtype VarID = VarID {unVarID :: Int} deriving (Show, Eq)

data ExprKind
    = GlobalStr StringToken
    | Constant Scientific
    | Negate Expr
    | Not Expr
    | Arith Expr ArithOp Expr
    | Cmp Expr CmpOp Expr
    | Cast Expr Type
    | ScfIfExpr Expr Expr Expr
    | Var VarID
    | RcWrap Expr Capability
    | Let
        { letVarSpan :: (Int64, Int64)
        , letVarID :: VarID
        , letVarName :: Identifier
        , letVarExpr :: Expr
        , letBodyExpr :: Expr
        }
    | Poison
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    }
    deriving (Show, Eq)
