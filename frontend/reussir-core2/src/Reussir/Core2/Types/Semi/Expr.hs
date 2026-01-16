module Reussir.Core2.Types.Semi.Expr where

import Data.Hashable
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Core2.Types.Semi.Type (Type)
import Reussir.Core2.Types.String (StringToken)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path)

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
    | Regional Expr
    | ProjChain Expr [Int]
    | Assign Expr Int Expr -- for now, we only allow single-field assignment
    | Let
        { letVarSpan :: Maybe (Int64, Int64)
        , letVarID :: VarID
        , letVarName :: Identifier
        , letVarExpr :: Expr
        , letBodyExpr :: Expr
        }
    | FuncCall
        { funcCallTarget :: Path
        , funcCallTyArgs :: [Type]
        , funcCallArgs :: [Expr]
        , funcCallRegional :: Bool
        }
    | Poison
    | CompoundCall
        { compoundCallTarget :: Path
        , compoundCallTyArgs :: [Type]
        , compoundCallArgs :: [Expr]
        }
    | VariantCall
        { variantCallTarget :: Path
        , variantCallTyArgs :: [Type]
        , variantCallVariant :: Int
        , variantCallArg :: Expr
        }
    | NullableCall (Maybe Expr)
    deriving (Show, Eq)

newtype ExprID = ExprID {unExprID :: Int} deriving (Show, Eq, Hashable)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
