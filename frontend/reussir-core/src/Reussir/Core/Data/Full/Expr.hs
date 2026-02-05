module Reussir.Core.Data.Full.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.Vector.Unboxed qualified as UV

import Reussir.Core.Data.Full.Type (Type)
import Reussir.Core.Data.Operator (ArithOp, CmpOp)
import Reussir.Core.Data.String (StringToken)
import Reussir.Core.Data.UniqueID (ExprID, VarID)

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
    | RegionRun Expr
    | Proj Expr (UV.Vector Int)
    | Assign Expr Int Expr -- for now, we only allow single-field assignment
    | Let
        { letVarSpan :: Maybe (Int64, Int64)
        , letVarID :: VarID
        , letVarName :: Identifier
        , letVarExpr :: Expr
        }
    | FuncCall
        { funcCallTarget :: Symbol
        , funcCallArgs :: [Expr]
        , funcCallRegional :: Bool
        }
    | Poison
    | CompoundCall [Expr]
    | VariantCall Int Expr
    | NullableCall (Maybe Expr)
    | IntrinsicCall
        { intrinsicCallTarget :: Path
        , intrinsicCallArgs :: [Expr]
        }
    | RcWrap Expr
    | Sequence [Expr]
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
