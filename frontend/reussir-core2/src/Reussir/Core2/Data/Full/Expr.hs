module Reussir.Core2.Data.Full.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Vector.Unboxed qualified as UV
import Reussir.Core2.Data.Full.Type (Type)
import Reussir.Core2.Data.Operator (ArithOp, CmpOp)
import Reussir.Core2.Data.String (StringToken)
import Reussir.Core2.Data.UniqueID (ExprID, VarID)
import Reussir.Parser.Types.Lexer (Identifier, Path)

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
        , letBodyExpr :: Expr
        }
    | FuncCall
        { funcCallTarget :: Path
        , funcCallTyArgs :: [Type]
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
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
