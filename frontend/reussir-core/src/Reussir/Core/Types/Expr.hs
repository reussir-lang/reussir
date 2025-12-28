module Reussir.Core.Types.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Codegen.Intrinsics (Intrinsic)
import Reussir.Core.Types.String (StringToken)
import Reussir.Core.Types.Type (Type)

data ExprKind
    = GlobalStr StringToken
    | Constant Scientific
    | IntrinsicCall Intrinsic [Expr]
    | Poison
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    }
    deriving (Show, Eq)
