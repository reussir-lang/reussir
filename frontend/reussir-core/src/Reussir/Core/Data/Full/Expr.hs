module Reussir.Core.Data.Full.Expr where

import Data.Digest.XXHash.FFI
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
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
    | Match Expr DecisionTree
    | RcWrap Expr
    | Sequence [Expr]
    | LambdaExpr
        { lamClosure :: [(VarID, Type)]
        -- ^ Captured free variables (implementation detail, not in the user-visible type)
        , lamArgs :: [(VarID, Type)]
        -- ^ Declared parameters
        , lamBody :: Expr
        -- ^ Body expression (runs at call time, not creation time)
        }
    | ClosureCall
        { closureCallTarget :: VarID
        -- ^ Local variable holding the closure
        , closureCallArgs :: [Expr]
        -- ^ Arguments to pass
        }
    deriving (Show, Eq)

newtype PatternVarRef = PatternVarRef {unPatternVarRef :: Seq.Seq Int}
    deriving (Show, Eq, Ord)

data DecisionTree
    = DTUncovered
    | DTUnreachable
    | DTLeaf
        { dtLeafBody :: Expr
        , dtLeafBindings :: IntMap.IntMap PatternVarRef
        }
    | DTGuard
        { dtGuardBindings :: IntMap.IntMap PatternVarRef
        , dtGuardExpr :: Expr
        , dtGuardTrue :: DecisionTree
        , dtGuardFalse :: DecisionTree
        }
    | DTSwitch
        { dtSwitchVarRef :: PatternVarRef
        , dtSwitchCases :: DTSwitchCases
        }
    deriving (Show, Eq)

data DTSwitchCases
    = DTSwitchInt
        { dtSwitchIntMap :: IntMap.IntMap DecisionTree
        , dtSwitchIntDefault :: DecisionTree
        }
    | DTSwitchBool
        { dtSwitchBoolTrue :: DecisionTree
        , dtSwitchBoolFalse :: DecisionTree
        }
    | DTSwitchCtor
        { dtSwitchCtorCases :: V.Vector DecisionTree
        }
    | DTSwitchString
        { dtSwitchStringMap :: HashMap.HashMap (XXH3 T.Text) DecisionTree
        , dtSwitchStringDefault :: DecisionTree
        }
    | DTSwitchNullable
        { dtSwitchNullableJust :: DecisionTree
        , dtSwitchNullableNothing :: DecisionTree
        }
    deriving (Show, Eq)

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
