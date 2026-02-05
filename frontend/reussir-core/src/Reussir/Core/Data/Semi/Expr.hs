module Reussir.Core.Data.Semi.Expr where

import Data.Digest.XXHash.FFI
import Data.Hashable
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV

import Reussir.Core.Data.Operator (ArithOp, CmpOp)
import Reussir.Core.Data.Semi.Type (Type)
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
    | Sequence [Expr]
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
    | IntrinsicCall
        { intrinsicCallTarget :: Path
        , intrinsicCallArgs :: [Expr]
        }
    deriving (Show, Eq)

newtype PatternVarRef = PatternVarRef {unPatternVarRef :: Seq.Seq Int}
    deriving (Show, Eq, Ord, Hashable)

data DecisionTree a
    = DTUncovered
    | DTUnreachable
    | DTLeaf
        { dtLeafBody :: a
        , dtLeafBindings :: IntMap.IntMap PatternVarRef
        }
    | DTGuard
        { dtGuardBindings :: IntMap.IntMap PatternVarRef
        , dtGuardExpr :: a
        , dtGuardTrue :: DecisionTree a
        , dtGuardFalse :: DecisionTree a
        }
    | DTSwitch
        { dtSwitchVarRef :: PatternVarRef -- DB index of the switch
        , dtSwitchCases :: DTSwitchCases a
        }

data DTSwitchCases a
    = DTSwitchInt
        { dtSwitchIntMap :: IntMap.IntMap (DecisionTree a)
        , dtSwitchIntDefault :: DecisionTree a
        }
    | DTSwitchBool
        { dtSwitchBoolTrue :: DecisionTree a
        , dtSwitchBoolFalse :: DecisionTree a
        }
    | DTSwitchCtor
        { dtSwitchCtorCases :: V.Vector (DecisionTree a)
        , dtSwitchCtorDefault :: DecisionTree a
        }
    | DTSwitchString
        { dtSwitchStringMap :: HashMap.HashMap (XXH3 T.Text) (DecisionTree a)
        , dtSwitchStringDefault :: DecisionTree a
        }
    | DTSwitchNullable
        { -- special case, since we specialize nullable types
          dtSwitchNullableJust :: DecisionTree a
        , dtSwitchNullableNothing :: DecisionTree a
        }

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
