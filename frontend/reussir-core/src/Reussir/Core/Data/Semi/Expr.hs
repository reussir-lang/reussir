module Reussir.Core.Data.Semi.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Vector.Unboxed qualified as UV
import Reussir.Core.Data.Operator (ArithOp, CmpOp)
import Reussir.Core.Data.Semi.Type (Type)
import Reussir.Core.Data.String (StringToken)
import Reussir.Core.Data.UniqueID (ExprID, VarID)
import Reussir.Parser.Types.Lexer (Identifier, Path)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Strict as V
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Digest.XXHash.FFI

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

data DecisionTree a
    = DTUncovered
    | DTUnreachable
    | DTLeaf {
        dtLeafBody :: a,
        dtLeafBindings :: [Maybe VarID] -- positional bindings (DB Level order)
    }
    | DTGuard {
        dtGuardBindings :: [Maybe VarID],
        dtGuardExpr :: a,
        dtGuardTrue :: DecisionTree a,
        dtGuardFalse :: DecisionTree a
    }
    | DTSwitch {
        dtSwitchDeBruijnLvl :: Int,  -- DB Level of the switch
        dtSwitchCases :: DTSwitchCases a
    }

data DTSwitchCases a
    = DTSwitchInt {
        dtSwitchIntMap :: IntMap.IntMap (DecisionTree a),
        dtSwitchIntDefault :: DecisionTree a
    }
    | DTSwitchBool {
        dtSwitchBoolTrue :: DecisionTree a,
        dtSwitchBoolFalse :: DecisionTree a
    }
    | DTSwitchCtor {
        dtSwitchCtorCases :: V.Vector (DecisionTree a),
        dtSwitchCtorDefault :: DecisionTree a
    }
    | DTSwitchString {
        dtSwitchStringMap :: HashMap.HashMap (XXH3 T.Text) (DecisionTree a),
        dtSwitchStringDefault :: DecisionTree a
    }

data Expr = Expr
    { exprKind :: ExprKind
    , exprSpan :: Maybe (Int64, Int64)
    , exprType :: Type
    , exprID :: ExprID -- unique identifier for each expression inside a function
    }
    deriving (Show, Eq)
