module Reussir.Core.Data.Ownership (
    OwnershipOp (..),
    OwnershipAction (..),
    OwnershipAnnotations (..),
    ExprFlux (..),
    emptyAction,
    emptyFlux,
    lookupAnnotation,
    insertAnnotation,
    mergeAction,
) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)

import Reussir.Core.Data.UniqueID (ExprID (..), VarID (..))

-- | Operations to emit for ownership management
data OwnershipOp
    = -- | rc.inc on the value produced by this expr
      OInc
    | -- | rc.dec on the value produced by this expr
      ODec
    | -- | rc.dec on a variable (by its VarID)
      ODecVar VarID
    deriving (Show, Eq)

-- | Where to emit relative to the expression
data OwnershipAction = OwnershipAction
    { oaBefore :: [OwnershipOp]
    -- ^ emit before evaluating this expr
    , oaAfter :: [OwnershipOp]
    -- ^ emit after evaluating this expr
    }
    deriving (Show, Eq)

-- | The result of the analysis: ExprID -> actions
newtype OwnershipAnnotations = OwnershipAnnotations
    { unAnnotations :: IntMap OwnershipAction
    }
    deriving (Show, Eq)

-- | Per-expression flux (intermediate, used during analysis)
data ExprFlux = ExprFlux
    { fluxUsed :: IntSet
    -- ^ VarIDs that must be alive (not consumed)
    , fluxConsumed :: IntMap Int
    -- ^ VarID -> consumption count
    , fluxCreated :: IntSet
    -- ^ VarIDs of new ownership objects created
    , fluxFreeVars :: IntSet
    -- ^ all RR-typed free vars (union of used + consumed keys)
    }
    deriving (Show, Eq)

emptyAction :: OwnershipAction
emptyAction = OwnershipAction [] []

emptyFlux :: ExprFlux
emptyFlux =
    ExprFlux
        { fluxUsed = mempty
        , fluxConsumed = IntMap.empty
        , fluxCreated = mempty
        , fluxFreeVars = mempty
        }

lookupAnnotation :: ExprID -> OwnershipAnnotations -> Maybe OwnershipAction
lookupAnnotation (ExprID eid) (OwnershipAnnotations m) = IntMap.lookup eid m

insertAnnotation :: ExprID -> OwnershipAction -> OwnershipAnnotations -> OwnershipAnnotations
insertAnnotation (ExprID eid) action (OwnershipAnnotations m) =
    OwnershipAnnotations $ IntMap.insert eid action m

mergeAction :: OwnershipAction -> OwnershipAction -> OwnershipAction
mergeAction (OwnershipAction b1 a1) (OwnershipAction b2 a2) =
    OwnershipAction (b1 ++ b2) (a1 ++ a2)
