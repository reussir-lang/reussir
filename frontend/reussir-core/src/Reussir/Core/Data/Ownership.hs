{- |
Module      : Reussir.Core.Data.Ownership
Description : Data types for ownership analysis results

This module defines the data structures produced by the ownership analysis
pass ('Reussir.Core.Ownership'). These are consumed by the code generation
phase to emit RC (reference counting) operations.

== Data Flow

@
  Ownership Analysis           Code Generation (Lowering)
  ──────────────────           ──────────────────────────
  analyzeFunction              lowerExpr
       │                            │
       │ produces                   │ consumes
       v                            v
  OwnershipAnnotations ────> lookupAnnotation(exprID)
       │                            │
       │ contains                   │ reads
       v                            v
  ExprID -> OwnershipAction     oaBefore: [OIncVar v3]
               │                oaAfter:  [ODecVar v1]
               │                     │
               v                     v
  [OInc, ODec, ODecVar, OIncVar]   emit reussir.rc.inc / reussir.rc.dec
@
-}
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
import Data.IntSet (IntSet)

import Data.IntMap.Strict qualified as IntMap

import Reussir.Core.Data.UniqueID (ExprID (..), VarID (..))

{- | Operations to emit for ownership management.

These map directly to MLIR operations in the generated code:

@
  OInc          -> reussir.rc.inc %expr_result
                   (increment RC of the expression's produced value)

  ODec          -> reussir.rc.dec %expr_result
                   (decrement RC of the expression's produced value)

  ODecVar vid   -> reussir.rc.dec %variable
                   (decrement RC of a specific variable, may trigger dealloc)

  OIncVar vid   -> reussir.rc.inc %variable
                   (increment RC of a variable before it's consumed multiple times)
@

=== When Each Op Is Used

@
  OInc:    Projection (x.field) produces an RC value borrowed from x.
           We need to inc it because borrowing doesn't transfer ownership.

  ODec:    (Currently unused in the analysis, reserved for future use)

  ODecVar: Variable is no longer needed. Placed at end-of-scope,
           in early-dec positions, or in branch reconciliation.

  OIncVar: A variable is passed to TWO function calls in the same arg list.
           The first call consumes it, so we need to inc before to keep
           a copy for the second call.

           Example:  foo(x, bar(x))
                            ^^^ x consumed by bar
                     ^^^ x also consumed by foo -> need OIncVar x first
@
-}
data OwnershipOp
    = OInc
    -- ^ rc.inc on the value produced by this expression
    | ODec
    -- ^ rc.dec on the value produced by this expression
    | ODecVar VarID
    -- ^ rc.dec on a specific variable (by VarID)
    | OIncVar VarID
    -- ^ rc.inc on a specific variable, used when a var is consumed multiple times
    deriving (Show, Eq)

{- | Where to emit RC operations relative to an expression.

Each expression can have operations emitted /before/ and\/or /after/ it.

@
  oaBefore: executed BEFORE the expression is evaluated
            (e.g., OIncVar to bump RC before a consuming call)

  oaAfter:  executed AFTER the expression is evaluated
            (e.g., ODecVar to drop a variable after its last use)

  Timeline for expression E with annotations:

    [oaBefore ops]  -->  evaluate E  -->  [oaAfter ops]
@
-}
data OwnershipAction = OwnershipAction
    { oaBefore :: [OwnershipOp]
    -- ^ Emit before evaluating this expression
    , oaAfter :: [OwnershipOp]
    -- ^ Emit after evaluating this expression
    }
    deriving (Show, Eq)

-- | The result of the analysis: ExprID -> actions
newtype OwnershipAnnotations = OwnershipAnnotations
    { unAnnotations :: IntMap OwnershipAction
    }
    deriving (Show, Eq)

{- | Per-expression flux: intermediate ownership information flowing
out of an expression during analysis.

This is the "return value" of 'analyzeExpr'. It tells the caller
what ownership effects the expression had.

@
  Example: analyzing expression "x" (a variable reference)
    if x is RR-typed:
      ExprFlux { fluxFreeVars = {x_id} }
    if x is NOT RR-typed:
      emptyFlux

  The caller (e.g., analyzeConsumingCall) uses fluxFreeVars to know
  which variables to consume (decrement ownership count).
@

Note: Currently only 'fluxFreeVars' is actively used by the analysis.
The other fields (fluxUsed, fluxConsumed, fluxCreated) are defined
for potential future use in more sophisticated analyses.
-}
data ExprFlux = ExprFlux
    { fluxUsed :: IntSet
    -- ^ VarIDs that must be alive (not consumed)
    , fluxConsumed :: IntMap Int
    -- ^ VarID -> consumption count
    , fluxCreated :: IntSet
    -- ^ VarIDs of new ownership objects created
    , fluxFreeVars :: IntSet
    -- ^ All RR-typed free variable IDs referenced by this expression.
    -- This is the primary field used during analysis.
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

insertAnnotation ::
    ExprID -> OwnershipAction -> OwnershipAnnotations -> OwnershipAnnotations
insertAnnotation (ExprID eid) action (OwnershipAnnotations m) =
    OwnershipAnnotations $ IntMap.insert eid action m

mergeAction :: OwnershipAction -> OwnershipAction -> OwnershipAction
mergeAction (OwnershipAction b1 a1) (OwnershipAction b2 a2) =
    OwnershipAction (b1 ++ b2) (a1 ++ a2)
