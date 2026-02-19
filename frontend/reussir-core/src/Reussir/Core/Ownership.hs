{- |
Module      : Reussir.Core.Ownership
Description : Ownership analysis and reference counting annotation

= Overview

This module performs /ownership analysis/ on fully-elaborated Reussir
expressions. It determines where to insert reference counting operations
(@rc.inc@ and @rc.dec@) to manage memory safely and efficiently.

== What Problem Does This Solve?

Reussir uses reference counting (RC) for heap-allocated values. Every RC
value has an ownership count. When you create a value, it starts with count 1.
When you pass it to a function, the ownership transfers. When nobody owns it
anymore, it gets freed.

This module answers: "For each expression in a function, what RC operations
(inc/dec) need to happen, and when?"

== Key Concepts

=== Ownership Count (the \"Ledger\")

The analysis maintains a ledger: @VarID -> Int@ tracking how many
outstanding ownership units each variable has.

@
  fn example(x: Rc<Foo>) -> Rc<Foo> {
      let y = x;       // x: 1 -> 0 (consumed), y: 0 -> 1 (granted)
      let z = y;       // y: 1 -> 0 (consumed), z: 0 -> 1 (granted)
      z                // z: 1 -> 0 (returned to caller)
  }
  // At function end: all counts should be 0
@

=== Resource-Relevant Types (isRR)

Not all types need RC tracking. A type is /resource-relevant/ (RR) if it
directly or transitively contains an RC-managed value:

@
  i32                    -> NOT RR (plain integer)
  bool                   -> NOT RR (plain boolean)
  Rc<Foo>                -> RR (directly RC)
  Nullable<Rc<Foo>>      -> RR (transitively contains RC)
  Point { x: i32, y: Rc<Bar> } -> RR (field y is RC)
  Vec<i32>               -> RR (Vec itself is RC-managed, shared cap)
@

=== ExprFlux (Information Flow)

Each expression analysis returns an 'ExprFlux' describing what ownership
"flows" out of the expression:

@
  ExprFlux {
    fluxFreeVars = {v0, v3}  -- these RR-typed vars appear in this expr
  }
@

The caller uses this to know which variables were referenced and whether
to consume their ownership.

== The Analysis Algorithm

=== 1. Function Entry

@
  analyzeFunction(fn foo(x: Rc<A>, y: i32, z: Rc<B>) -> ...) {
      Initial ledger: { x: 1, z: 1 }    // RR params get ownership 1
                      // y: not tracked  // non-RR params ignored
  }
@

=== 2. Expression-by-Expression Analysis

For each expression kind, the analysis decides:

@
  ┌──────────────────────┬────────────────────────────────────────┐
  │ Expression Kind      │ Ownership Effect                       │
  ├──────────────────────┼────────────────────────────────────────┤
  │ Var x                │ Returns x in fluxFreeVars (no consume) │
  │ Let y = expr         │ Consume expr's free vars, grant y:1    │
  │ FuncCall(f, args)    │ Consume each arg's free vars           │
  │ CompoundCall(args)   │ Consume all args (creating new value)  │
  │ Proj base.field      │ Borrow base (no consume); if field is  │
  │                      │   RC, emit OInc on result              │
  │ If cond then else    │ Analyze branches separately, reconcile │
  │ Match scr { dt }     │ Borrow scrutinee, analyze DT branches  │
  │ Sequence [e1,..,en]  │ Analyze sequentially with early decs   │
  │ Constant / Arith     │ No ownership effect                    │
  └──────────────────────┴────────────────────────────────────────┘
@

=== 3. Early Dec Placement (Sequence Analysis)

The analysis eagerly places @rc.dec@ as soon as a variable is no longer
needed by any subsequent expression:

@
  {
      let a = make_rc();    // ledger: { a: 1 }
      let b = use(a);       // a consumed -> ledger: { b: 1 }
      let c = pure_fn();    // a not in suffix free vars -> dec a HERE
      use(b);               // b consumed
      c                     // return c
  }
@

The suffix free-var sets are precomputed:

@
  exprs:    [let a = .., let b = use(a), let c = .., use(b), c]
  freeVars: [{},         {a},            {},          {b},    {c}]
  suffixes: [{a},        {b},            {b,c},       {c},   --]
                                                              (last has none)
@

After evaluating @let b = use(a)@, we check: is @a@ in suffix @{b,c}@? No.
So we emit @ODecVar a@ right after that expression.

=== 4. Branch Reconciliation

When control flow splits (if/else, match), different branches may consume
different variables. The analysis /reconciles/ by finding the minimum
ownership count across branches and inserting dec operations in branches
that under-consumed:

@
  if cond {
      use(x);           // x: 1 -> 0 (consumed)
      // ledger: { x: 0 }
  } else {
      // x not used      // x: 1 (still owned!)
      // ledger: { x: 1 }
      // RECONCILE: emit ODecVar(x) here to match the "then" branch
  }
  // After reconciliation: ledger: { x: 0 }
@

=== 5. Pattern Match + Ownership

For @match@ expressions, the scrutinee is /borrowed/ (not consumed).
Each branch may bind variables from inside the scrutinee. RC-typed
bindings get an @rc.inc@ (because they're extracted from a borrowed value):

@
  match some_rc_enum {           // borrow some_rc_enum
    Variant::A(inner_rc) => {    // inner_rc needs rc.inc (borrowed ref)
        use(inner_rc);           // consume inner_rc
        // emit dec for scrutinee in this branch
    }
    Variant::B => {
        // emit dec for scrutinee in this branch
    }
  }
@

=== 6. Function Exit

At function end, any variable still owned (count > 0) gets a dec annotation.
This handles variables that are never explicitly consumed.

== Output

The analysis produces 'OwnershipAnnotations': a map from ExprID to
'OwnershipAction', which tells codegen what to emit before\/after each
expression.

@
  OwnershipAnnotations = {
      expr_42: OwnershipAction { before: [OIncVar v3], after: [ODecVar v1] }
      expr_57: OwnershipAction { before: [], after: [ODec] }
  }
@

These annotations are consumed by the lowering phase (Reussir.Core.Lowering)
which emits the actual @reussir.rc.inc@ and @reussir.rc.dec@ MLIR operations.
-}
module Reussir.Core.Ownership (
    analyzeFunction,
    isRC,
    isRR,
) where

import Control.Monad (filterM)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as HT
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Vector.Strict qualified as V
import Reussir.Codegen.Type.Data qualified as IRType

import Reussir.Core.Data.Ownership
import Reussir.Core.Data.UniqueID (ExprID (..), VarID (..))

import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Data.Full.Function qualified as Full
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full

{- | Is the type directly RC-managed (reference counted)?

A type is directly RC if it is:

  * @Rc<T, cap>@ with a sharing capability (Shared, Flex, or Rigid)
  * A closure (closures are always heap-allocated and RC'd)

@
  isRC (TypeRc Foo Shared)  = True    -- Rc<Foo, Shared>
  isRC (TypeRc Foo Flex)    = True    -- Rc<Foo, Flex>
  isRC (TypeClosure _ _)    = True    -- fn(i32) -> i32
  isRC TypeI32              = False   -- plain integer
  isRC (TypeRecord _)       = False   -- value-type record (check isRR for transitive)
@
-}
isRC :: Full.Type -> Bool
isRC (Full.TypeRc _ cap) = cap `elem` [IRType.Shared, IRType.Flex, IRType.Rigid]
isRC (Full.TypeClosure _ _) = True
isRC _ = False

{- | Is the type resource-relevant (transitively contains RC)?

A type is RR if it directly is RC, or transitively contains an RC type.
This requires the FullRecordTable to look up record field types.

@
  Type hierarchy for RR check:

  isRR(ty)
    │
    ├── isRC(ty)?  ────────── YES -> True
    │
    ├── TypeNullable inner ── isRR(inner)
    │
    ├── TypeRecord sym
    │     │
    │     ├── defaultCap == Shared or Regional? ── True
    │     │
    │     └── Components fields?
    │           └── ANY field isRR? ── True/False
    │
    └── otherwise ──────────── False
@

Note: Variant-only records (enums without fields) return False because
they are represented as plain tags with no heap data.
-}
isRR :: Full.FullRecordTable -> Full.Type -> IO Bool
isRR _ ty | isRC ty = pure True
isRR tbl (Full.TypeNullable inner) = isRR tbl inner
isRR tbl (Full.TypeRecord sym) = do
    rec <- HT.lookup tbl sym
    case rec of
        Nothing -> pure False
        Just r | Full.recordDefaultCap r == IRType.Shared -> pure True
        Just r | Full.recordDefaultCap r == IRType.Regional -> pure True
        Just r -> case Full.recordFields r of
            Full.Components fields ->
                V.foldM'
                    (\acc (_, fty, _) -> if acc then pure True else isRR tbl fty)
                    False
                    fields
            Full.Variants _ -> pure False
isRR _ _ = pure False

{- | Internal analysis state threaded through the traversal.

The state has two parts:

  1. The /ledger/: tracks how many ownership units each variable currently holds.
     A positive count means the variable owns RC resources that must eventually
     be dec'd. A zero count means ownership was transferred elsewhere.

  2. The /annotations/: accumulates the RC operations (inc/dec) to emit at each
     expression site. This is the final output of the analysis.

@
  AnalysisState example during analysis of:
    fn foo(x: Rc<A>) {
      let y = bar(x);   // <-- analyzing this line
      baz(y);
    }

  Before "let y = bar(x)":
    ledger      = { x: 1 }
    annotations = {}

  After "let y = bar(x)":
    ledger      = { x: 0, y: 1 }    // x consumed by bar, y granted
    annotations = {}                 // no extra inc/dec needed yet
@
-}
data AnalysisState = AnalysisState
    { asLedger :: IntMap Int
    -- ^ VarID -> current ownership count.
    -- Positive = variable owns RC resources. Zero = ownership transferred.
    , asAnnotations :: IntMap OwnershipAction
    -- ^ ExprID -> RC operations to emit before\/after that expression.
    }

emptyState :: AnalysisState
emptyState = AnalysisState IntMap.empty IntMap.empty

-- | Add an annotation for an expression
annotate :: ExprID -> OwnershipAction -> AnalysisState -> AnalysisState
annotate eid action st =
    let currentActions =
            IntMap.findWithDefault (OwnershipAction [] []) (unExprID eid) (asAnnotations st)
        newActions = mergeAction currentActions action
     in st{asAnnotations = IntMap.insert (unExprID eid) newActions (asAnnotations st)}

-- | Grant ownership of a variable (set count to 1 or increment)
grantOwnership :: VarID -> AnalysisState -> AnalysisState
grantOwnership (VarID vid) st =
    st{asLedger = IntMap.insertWith (+) vid 1 (asLedger st)}

-- | Consume one unit of ownership for a variable
consumeOwnership :: VarID -> AnalysisState -> AnalysisState
consumeOwnership (VarID vid) st =
    st{asLedger = IntMap.adjust (\n -> n - 1) vid (asLedger st)}

-- | Get current ownership count for a variable
ownershipCount :: VarID -> AnalysisState -> Int
ownershipCount (VarID vid) st =
    IntMap.findWithDefault 0 vid (asLedger st)

-- | Remove a variable from the ledger
removeFromLedger :: VarID -> AnalysisState -> AnalysisState
removeFromLedger (VarID vid) st =
    st{asLedger = IntMap.delete vid (asLedger st)}

{- | Analyze an entire function and produce ownership annotations.

This is the top-level entry point. It walks the function body and
determines where every @rc.inc@ and @rc.dec@ should go.

=== Algorithm

@
  analyzeFunction(fn foo(x: Rc<A>, y: i32) -> Rc<B> { body })
    │
    ├── 1. Initialize ledger from parameters
    │      { x: 1 }  (x is RR, y is not)
    │
    ├── 2. Analyze body expression recursively
    │      -> (finalState, bodyFlux)
    │
    ├── 3. Consume body's free vars (return value transfers ownership)
    │
    └── 4. Emit end-of-scope decs for any remaining owned vars
           (variables that are still owned but not returned)
@
-}
analyzeFunction ::
    Full.FullRecordTable -> Full.Function -> IO OwnershipAnnotations
analyzeFunction tbl func = case Full.funcBody func of
    Nothing -> pure $ OwnershipAnnotations IntMap.empty
    Just body -> do
        -- Initialize state: each RR-typed parameter gets ownership 1
        let params = Full.funcParams func
        initialState <- initParamOwnership tbl params 0 emptyState
        (finalState, bodyFlux) <- analyzeExpr tbl body initialState
        -- The return value consumes the body's free vars (ownership transfers to caller)
        let finalState' = consumeFreeVars bodyFlux finalState
        -- At function end, any remaining owned vars need dec
        let finalState'' = emitEndOfScopeDecs finalState' (Full.exprID body)
        pure $ OwnershipAnnotations (asAnnotations finalState'')

-- | Initialize ownership for function parameters
initParamOwnership ::
    Full.FullRecordTable ->
    [(a, Full.Type)] ->
    Int ->
    AnalysisState ->
    IO AnalysisState
initParamOwnership _ [] _ st = pure st
initParamOwnership tbl ((_, ty) : rest) idx st = do
    rr <- isRR tbl ty
    let st' =
            if rr
                then grantOwnership (VarID idx) st
                else st
    initParamOwnership tbl rest (idx + 1) st'

-- | Emit ODecVar for all variables still owned at end of scope
emitEndOfScopeDecs :: AnalysisState -> ExprID -> AnalysisState
emitEndOfScopeDecs st eid =
    let decs =
            [ ODecVar (VarID vid)
            | (vid, count) <- IntMap.toList (asLedger st)
            , count > 0
            ]
     in if null decs
            then st
            else annotate eid (OwnershipAction [] decs) st

{- | Emit early decs for owned variables not referenced by remaining expressions.

This is the core of /eager deallocation/. After evaluating an expression,
we check which owned variables are NOT in the suffix free-var set (i.e.,
not needed by any later expression). Those can be dec'd immediately.

@
  Sequence: [let a = .., use(a), let b = .., use(b)]
  Suffix free vars after "use(a)": {b}  (only b is needed later)

  Variable a has count 1 but is NOT in suffix {b}
  -> Emit ODecVar(a) after "use(a)"
  -> Set a's count to 0

  This frees a's memory as early as possible, rather than waiting
  until the end of the function.
@

Returns the updated state with dec annotations added and ownership consumed.
-}
emitEarlyDecs :: AnalysisState -> ExprID -> IntSet.IntSet -> AnalysisState
emitEarlyDecs st eid suffixFreeVars =
    let toDec =
            [ (vid, count)
            | (vid, count) <- IntMap.toList (asLedger st)
            , count > 0
            , not (IntSet.member vid suffixFreeVars)
            ]
        decOps = concatMap (\(vid, count) -> replicate count (ODecVar (VarID vid))) toDec
        st' =
            if null decOps
                then st
                else annotate eid (OwnershipAction [] decOps) st
     in -- Consume ownership for dec'd variables
        foldl'
            ( \s (vid, count) ->
                foldl' (\s' _ -> consumeOwnership (VarID vid) s') s [1 .. count]
            )
            st'
            toDec

{- | Emit inc operations for variables that a consuming expression will consume
but are still needed in later sequence expressions. This handles the case where
a variable is passed to a call AND used again after the call returns.
-}
emitSequenceLevelIncs ::
    Full.Expr ->
    IntSet.IntSet ->
    AnalysisState ->
    IO AnalysisState
emitSequenceLevelIncs e suffixVars st = do
    let exprFreeVars = collectFreeVars e
    let neededLater = IntSet.intersection exprFreeVars suffixVars
    -- Only inc vars that are RR-typed and currently owned
    incs <- filterM
        (\vid -> do
            let varID = VarID vid
            if ownershipCount varID st > 0
                then pure True
                else pure False
        )
        (IntSet.toList neededLater)
    let incVarIDs = map VarID incs
    let incOps = map OIncVar incVarIDs
    if null incOps
        then pure st
        else do
            let stAnnotated = annotate (Full.exprID e) (OwnershipAction incOps []) st
            pure $ foldl' (\s vid -> grantOwnership vid s) stAnnotated incVarIDs

{- | Collect all free variable references (VarIDs) in an expression.
This is a pure syntactic traversal.
-}
collectFreeVars :: Full.Expr -> IntSet.IntSet
collectFreeVars expr = case Full.exprKind expr of
    Full.Var (VarID vid) -> IntSet.singleton vid
    Full.GlobalStr _ -> IntSet.empty
    Full.Constant _ -> IntSet.empty
    Full.Poison -> IntSet.empty
    Full.Negate e -> collectFreeVars e
    Full.Not e -> collectFreeVars e
    Full.Arith l _ r -> collectFreeVars l <> collectFreeVars r
    Full.Cmp l _ r -> collectFreeVars l <> collectFreeVars r
    Full.Cast e _ -> collectFreeVars e
    Full.ScfIfExpr c t e -> collectFreeVars c <> collectFreeVars t <> collectFreeVars e
    Full.RegionRun e -> collectFreeVars e
    Full.Proj e _ -> collectFreeVars e
    Full.Assign d _ s -> collectFreeVars d <> collectFreeVars s
    Full.Let{Full.letVarExpr = ve} -> collectFreeVars ve
    Full.FuncCall{Full.funcCallArgs = args} -> IntSet.unions (map collectFreeVars args)
    Full.IntrinsicCall{Full.intrinsicCallArgs = args} -> IntSet.unions (map collectFreeVars args)
    Full.CompoundCall args -> IntSet.unions (map collectFreeVars args)
    Full.VariantCall _ e -> collectFreeVars e
    Full.NullableCall me -> maybe IntSet.empty collectFreeVars me
    Full.RcWrap e -> collectFreeVars e
    Full.Match scr dt -> collectFreeVars scr <> collectFreeVarsDT dt
    Full.Sequence es -> IntSet.unions (map collectFreeVars es)

-- | Collect free variable references in a decision tree.
collectFreeVarsDT :: Full.DecisionTree -> IntSet.IntSet
collectFreeVarsDT Full.DTUncovered = IntSet.empty
collectFreeVarsDT Full.DTUnreachable = IntSet.empty
collectFreeVarsDT (Full.DTLeaf body _) = collectFreeVars body
collectFreeVarsDT (Full.DTGuard _ guardExpr dtT dtF) =
    collectFreeVars guardExpr <> collectFreeVarsDT dtT <> collectFreeVarsDT dtF
collectFreeVarsDT (Full.DTSwitch _ cases) = collectFreeVarsCases cases

-- | Collect free variable references in decision tree switch cases.
collectFreeVarsCases :: Full.DTSwitchCases -> IntSet.IntSet
collectFreeVarsCases (Full.DTSwitchBool dtT dtF) =
    collectFreeVarsDT dtT <> collectFreeVarsDT dtF
collectFreeVarsCases (Full.DTSwitchCtor cases) =
    V.foldl' (\acc dt -> acc <> collectFreeVarsDT dt) IntSet.empty cases
collectFreeVarsCases (Full.DTSwitchNullable dtJ dtN) =
    collectFreeVarsDT dtJ <> collectFreeVarsDT dtN
collectFreeVarsCases (Full.DTSwitchInt intMap dtDef) =
    IntMap.foldl' (\acc dt -> acc <> collectFreeVarsDT dt) IntSet.empty intMap
        <> collectFreeVarsDT dtDef
collectFreeVarsCases (Full.DTSwitchString strMap dtDef) =
    HashMap.foldl' (\acc dt -> acc <> collectFreeVarsDT dt) IntSet.empty strMap
        <> collectFreeVarsDT dtDef

{- | Core recursive analysis: dispatch on expression kind.

This is the heart of the ownership analysis. It pattern-matches on
every possible expression kind and determines ownership effects.

=== Key dispatch rules

@
  Pure (no ownership effect):
    GlobalStr, Constant, Poison, Negate, Not, Arith, Cmp, Cast

  Variable reference:
    Var x -> report x in fluxFreeVars (caller decides to consume or borrow)

  Ownership transfer (consuming calls):
    FuncCall, IntrinsicCall, CompoundCall, VariantCall, NullableCall, RcWrap
    -> all arguments are consumed (ownership transferred to callee)

  Borrowing:
    Proj base.field -> borrow base (no consume), OInc if result is RC
    Assign dst.f = src -> borrow dst, consume src

  Branching (requires reconciliation):
    ScfIfExpr  -> fork ledger, analyze branches, reconcile
    Match      -> borrow scrutinee, analyze decision tree

  Sequences:
    Sequence   -> delegate to analyzeSequence for early dec placement

  Let binding:
    Let x = e  -> consume e's free vars, grant ownership to x
@
-}
analyzeExpr ::
    Full.FullRecordTable ->
    Full.Expr ->
    AnalysisState ->
    IO (AnalysisState, ExprFlux)
analyzeExpr tbl expr st = do
    let eid = Full.exprID expr
    let ty = Full.exprType expr
    case Full.exprKind expr of
        -- Pure expressions: no ownership effect
        Full.GlobalStr _ -> pure (st, emptyFlux)
        Full.Constant _ -> pure (st, emptyFlux)
        Full.Poison -> pure (st, emptyFlux)
        -- Unary expressions on non-RR types
        Full.Negate inner -> analyzeExpr tbl inner st
        Full.Not inner -> analyzeExpr tbl inner st
        -- Arithmetic/comparison: operands are not RR
        Full.Arith lhs _ rhs -> do
            (st1, _) <- analyzeExpr tbl lhs st
            (st2, _) <- analyzeExpr tbl rhs st1
            pure (st2, emptyFlux)
        Full.Cmp lhs _ rhs -> do
            (st1, _) <- analyzeExpr tbl lhs st
            (st2, _) <- analyzeExpr tbl rhs st1
            pure (st2, emptyFlux)
        Full.Cast inner _ -> analyzeExpr tbl inner st
        -- Variable reference
        Full.Var varID -> do
            rr <- isRR tbl ty
            if rr
                then
                    pure
                        ( st
                        , emptyFlux{fluxFreeVars = IntSet.singleton (unVarID varID)}
                        )
                else pure (st, emptyFlux)
        -- Sequence
        Full.Sequence subexprs -> analyzeSequence tbl subexprs st
        -- Let (should only appear inside Sequence, but handle standalone)
        Full.Let{Full.letVarID = varID, Full.letVarExpr = varExpr} -> do
            (st1, _exprFlux) <- analyzeExpr tbl varExpr st
            rr <- isRR tbl (Full.exprType varExpr)
            let st2 = if rr then grantOwnership varID st1 else st1
            pure (st2, emptyFlux)
        -- Function call: arguments are consumed
        Full.FuncCall{Full.funcCallArgs = args} ->
            analyzeConsumingCall tbl args st eid ty
        -- Intrinsic call: arguments are consumed
        Full.IntrinsicCall{Full.intrinsicCallArgs = args} ->
            analyzeConsumingCall tbl args st eid ty
        -- Compound creation: all args consumed, creates new RR value
        Full.CompoundCall args ->
            analyzeConsumingCall tbl args st eid ty
        -- Variant creation: arg consumed
        Full.VariantCall _ arg ->
            analyzeConsumingCall tbl [arg] st eid ty
        -- Nullable creation
        Full.NullableCall maybeArg ->
            analyzeConsumingCall tbl (maybe [] (: []) maybeArg) st eid ty
        -- RcWrap: inner consumed, new RC created
        Full.RcWrap inner ->
            analyzeConsumingCall tbl [inner] st eid ty
        -- Projection: base is borrowed (not consumed), result may need OInc
        Full.Proj baseExpr _ -> do
            (st1, baseFlux) <- analyzeExpr tbl baseExpr st
            -- Mark base vars as used (not consumed)
            let st2 = markUsed baseFlux st1
            -- If the projection result is RC-typed, we need an OInc
            let rc = isRC ty
            let st3 =
                    if rc
                        then annotate eid (OwnershipAction [] [OInc]) st2
                        else st2
            -- Don't propagate base's freeVars: borrowing doesn't transfer ownership.
            -- The base variable's liveness is tracked by collectFreeVars for early-dec.
            pure (st3, emptyFlux)
        -- Assignment: dst is used, src is consumed
        Full.Assign dstExpr _ srcExpr -> do
            (st1, _) <- analyzeExpr tbl dstExpr st
            (st2, srcFlux) <- analyzeExpr tbl srcExpr st1
            -- Consume source's free vars
            let st3 = consumeFreeVars srcFlux st2
            pure (st3, emptyFlux)
        -- If expression: branch reconciliation
        Full.ScfIfExpr condExpr thenExpr elseExpr -> do
            (st1, _) <- analyzeExpr tbl condExpr st
            -- Save ledger before branches
            let ledgerBefore = asLedger st1
            (st2, thenFlux) <- analyzeExpr tbl thenExpr st1
            -- Consume the then-branch result (it's yielded)
            let st2' = consumeFreeVars thenFlux st2
            let thenLedger = asLedger st2'
            -- Restore ledger for else branch
            let st3 = st2'{asLedger = ledgerBefore}
            (st4, elseFlux) <- analyzeExpr tbl elseExpr st3
            -- Consume the else-branch result (it's yielded)
            let st4' = consumeFreeVars elseFlux st4
            let elseLedger = asLedger st4'
            -- Reconcile branches
            let (reconciledLedger, st5) =
                    reconcileBranches
                        [ (thenLedger, Full.exprID thenExpr, collectFreeVars thenExpr)
                        , (elseLedger, Full.exprID elseExpr, collectFreeVars elseExpr)
                        ]
                        st4'
            pure (st5{asLedger = reconciledLedger}, emptyFlux)
        -- Match expression
        Full.Match scrutinee dt -> do
            (st1, scrutFlux) <- analyzeExpr tbl scrutinee st
            -- Scrutinee is used (not consumed) for borrowing
            let st2 = markUsed scrutFlux st1
            -- Pass scrutinee var IDs so branches can dec them
            let scrutVarIDs = fluxFreeVars scrutFlux
            analyzeDecisionTree tbl dt st2 (Full.exprType scrutinee) scrutVarIDs
        -- Region run
        Full.RegionRun bodyExpr -> analyzeExpr tbl bodyExpr st

{- | Analyze a sequence of expressions with early dec placement.

This is one of the most important functions in the ownership analysis.
It processes a sequence of expressions (a block) and eagerly places
@rc.dec@ operations as soon as a variable is no longer needed.

=== How Suffix Free-Var Sets Work

@
  Sequence: [e0, e1, e2, e3]

  Step 1: Compute free vars of each expression
    freeVars = [fv(e0), fv(e1), fv(e2), fv(e3)]

  Step 2: Compute suffix unions (what's needed by ALL remaining exprs)
    suffixes[0] = fv(e1) U fv(e2) U fv(e3)   -- after e0, what's still needed?
    suffixes[1] = fv(e2) U fv(e3)             -- after e1, what's still needed?
    suffixes[2] = fv(e3)                      -- after e2, what's still needed?
    (last expression has no suffix)

  Step 3: After evaluating each non-last expression:
    For each owned variable v:
      if v NOT IN suffixes[i]:
        emit ODecVar(v)   -- v is dead after this point, free it now!
@

=== Concrete Example

@
  {
    let a = make();     // fv = {}         suffix = {a, b}
    let b = use(a);     // fv = {a}        suffix = {b}
    print(b);           // fv = {b}        suffix = {}
    42                  // fv = {}         (last, no suffix)
  }

  After "let a = make()":  ledger = {a:1}
    a IN suffix {a,b}? YES -> keep

  After "let b = use(a)":  ledger = {a:0, b:1}
    a NOT IN suffix {b}? a has count 0, skip
    b IN suffix {b}? YES -> keep

  After "print(b)":  ledger = {a:0, b:0}
    Nothing to dec (all counts 0)
@
-}
analyzeSequence ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    AnalysisState ->
    IO (AnalysisState, ExprFlux)
analyzeSequence _ [] st = pure (st, emptyFlux)
analyzeSequence tbl [e] st = analyzeExpr tbl e st
analyzeSequence tbl exprs st = do
    -- Precompute free vars of the suffix (remaining expressions) for each position.
    -- suffixes[i] = free vars needed by exprs[i+1..]
    let freeVarSets = map collectFreeVars exprs
    let suffixes = drop 1 $ scanr IntSet.union IntSet.empty freeVarSets
    analyzeSequenceEarly tbl (zip exprs suffixes) st []

{- | Process a sequence of expressions with early dec placement.
After each non-last expression, emit decs for owned variables not
referenced by any remaining expression.
-}
analyzeSequenceEarly ::
    Full.FullRecordTable ->
    -- | Pairs of (expression, free vars of all subsequent expressions)
    [(Full.Expr, IntSet.IntSet)] ->
    AnalysisState ->
    -- | Let-bound variables in scope (to clean up from ledger)
    [VarID] ->
    IO (AnalysisState, ExprFlux)
analyzeSequenceEarly _ [] st scopedVars = do
    -- Clean up let-bound variables from ledger
    let st' = foldl' (\s v -> removeFromLedger v s) st scopedVars
    pure (st', emptyFlux)
analyzeSequenceEarly tbl [(e, _)] st scopedVars = do
    -- Last expression: return its flux to the caller
    (st1, flux) <- analyzeExpr tbl e st
    -- Clean up let-bound variables from ledger
    let st2 = foldl' (\s v -> removeFromLedger v s) st1 scopedVars
    pure (st2, flux)
analyzeSequenceEarly tbl ((e, suffixVars) : rest) st scopedVars =
    case Full.exprKind e of
        Full.Let{Full.letVarID = varID, Full.letVarExpr = varExpr} -> do
            -- Analyze the bound expression
            (st1, exprFlux) <- analyzeExpr tbl varExpr st
            -- Consume any free vars the expression uses
            let st2 = consumeFreeVars exprFlux st1
            -- Grant ownership if the bound value is RR
            rr <- isRR tbl (Full.exprType varExpr)
            let st3 = if rr then grantOwnership varID st2 else st2
            -- Emit early decs for owned variables no longer needed
            let st4 = emitEarlyDecs st3 (Full.exprID e) suffixVars
            analyzeSequenceEarly tbl rest st4 (varID : scopedVars)
        _ -> do
            -- Before analyzing a consuming expression, emit inc for vars that
            -- will be consumed but are still needed in later expressions.
            st0 <- case Full.exprKind e of
                Full.FuncCall{} -> emitSequenceLevelIncs e suffixVars st
                Full.CompoundCall{} -> emitSequenceLevelIncs e suffixVars st
                Full.VariantCall{} -> emitSequenceLevelIncs e suffixVars st
                Full.NullableCall{} -> emitSequenceLevelIncs e suffixVars st
                Full.RcWrap{} -> emitSequenceLevelIncs e suffixVars st
                _ -> pure st
            (st1, _) <- analyzeExpr tbl e st0
            -- Emit early decs for owned variables no longer needed
            let st2 = emitEarlyDecs st1 (Full.exprID e) suffixVars
            analyzeSequenceEarly tbl rest st2 scopedVars

{- | Analyze a function\/intrinsic\/constructor call where arguments are consumed.

All arguments to function calls transfer ownership. If the same variable
appears in multiple arguments, we must emit @OIncVar@ to bump its RC
before the first consumption so later arguments can still use it.

=== Example: shared variable across args

@
  foo(x, bar(x))
  Args: [x, bar(x)]
  Free vars: [{x}, {x}]
  Suffixes:  [{x}]       -- x needed by arg 1 (bar(x)) after arg 0 (x)

  Processing arg 0 (x):
    x is in suffix {x} AND has ownership > 0
    -> emit OIncVar(x) before this arg
    -> x's ownership: 1 -> 2 (inc'd)
    -> consume x: 2 -> 1

  Processing arg 1 (bar(x)):
    x has ownership 1
    -> consume x: 1 -> 0
    -> no further args, no inc needed
@
-}
analyzeConsumingCall ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    AnalysisState ->
    ExprID ->
    Full.Type ->
    IO (AnalysisState, ExprFlux)
analyzeConsumingCall tbl args st _eid _resultTy = do
    -- Analyze each argument and consume their free vars (handling sequential dependencies)
    st' <- analyzeConsumedArgs tbl args st
    pure (st', emptyFlux)

-- | Analyze arguments that are consumed by a call
analyzeConsumedArgs ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    AnalysisState ->
    IO AnalysisState
analyzeConsumedArgs tbl args st = do
    let freeVarSets = map collectFreeVars args
    -- suffixes[i] = free vars needed by args[i+1..]
    let suffixes = drop 1 $ scanr IntSet.union IntSet.empty freeVarSets
    analyzeConsumedArgsLoop tbl args freeVarSets suffixes st

analyzeConsumedArgsLoop ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    [IntSet.IntSet] ->
    [IntSet.IntSet] ->
    AnalysisState ->
    IO AnalysisState
analyzeConsumedArgsLoop _ [] _ _ st = pure st
analyzeConsumedArgsLoop tbl (arg : restArgs) (vars : restVars) (suffix : restSuffix) st = do
    -- Identify vars that are in this arg AND in suffix (needed later)
    -- We must inc them before this arg consumes them.
    let neededLater = IntSet.intersection vars suffix
    let incs =
            [ VarID vid | vid <- IntSet.toList neededLater, ownershipCount (VarID vid) st > 0
            ]

    let incOps = map OIncVar incs
    let stBefore =
            if null incOps
                then st
                else annotate (Full.exprID arg) (OwnershipAction incOps []) st
    -- Grant ownership for each inc so consumption tracking is correct
    let stInc = foldl' (\s vid -> grantOwnership vid s) stBefore incs

    (stAfter, argFlux) <- analyzeExpr tbl arg stInc
    -- Consume free vars from this argument
    let stConsumed = consumeFreeVars argFlux stAfter
    analyzeConsumedArgsLoop tbl restArgs restVars restSuffix stConsumed
analyzeConsumedArgsLoop _ _ _ _ st = pure st -- Should not happen with zip logic

-- | Consume ownership of all free vars in a flux
consumeFreeVars :: ExprFlux -> AnalysisState -> AnalysisState
consumeFreeVars flux st =
    IntSet.foldl' (\s vid -> consumeOwnership (VarID vid) s) st (fluxFreeVars flux)

-- | Mark free vars as used (not consumed) — no ownership change
markUsed :: ExprFlux -> AnalysisState -> AnalysisState
markUsed _flux st = st -- using doesn't change ownership count

{- | Analyze ownership through a decision tree (from match expressions).

The scrutinee is borrowed (not consumed). Each leaf\/guard node may have
pattern bindings that extract values from inside the scrutinee.

=== Ownership flow through a decision tree

@
  match scrutinee {        // scrutinee is BORROWED (used, not consumed)
    Ctor(a, b) => body     // a, b are pattern bindings
  }

  DTLeaf analysis:
    1. Grant ownership for RC-typed pattern bindings (a, b get rc.inc)
    2. Analyze the leaf body
    3. Consume body's free vars (return value ownership)
    4. Dec the scrutinee vars inside this branch
    5. Clean up pattern-local vars from ledger

  DTGuard analysis:
    1. Grant ownership for bindings
    2. Analyze guard expression
    3. Fork: analyze true branch and false branch separately
    4. Reconcile ownership between branches

  DTSwitch analysis:
    -> Delegate to analyzeDTSwitchCases for each case kind
       (Bool, Ctor, Nullable, Int, String)
    -> Each case is analyzed independently, then reconciled
@
-}
analyzeDecisionTree ::
    Full.FullRecordTable ->
    Full.DecisionTree ->
    AnalysisState ->
    Full.Type ->
    -- | Scrutinee var IDs (to dec inside each branch)
    IntSet.IntSet ->
    IO (AnalysisState, ExprFlux)
analyzeDecisionTree _ Full.DTUncovered st _ _ = pure (st, emptyFlux)
analyzeDecisionTree _ Full.DTUnreachable st _ _ = pure (st, emptyFlux)
analyzeDecisionTree tbl (Full.DTLeaf body bindings) st scrutTy scrutVarIDs = do
    -- Grant ownership for RC-typed pattern bindings
    st1 <- grantPatternBindingOwnership tbl bindings st scrutTy
    -- Analyze the leaf body
    (st2, bodyFlux) <- analyzeExpr tbl body st1
    -- The body's result is consumed (yielded to match)
    let st3 = consumeFreeVars bodyFlux st2
    -- Dec the scrutinee vars inside this branch (before the body)
    let scrutDecs =
            [ ODecVar (VarID vid)
            | vid <- IntSet.toList scrutVarIDs
            , ownershipCount (VarID vid) st3 > 0
            ]
    let st4 =
            if null scrutDecs
                then st3
                else annotate (Full.exprID body) (OwnershipAction scrutDecs []) st3
    -- Consume the scrutinee's ownership
    let st5 =
            IntSet.foldl'
                ( \s vid ->
                    let c = ownershipCount (VarID vid) s
                     in if c > 0 then consumeOwnership (VarID vid) s else s
                )
                st4
                scrutVarIDs
    -- Clean up pattern-local vars from ledger
    let patternVarIDs = IntMap.keys bindings
    let st6 =
            foldl'
                (\s vid -> removeFromLedger (VarID (fromIntegral vid)) s)
                st5
                patternVarIDs
    pure (st6, emptyFlux)
analyzeDecisionTree tbl (Full.DTGuard bindings guardExpr dtTrue dtFalse) st scrutTy scrutVarIDs = do
    -- Process bindings
    st1 <- grantPatternBindingOwnership tbl bindings st scrutTy
    -- Analyze guard expression
    (st2, _) <- analyzeExpr tbl guardExpr st1
    -- Save ledger before branches
    let ledgerBefore = asLedger st2
    -- Analyze true branch
    (st3, _) <- analyzeDecisionTree tbl dtTrue st2 scrutTy scrutVarIDs
    let trueLedger = asLedger st3
    -- Restore ledger for false branch
    let st4 = st3{asLedger = ledgerBefore}
    (st5, _) <- analyzeDecisionTree tbl dtFalse st4 scrutTy scrutVarIDs
    let falseLedger = asLedger st5
    -- Reconcile
    let (reconciledLedger, st6) =
            reconcileBranchesWithDT
                [(trueLedger, dtTrue), (falseLedger, dtFalse)]
                st5
    pure (st6{asLedger = reconciledLedger}, emptyFlux)
analyzeDecisionTree tbl (Full.DTSwitch _ cases) st scrutTy scrutVarIDs =
    analyzeDTSwitchCases tbl cases st scrutTy scrutVarIDs

{- | Grant ownership for pattern bindings that have RC types.

Pattern bindings extract values from inside the /borrowed/ scrutinee.
Because the scrutinee is borrowed (not consumed), any RC-typed field
extracted from it must have its reference count incremented.

@
  match rc_value {                 // borrow rc_value
    SomeStruct { inner } => {     // inner: Rc<T>
        // inner was extracted from a borrowed ref
        // DT lowering emits rc.inc for inner
        // This function grants ownership: ledger[inner] = 1
        use(inner);               // consume inner: ledger[inner] = 0
    }
  }
@

The binding path (PatternVarRef) tells us how to navigate from the
scrutinee to the bound value. We resolve the type at the end of
that path to determine if it's RR.
-}
grantPatternBindingOwnership ::
    Full.FullRecordTable ->
    IntMap Full.PatternVarRef ->
    AnalysisState ->
    Full.Type ->
    IO AnalysisState
grantPatternBindingOwnership tbl bindings st scrutTy = do
    foldlM'
        ( \s (vid, pvRef) -> do
            -- Resolve the type at the end of the projection path
            -- Drop the first index (root var ID) from the path
            bindingTy <-
                resolveBindingType tbl scrutTy (drop 1 $ toList (Full.unPatternVarRef pvRef))
            rr <- isRR tbl bindingTy
            pure $ if rr then grantOwnership (VarID (fromIntegral vid)) s else s
        )
        st
        (IntMap.toList bindings)

-- | Resolve the type of a pattern binding by following the projection path.
resolveBindingType :: Full.FullRecordTable -> Full.Type -> [Int] -> IO Full.Type
resolveBindingType _ ty [] = pure ty
resolveBindingType tbl ty (idx : rest) = do
    case ty of
        Full.TypeRecord s -> resolveFromRecord s
        Full.TypeRc (Full.TypeRecord s) _ -> resolveFromRecord s
        _ -> pure ty -- Can't project into this type, return as-is
  where
    resolveFromRecord s = do
        rec <- HT.lookup tbl s
        case rec of
            Just r -> case Full.recordFields r of
                Full.Components fields
                    | idx < V.length fields -> do
                        let (_, fieldTy, _) = fields V.! idx
                        resolveBindingType tbl fieldTy rest
                Full.Variants variantSyms
                    | idx < V.length variantSyms -> do
                        let varSym = variantSyms V.! idx
                        resolveBindingType tbl (Full.TypeRecord varSym) rest
                _ -> pure ty
            Nothing -> pure ty

-- | Analyze switch cases in a decision tree
analyzeDTSwitchCases ::
    Full.FullRecordTable ->
    Full.DTSwitchCases ->
    AnalysisState ->
    Full.Type ->
    IntSet.IntSet ->
    IO (AnalysisState, ExprFlux)
analyzeDTSwitchCases tbl cases st scrutTy scrutVarIDs = do
    let ledgerBefore = asLedger st
    case cases of
        Full.DTSwitchBool dtTrue dtFalse -> do
            -- Bool doesn't refine type structurally (just value)
            (st1, _) <- analyzeDecisionTree tbl dtTrue st scrutTy scrutVarIDs
            let trueLedger = asLedger st1
            let st2 = st1{asLedger = ledgerBefore}
            (st3, _) <- analyzeDecisionTree tbl dtFalse st2 scrutTy scrutVarIDs
            let falseLedger = asLedger st3
            let (reconciledLedger, st4) =
                    reconcileBranchesWithDT
                        [(trueLedger, dtTrue), (falseLedger, dtFalse)]
                        st3
            pure (st4{asLedger = reconciledLedger}, emptyFlux)
        Full.DTSwitchCtor ctorCases -> do
            -- Refine scrutTy for each variant
            results <-
                V.imapM
                    ( \idx dt -> do
                        (l, dt', s') <- analyzeCtorBranch tbl ledgerBefore scrutTy scrutVarIDs st idx dt
                        pure (l, dt', s')
                    )
                    ctorCases

            let branchLedgers = V.toList $ V.map (\(l, dt', _) -> (l, dt')) results
            let branchStates = V.toList $ V.map (\(_, _, s) -> s) results

            -- Reconcile ledgers to find common exit state
            let (reconciledLedger, stReconciledBase) =
                    reconcileBranchesWithDT branchLedgers st

            -- MERGE ANNOTATIONS: Union the annotations from all branches into the reconciled state
            let mergedAnnotations =
                    foldl'
                        (\acc s -> IntMap.union acc (asAnnotations s))
                        (asAnnotations stReconciledBase)
                        branchStates

            let stReconciled =
                    stReconciledBase
                        { asLedger = reconciledLedger
                        , asAnnotations = mergedAnnotations
                        }

            pure (stReconciled, emptyFlux)
        Full.DTSwitchNullable dtJust dtNothing -> do
            -- Refine scrutTy for just case: extract inner type
            innerTy <- case scrutTy of
                Full.TypeNullable t -> pure t
                _ -> pure scrutTy -- Should be nullable, but fallback
            (st1, _) <- analyzeDecisionTree tbl dtJust st innerTy scrutVarIDs
            let justLedger = asLedger st1
            let st2 = st1{asLedger = ledgerBefore}
            -- Nothing case uses Unit? Or original null?
            -- Usually nothing binds to Unit.
            (st3, _) <- analyzeDecisionTree tbl dtNothing st2 Full.TypeUnit scrutVarIDs
            let nothingLedger = asLedger st3
            let (reconciledLedger, st4) =
                    reconcileBranchesWithDT
                        [(justLedger, dtJust), (nothingLedger, dtNothing)]
                        st3
            pure (st4{asLedger = reconciledLedger}, emptyFlux)
        Full.DTSwitchInt intMap dtDefault -> do
            let intCases = IntMap.toList intMap
            results <-
                mapM
                    ( \(_, dt) -> do
                        let s' = st{asLedger = ledgerBefore}
                        (s'', _) <- analyzeDecisionTree tbl dt s' scrutTy scrutVarIDs
                        pure (asLedger s'', dt, s'')
                    )
                    intCases

            let stDefaultStart = st{asLedger = ledgerBefore}
            (stDefault, _) <-
                analyzeDecisionTree tbl dtDefault stDefaultStart scrutTy scrutVarIDs

            let branchLedgers = ((asLedger stDefault, dtDefault) : map (\(l, dt, _) -> (l, dt)) results)
            let branchStates = (stDefault : map (\(_, _, s) -> s) results)

            let (reconciledLedger, stReconciledBase) = reconcileBranchesWithDT branchLedgers st

            -- MERGE ANNOTATIONS
            let mergedAnnotations =
                    foldl'
                        (\acc s -> IntMap.union acc (asAnnotations s))
                        (asAnnotations stReconciledBase)
                        branchStates

            let stReconciled =
                    stReconciledBase
                        { asLedger = reconciledLedger
                        , asAnnotations = mergedAnnotations
                        }
            pure (stReconciled, emptyFlux)
        Full.DTSwitchString _stringMap dtDefault -> do
            -- String switch
            (st1, flux) <- analyzeDecisionTree tbl dtDefault st scrutTy scrutVarIDs
            pure (st1, flux)

-- | Helper for analyzing ctor branches
analyzeCtorBranch ::
    Full.FullRecordTable ->
    IntMap Int ->
    Full.Type ->
    IntSet.IntSet ->
    AnalysisState ->
    Int ->
    Full.DecisionTree ->
    IO (IntMap Int, Full.DecisionTree, AnalysisState)
analyzeCtorBranch tbl ledgerBefore scrutTy scrutVarIDs st idx dt = do
    let st' = st{asLedger = ledgerBefore}
    -- Determine variant type
    variantTy <- case scrutTy of
        Full.TypeRecord s -> getVariantType s
        Full.TypeRc (Full.TypeRecord s) _ -> getVariantType s
        _ -> pure scrutTy

    (st'', _) <- analyzeDecisionTree tbl dt st' variantTy scrutVarIDs
    pure (asLedger st'', dt, st'')
  where
    getVariantType s = do
        rec <- HT.lookup tbl s
        case rec of
            Just r
                | Full.Variants vs <- Full.recordFields r
                , idx < V.length vs ->
                    pure $ Full.TypeRecord (vs V.! idx)
            _ -> pure scrutTy

{- | Reconcile ownership counts across diverging branches.

When control flow splits (if\/else, match arms), each branch may consume
a different set of variables. At the join point, ownership counts must agree.
This function ensures consistency by inserting dec operations in branches
that "under-consumed" compared to the branch that consumed the most.

=== Example

@
  if cond {
      use(x);   // consumes x  -> ledger: { x: 0, y: 1 }
  } else {
      use(y);   // consumes y  -> ledger: { x: 1, y: 0 }
  }

  Branch ledgers:
    then: { x: 0, y: 1 }
    else: { x: 1, y: 0 }

  Min counts (most consumed):
    x: min(0, 1) = 0
    y: min(1, 0) = 0

  Reconciliation:
    then-branch: y has excess 1 (1 - 0) -> emit ODecVar(y) in then
    else-branch: x has excess 1 (1 - 0) -> emit ODecVar(x) in else

  Reconciled ledger: { x: 0, y: 0 }
@

The "excess" decs are placed at the branch's exit expression (branchEid).
If a variable is used within the branch (member of usedVars), the dec goes
in 'oaAfter' (late drop). Otherwise it goes in 'oaBefore' (early drop).
-}
reconcileBranches ::
    [(IntMap Int, ExprID, IntSet.IntSet)] ->
    AnalysisState ->
    (IntMap Int, AnalysisState)
reconcileBranches branches st =
    let allVars = IntSet.unions $ map (\(l, _, _) -> IntMap.keysSet l) branches
        -- For each var, find the minimum count across branches (most consumed)
        minCounts = IntSet.foldl' findMin IntMap.empty allVars
          where
            findMin acc vid =
                let counts = map (\(ledger, _, _) -> IntMap.findWithDefault 0 vid ledger) branches
                    minCount = minimum counts
                 in IntMap.insert vid minCount acc
        -- Emit decs for under-consuming branches
        st' = foldl' reconcileBranch st branches
          where
            reconcileBranch s (ledger, branchEid, usedVars) =
                IntSet.foldl'
                    ( \s' vid ->
                        let branchCount = IntMap.findWithDefault 0 vid ledger
                            targetCount = IntMap.findWithDefault 0 vid minCounts
                            excess = branchCount - targetCount
                         in if excess > 0
                                then
                                    let action =
                                            if vid `IntSet.member` usedVars
                                                then OwnershipAction [] (replicate excess (ODecVar (VarID vid))) -- Late drop
                                                else OwnershipAction (replicate excess (ODecVar (VarID vid))) [] -- Early drop
                                     in annotate
                                            branchEid
                                            action
                                            s'
                                else s'
                    )
                    s
                    allVars
     in (minCounts, st')

-- | Reconcile ownership across decision tree branches
reconcileBranchesWithDT ::
    [(IntMap Int, Full.DecisionTree)] ->
    AnalysisState ->
    (IntMap Int, AnalysisState)
reconcileBranchesWithDT branches st =
    let
        -- Convert DT branches to ExprID branches where possible
        exprBranches =
            [ (ledger, eid, used)
            | (ledger, dt) <- branches
            , Just (eid, used) <- [dtExitExprInfo dt]
            ]
     in
        if null exprBranches
            then
                -- If no branches have exit ExprIDs, just pick first ledger
                case branches of
                    [] -> (asLedger st, st)
                    ((ledger, _) : _) -> (ledger, st)
            else reconcileBranches exprBranches st

-- | Get the exit ExprID and used vars for optimization
dtExitExprInfo :: Full.DecisionTree -> Maybe (ExprID, IntSet.IntSet)
dtExitExprInfo (Full.DTLeaf body _) = Just (Full.exprID body, collectFreeVars body)
dtExitExprInfo (Full.DTGuard _ guardExpr _ _) = Just (Full.exprID guardExpr, collectFreeVars guardExpr)
dtExitExprInfo _ = Nothing

-- | Strict left fold for IO
foldlM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ z [] = pure z
foldlM' f z (x : xs) = do
    z' <- f z x
    z' `seq` foldlM' f z' xs
