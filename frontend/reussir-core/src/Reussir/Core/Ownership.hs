module Reussir.Core.Ownership (
    analyzeFunction,
    isRC,
    isRR,
) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Strict qualified as V

import Data.HashTable.IO qualified as HT
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Data.Full.Function qualified as Full
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Ownership
import Reussir.Core.Data.UniqueID (ExprID (..), VarID (..))

-- | Is the type directly RC-managed?
isRC :: Full.Type -> Bool
isRC (Full.TypeRc _ cap) = cap `elem` [IRType.Shared, IRType.Flex, IRType.Rigid]
isRC (Full.TypeClosure _ _) = True
isRC _ = False

-- | Is the type resource-relevant (transitively contains RC)?
-- Needs FullRecordTable for record field lookup.
isRR :: Full.FullRecordTable -> Full.Type -> IO Bool
isRR _ ty | isRC ty = pure True
isRR tbl (Full.TypeNullable inner) = isRR tbl inner
isRR tbl (Full.TypeRecord sym) = do
    rec <- HT.lookup tbl sym
    case rec of
        Nothing -> pure False
        Just r -> case Full.recordFields r of
            Full.Components fields ->
                V.foldM'
                    (\acc (_, fty, _) -> if acc then pure True else isRR tbl fty)
                    False
                    fields
            Full.Variants _ -> pure False
isRR _ _ = pure False

-- | Internal analysis state threaded through the traversal
data AnalysisState = AnalysisState
    { asLedger :: IntMap Int
    -- ^ VarID -> current ownership count
    , asAnnotations :: IntMap OwnershipAction
    -- ^ ExprID -> actions (accumulated)
    }

emptyState :: AnalysisState
emptyState = AnalysisState IntMap.empty IntMap.empty

-- | Add an annotation for an expression
annotate :: ExprID -> OwnershipAction -> AnalysisState -> AnalysisState
annotate (ExprID eid) action st =
    st
        { asAnnotations =
            IntMap.insertWith mergeAction eid action (asAnnotations st)
        }

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

-- | Analyze an entire function and produce ownership annotations
analyzeFunction :: Full.FullRecordTable -> Full.Function -> IO OwnershipAnnotations
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
    Full.FullRecordTable -> [(a, Full.Type)] -> Int -> AnalysisState -> IO AnalysisState
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

-- | Emit early decs for owned variables not referenced by remaining expressions.
-- Returns updated state with decs annotated and ownership consumed.
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

-- | Collect all free variable references (VarIDs) in an expression.
-- This is a pure syntactic traversal.
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

-- | Core recursive analysis of an expression
analyzeExpr ::
    Full.FullRecordTable -> Full.Expr -> AnalysisState -> IO (AnalysisState, ExprFlux)
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
        -- Projection: base is used (not consumed), result may need OInc
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
            pure (st3, emptyFlux{fluxFreeVars = fluxFreeVars baseFlux})
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
                        [(thenLedger, Full.exprID thenExpr), (elseLedger, Full.exprID elseExpr)]
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

-- | Analyze a sequence of expressions (handling Let bindings).
-- Uses precomputed suffix free-var sets to place decs at the earliest
-- possible point after a variable's last use.
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

-- | Process a sequence of expressions with early dec placement.
-- After each non-last expression, emit decs for owned variables not
-- referenced by any remaining expression.
analyzeSequenceEarly ::
    Full.FullRecordTable ->
    [(Full.Expr, IntSet.IntSet)] ->
    -- ^ Pairs of (expression, free vars of all subsequent expressions)
    AnalysisState ->
    [VarID] ->
    -- ^ Let-bound variables in scope (to clean up from ledger)
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
            (st1, _) <- analyzeExpr tbl e st
            -- Emit early decs for owned variables no longer needed
            let st2 = emitEarlyDecs st1 (Full.exprID e) suffixVars
            analyzeSequenceEarly tbl rest st2 scopedVars

-- | Analyze a function/intrinsic/constructor call where arguments are consumed
analyzeConsumingCall ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    AnalysisState ->
    ExprID ->
    Full.Type ->
    IO (AnalysisState, ExprFlux)
analyzeConsumingCall tbl args st _eid _resultTy = do
    -- Analyze each argument and consume their free vars
    st' <- analyzeConsumedArgs tbl args st
    pure (st', emptyFlux)

-- | Analyze arguments that are consumed by a call
analyzeConsumedArgs ::
    Full.FullRecordTable ->
    [Full.Expr] ->
    AnalysisState ->
    IO AnalysisState
analyzeConsumedArgs _ [] st = pure st
analyzeConsumedArgs tbl (arg : rest) st = do
    (st1, argFlux) <- analyzeExpr tbl arg st
    -- Consume free vars from this argument
    let st2 = consumeFreeVars argFlux st1
    analyzeConsumedArgs tbl rest st2

-- | Consume ownership of all free vars in a flux
consumeFreeVars :: ExprFlux -> AnalysisState -> AnalysisState
consumeFreeVars flux st =
    IntSet.foldl' (\s vid -> consumeOwnership (VarID vid) s) st (fluxFreeVars flux)

-- | Mark free vars as used (not consumed) — no ownership change
markUsed :: ExprFlux -> AnalysisState -> AnalysisState
markUsed _flux st = st -- using doesn't change ownership count

-- | Analyze a decision tree
analyzeDecisionTree ::
    Full.FullRecordTable ->
    Full.DecisionTree ->
    AnalysisState ->
    Full.Type ->
    IntSet.IntSet ->
    -- ^ Scrutinee var IDs (to dec inside each branch)
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
    let st6 = foldl' (\s vid -> removeFromLedger (VarID (fromIntegral vid)) s) st5 patternVarIDs
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

-- | Grant ownership for pattern bindings that have RC types.
-- Pattern bindings extract values from inside the borrowed scrutinee.
-- RC-typed bindings need rc.inc (emitted by the DT lowering) and ownership tracking.
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
            bindingTy <- resolveBindingType tbl scrutTy (toList (Full.unPatternVarRef pvRef))
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
            branchResults <- V.foldM' (analyzeBranch tbl ledgerBefore scrutTy scrutVarIDs) ([], st) ctorCases
            let (branchLedgers, stFinal) = branchResults
            let (reconciledLedger, stReconciled) =
                    reconcileBranchesWithDT branchLedgers stFinal
            pure (stReconciled{asLedger = reconciledLedger}, emptyFlux)
        Full.DTSwitchNullable dtJust dtNothing -> do
            (st1, _) <- analyzeDecisionTree tbl dtJust st scrutTy scrutVarIDs
            let justLedger = asLedger st1
            let st2 = st1{asLedger = ledgerBefore}
            (st3, _) <- analyzeDecisionTree tbl dtNothing st2 scrutTy scrutVarIDs
            let nothingLedger = asLedger st3
            let (reconciledLedger, st4) =
                    reconcileBranchesWithDT
                        [(justLedger, dtJust), (nothingLedger, dtNothing)]
                        st3
            pure (st4{asLedger = reconciledLedger}, emptyFlux)
        Full.DTSwitchInt intMap dtDefault -> do
            let intCases = IntMap.toList intMap
            (branchLedgers1, st1) <-
                foldlM'
                    ( \(acc, s) (_, dt) -> do
                        let s' = s{asLedger = ledgerBefore}
                        (s'', _) <- analyzeDecisionTree tbl dt s' scrutTy scrutVarIDs
                        pure ((asLedger s'', dt) : acc, s'')
                    )
                    ([], st)
                    intCases
            let st2 = st1{asLedger = ledgerBefore}
            (st3, _) <- analyzeDecisionTree tbl dtDefault st2 scrutTy scrutVarIDs
            let defaultLedger = asLedger st3
            let allBranches = (defaultLedger, dtDefault) : branchLedgers1
            let (reconciledLedger, st4) = reconcileBranchesWithDT allBranches st3
            pure (st4{asLedger = reconciledLedger}, emptyFlux)
        Full.DTSwitchString _stringMap dtDefault -> do
            -- TODO: handle string switch cases properly
            (st1, flux) <- analyzeDecisionTree tbl dtDefault st scrutTy scrutVarIDs
            pure (st1, flux)

-- | Helper for analyzing branches in a fold
analyzeBranch ::
    Full.FullRecordTable ->
    IntMap Int ->
    Full.Type ->
    IntSet.IntSet ->
    ([(IntMap Int, Full.DecisionTree)], AnalysisState) ->
    Full.DecisionTree ->
    IO ([(IntMap Int, Full.DecisionTree)], AnalysisState)
analyzeBranch tbl ledgerBefore scrutTy scrutVarIDs (acc, st) dt = do
    let st' = st{asLedger = ledgerBefore}
    (st'', _) <- analyzeDecisionTree tbl dt st' scrutTy scrutVarIDs
    pure ((asLedger st'', dt) : acc, st'')

-- | Reconcile ownership across branches (for if/else).
-- For each variable, take the minimum consumption (= maximum remaining count).
-- Under-consuming branches get additional dec annotations at their exit.
reconcileBranches ::
    [(IntMap Int, ExprID)] ->
    AnalysisState ->
    (IntMap Int, AnalysisState)
reconcileBranches branches st =
    let allVars = IntSet.unions $ map (IntMap.keysSet . fst) branches
        -- For each var, find the minimum count across branches (most consumed)
        minCounts = IntSet.foldl' findMin IntMap.empty allVars
          where
            findMin acc vid =
                let counts = map (\(ledger, _) -> IntMap.findWithDefault 0 vid ledger) branches
                    minCount = minimum counts
                 in IntMap.insert vid minCount acc
        -- Emit decs for under-consuming branches
        st' = foldl' reconcileBranch st branches
          where
            reconcileBranch s (ledger, branchEid) =
                IntSet.foldl'
                    ( \s' vid ->
                        let branchCount = IntMap.findWithDefault 0 vid ledger
                            targetCount = IntMap.findWithDefault 0 vid minCounts
                            excess = branchCount - targetCount
                         in if excess > 0
                                then
                                    annotate
                                        branchEid
                                        (OwnershipAction [] (replicate excess (ODecVar (VarID vid))))
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
    let -- Convert DT branches to ExprID branches where possible
        exprBranches =
            [ (ledger, eid)
            | (ledger, dt) <- branches
            , Just eid <- [dtExitExprID dt]
            ]
     in if null exprBranches
            then
                -- If no branches have exit ExprIDs, just pick first ledger
                case branches of
                    [] -> (asLedger st, st)
                    ((ledger, _) : _) -> (ledger, st)
            else reconcileBranches exprBranches st

-- | Get the exit ExprID of a decision tree (the last expression evaluated)
dtExitExprID :: Full.DecisionTree -> Maybe ExprID
dtExitExprID (Full.DTLeaf body _) = Just (Full.exprID body)
dtExitExprID (Full.DTGuard _ guardExpr _ _) = Just (Full.exprID guardExpr)
dtExitExprID _ = Nothing

-- | Strict left fold for IO
foldlM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ z [] = pure z
foldlM' f z (x : xs) = do
    z' <- f z x
    z' `seq` foldlM' f z' xs
