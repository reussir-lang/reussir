{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.PatternMatch where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (groupBy)
import Data.Maybe (isJust)
import Effectful (liftIO)
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap

import Data.RRBVector qualified as RRB
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful.State.Static.Local qualified as State
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Type qualified as SynType

import Reussir.Core.Data.Semi.Context (SemiContext (..), SemiEff, knownRecords)
import Reussir.Core.Data.Semi.Expr (
    DTSwitchCases (..),
    DecisionTree (..),
    PatternVarRef (..),
 )
import Reussir.Core.Data.Semi.Record (Record (..), RecordFields (..))
import Reussir.Core.Data.UniqueID (VarID (..))
import Reussir.Core.Semi.Context (addErrReportMsg)

import Reussir.Core.Data.Semi.Expr qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as Semi

-- normalize a ctor pattern into a positional applied form.
-- fill in wildcards for ignored fields if ellipsis is present.
-- return Nothing if the normalization fail
normalizeCtorPattern ::
    Path ->
    V.Vector Syn.PatternCtorArg ->
    Bool ->
    SemiEff (Maybe (V.Vector Syn.PatternKind))
normalizeCtorPattern recordPath args hasEllipsis = do
    records <- State.gets knownRecords
    mRecord <- liftIO $ H.lookup records recordPath
    case mRecord of
        Nothing -> do
            addErrReportMsg $ "Record not found: " <> T.pack (show recordPath)
            return Nothing
        Just record -> do
            mFields <- readIORef' (recordFields record)
            case mFields of
                Nothing -> do
                    addErrReportMsg $ "Record fields not populated: " <> T.pack (show recordPath)
                    return Nothing
                Just (Named fields) -> normalizeNamed fields
                Just (Unnamed fields) -> normalizeUnnamed fields
                Just (Variants _) -> do
                    addErrReportMsg $
                        "Internal error: Variants in normalizeCtorPattern for "
                            <> T.pack (show recordPath)
                    return Nothing
  where
    getName :: Syn.PatternCtorArg -> Maybe Identifier
    getName arg =
        Syn.patCtorArgField arg <|> case Syn.patCtorArgKind arg of
            Syn.BindPat n -> Just n
            _ -> Nothing

    normalizeNamed fields = do
        let fieldMap =
                HashMap.fromList $
                    V.toList $
                        V.map (\arg -> (getName arg, arg)) args

        -- Check for positional args (Nothing name)
        if isJust (HashMap.lookup Nothing fieldMap)
            then do
                addErrReportMsg "Positional arguments are not allowed in named record patterns"
                return Nothing
            else do
                let namedArgs =
                        HashMap.fromList $
                            [ (n, Syn.patCtorArgKind arg)
                            | arg <- V.toList args
                            , Just n <- [getName arg]
                            ]

                -- Check for extra fields
                let unknownFields = diff (HashMap.keys namedArgs) [n | WithSpan (n, _, _) _ _ <- V.toList fields]

                if not (null unknownFields)
                    then do
                        addErrReportMsg $
                            "Unknown fields in pattern: "
                                <> T.intercalate ", " [unIdentifier n | n <- unknownFields]
                        return Nothing
                    else do
                        -- Build result vector
                        let result = V.generate (V.length fields) $ \i ->
                                let WithSpan (name, _, _) _ _ = fields V.! i
                                 in case HashMap.lookup name namedArgs of
                                        Just pat -> pat
                                        Nothing -> if hasEllipsis then Syn.WildcardPat else Syn.WildcardPat -- Placeholder

                        -- Validation check
                        let missingFields =
                                [ n
                                | WithSpan (n, _, _) _ _ <- V.toList fields
                                , not (HashMap.member n namedArgs)
                                ]

                        if not (null missingFields) && not hasEllipsis
                            then do
                                addErrReportMsg $
                                    "Missing fields: " <> T.intercalate ", " [unIdentifier n | n <- missingFields]
                                return Nothing
                            else return $ Just result

    normalizeUnnamed fields = do
        -- Check for named args
        let namedArgs = V.filter (isJust . Syn.patCtorArgField) args
        if not (V.null namedArgs)
            then do
                addErrReportMsg "Named arguments are not allowed in positional record patterns"
                return Nothing
            else do
                let argLen = V.length args
                let fieldLen = V.length fields

                if hasEllipsis
                    then do
                        if argLen > fieldLen
                            then do
                                addErrReportMsg $
                                    "Too many arguments: expected at most "
                                        <> T.pack (show fieldLen)
                                        <> ", got "
                                        <> T.pack (show argLen)
                                return Nothing
                            else do
                                -- Fill rest with Wildcard
                                let provided = V.map Syn.patCtorArgKind args
                                let extras = V.replicate (fieldLen - argLen) Syn.WildcardPat
                                return $ Just (provided V.++ extras)
                    else do
                        if argLen /= fieldLen
                            then do
                                addErrReportMsg $
                                    "Argument count mismatch: expected "
                                        <> T.pack (show fieldLen)
                                        <> ", got "
                                        <> T.pack (show argLen)
                                return Nothing
                            else return $ Just (V.map Syn.patCtorArgKind args)

    diff l1 l2 = filter (`notElem` l2) l1

-- Consider a set of pattern
--  Foo::A(..)
--  Foo::B(..) if ...
--  Foo::A(..)
--  _ if ...
--  Foo::A(..)
--  Foo::B(..)
--  Foo::C(..)
--  _

data PMRow = PMRow
    -- store distinguishable patterns (ctor or constant)
    -- and the de Bruijn level of the pattern.
    -- empty patterns means this raw is catching all patterns.
    { rowPatterns :: RRB.Vector (PatternVarRef, Syn.PatternKind)
    , rowBindings :: HashMap.HashMap Identifier PatternVarRef
    , rowGuard :: Maybe Syn.Expr
    , rowBody :: Syn.Expr
    }

data PMMatrix = PMMatrix
    { matrixCursor :: PatternVarRef
    , matrixRows :: RRB.Vector PMRow
    , matrixTypes :: HashMap.HashMap PatternVarRef Semi.Type
    }

data SplitResult
    = SplitResult
    { leadingNonWildcardRows :: RRB.Vector PMRow
    , wildcardRows :: RRB.Vector PMRow
    , trailingNonWildcardRows :: RRB.Vector PMRow
    }
data DispatchKind
    = DispatchInt
    | DispatchBool
    | DispatchCtor Path
    | DispatchString
    | DispatchFP
    | DispatchNullable

initializePMMatrix :: V.Vector (Syn.Pattern, Syn.Expr) -> Semi.Type -> PMMatrix
initializePMMatrix patterns semiType =
    PMMatrix zeroSingleton inner hashSingleton
  where
    zeroSingleton :: PatternVarRef
    zeroSingleton = PatternVarRef $ Seq.singleton 0

    hashSingleton :: HashMap.HashMap PatternVarRef Semi.Type
    hashSingleton = HashMap.singleton zeroSingleton semiType

    inner :: RRB.Vector PMRow
    inner = V.foldl' (\acc x -> acc RRB.|> patternToRow x) mempty patterns

    patternToRow :: (Syn.Pattern, Syn.Expr) -> PMRow
    patternToRow (Syn.Pattern kind guard, expr) = case kind of
        Syn.WildcardPat -> PMRow mempty mempty guard expr
        Syn.BindPat identifier ->
            PMRow mempty (HashMap.singleton identifier zeroSingleton) guard expr
        _ -> PMRow (RRB.singleton (zeroSingleton, kind)) mempty guard expr

-- A row is a wildcard with respect to a prefix if it has no pattern
-- or its left-most pattern is larger than the prefix.
rowIsWildcardAtPrefix :: PMRow -> PatternVarRef -> Bool
rowIsWildcardAtPrefix row prefix = case RRB.viewl (rowPatterns row) of
    Just ((hd, _), _) -> prefix < hd
    Nothing -> null (rowPatterns row)

findWildcardRow :: PMMatrix -> Maybe Int
findWildcardRow (PMMatrix prefix rows _) =
    RRB.findIndexL (flip rowIsWildcardAtPrefix prefix) rows

-- assume current matrix have no wildcard, get the dispatch kind of the matrix
-- query the first row should be enough, error otherwise
-- Notice that for CtorPattern, if path is Nullable::NonNull/Null, it is a dispatch on Nullable
-- Also notice that for other ctor patterns, the dispatch kind record the prefix path
-- e.g. the path with variant dropped
getDispatchKind :: PMMatrix -> DispatchKind
getDispatchKind (PMMatrix _ rows _) =
    case RRB.viewl rows of
        Nothing -> error "getDispatchKind: empty matrix"
        Just (firstRow, _) ->
            case RRB.viewl (rowPatterns firstRow) of
                Nothing -> error "getDispatchKind: empty row patterns"
                Just ((_, kind), _) -> patKindToDispatch kind
  where
    patKindToDispatch :: Syn.PatternKind -> DispatchKind
    patKindToDispatch (Syn.ConstPat (Syn.ConstInt _)) = DispatchInt
    patKindToDispatch (Syn.ConstPat (Syn.ConstBool _)) = DispatchBool
    patKindToDispatch (Syn.ConstPat (Syn.ConstString _)) = DispatchString
    patKindToDispatch (Syn.ConstPat (Syn.ConstDouble _)) = DispatchFP
    patKindToDispatch (Syn.CtorPat path _ _ _)
        | isNullablePath path = DispatchNullable
        | otherwise = DispatchCtor (droppedVariantPath path)
    patKindToDispatch _ = error "getDispatchKind: unsupported or wildcard pattern"

-- check if all rows are distinguishable via the same dispatch kind
validateDistinguishable :: PMMatrix -> Bool
validateDistinguishable (PMMatrix cursor rows types) =
    all rowCompatible rows
  where
    semiTy = case HashMap.lookup cursor types of
        Nothing -> error "validateDistinguishable: pattern ref not found"
        Just ty -> ty
    rowCompatible row =
        case RRB.viewl (rowPatterns row) of
            Nothing -> True -- Should not happen if no wildcards assumed, but safe default
            Just ((_, kind), _) -> isCompatible semiTy kind

    isCompatible :: Semi.Type -> Syn.PatternKind -> Bool
    isCompatible (Semi.TypeIntegral _) (Syn.ConstPat (Syn.ConstInt _)) = True
    isCompatible Semi.TypeBool (Syn.ConstPat (Syn.ConstBool _)) = True
    isCompatible Semi.TypeStr (Syn.ConstPat (Syn.ConstString _)) = True
    isCompatible (Semi.TypeFP _) (Syn.ConstPat (Syn.ConstDouble _)) = True
    isCompatible (Semi.TypeNullable _) (Syn.CtorPat path _ _ _) = isNullablePath path
    isCompatible (Semi.TypeRecord tyPath _ _) (Syn.CtorPat ctorPath _ _ _) =
        not (isNullablePath ctorPath) && tyPath == droppedVariantPath ctorPath
    isCompatible _ _ = False

isNullablePath :: Path -> Bool
isNullablePath (Path base segs) =
    (base == "NonNull" || base == "Null")
        && (not (null segs) && last segs == "Nullable")

droppedVariantPath :: Path -> Path
droppedVariantPath (Path _ []) = error "Internal error: CtorPat path has no segments to drop"
droppedVariantPath (Path _ segs) = Path (last segs) (init segs)

stableSortDistinguishable :: PMMatrix -> PMMatrix
stableSortDistinguishable mat@PMMatrix{matrixRows} = mat{matrixRows = RRB.sortBy compare' matrixRows}
  where
    compare' :: PMRow -> PMRow -> Ordering
    compare' r1 r2 =
        let l1 = RRB.viewl (rowPatterns r1)
            l2 = RRB.viewl (rowPatterns r2)
         in case (l1, l2) of
                ( Just ((_, Syn.CtorPat{patCtorPath = p1}), _)
                    , Just ((_, Syn.CtorPat{patCtorPath = p2}), _)
                    ) -> p1 `compare` p2
                ( Just ((_, Syn.ConstPat (Syn.ConstBool b1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstBool b2)), _)
                    ) -> b1 `compare` b2
                ( Just ((_, Syn.ConstPat (Syn.ConstInt i1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstInt i2)), _)
                    ) -> i1 `compare` i2
                ( Just ((_, Syn.ConstPat (Syn.ConstString s1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstString s2)), _)
                    ) -> s1 `compare` s2
                ( Just ((_, Syn.ConstPat (Syn.ConstDouble d1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstDouble d2)), _)
                    ) -> d1 `compare` d2
                _ -> error "stableSortDistinguishable applied to invalid pattern matrix"

splitAtFirstWildcard :: PMMatrix -> SplitResult
splitAtFirstWildcard mat@PMMatrix{matrixRows, matrixCursor} =
    -- Attempt to find the index of the first row that acts as a wildcard
    -- with respect to the current matrix cursor (prefix).
    case findWildcardRow mat of
        -- If no wildcard row is found, all rows are leading non-wildcard rows.
        Nothing -> SplitResult matrixRows mempty mempty
        -- If a wildcard row is found at index i:
        Just i ->
            -- Split the rows into 'leading' (before data i) and 'rest' (starting at i).
            let (leading, rest) = RRB.splitAt i matrixRows
                -- 'rest' starts with a wildcard row.
                -- Find the index of the first non-wildcard row in 'rest' to split wildcards and trailing.
                splitIdx = case RRB.findIndexL (not . (`rowIsWildcardAtPrefix` matrixCursor)) rest of
                     Nothing -> length rest
                     Just idx -> idx
                (wildcards, trailing) = RRB.splitAt splitIdx rest
             in SplitResult leading wildcards trailing


normalizeVarRefLevel :: PMMatrix -> PMMatrix
normalizeVarRefLevel mat@PMMatrix{matrixRows} =
    case [ ref
         | row <- toList matrixRows
         , Just ((ref, _), _) <- [RRB.viewl (rowPatterns row)]
         ] of
        [] -> mat
        refs -> mat{matrixCursor = minimum refs}

-- first apply stable sort, then divide the matrix into multiple groups and each
-- group of rows have the same value. Again, this assumes no wildcard in the matrix
divideDistinguishable :: PMMatrix -> [PMMatrix]
divideDistinguishable mat =
    let sortedMat = stableSortDistinguishable mat
        rows = toList (matrixRows sortedMat)
        groups = groupBy sameGroup rows
     in map (\rows' -> mat{matrixRows = RRB.fromList rows'}) groups
  where
    sameGroup r1 r2 =
        let l1 = RRB.viewl (rowPatterns r1)
            l2 = RRB.viewl (rowPatterns r2)
         in case (l1, l2) of
                ( Just ((_, Syn.CtorPat{patCtorPath = p1}), _)
                    , Just ((_, Syn.CtorPat{patCtorPath = p2}), _)
                    ) -> p1 == p2
                ( Just ((_, Syn.ConstPat (Syn.ConstBool b1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstBool b2)), _)
                    ) -> b1 == b2
                ( Just ((_, Syn.ConstPat (Syn.ConstInt i1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstInt i2)), _)
                    ) -> i1 == i2
                ( Just ((_, Syn.ConstPat (Syn.ConstString s1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstString s2)), _)
                    ) -> s1 == s2
                ( Just ((_, Syn.ConstPat (Syn.ConstDouble d1)), _)
                    , Just ((_, Syn.ConstPat (Syn.ConstDouble d2)), _)
                    ) -> d1 == d2
                _ -> error "divideDistinguishable applied to invalid pattern matrix"

data TyckCPS = TyckCPS
    { inferType :: Syn.Expr -> SemiEff Semi.Expr
    , checkType :: Syn.Expr -> Semi.Type -> SemiEff Semi.Expr
    , evalType :: SynType.Type -> Semi.Type
    , bindVar ::
        forall a.
        Identifier ->
        Maybe (Int64, Int64) ->
        Semi.Type ->
        (VarID -> SemiEff a) ->
        SemiEff a
    }

translatePMToDT :: TyckCPS -> PMMatrix -> SemiEff (DecisionTree Semi.Expr)
translatePMToDT _ _ = undefined

-- | Recursively substitute 'DTUncovered' nodes in a Decision Tree with a fallback Decision Tree.
-- This is used to merge the results of a wildcard match (which may fail/be uncovered)
-- with a fallback strategy (the rest of the matrix).
substituteUncovered :: DecisionTree Semi.Expr -> DecisionTree Semi.Expr -> DecisionTree Semi.Expr
substituteUncovered DTUncovered fallback = fallback
substituteUncovered (DTGuard bindings expr trueBr falseBr) fallback =
    DTGuard bindings expr (substituteUncovered trueBr fallback) (substituteUncovered falseBr fallback)
substituteUncovered (DTSwitch ref cases) fallback =
    DTSwitch ref (substituteCases cases)
  where
    substituteCases (DTSwitchInt m def) =
        DTSwitchInt (fmap (`substituteUncovered` fallback) m) (substituteUncovered def fallback)
    substituteCases (DTSwitchBool t f) =
        DTSwitchBool (substituteUncovered t fallback) (substituteUncovered f fallback)
    substituteCases (DTSwitchCtor cs def) =
        DTSwitchCtor (V.map (`substituteUncovered` fallback) cs) (substituteUncovered def fallback)
    substituteCases (DTSwitchString m def) =
        DTSwitchString (fmap (`substituteUncovered` fallback) m) (substituteUncovered def fallback)
    substituteCases (DTSwitchNullable j n) =
        DTSwitchNullable (substituteUncovered j fallback) (substituteUncovered n fallback)
substituteUncovered node _ = node -- Leaf, Unreachable

-- Let's discuss the situations where we have a set of rows and first several of
-- them are wildcard patterns at this level.
-- 1. if there is no further pattern in the first wildcard row
--    1.a if there is no guard. then this basically discard all further rows. we
--        hit a leaf case. We probably want to emit warnings if there are still
--        rows left.
--    1.b if there is a guard. then we emit a guard node. the true branch is 
--        just a leaf case. on the false branch, we popout the leading wildcard
--        pattern:
--        1.b.i if there is no further wildcard row after poping out, we just recurse
--              on the fallback matrix with normal translation
--        1.b.ii if there are further wildcard rows, we end up with 2).
-- 2. if there are further patterns in the first wildcard rows. We normalize
--    the wildcard matrix and translate it as normal matrix. We also translate
--    the fallback matrix as normal matrix. Then we combine them by subsituting
--    Uncovered nodes in wildcard decision tree with the fallback decision tree.
--    Notice that rows in leading wildcards are all wildcards at the current level
--    so normalization will always advance it. We will not stuck in it.
translateWithLeadingWildcards ::
    TyckCPS -> -- ^ Tyck utils and CPS context
    PatternVarRef -> -- ^ Current pattern variable reference (cursor)
    HashMap.HashMap PatternVarRef Semi.Type -> -- ^ Current type bindings
    RRB.Vector PMRow -> -- ^ Rows being a wildcard at the current position
    RRB.Vector PMRow -> -- ^ Rows without leading wildcards as fallback
    SemiEff (DecisionTree Semi.Expr)
translateWithLeadingWildcards cps cursor typeMap wildcards fallback = do
    -- We assume 'wildcards' is non-empty given the context of calling this function.
    case RRB.viewl wildcards of
        Nothing -> error "translateWithLeadingWildcards: empty wildcards list"
        Just (firstRow, restWildcards) ->
            -- Check if the first wildcard row is exhausted (no more patterns).
            -- If `rowPatterns` is empty, this row matches everything remaining.
            if null (rowPatterns firstRow)
                then handleLeafRow firstRow restWildcards
                else handleWildcardRecursion
  where
    -- Helper to recursively bind variables from a row's bindings
    bindRowVars ::
        HashMap.HashMap Identifier PatternVarRef ->
        (IntMap.IntMap PatternVarRef -> SemiEff a) ->
        SemiEff a
    bindRowVars rowBinds k = go (HashMap.toList rowBinds) IntMap.empty
      where
        go [] acc = k acc
        go ((ident, ref) : rest) acc =
            case HashMap.lookup ref typeMap of
                Nothing -> error "Type not found for pattern var ref in bindRowVars"
                Just ty ->
                    (bindVar cps) ident Nothing ty $ \(VarID vid) ->
                        go rest (IntMap.insert vid ref acc)

    -- Case 1: The row matches unconditionally at this level and has no more patterns.
    -- It acts as a leaf (success) or a guarded leaf.
    handleLeafRow row rest = do
        case rowGuard row of
            Nothing -> do
                -- Case 1.a: No guard. This is a leaf node.
                -- It catches all cases, so we discard any potential fallback rows (and subsequent wildcard rows).
                -- We bind the variables collected in this row and translate the body.
                bindRowVars (rowBindings row) $ \mapping -> do
                    body <- inferType cps (rowBody row)
                    return $ DTLeaf body mapping
            Just guard -> do
                -- Case 1.b: There is a guard.
                -- Bind variables to check the guard.
                bindRowVars (rowBindings row) $ \mapping -> do
                    -- Check the guard expression type (must be Bool).
                    guardExpr <- checkType cps guard Semi.TypeBool

                    -- True branch: The guard succeeds, execute the body (Leaf).
                    trueBranch <- do
                        body <- inferType cps (rowBody row)
                        return $ DTLeaf body mapping

                    -- False branch: The guard fails.
                    -- We effectively "pop" this row and try the next strategy.
                    falseBranch <-
                         if null rest
                                then
                                    -- Case 1.b.i: No more wildcard rows.
                                    -- Recurse on the original fallback rows.
                                    -- We construct a matrix from the fallback rows to use translatePMToDT.
                                    let fallbackMat = PMMatrix cursor fallback typeMap
                                     in translatePMToDT cps fallbackMat
                                else
                                    -- Case 1.b.ii: There are more wildcard rows.
                                    -- Recurse with the remaining wildcard rows and the same fallback.
                                    translateWithLeadingWildcards cps cursor typeMap rest fallback

                    return $ DTGuard mapping guardExpr trueBranch falseBranch

    -- Case 2: The wildcard rows have further patterns (they are just wildcards *at this level*).
    handleWildcardRecursion = do
        -- 1. Translate the wildcard rows.
        -- We construct a matrix from the wildcard rows.
        let wildcardMat = PMMatrix cursor wildcards typeMap
        -- We MUST normalize this matrix. Since they are wildcards at 'cursor',
        -- they must have patterns at some 'cursor' > 'cursor'.
        -- 'normalizeVarRefLevel' will advance the cursor to the next relevant column.
        let normalizedWildcardMat = normalizeVarRefLevel wildcardMat

        dtWildcard <- translatePMToDT cps normalizedWildcardMat

        -- 2. Translate the fallback rows.
        -- These are the rows that were valid *at `cursor`* (or after).
        -- We process them as a normal matrix starting from `cursor`.
        let fallbackMat = PMMatrix cursor fallback typeMap
        dtFallback <- translatePMToDT cps fallbackMat

        -- 3. Combine them.
        -- If the wildcard path results in 'Uncovered', we fall back to 'dtFallback'.
        return $ substituteUncovered dtWildcard dtFallback
