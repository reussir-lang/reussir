{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reussir.Core.Semi.PatternMatch
Description : Pattern matching compilation via decision trees

= Overview

This module compiles high-level pattern matching syntax (as written by the
programmer) into efficient /decision trees/ suitable for code generation.
The approach is based on the classic "pattern matrix" algorithm, similar
to the one described by Maranget (2008) and used in OCaml/Rust compilers.

== What is a Decision Tree?

A decision tree is a tree structure that tells the runtime how to dispatch
on a matched value, step by step. Instead of testing every pattern arm
top-to-bottom (which could be O(n*m) for n arms with m sub-patterns), a
decision tree shares tests across arms and achieves much better performance.

@
   Source Code                              Decision Tree
   ----------                              -------------
   match x {                               DTSwitch x.tag
     Result::Ok(0) => 1,                     |
     Result::Ok(n) => n,        ====>        +-- ctor\@0 (Ok)
     Result::Err(0) => 2,                   |    DTSwitch x.0
     Result::Err(_) => 3                    |      +-- 0 => Leaf(1)
   }                                        |      +-- _ => Leaf(n) [bind n = x.0]
                                            |
                                            +-- ctor\@1 (Err)
                                                 DTSwitch x.0
                                                   +-- 0 => Leaf(2)
                                                   +-- _ => Leaf(3)
@

== Architecture Pipeline

@
  User Source (.rr)
       |
       v
  Parser (Reussir.Parser.Types.Expr)
       |  Produces: Vector (Pattern, Expr)
       v
  initializePMMatrix           <--- THIS MODULE (entry point)
       |  Produces: PMMatrix
       v
  translatePMToDT              <--- THIS MODULE (main algorithm)
       |  Produces: DecisionTree (Semi)
       v
  Full Elaboration
       |  Produces: DecisionTree (Full)
       v
  lowerMatch (DecisionTree.hs) <--- Lowering to IR
       |  Produces: MLIR instructions
       v
  MLIR/LLVM Backend
@

== Key Concepts for First-Time Readers

=== PatternVarRef (The "Cursor")

A 'PatternVarRef' is a /path/ into the scrutinee value, represented as a
sequence of integers. Think of it like an address:

@
  PatternVarRef [0]       -- the scrutinee itself
  PatternVarRef [0, 0]    -- first field of the scrutinee
  PatternVarRef [0, 1]    -- second field of the scrutinee
  PatternVarRef [0, 0, 2] -- third field of the first field
@

Example with a nested type:

@
  enum Result { Ok(i32), Err(i32) }

  match result {            Scrutinee layout:
    Result::Ok(0) => ...       result        = PatternVarRef [0]
    ...                        result.val    = PatternVarRef [0, 0]
  }
@

=== PMRow (A Single Pattern Arm)

Each arm in a @match@ becomes one 'PMRow'. A row contains:

  * 'rowPatterns'  -- The remaining sub-patterns to match (as (ref, kind) pairs)
  * 'rowBindings'  -- Variable bindings collected so far (name -> ref)
  * 'rowGuard'     -- Optional guard expression (@if condition@)
  * 'rowBody'      -- The body expression to evaluate on match

=== PMMatrix (The Pattern Matrix)

A 'PMMatrix' groups all rows together with:

  * 'matrixCursor'  -- Which PatternVarRef we are currently dispatching on
  * 'matrixRows'    -- All pattern rows
  * 'matrixTypes'   -- Type information for each PatternVarRef

=== The Pattern Matrix Visualized

Consider this Reussir code:

@
  enum Shape { Circle(f64), Rect(f64, f64) }

  match shape {
    Shape::Circle(0.0) => 1,    -- row 0
    Shape::Circle(r)   => 2,    -- row 1
    Shape::Rect(w, h)  => 3,    -- row 2
  }
@

The initial PMMatrix (cursor = [0]) looks like:

@
  ┌─────────┬──────────────────────────────┬──────────┬──────┐
  │  Row    │  rowPatterns                 │ bindings │ body │
  ├─────────┼──────────────────────────────┼──────────┼──────┤
  │  row 0  │  [([0], Circle(0.0))]       │  {}      │  1   │
  │  row 1  │  [([0], Circle(r))]         │  {}      │  2   │
  │  row 2  │  [([0], Rect(w, h))]        │  {}      │  3   │
  └─────────┴──────────────────────────────┴──────────┴──────┘
  cursor = [0]
  types  = { [0] : Shape }
@

After splitting on Shape (ctor dispatch at [0]):

  Group \"Circle\" (rows 0, 1) -- after expanding Circle(x) sub-patterns:

@
  ┌─────────┬──────────────────────────────┬──────────┬──────┐
  │  Row    │  rowPatterns                 │ bindings │ body │
  ├─────────┼──────────────────────────────┼──────────┼──────┤
  │  row 0  │  [([0,0], ConstDouble 0.0)] │  {}      │  1   │
  │  row 1  │  []                         │  {r=[0,0]}│  2   │
  └─────────┴──────────────────────────────┴──────────┴──────┘
  cursor = [0,0]
  types  = { [0] : Shape, [0,0] : f64 }
@

  Group \"Rect\" (row 2) -- after expanding Rect(w, h):

@
  ┌─────────┬──────────────────────────────┬──────────────────┬──────┐
  │  Row    │  rowPatterns                 │ bindings         │ body │
  ├─────────┼──────────────────────────────┼──────────────────┼──────┤
  │  row 2  │  []                         │ {w=[0,0],h=[0,1]}│  3   │
  └─────────┴──────────────────────────────┴──────────────────┴──────┘
  cursor advances past [0] (no more sub-patterns to match)
@

== The Main Algorithm: translatePMToDT

The core loop works as follows:

@
  translatePMToDT(matrix):
      1. If matrix is empty -> return DTUncovered
      2. Split rows into 3 groups using splitAtFirstWildcard:
         ┌──────────────────────────────────────────┐
         │  Leading distinguishable rows             │ <-- have specific patterns at cursor
         ├──────────────────────────────────────────┤
         │  Wildcard rows                           │ <-- are wildcards at cursor
         ├──────────────────────────────────────────┤
         │  Trailing distinguishable rows           │ <-- have specific patterns after wildcards
         └──────────────────────────────────────────┘
      3a. If no leading distinguishable rows:
          -> translateWithLeadingWildcards
      3b. If there are leading distinguishable rows:
          -> translateWithLeadingDistinguishable
@

=== What splitAtFirstWildcard Does

@
  Input rows (cursor = [0]):
    row 0:  [([0], ConstInt 0)]     -- distinguishable (specific value)
    row 1:  [([0], ConstInt 1)]     -- distinguishable
    row 2:  []                      -- WILDCARD (no pattern at [0])
    row 3:  [([1], ConstBool True)] -- WILDCARD at [0] (pattern is at [1] > [0])
    row 4:  [([0], ConstInt 2)]     -- distinguishable (trailing)

  Result:
    leadingNonWildcard = [row 0, row 1]
    wildcardRows       = [row 2, row 3]
    trailingRows       = [row 4]
@

=== Wildcard Handling Cases (translateWithLeadingWildcards)

@
  Case 1a: Wildcard row with NO more patterns and NO guard
    -> DTLeaf (this arm catches everything)

  Case 1b: Wildcard row with NO more patterns but HAS a guard
    -> DTGuard { true: DTLeaf, false: <recurse on rest> }

  Case 2:  Wildcard rows HAVE further patterns (just not at cursor)
    -> Normalize cursor, recurse, then substituteUncovered
       with the fallback matrix
@

=== Distinguishable Handling (translateWithLeadingDistinguishable)

@
  1. Validate all rows are compatible with the scrutinee type
  2. Sort and divide into groups (by constructor/constant value)
  3. For constant dispatch (Int, Bool, String):
     - Pop the front pattern from each row
     - Recurse on each group
     - Emit DTSwitch with appropriate case structure
  4. For constructor dispatch:
     - Normalize ctor sub-patterns into positional form
     - Prepend new sub-pattern columns to each row
     - Update type map with field types
     - Recurse on each group
  5. Replace DTUncovered nodes with fallback tree
@

== Constructor Pattern Normalization (normalizeCtorPattern)

Reussir supports both named and positional constructor patterns:

@
  -- Named (order doesn't matter, can use `..` for rest):
  Point { y: 0, x }       -- x binds to field "x", y matched against 0
  Point { x, .. }         -- only match x, ignore rest

  -- Positional (order matters):
  Tuple(a, b)             -- a = field 0, b = field 1
  Tuple(a, ..)            -- only match first field
@

normalizeCtorPattern converts both forms into a positional vector:

@
  Named { y: 0, x } with fields [x, y]:
    -> [BindPat "x", ConstPat 0]    (reordered to match field declaration order)

  Positional (a, ..) with 3 fields:
    -> [BindPat "a", WildcardPat, WildcardPat]    (padded with wildcards)
@

-}
module Reussir.Core.Semi.PatternMatch where

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, when)
import Data.Digest.XXHash.FFI (XXH3 (..))
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (groupBy, partition)
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

import Reussir.Core.Data.Class (Class (..))
import Reussir.Core.Data.Semi.Context (SemiContext (..), SemiEff, knownRecords)
import Reussir.Core.Data.Semi.Expr (
    DTSwitchCases (..),
    DecisionTree (..),
    PatternVarRef (..),
 )
import Reussir.Core.Data.Semi.Record (Record (..), RecordFields (..))
import Reussir.Core.Data.UniqueID (GenericID (..), VarID (..))
import Reussir.Core.Semi.Context (addErrReportMsg, runUnification)
import Reussir.Core.Semi.Type (substituteGenericMap)
import Reussir.Core.Semi.Unification (force, satisfyBounds)

import Reussir.Core.Data.Semi.Expr qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as Semi

{- | Normalize a constructor pattern into positional (applied) form.

Given a constructor path like @Point@ and user-written arguments, this
function reorders named arguments to match the declaration order and fills
in wildcards for any fields omitted via @..@ (ellipsis).

Returns 'Nothing' if normalization fails (e.g., unknown fields, arity mismatch).

=== Examples

@
  -- Record declaration: struct Point { x: i32, y: i32 }

  -- Input: Point { y: 0, x }   hasEllipsis=False
  -- Output: Just [BindPat "x", ConstPat 0]
  --         (reordered to declaration order: x first, y second)

  -- Input: Point { x }         hasEllipsis=False
  -- Output: Nothing            (missing field y, no ellipsis)

  -- Input: Point { x, .. }     hasEllipsis=True
  -- Output: Just [BindPat "x", WildcardPat]
  --         (y filled with wildcard because of ..)
@

=== Flow

@
  normalizeCtorPattern(path, args, ellipsis)
       │
       ├─ Look up record in knownRecords
       │     │
       │     ├─ Not found ────────> error: "Record not found"
       │     │
       │     └─ Found
       │          │
       │          ├─ Named fields ──> normalizeNamed
       │          │     │
       │          │     ├─ Check for positional args (rejected)
       │          │     ├─ Check for unknown fields (rejected)
       │          │     ├─ Check for missing fields (rejected unless ellipsis)
       │          │     └─ Reorder to declaration order, fill gaps with _
       │          │
       │          ├─ Unnamed fields -> normalizeUnnamed
       │          │     │
       │          │     ├─ Check for named args (rejected)
       │          │     ├─ Check arity (exact or <= if ellipsis)
       │          │     └─ Pad with wildcards if ellipsis
       │          │
       │          └─ Variants ──────> error (shouldn't happen here)
@
-}
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

{- | A single row in the pattern matrix.

Consider a @match@ expression with multiple arms:

@
  match x {
    Foo::A(..)        -- row 0: distinguishable (ctor A)
    Foo::B(..) if ... -- row 1: distinguishable (ctor B) + guard
    Foo::A(..)        -- row 2: distinguishable (ctor A, duplicate)
    _ if ...          -- row 3: wildcard + guard
    Foo::A(..)        -- row 4: distinguishable (trailing after wildcard)
    Foo::B(..)        -- row 5: distinguishable (trailing)
    Foo::C(..)        -- row 6: distinguishable (trailing)
    _                 -- row 7: wildcard (catch-all)
  }
@

Each row tracks:

  * Which sub-patterns remain to be matched ('rowPatterns')
  * Which variables have been bound so far ('rowBindings')
  * Whether there is a guard condition ('rowGuard')
  * What expression to evaluate on successful match ('rowBody')

A row is considered a /wildcard/ at a given cursor position if:

  * It has no patterns at all (matches everything), or
  * Its leftmost pattern's 'PatternVarRef' is strictly greater
    than the current cursor (it doesn't care about this position)
-}
data PMRow = PMRow
    { rowPatterns :: RRB.Vector (PatternVarRef, Syn.PatternKind)
    -- ^ Remaining patterns to match, paired with their position reference.
    -- Each entry is @(where_to_look, what_to_match)@.
    -- An empty vector means this row is a catch-all (wildcard) at the current level.
    , rowBindings :: HashMap.HashMap Identifier PatternVarRef
    , rowGuard :: Maybe Syn.Expr
    , rowBody :: Syn.Expr
    }

{- | The pattern matrix: all rows being matched against, plus context.

=== Visualization

@
  PMMatrix:
    cursor = [0]              <-- "which position are we deciding on now?"
    types  = { [0]: i32 }     <-- type of each position
    rows:
      ┌─────┬──────────────────────┬──────────┬──────┬───────┐
      │ Row │ rowPatterns           │ bindings │guard │ body  │
      ├─────┼──────────────────────┼──────────┼──────┼───────┤
      │  0  │ [([0], ConstInt 0)]  │ {}       │ None │ 100   │
      │  1  │ [([0], ConstInt 1)]  │ {}       │ None │ 200   │
      │  2  │ []                   │ {y=[0]}  │ None │ y     │
      └─────┴──────────────────────┴──────────┴──────┴───────┘
@

The matrix is consumed column-by-column: we look at the current 'matrixCursor',
split rows into groups, generate a switch node, then recurse on sub-matrices.
-}
data PMMatrix = PMMatrix
    { matrixCursor :: PatternVarRef
    -- ^ The current column being dispatched on
    , matrixRows :: RRB.Vector PMRow
    -- ^ All pattern rows (in source order, which determines priority)
    , matrixTypes :: HashMap.HashMap PatternVarRef Semi.Type
    -- ^ Known types for each PatternVarRef (used for type-checking dispatch)
    }

{- | Result of splitting rows at the first wildcard position.

@
  Before split (cursor = [0]):
    ┌─────────────────────────────────┐
    │  row 0: ConstInt 0              │──┐
    │  row 1: ConstInt 1              │  │ leadingNonWildcardRows
    ├─────────────────────────────────┤──┘
    │  row 2: _ (wildcard)            │──┐
    │  row 3: _ if flag (wildcard+gd) │  │ wildcardRows
    ├─────────────────────────────────┤──┘
    │  row 4: ConstInt 2              │──── trailingNonWildcardRows
    └─────────────────────────────────┘
@

The split preserves source order within each group, which is essential
for correct pattern matching semantics (first-match wins).
-}
data SplitResult
    = SplitResult
    { leadingNonWildcardRows :: RRB.Vector PMRow
    -- ^ Rows before the first wildcard (these dispatch on the cursor)
    , wildcardRows :: RRB.Vector PMRow
    -- ^ Contiguous block of rows that are wildcards at the cursor
    , trailingNonWildcardRows :: RRB.Vector PMRow
    -- ^ Rows after the wildcard block (used as fallback)
    }
{- | The kind of dispatch (switch) to emit for a column.

Determined by looking at the first row's pattern in the current column.
All rows in a distinguishable block must agree on the dispatch kind.

@
  ConstPat (ConstInt _)    -> DispatchInt      (integer switch)
  ConstPat (ConstBool _)   -> DispatchBool     (boolean if/else)
  ConstPat (ConstString _) -> DispatchString   (string hash switch)
  ConstPat (ConstDouble _) -> DispatchFP       (currently unsupported!)
  CtorPat Nullable::*      -> DispatchNullable (null/non-null check)
  CtorPat SomeEnum::*      -> DispatchCtor     (variant tag switch)
@
-}
data DispatchKind
    = DispatchInt
    | DispatchBool
    | DispatchCtor Path
    -- ^ The Path is the /parent enum/ path (variant name is dropped)
    | DispatchString
    | DispatchFP
    | DispatchNullable

{- | Create the initial pattern matrix from parsed match arms.

This is the /entry point/ to the pattern matching compiler. It converts
the user's @match@ arms into the initial matrix with cursor at [0].

=== Example

Given this source:

@
  match x {
    0 => 100,
    1 => 200,
    y => y
  }
@

Produces this initial matrix:

@
  cursor = PatternVarRef [0]
  types  = { PatternVarRef [0] : <type of x> }

  ┌─────┬──────────────────────────┬──────────────┬──────┐
  │ Row │ rowPatterns              │ rowBindings  │ body │
  ├─────┼──────────────────────────┼──────────────┼──────┤
  │  0  │ [([0], ConstInt 0)]     │ {}           │ 100  │
  │  1  │ [([0], ConstInt 1)]     │ {}           │ 200  │
  │  2  │ []                      │ {y = [0]}    │ y    │
  └─────┴──────────────────────────┴──────────────┴──────┘

  Note: row 2 (BindPat "y") becomes an empty rowPatterns
        (it's a wildcard) but records the binding y -> [0].
@
-}
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

{- | Check if a row is a "wildcard" at the given cursor position.

A row is a wildcard at cursor @c@ if:

  * It has no patterns left at all (it matches everything), OR
  * Its leftmost pattern's 'PatternVarRef' is strictly greater than @c@
    (meaning it has patterns, but at a deeper/later position -- it doesn't
    constrain position @c@).

@
  cursor = [0]

  row A: patterns = [([0], ConstInt 5)]   -- NOT wildcard (matches at [0])
  row B: patterns = []                    -- wildcard (matches everything)
  row C: patterns = [([1], ConstBool T)]  -- wildcard at [0] (its pattern is at [1])
  row D: patterns = [([0,1], ConstInt 3)] -- wildcard at [0] ([0,1] > [0])
@
-}
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
validateDistinguishable :: PMMatrix -> SemiEff Bool
validateDistinguishable (PMMatrix cursor rows types) = do
    let semiTyRaw = HashMap.lookupDefault Semi.TypeBottom cursor types
    semiTy <- runUnification $ force semiTyRaw
    results <- forM (toList rows) $ \row -> do
        case RRB.viewl (rowPatterns row) of
            Nothing -> return True
            Just ((_, kind), _) -> isCompatible semiTy kind
    return $ all id results
  where
    isCompatible :: Semi.Type -> Syn.PatternKind -> SemiEff Bool
    isCompatible ty (Syn.ConstPat (Syn.ConstInt _)) =
        runUnification $ satisfyBounds ty [Class (Path "Integral" [])]
    isCompatible Semi.TypeBool (Syn.ConstPat (Syn.ConstBool _)) = return True
    isCompatible Semi.TypeStr (Syn.ConstPat (Syn.ConstString _)) = return True
    isCompatible ty (Syn.ConstPat (Syn.ConstDouble _)) =
        runUnification $ satisfyBounds ty [Class (Path "FloatingPoint" [])]
    isCompatible (Semi.TypeNullable _) (Syn.CtorPat path _ _ _) = return $ isNullablePath path
    isCompatible (Semi.TypeRecord tyPath _ _) (Syn.CtorPat ctorPath _ _ _) =
        return $ not (isNullablePath ctorPath) && tyPath == droppedVariantPath ctorPath
    isCompatible _ _ = return False

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

{- | Split the matrix rows into three groups at the first wildcard boundary.

This is the key structural decomposition that drives the whole algorithm.
It partitions rows while preserving source order (which determines priority).

=== Algorithm

@
  1. Scan rows top-to-bottom for the first wildcard at cursor
  2. Everything before it = leadingNonWildcard
  3. The wildcard + any contiguous wildcards = wildcardRows
  4. Everything after the wildcard block = trailing

  Example (cursor = [0]):

    rows:  [Int 0] [Int 1] [_] [_ if g] [Int 2]
            ~~~~~~~~~~~     ~~~~~~~~     ~~~~~~
            leading         wildcards    trailing
            (idx 0,1)       (idx 2,3)    (idx 4)
@

=== Why This Matters

The split determines the dispatch strategy:

  * If there ARE leading distinguishable rows: emit a switch for them,
    using the wildcard+trailing as the default/fallback.

  * If the FIRST row is a wildcard: handle it directly (leaf or guard),
    then fall through to the rest.

This ensures first-match semantics: specific patterns before a wildcard
take priority, but the wildcard acts as a catch-all for unmatched values.
-}
splitAtFirstWildcard :: PMMatrix -> SplitResult
splitAtFirstWildcard mat@PMMatrix{matrixRows, matrixCursor} =
    case findWildcardRow mat of
        Nothing -> SplitResult matrixRows mempty mempty
        Just i ->
            let (leading, rest) = RRB.splitAt i matrixRows
                splitIdx = case RRB.findIndexL (not . (`rowIsWildcardAtPrefix` matrixCursor)) rest of
                    Nothing -> length rest
                    Just idx -> idx
                (wildcards, trailing) = RRB.splitAt splitIdx rest
             in SplitResult leading wildcards trailing

{- | Advance the matrix cursor to the minimum PatternVarRef among all rows.

After popping the front pattern or handling wildcards, the cursor may
no longer point to the column that rows care about. This function
scans all rows and sets the cursor to the smallest remaining ref.

@
  Before: cursor = [0], but all row patterns reference [0, 0] or higher
  After:  cursor = [0, 0]    (advanced to the actual next column)
@

This is essential to avoid infinite loops: after processing wildcards,
the cursor MUST advance, and normalizeVarRefLevel guarantees that.
-}
normalizeVarRefLevel :: PMMatrix -> PMMatrix
normalizeVarRefLevel mat@PMMatrix{matrixRows} =
    case [ ref
         | row <- toList matrixRows
         , Just ((ref, _), _) <- [RRB.viewl (rowPatterns row)]
         ] of
        [] -> mat
        refs -> mat{matrixCursor = minimum refs}

{- | Sort rows by their leading pattern value, then group consecutive equal ones.

This is used to partition distinguishable rows into per-value groups,
so each group becomes one arm of the emitted switch.

=== Example

@
  Input rows (sorted by stableSortDistinguishable):
    [([0], CtorPat Circle), ([0], CtorPat Circle), ([0], CtorPat Rect)]

  Output groups:
    Group 0: [CtorPat Circle, CtorPat Circle]  -- 2 rows matching Circle
    Group 1: [CtorPat Rect]                    -- 1 row  matching Rect
@

Assumes the matrix contains NO wildcard rows (only called after splitting).
-}
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

{- | Continuation-passing interface for type-checking within pattern compilation.

Pattern matching compilation needs to interact with the type checker to:

  * Infer expression types ('inferType')
  * Check guard expressions against Bool ('checkType')
  * Bind pattern variables into scope ('bindVar')

These are passed as callbacks because pattern matching compilation
happens /during/ type checking (semi-elaboration phase), not after.
-}
data TyckCPS = TyckCPS
    { inferType :: Syn.Expr -> SemiEff Semi.Expr
    , checkType :: Syn.Expr -> Semi.Type -> SemiEff Semi.Expr
    , bindVar ::
        forall a.
        Identifier ->
        Maybe (Int64, Int64) ->
        Semi.Type ->
        (VarID -> SemiEff a) ->
        SemiEff a
    }

{- | The main entry point: translate a pattern matrix into a decision tree.

=== Complete Control Flow

@
  translatePMToDT(matrix)
    │
    ├── matrix empty?
    │     └── YES: return DTUncovered
    │               (no arms left = uncovered case)
    │
    └── NO: splitAtFirstWildcard(matrix)
              │
              ├── Has leading distinguishable rows?
              │     │
              │     ├── YES: translateWithLeadingDistinguishable
              │     │          │
              │     │          ├── Validate types match dispatch kind
              │     │          ├── Compute fallback from wildcards+trailing
              │     │          ├── Sort & divide into per-value groups
              │     │          ├── For each group:
              │     │          │     ├── Const: pop front, recurse
              │     │          │     └── Ctor:  normalize sub-patterns,
              │     │          │                prepend new columns, recurse
              │     │          ├── Emit DTSwitch (Int/Bool/Ctor/String/Nullable)
              │     │          └── substituteUncovered with fallback DT
              │     │
              │     └── NO: translateWithLeadingWildcards
              │               │
              │               ├── First wildcard exhausted (no more patterns)?
              │               │     ├── No guard: DTLeaf (match!)
              │               │     └── Has guard: DTGuard
              │               │           ├── true:  DTLeaf
              │               │           └── false: recurse on rest
              │               │
              │               └── Wildcards have further patterns?
              │                     ├── Normalize cursor (advance)
              │                     ├── Recurse on wildcard matrix
              │                     ├── Recurse on fallback matrix
              │                     └── substituteUncovered to merge
@
-}
translatePMToDT :: TyckCPS -> PMMatrix -> SemiEff DecisionTree
translatePMToDT cps mat@PMMatrix{matrixCursor, matrixRows, matrixTypes} =
    if null matrixRows
        then
            -- No rows left -> uncovered case.
            -- DTUncovered is a placeholder that gets replaced by substituteUncovered
            -- if there's a fallback, or flagged as a warning if it survives to the end.
            return DTUncovered
        else do
            let SplitResult splitLeading splitWildcards splitTrailing = splitAtFirstWildcard mat
            if null splitLeading
                then
                    translateWithLeadingWildcards
                        cps
                        matrixCursor
                        matrixTypes
                        splitWildcards
                        splitTrailing
                else do
                    let leadingDistinguishable = mat{matrixRows = splitLeading}
                    translateWithLeadingDistinguishable
                        cps
                        leadingDistinguishable
                        splitWildcards
                        splitTrailing

{- | Replace all 'DTUncovered' leaves in a decision tree with a fallback tree.

This is the "glue" that merges partial decision trees. When we compile
the wildcard portion of a matrix, some paths may end up uncovered.
We then splice in the fallback tree (from trailing rows) at those points.

=== Example

@
  Wildcard tree:                   Fallback tree:
    DTGuard g                        DTLeaf "fallback"
      true:  DTLeaf "ok"
      false: DTUncovered  <--- this gets replaced

  Result after substituteUncovered:
    DTGuard g
      true:  DTLeaf "ok"
      false: DTLeaf "fallback"
@

This operation is recursive: it descends into all DTGuard and DTSwitch
nodes to find every DTUncovered leaf.
-}
substituteUncovered :: DecisionTree -> DecisionTree -> DecisionTree
substituteUncovered DTUncovered fallback = fallback
substituteUncovered (DTGuard bindings expr trueBr falseBr) fallback =
    DTGuard
        bindings
        expr
        (substituteUncovered trueBr fallback)
        (substituteUncovered falseBr fallback)
substituteUncovered (DTSwitch ref cases) fallback =
    DTSwitch ref (substituteCases cases)
  where
    substituteCases (DTSwitchInt m def) =
        DTSwitchInt
            (fmap (`substituteUncovered` fallback) m)
            (substituteUncovered def fallback)
    substituteCases (DTSwitchBool t f) =
        DTSwitchBool (substituteUncovered t fallback) (substituteUncovered f fallback)
    substituteCases (DTSwitchCtor cs) =
        DTSwitchCtor
            (V.map (`substituteUncovered` fallback) cs)
    substituteCases (DTSwitchString m def) =
        DTSwitchString
            (fmap (`substituteUncovered` fallback) m)
            (substituteUncovered def fallback)
    substituteCases (DTSwitchNullable j n) =
        DTSwitchNullable
            (substituteUncovered j fallback)
            (substituteUncovered n fallback)
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
    -- | Tyck utils and CPS context
    TyckCPS ->
    -- | Current pattern variable reference (cursor)
    PatternVarRef ->
    -- | Current type bindings
    HashMap.HashMap PatternVarRef Semi.Type ->
    -- | Rows being a wildcard at the current position
    RRB.Vector PMRow ->
    -- | Rows without leading wildcards as fallback
    RRB.Vector PMRow ->
    SemiEff DecisionTree
translateWithLeadingWildcards cps cursor typeMap wildcards fallback = do
    -- We assume 'wildcards' is non-empty given the context of calling this function.
    case RRB.viewl wildcards of
        Nothing | null fallback -> return DTUncovered
        Nothing -> do
            let normalizedFallback = normalizeVarRefLevel $ PMMatrix cursor fallback typeMap
            translatePMToDT cps normalizedFallback
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

-- Now we consider the cases where we start with distinguishable rows.
-- We first check that the matrix is valid for the current cursor.
-- Then we sort and divide the rows into groups.
--
-- For constant switches, the translation is relatively easy, just emit a
-- decision tree accordingly, where each group is assigned to its corresponding
-- branch and the default branch is set to Uncovered. We then pop the front
-- element of each row, normalize it and recursively generate the inner subtrees.
-- Finally, we replace all uncovered node in the result with the fallback tree
-- generated from wildcards and trailing fallback rows.
--
-- For ctor case, it is a bit more complicated. We still generate the switch
-- branch accordingly. However, in order to continue matching on subtrees, we
-- need to normalize the front ctor pattern of each row in each group.
-- We prepend the normalized, non-wildcard new conditions to the row in order,
-- and also update the binding map and type map accordingly. Finally, we handle
-- fallback rows and wildcards as before.
translateWithLeadingDistinguishable ::
    -- | Tyck utils and CPS context
    TyckCPS ->
    -- | Rows being distinguishable at the current position
    PMMatrix ->
    -- | Rows being a wildcard at the current position
    RRB.Vector PMRow ->
    -- | Rows without leading wildcards as fallback
    RRB.Vector PMRow ->
    SemiEff DecisionTree
translateWithLeadingDistinguishable cps distinguishable@(PMMatrix matCursor matRows matTypeMap) wildcards fallback = do
    when (null matRows) $
        error "translateWithLeadingDistinguishable: No distinguishable rows"

    valid <- validateDistinguishable distinguishable
    unless valid $
        addErrReportMsg "Indistinguishable patterns in the same column"

    fallbackDT <-
        translateWithLeadingWildcards cps matCursor matTypeMap wildcards fallback

    let sortedDist = stableSortDistinguishable distinguishable
    let dispatch = getDispatchKind sortedDist
    let groups = divideDistinguishable sortedDist

    let
        processGroup :: PMMatrix -> SemiEff (Syn.PatternKind, DecisionTree)
        processGroup group = do
            -- Safe because group comes from divideDistinguishable which ensures non-empty rows
            let (firstRow, _) = case RRB.viewl (matrixRows group) of
                    Just x -> x
                    Nothing -> error "processGroup: empty group"

            let leadingKind = case RRB.viewl (rowPatterns firstRow) of
                    Just ((_, k), _) -> k
                    Nothing -> error "processGroup: empty row patterns"

            subDT <- case dispatch of
                DispatchCtor _ -> processCtorGroup group leadingKind
                DispatchNullable -> processCtorGroup group leadingKind
                _ -> processConstGroup group
            return (leadingKind, subDT)

        processConstGroup group = do
            let popLeading row =
                    case RRB.viewl (rowPatterns row) of
                        Just (_, rest) -> Just row{rowPatterns = rest}
                        Nothing -> Just row -- Should unlikely happen for const group rows
            groupRows <-
                RRB.fromList . catMaybes
                    <$> forM (toList (matrixRows group)) (\row -> return $ popLeading row)

            wildcardRowsExpanded <-
                RRB.fromList . catMaybes
                    <$> forM (toList wildcards) (\row -> return $ Just row)

            fallbackRowsExpanded <-
                RRB.fromList . catMaybes
                    <$> forM
                        (toList fallback)
                        ( \row -> do
                            if rowIsWildcardAtPrefix row matCursor
                                then return $ Just row
                                else case RRB.viewl (rowPatterns row) of
                                    Just ((_, pat), _) ->
                                        -- Check if pattern matches the group's constant.
                                        -- We assume all rows in `group` share the same constant value,
                                        -- so checking against the first row's pattern is sufficient.
                                        case RRB.viewl (matrixRows group) of
                                            Just (firstRow, _) ->
                                                case RRB.viewl (rowPatterns firstRow) of
                                                    Just ((_, groupKind), _) ->
                                                        if pat == groupKind
                                                            then return $ popLeading row
                                                            else return Nothing
                                                    Nothing -> return Nothing
                                            Nothing -> return Nothing
                                    Nothing -> return Nothing
                        )

            let newRows = groupRows <> wildcardRowsExpanded <> fallbackRowsExpanded
            let newGroup = PMMatrix matCursor newRows matTypeMap
            let finalGroup = normalizeVarRefLevel newGroup
            translatePMToDT cps finalGroup

        processCtorGroup group leadingKind = do
            let ctorPath = case leadingKind of
                    Syn.CtorPat p _ _ _ -> p
                    _ -> error "Expected CtorPat"

            let parentType = HashMap.lookupDefault Semi.TypeBottom matCursor matTypeMap
            parentType' <- runUnification $ force parentType
            fieldTypes <- resolveFieldTypes parentType' ctorPath

            let mapRow row = do
                    let rowPatsView = RRB.viewl (rowPatterns row)
                    case rowPatsView of
                        Just ((_, pat), restPats) ->
                            case pat of
                                Syn.CtorPat p args ell _ -> do
                                    if p == ctorPath
                                        then do
                                            -- Convert args to strict vector
                                            let argsStrict = V.fromList (toList args)
                                            mNorm <- normalizeCtorPattern p argsStrict ell
                                            case mNorm of
                                                Nothing -> return $ Just row{rowPatterns = restPats}
                                                Just subPats -> do
                                                    let newCols =
                                                            V.imap
                                                                ( \i subPat ->
                                                                    let ref = extendRef matCursor i
                                                                     in (ref, subPat)
                                                                )
                                                                subPats

                                                    let validCols =
                                                            V.filter
                                                                ( \(_, pat') -> case pat' of
                                                                    Syn.WildcardPat -> False
                                                                    Syn.BindPat _ -> False
                                                                    _ -> True
                                                                )
                                                                newCols

                                                    let newRefPats = RRB.fromList (V.toList validCols)

                                                    let newBindings =
                                                            V.foldl'
                                                                ( \acc (ref, subPat) ->
                                                                    case subPat of
                                                                        Syn.BindPat ident -> HashMap.insert ident ref acc
                                                                        _ -> acc
                                                                )
                                                                (rowBindings row)
                                                                newCols

                                                    return $
                                                        Just row{rowPatterns = newRefPats <> restPats, rowBindings = newBindings}
                                        else return Nothing -- Constructor mismatch, drop row from this branch
                                Syn.WildcardPat -> do
                                    -- Wildcard matches everything, so we just pop it.
                                    return $ Just row{rowPatterns = restPats}
                                _ -> return Nothing -- Should not happen for group rows, but can for fallback
                        Nothing -> return $ Just row -- Should not happen for group rows
            groupRows <-
                RRB.fromList . catMaybes
                    <$> forM (toList (matrixRows group)) mapRow

            -- \| Expand wildcard rows for the current constructor.
            -- Since these rows are wildcards at the current cursor, they match any constructor.
            -- We just need to ensure they are preserved in the new matrix, potentially implicitly
            -- handled if they don't have patterns for the new fields.
            wildcardRowsExpanded <-
                RRB.fromList . catMaybes
                    <$> forM (toList wildcards) (\row -> return $ Just row)

            -- \| Expand fallback rows.
            -- Fallback rows are patterns that appear *after* the current block of distinguishable patterns.
            -- They are included in the new matrix if:
            -- 1. They are wildcards at the current cursor (match this constructor).
            -- 2. They explictly match the current constructor.
            -- If they match a different constructor, they are excluded from this branch.
            fallbackRowsExpanded <-
                RRB.fromList . catMaybes
                    <$> forM
                        (toList fallback)
                        ( \row -> do
                            if rowIsWildcardAtPrefix row matCursor
                                then return $ Just row
                                else mapRow row
                        )

            let newRows = groupRows <> wildcardRowsExpanded <> fallbackRowsExpanded

            let newTypeMap = V.imap (\i ty -> (extendRef matCursor i, ty)) fieldTypes
            -- combine the new field types with existing types.
            -- Note: `wildcards` and `fallback` might depend on `matCursor` type which is already present.
            let combinedTypeMap = HashMap.union (HashMap.fromList (V.toList newTypeMap)) matTypeMap

            let newGroup = PMMatrix matCursor newRows combinedTypeMap
            let finalGroup = normalizeVarRefLevel newGroup

            translatePMToDT cps finalGroup

        catMaybes :: [Maybe a] -> [a]
        catMaybes = mapMaybe id
          where
            mapMaybe _ [] = []
            mapMaybe f (Nothing : xs) = mapMaybe f xs
            mapMaybe f (Just x : xs) = x : mapMaybe f xs

    groupsResults <- mapM processGroup groups

    case dispatch of
        DispatchInt -> do
            let cases =
                    IntMap.fromList
                        [(i, dt) | (Syn.ConstPat (Syn.ConstInt i), dt) <- groupsResults]
            return $ DTSwitch matCursor (DTSwitchInt cases fallbackDT)
        DispatchBool -> do
            let trueBranch = lookupBranch (Syn.ConstPat (Syn.ConstBool True)) groupsResults fallbackDT
            let falseBranch = lookupBranch (Syn.ConstPat (Syn.ConstBool False)) groupsResults fallbackDT
            return $ DTSwitch matCursor (DTSwitchBool trueBranch falseBranch)
        DispatchString -> do
            let cases =
                    HashMap.fromList
                        [(hash s, dt) | (Syn.ConstPat (Syn.ConstString s), dt) <- groupsResults]
            return $ DTSwitch matCursor (DTSwitchString cases fallbackDT)
        DispatchFP -> do
            addErrReportMsg "Floating point pattern matching is not supported"
            return DTUnreachable
        DispatchCtor typePath -> do
            variants <- getVariants typePath
            let caseMap = HashMap.fromList [(ctorPathName kind, dt) | (kind, dt) <- groupsResults]

            let dtCases =
                    V.imap
                        ( \i variantName ->
                            let fullPath = extendPath typePath variantName
                             in case HashMap.lookup fullPath caseMap of
                                    Just dt -> dt
                                    Nothing -> specializeDT matCursor i fallbackDT
                        )
                        variants

            return $ DTSwitch matCursor (DTSwitchCtor dtCases)
        DispatchNullable -> do
            let isNull (Syn.CtorPat p _ _ _) =
                    let Path base segs = p
                     in (unIdentifier base == "Null" || unIdentifier base == "Nothing")
                            || ( not (null segs)
                                    && (unIdentifier (last segs) == "Null" || unIdentifier (last segs) == "Nothing")
                               )
                -- Note: checking "Null" or "Nothing" is heuristic.
                isNull _ = False

            let (nulls, nonNulls) = partition (\(k, _) -> isNull k) groupsResults
            let dtNull = case nulls of
                    [] -> specializeDTNullable matCursor True fallbackDT
                    ((_, dt) : _) -> dt
            let dtNonNull = case nonNulls of
                    [] -> specializeDTNullable matCursor False fallbackDT
                    ((_, dt) : _) -> dt

            return $ DTSwitch matCursor (DTSwitchNullable dtNonNull dtNull)
  where
    specializeDT :: PatternVarRef -> Int -> DecisionTree -> DecisionTree
    specializeDT targetRef index dt = case dt of
        DTGuard v s t f ->
            DTGuard v s (specializeDT targetRef index t) (specializeDT targetRef index f)
        DTSwitch sVar cases | sVar == targetRef ->
            case cases of
                DTSwitchCtor vec ->
                    if index >= 0 && index < V.length vec
                        then vec V.! index
                        else DTUncovered -- Should not happen if indices match
                _ -> dt -- Should not happen if types match
        _ -> dt

    specializeDTNullable :: PatternVarRef -> Bool -> DecisionTree -> DecisionTree
    specializeDTNullable targetRef isNull dt = case dt of
        DTGuard v s t f ->
            DTGuard
                v
                s
                (specializeDTNullable targetRef isNull t)
                (specializeDTNullable targetRef isNull f)
        DTSwitch sVar cases | sVar == targetRef ->
            case cases of
                DTSwitchNullable j n -> if isNull then n else j
                _ -> dt
        _ -> dt

    extendRef (PatternVarRef s) i = PatternVarRef (s Seq.|> i)

    lookupBranch key results def =
        case lookup key results of
            Just dt -> dt
            Nothing -> def

    ctorPathName (Syn.CtorPat p _ _ _) = p
    ctorPathName _ = error "Not a ctor pattern"

    extendPath (Path base segs) ident = Path ident (segs ++ [base])

    unIdentifier (Identifier t) = t

    resolveFieldTypes :: Semi.Type -> Path -> SemiEff (V.Vector Semi.Type)
    resolveFieldTypes (Semi.TypeNullable t) _ = return $ V.singleton t -- NonNull has 1 field
    resolveFieldTypes (Semi.TypeRecord _ tyParams _) ctorPath = do
        records <- State.gets knownRecords
        mRecord <- liftIO $ H.lookup records ctorPath
        case mRecord of
            Nothing -> do
                addErrReportMsg $ "Record not found: " <> T.pack (show ctorPath)
                return V.empty
            Just record -> do
                let genIds = map (\(_, GenericID gid) -> fromIntegral gid) (recordTyParams record)
                let substMap = IntMap.fromList (zip genIds tyParams)
                let substitute = flip substituteGenericMap substMap

                mFields <- readIORef' (recordFields record)
                case mFields of
                    Just (Named fields) ->
                        return $ V.map (\(WithSpan (_, ty, _) _ _) -> substitute ty) fields
                    Just (Unnamed fields) ->
                        return $ V.map (\(WithSpan (ty, _) _ _) -> substitute ty) fields
                    _ -> return V.empty
    resolveFieldTypes _ _ = return V.empty

    getVariants :: Path -> SemiEff (V.Vector Identifier)
    getVariants path = do
        records <- State.gets knownRecords
        mRecord <- liftIO $ H.lookup records path
        case mRecord of
            Just record -> do
                mFields <- readIORef' (recordFields record)
                case mFields of
                    Just (Variants vs) -> return $ V.map (\(WithSpan ident _ _) -> ident) vs
                    _ -> return V.empty
            Nothing -> return V.empty

    hash :: T.Text -> XXH3 T.Text
    hash = XXH3
