{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.PatternMatch where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (groupBy)
import Data.Maybe (isJust)
import Effectful (liftIO)
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.RRBVector qualified as RRB
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful.State.Static.Local qualified as State
import Reussir.Parser.Types.Expr qualified as Syn

import Reussir.Core.Data.Semi.Context (SemiContext (..), SemiEff, knownRecords)
import Reussir.Core.Data.Semi.Expr (PatternVarRef (..))
import Reussir.Core.Data.Semi.Record (Record (..), RecordFields (..))
import Reussir.Core.Semi.Context (addErrReportMsg)
import qualified Reussir.Core.Data.Semi.Type as Semi

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
    { matrixPrefix :: PatternVarRef
    , matrixRows :: RRB.Vector PMRow
    , matrixSemiType :: Semi.Type
    }

data SplitResult
    = SplitResult
    | NoPivot PMMatrix
    | HasPivot PMMatrix PMRow (Maybe PMMatrix)
    | FirstPivot PMRow (Maybe PMMatrix)

data DispatchKind
    = DispatchInt
    | DispatchBool
    | DispatchCtor Path
    | DispatchString
    | DispatchFP
    | DispatchNullable

initializePMMatrix :: V.Vector (Syn.Pattern, Syn.Expr) -> Semi.Type -> PMMatrix
initializePMMatrix patterns semiType = PMMatrix zeroSingleton inner semiType
  where
    zeroSingleton :: PatternVarRef
    zeroSingleton = PatternVarRef $ Seq.singleton 0

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
validateDistinguishable (PMMatrix _ rows semiTy) =
    all rowCompatible rows
  where
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
splitAtFirstWildcard mat@PMMatrix{matrixRows} = case findWildcardRow mat of
    Nothing -> NoPivot mat
    Just 0 -> case RRB.viewl matrixRows of
        Just (pivot, rest) ->
            FirstPivot pivot $
                if null rest
                    then Nothing
                    else Just $ mat { matrixRows = rest }
        Nothing -> error "Internal error: splitAtFirstWildcard pivot lookup failed"
    Just i ->
        let (before, after) = RRB.splitAt i matrixRows
         in case RRB.viewl after of
                Just (pivot, rest) ->
                    let matBefore = mat { matrixRows = before }
                        matAfter =
                            if null rest
                                then Nothing
                                else Just $ mat { matrixRows = rest }
                     in HasPivot matBefore pivot matAfter
                Nothing -> error "Internal error: splitAtFirstWildcard pivot lookup failed"

-- first apply stable sort, then divide the matrix into multiple groups and each
-- group of rows have the same value. Again, this assumes no wildcard in the matrix
divideDistinguishable :: PMMatrix -> [PMMatrix]
divideDistinguishable mat =
    let sortedMat = stableSortDistinguishable mat
        rows = toList (matrixRows sortedMat)
        groups = groupBy sameGroup rows
     in map (\rows' -> mat { matrixRows = RRB.fromList rows' }) groups
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
