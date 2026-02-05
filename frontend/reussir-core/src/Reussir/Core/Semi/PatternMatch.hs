{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.PatternMatch where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import Effectful (liftIO)
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Parser.Types.Lexer (Identifier (..), Path, WithSpan (..))

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful.State.Static.Local qualified as State
import Reussir.Parser.Types.Expr qualified as Syn

import Reussir.Core.Data.Semi.Context (SemiContext (..), SemiEff, knownRecords)
import Reussir.Core.Data.Semi.Expr (PatternVarRef (..))
import Reussir.Core.Data.Semi.Record (Record (..), RecordFields (..))
import Reussir.Core.Semi.Context (addErrReportMsg)

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
    { rowPatterns :: Seq.Seq (PatternVarRef, Syn.PatternKind)
    , rowBindings :: HashMap.HashMap Identifier PatternVarRef
    , rowGuard :: Maybe Syn.Expr
    , rowBody :: Syn.Expr
    }
data PMMatrix = PMMatrix
    { matrixPrefix :: PatternVarRef
    , matrixRows :: V.Vector PMRow
    }
data SplitResult
    = SplitResult
    | NoPivot PMMatrix
    | HasPivot PMMatrix PMRow (Maybe PMMatrix)

initializePMMatrix :: V.Vector (Syn.Pattern, Syn.Expr) -> SemiEff PMMatrix
initializePMMatrix patterns = PMMatrix zeroSingleton <$> inner
  where
    zeroSingleton :: PatternVarRef
    zeroSingleton = PatternVarRef $ Seq.singleton 0

    inner :: SemiEff (V.Vector PMRow)
    inner = V.forM patterns $ \(Syn.Pattern kind guard, expr) -> case kind of
        Syn.WildcardPat -> do
            return $ PMRow mempty mempty guard expr
        Syn.BindPat identifier -> do
            return $ PMRow mempty (HashMap.singleton identifier zeroSingleton) guard expr
        _ -> return $ PMRow (Seq.singleton (zeroSingleton, kind)) mempty guard expr

rowIsWildcard :: PMRow -> Bool
rowIsWildcard row = Seq.null (rowPatterns row)
