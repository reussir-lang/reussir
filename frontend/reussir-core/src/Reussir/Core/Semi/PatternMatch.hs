{-# LANGUAGE OverloadedStrings #-}
module Reussir.Core.Semi.PatternMatch where

import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path, Identifier(..), WithSpan(..))
import qualified Data.Vector.Strict as V
import Reussir.Core.Data.Semi.Context (SemiEff, SemiContext(..), knownRecords)
import Reussir.Core.Data.Semi.Record (Record(..), RecordFields(..))
import Reussir.Core.Semi.Context (addErrReportMsg)
import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import Effectful (liftIO)
import Effectful.State.Static.Local qualified as State
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Core.Data.Semi.Expr (DecisionTree, Expr)

-- normalize a ctor pattern into a positional applied form.
-- fill in wildcards for ignored fields if ellipsis is present.
-- return Nothing if the normalization fail
normalizeCtorPattern 
    :: Path 
    -> V.Vector Syn.PatternCtorArg 
    -> Bool 
    -> SemiEff (Maybe (V.Vector Syn.PatternKind))
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
                    addErrReportMsg $ "Internal error: Variants in normalizeCtorPattern for " <> T.pack (show recordPath)
                    return Nothing
  where
    getName :: Syn.PatternCtorArg -> Maybe Identifier
    getName arg = 
        Syn.patCtorArgField arg <|> case Syn.patCtorArgKind arg of
            Syn.BindPat n -> Just n
            _ -> Nothing

    normalizeNamed fields = do
        let fieldMap = HashMap.fromList $ 
                V.toList $ V.map (\arg -> (getName arg, arg)) args
        
        -- Check for positional args (Nothing name)
        if isJust (HashMap.lookup Nothing fieldMap)
            then do
                addErrReportMsg "Positional arguments are not allowed in named record patterns"
                return Nothing
            else do
                let namedArgs = HashMap.fromList $ 
                        [ (n, Syn.patCtorArgKind arg) 
                        | arg <- V.toList args
                        , Just n <- [getName arg] 
                        ]
                
                -- Check for extra fields
                let unknownFields = diff (HashMap.keys namedArgs) [ n | WithSpan (n, _, _) _ _ <- V.toList fields ]
                
                if not (null unknownFields) 
                    then do
                         addErrReportMsg $ "Unknown fields in pattern: " <> T.intercalate ", " [unIdentifier n | n <- unknownFields]
                         return Nothing
                    else do
                        -- Build result vector
                        let result = V.generate (V.length fields) $ \i ->
                                let WithSpan (name, _, _) _ _ = fields V.! i
                                in case HashMap.lookup name namedArgs of
                                    Just pat -> pat
                                    Nothing -> if hasEllipsis then Syn.WildcardPat else Syn.WildcardPat -- Placeholder
                        
                        -- Validation check
                        let missingFields = [ n | WithSpan (n, _, _) _ _ <- V.toList fields
                                                  , not (HashMap.member n namedArgs) ]
                        
                        if not (null missingFields) && not hasEllipsis
                            then do
                                 addErrReportMsg $ "Missing fields: " <> T.intercalate ", " [unIdentifier n | n <- missingFields]
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
                            addErrReportMsg $ "Too many arguments: expected at most " <> T.pack (show fieldLen) <> ", got " <> T.pack (show argLen)
                            return Nothing
                        else do
                            -- Fill rest with Wildcard
                            let provided = V.map Syn.patCtorArgKind args
                            let extras = V.replicate (fieldLen - argLen) Syn.WildcardPat
                            return $ Just (provided V.++ extras)
                else do
                    if argLen /= fieldLen
                        then do
                            addErrReportMsg $ "Argument count mismatch: expected " <> T.pack (show fieldLen) <> ", got " <> T.pack (show argLen)
                            return Nothing
                        else return $ Just (V.map Syn.patCtorArgKind args)

    diff l1 l2 = filter (`notElem` l2) l1

-- elaborate a pattern into a decision within the semi-elaborated expression space.
-- return Nothing if the elaboration fail
-- At this stage, we do not perform exclusivity test yet. (TODO)
-- Consider a set of pattern
--  Foo::A(...)
--  Foo::B(..) if ...
--  Foo::A(...)
--  _ if ...
--  Foo::B(..)
--  Foo::C(..)
--  _
-- We need to first find a bind/wildcard pattern.
-- That pattern divides the patterns into two groups.
-- In the first group, we can do a case switch to form a decision, and recurse into inner patterns.
-- - The decision then fallback to an if-branch if the wildcard/bind pattern has a guard.
--   - We first evaluate the condition with additional bindings.
--   - On true branch, we just continue with the given body.
--   - On false branch, we continue with the remaining patterns with another switch.
-- - Otherwise, latter patterns are just filtered out.
--
-- Now we consider inner patterns.
-- Notice that due to backend implementation, whenever we recurse into inner patterns,
-- we will have all subfields reference. These bump the so-called de Bruijn levels;
-- but may not be actually used (e.g. loaded).
--   Foo::A(Bar::X, Baz::Y)
--   Foo::A(bind0, Baz::Z)
--   Foo::A(..)
-- If we have multiple patterns, we need to do them in positional order, this may
-- introduce more precondition to match during the process.
data PMStateCase = PMStateCase {
    pmscKind :: Syn.PatternKind,
    pmscRemaining :: [Syn.PatternKind],
    pmscGuard :: Maybe Expr,
    pmscBody :: Expr
}
data PMState = PMState {
    pmDeBruijnLevel :: Int,
    pmCases :: [PMStateCase]
}
patternToDecisionTree :: [(Syn.Pattern, Expr)] -> SemiEff (Maybe (DecisionTree Expr))
patternToDecisionTree = undefined
