{-# LANGUAGE OverloadedStrings #-}

{- |
  LSP Hover
  =========

  Provides hover information for Reussir source files.
  Uses the semi-elaboration context to look up type information
  for functions and records at a given cursor position.
-}
module Reussir.LSP.Hover (
    computeHover,
) where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful (Eff, IOE, (:>), liftIO)
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (readIORef')
import Language.LSP.Protocol.Types qualified as LSP
import Reussir.Core.Data.Semi.Context (SemiContext (..))
import Reussir.Core.Data.Semi.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core.Data.Semi.Record qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.LSP.Diagnostics (offsetsToRange)
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))

-- | A hover candidate: a span and the markdown text to show.
data HoverCandidate = HoverCandidate
    { hcStart :: !Int64
    , hcEnd :: !Int64
    , hcText :: !T.Text
    }

-- | Compute hover information at the given position.
computeHover :: (IOE :> es, Prim :> es) => T.Text -> LSP.Position -> SemiContext -> Eff es (Maybe LSP.Hover)
computeHover content pos ctx = do
    let offset = positionToOffset content pos
    candidates <- collectCandidates ctx
    -- Find the smallest enclosing span
    let matching =
            filter (\hc -> hcStart hc <= offset && offset < hcEnd hc) candidates
        sorted = sortOn (\hc -> hcEnd hc - hcStart hc) matching
    case sorted of
        [] -> return Nothing
        (best : _) ->
            let range = offsetsToRange content (hcStart best) (hcEnd best)
                hover =
                    LSP.Hover
                        (LSP.InL $ LSP.mkMarkdown (hcText best))
                        (Just range)
             in return $ Just hover

-- | Collect all hover candidates from the semi-elaboration context.
collectCandidates :: (IOE :> es, Prim :> es) => SemiContext -> Eff es [HoverCandidate]
collectCandidates ctx = do
    funcCandidates <- collectFunctionCandidates (functions ctx)
    recordCandidates <- collectRecordCandidates (knownRecords ctx)
    return (funcCandidates ++ recordCandidates)

-- | Collect hover candidates from the function table.
collectFunctionCandidates :: (IOE :> es) => FunctionTable -> Eff es [HoverCandidate]
collectFunctionCandidates (FunctionTable table) = do
    entries <- liftIO $ H.toList table
    return $ concatMap mkFuncCandidate entries
  where
    mkFuncCandidate (_, proto) =
        case funcSpan proto of
            Just (start, end) ->
                [ HoverCandidate
                    start
                    end
                    (formatFunctionSignature proto)
                ]
            Nothing -> []

-- | Collect hover candidates from the record table.
collectRecordCandidates :: (IOE :> es, Prim :> es) => H.CuckooHashTable Path Semi.Record -> Eff es [HoverCandidate]
collectRecordCandidates table = do
    entries <- liftIO $ H.toList table
    concat <$> mapM mkRecordCandidate entries
  where
    mkRecordCandidate (_, record) =
        case Semi.recordSpan record of
            Just (start, end) -> do
                txt <- formatRecordSummary record
                return [HoverCandidate start end txt]
            Nothing -> return []

-- | Format a function signature for hover display.
formatFunctionSignature :: FunctionProto -> T.Text
formatFunctionSignature proto =
    let name = unIdentifier (funcName proto)
        generics =
            if null (funcGenerics proto)
                then ""
                else "<" <> T.intercalate ", " (map (unIdentifier . fst) (funcGenerics proto)) <> ">"
        params = T.intercalate ", " $ map formatParam (funcParams proto)
        ret = showType (funcReturnType proto)
        regional = if funcIsRegional proto then "regional " else ""
     in "```reussir\n"
            <> regional
            <> "fn "
            <> name
            <> generics
            <> "("
            <> params
            <> ") -> "
            <> ret
            <> "\n```"
  where
    formatParam (ident, ty) =
        unIdentifier ident <> ": " <> showType ty



-- | Format a record summary for hover display.
formatRecordSummary :: (Prim :> es) => Semi.Record -> Eff es T.Text
formatRecordSummary record = do
    let name = showPath (Semi.recordName record)
        kind = case Semi.recordKind record of
            Semi.StructKind -> "struct"
            Semi.EnumKind -> "enum"
            Semi.EnumVariant{} -> "variant"
        generics =
            if null (Semi.recordTyParams record)
                then ""
                else "<" <> T.intercalate ", " (map (unIdentifier . fst) (Semi.recordTyParams record)) <> ">"
    mFields <- readIORef' (Semi.recordFields record)
    let fieldsText = case mFields of
            Nothing -> ""
            Just (Semi.Named fields) ->
                " { "
                    <> T.intercalate ", " (map (\(WithSpan (ident, ty, _) _ _) -> unIdentifier ident <> ": " <> showType ty) (V.toList fields))
                    <> " }"
            Just (Semi.Unnamed fields) ->
                "("
                    <> T.intercalate ", " (map (\(WithSpan (ty, _) _ _) -> showType ty) (V.toList fields))
                    <> ")"
            Just (Semi.Variants variants) ->
                " { "
                    <> T.intercalate ", " (map (\(WithSpan ident _ _) -> unIdentifier ident) (V.toList variants))
                    <> " }"
    return $
        "```reussir\n"
            <> kind
            <> " "
            <> name
            <> generics
            <> fieldsText
            <> "\n```"

-- | Show a semi-elaborated type as text.
showType :: Semi.Type -> T.Text
showType = T.pack . show

-- | Show a path as text (without the `$` prefix from Show).
showPath :: Path -> T.Text
showPath (Path base segs) =
    T.intercalate "::" (map unIdentifier (segs ++ [base]))

-- | Convert an LSP Position to a byte offset.
positionToOffset :: T.Text -> LSP.Position -> Int64
positionToOffset content (LSP.Position line col) =
    let allLines = T.splitOn "\n" content
        linesBefore = take (fromIntegral line) allLines
        lineOffset = sum (map (\l -> fromIntegral (T.length l) + 1) linesBefore)
     in lineOffset + fromIntegral col
