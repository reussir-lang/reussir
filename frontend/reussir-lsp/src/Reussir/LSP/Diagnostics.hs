{-# LANGUAGE OverloadedStrings #-}

{- |
  LSP Diagnostics Conversion
  ==========================

  Converts Reussir's internal diagnostic representation (byte-offset based)
  into LSP diagnostics (line/column based). Runs the parser and semi-elaboration
  pipeline to collect diagnostic reports.
-}
module Reussir.LSP.Diagnostics (
    -- * Diagnostic Conversion
    computeDiagnostics,
) where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Text qualified as T
import Effectful (inject, runEff)
import Effectful.Log qualified as L
import Effectful.Prim (runPrim)
import Effectful.State.Static.Local (runState)
import Language.LSP.Protocol.Types qualified as LSP
import Log (LogLevel (..))
import Log.Backend.StandardOutput qualified as L
import Reussir.Bridge qualified as B
import Reussir.Core.Data.Semi.Context (SemiContext (..))
import Reussir.Core.Semi.Context (emptySemiContext, populateRecordFields, scanStmt)
import Reussir.Core.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core.Semi.Tyck (checkFuncType)
import Reussir.Diagnostic.Report (CodeReference (..), Label (..), Report (..), TextWithFormat (..))
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import Text.Megaparsec (errorBundlePretty, runParser)

-- | Compute LSP diagnostics for a file by running the parser and semi-elaboration.
computeDiagnostics ::
    -- | File path (URI-derived)
    FilePath ->
    -- | File content
    T.Text ->
    IO [LSP.Diagnostic]
computeDiagnostics filePath content =
    case runParser parseProg filePath content of
        Left err -> do
            -- Parse error: convert the megaparsec error bundle to a single diagnostic
            let errMsg = T.pack $ errorBundlePretty err
            return
                [ LSP.Diagnostic
                    { LSP._range = LSP.Range (LSP.Position 0 0) (LSP.Position 0 1)
                    , LSP._severity = Just LSP.DiagnosticSeverity_Error
                    , LSP._code = Nothing
                    , LSP._codeDescription = Nothing
                    , LSP._source = Just "reussir"
                    , LSP._message = errMsg
                    , LSP._tags = Nothing
                    , LSP._relatedInformation = Nothing
                    , LSP._data_ = Nothing
                    }
                ]
        Right prog -> do
            -- Run semi-elaboration and collect diagnostic reports
            reports <- runSemiElab filePath prog
            return $ concatMap (reportToDiagnostics content) reports

-- | Run semi-elaboration and return diagnostic reports.
runSemiElab :: FilePath -> [Syn.Stmt] -> IO [Report]
runSemiElab filePath prog = do
    (_, finalState) <- L.withStdOutLogger $ \logger -> do
        runEff $ L.runLog "reussir-lsp" logger LogAttention $ runPrim $ do
            initState <- emptySemiContext B.LogWarning filePath
            runState initState $ do
                -- Scan all statements
                forM_ prog $ \stmt -> inject $ scanStmt stmt

                -- Populate record fields
                forM_ prog $ \stmt -> inject $ populateRecordFields stmt

                -- Elaborate all functions
                forM_ prog $ \stmt -> do
                    case unspanStmt stmt of
                        Syn.FunctionStmt f -> do
                            _ <- inject $ checkFuncType f
                            return ()
                        _ -> return ()

                -- Solve generics
                _ <- inject solveAllGenerics
                return ()
    return $ translationReports finalState
  where
    unspanStmt (Syn.SpannedStmt (WithSpan s _ _)) = unspanStmt s
    unspanStmt s = s

-- | Convert a Reussir diagnostic Report into a list of LSP Diagnostics.
reportToDiagnostics :: T.Text -> Report -> [LSP.Diagnostic]
reportToDiagnostics content report = go LSP.DiagnosticSeverity_Error report
  where
    go _severity (Labeled label inner) =
        go (labelToSeverity label) inner
    go severity (CodeRef codeRef mAnnotation) =
        let range = offsetsToRange content (codeStartOffset codeRef) (codeEndOffset codeRef)
            msg = case mAnnotation of
                Just twf -> textContent twf
                Nothing -> "Error at this location"
         in [ LSP.Diagnostic
                { LSP._range = range
                , LSP._severity = Just severity
                , LSP._code = Nothing
                , LSP._codeDescription = Nothing
                , LSP._source = Just "reussir"
                , LSP._message = msg
                , LSP._tags = Nothing
                , LSP._relatedInformation = Nothing
                , LSP._data_ = Nothing
                }
            ]
    go severity (FormattedText texts) =
        let msg = T.concat $ map textContent texts
         in [ LSP.Diagnostic
                { LSP._range = LSP.Range (LSP.Position 0 0) (LSP.Position 0 1)
                , LSP._severity = Just severity
                , LSP._code = Nothing
                , LSP._codeDescription = Nothing
                , LSP._source = Just "reussir"
                , LSP._message = msg
                , LSP._tags = Nothing
                , LSP._relatedInformation = Nothing
                , LSP._data_ = Nothing
                }
            ]
    go severity (Nested inner) = go severity inner
    go _ ReportNil = []
    go severity (ReportSeq r1 r2) = go severity r1 ++ go severity r2

-- | Convert a diagnostic label to an LSP severity.
labelToSeverity :: Label -> LSP.DiagnosticSeverity
labelToSeverity Error = LSP.DiagnosticSeverity_Error
labelToSeverity Warning = LSP.DiagnosticSeverity_Warning
labelToSeverity Info = LSP.DiagnosticSeverity_Information
labelToSeverity Hint = LSP.DiagnosticSeverity_Hint

-- | Convert byte offsets (Int64) to an LSP Range using the file content.
offsetsToRange :: T.Text -> Int64 -> Int64 -> LSP.Range
offsetsToRange content startOff endOff =
    let startPos = offsetToPosition content (fromIntegral startOff)
        endPos = offsetToPosition content (fromIntegral endOff)
     in LSP.Range startPos endPos

-- | Convert a character offset to an LSP Position (0-indexed line and column).
offsetToPosition :: T.Text -> Int -> LSP.Position
offsetToPosition content offset =
    let prefix = T.take offset content
        linesBefore = T.count "\n" prefix
        lastNewline = case T.breakOnEnd "\n" prefix of
            ("", _) -> 0
            (before, _) -> T.length before
        col = T.length prefix - lastNewline
     in LSP.Position (fromIntegral linesBefore) (fromIntegral col)
