{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  LSP Handlers
  ============

  Defines the request and notification handlers for the Reussir Language Server.

  Currently supports:
    - textDocument/didOpen — parse + elaborate, publish diagnostics
    - textDocument/didSave — re-run and publish diagnostics
    - textDocument/didClose — clear diagnostics
    - textDocument/hover — type information on hover
    - textDocument/semanticTokens/full — semantic token highlighting
-}
module Reussir.LSP.Handlers (
    handlers,
) where

import Control.Monad.IO.Class (liftIO)
import Effectful (Eff, IOE, runEff)
import Effectful.Log qualified as L
import Effectful.Prim (Prim, runPrim)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

import Reussir.LSP.Diagnostics (computeDiagnostics, elaborateFile)
import Reussir.LSP.Hover (computeHover)
import Reussir.LSP.SemanticTokens (computeSemanticTokens)
import Control.Exception (bracket)
import Data.Text.IO qualified as TIO
import Log qualified as Log

-- | All LSP handlers for the Reussir language server.
handlers :: Handlers (LspM ())
handlers =
    mconcat
        [ -- Keep these explicit no-op handlers to avoid noisy "no handler" protocol errors.
          notificationHandler SMethod_Initialized $ \_msg -> pure ()
        , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_msg -> pure ()
        , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem uri _ _ content)) = msg
                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
            diags <- runLspEff $ computeDiagnostics filePath content
            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
        , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidSaveTextDocumentParams (LSP.TextDocumentIdentifier uri) mText) = msg
            case mText of
                Just content -> do
                    let filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
                    diags <- runLspEff $ computeDiagnostics filePath content
                    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
                Nothing -> do
                    -- Fall back to VFS
                    mDoc <- getVirtualFile (LSP.toNormalizedUri uri)
                    case mDoc of
                        Nothing -> return ()
                        Just vf -> do
                            let content = virtualFileText vf
                                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
                            diags <- runLspEff $ computeDiagnostics filePath content
                            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
        , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidCloseTextDocumentParams (LSP.TextDocumentIdentifier uri)) = msg
            -- Clear diagnostics when the file is closed
            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing mempty
        , requestHandler SMethod_TextDocumentHover $ \_req responder -> do
            let TRequestMessage _ _ _ (LSP.HoverParams (LSP.TextDocumentIdentifier uri) pos _workDone) = _req
                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
            mDoc <- getVirtualFile (LSP.toNormalizedUri uri)
            case mDoc of
                Nothing -> responder (Right $ LSP.InR LSP.Null)
                Just vf -> do
                    let content = virtualFileText vf
                    result <- runLspEff $ elaborateFile filePath content
                    case result of
                        Left _ -> responder (Right $ LSP.InR LSP.Null)
                        Right (ctx, _, _) -> do
                            mHover <- runLspEff $ computeHover content pos ctx
                            case mHover of
                                Nothing -> responder (Right $ LSP.InR LSP.Null)
                                Just hover -> responder (Right $ LSP.InL hover)
        , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_req responder -> do
            let TRequestMessage _ _ _ (LSP.SemanticTokensParams _ _ (LSP.TextDocumentIdentifier uri)) = _req
                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
            mDoc <- getVirtualFile (LSP.toNormalizedUri uri)
            case mDoc of
                Nothing -> responder (Right $ LSP.InR LSP.Null)
                Just vf -> do
                    let content = virtualFileText vf
                    result <- runLspEff $ elaborateFile filePath content
                    case result of
                        Left _ -> responder (Right $ LSP.InR LSP.Null)
                        Right (_, prog, _) ->
                            case computeSemanticTokens content prog of
                                Nothing -> responder (Right $ LSP.InR LSP.Null)
                                Just tokens -> responder (Right $ LSP.InL tokens)
        ]

runLspEff :: Eff [Prim, L.Log, IOE] a -> LspM () a
runLspEff m = liftIO $ withFileLogger "/tmp/reussir-lsp.log" $ \logger ->
    runEff $ L.runLog "reussir-lsp" logger Log.LogAttention $ runPrim m

withFileLogger :: FilePath -> (Log.Logger -> IO a) -> IO a
withFileLogger path f = bracket
    (Log.mkLogger "file" $ \msg -> do
        let txt = Log.showLogMessage Nothing msg
        TIO.appendFile path (txt <> "\n"))
    Log.shutdownLogger
    f
