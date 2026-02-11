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
    - textDocument/hover — placeholder hover response
-}
module Reussir.LSP.Handlers (
    handlers,
) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

import Reussir.LSP.Diagnostics (computeDiagnostics)

-- | All LSP handlers for the Reussir language server.
handlers :: Handlers (LspM ())
handlers =
    mconcat
        [ notificationHandler SMethod_Initialized $ \_not -> do
            liftIO $ putStrLn "[reussir-lsp] Initialized"
        , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem uri _ _ content)) = msg
                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
            diags <- liftIO $ computeDiagnostics filePath content
            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
        , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidSaveTextDocumentParams (LSP.TextDocumentIdentifier uri) mText) = msg
            case mText of
                Just content -> do
                    let filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
                    diags <- liftIO $ computeDiagnostics filePath content
                    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
                Nothing -> do
                    -- Fall back to VFS
                    mDoc <- getVirtualFile (LSP.toNormalizedUri uri)
                    case mDoc of
                        Nothing -> return ()
                        Just vf -> do
                            let content = virtualFileText vf
                                filePath = maybe "<unknown>" id (LSP.uriToFilePath uri)
                            diags <- liftIO $ computeDiagnostics filePath content
                            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diags)
        , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
            let TNotificationMessage _ _ (LSP.DidCloseTextDocumentParams (LSP.TextDocumentIdentifier uri)) = msg
            -- Clear diagnostics when the file is closed
            publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing mempty
        , requestHandler SMethod_TextDocumentHover $ \_req responder -> do
            let TRequestMessage _ _ _ (LSP.HoverParams _doc pos _workDone) = _req
                rsp = LSP.Hover (LSP.InL ms) (Just range)
                ms = LSP.mkMarkdown "**Reussir Language Server**\n\nHover information coming soon."
                range = LSP.Range pos pos
            responder (Right $ LSP.InL rsp)
        ]
