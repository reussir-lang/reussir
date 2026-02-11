{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server

import Reussir.LSP.Handlers (handlers)

main :: IO Int
main =
    runServer $
        ServerDefinition
            { parseConfig = const $ const $ Right ()
            , onConfigChange = const $ pure ()
            , defaultConfig = ()
            , configSection = "reussir"
            , doInitialize = \env _req -> pure $ Right env
            , staticHandlers = \_caps -> handlers
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options =
                defaultOptions
                    { optTextDocumentSync = Just syncOptions
                    }
            }
  where
    syncOptions =
        LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True
            , LSP._change = Just LSP.TextDocumentSyncKind_Full
            , LSP._willSave = Just False
            , LSP._willSaveWaitUntil = Just False
            , LSP._save = Just (LSP.InR (LSP.SaveOptions (Just True)))
            }
