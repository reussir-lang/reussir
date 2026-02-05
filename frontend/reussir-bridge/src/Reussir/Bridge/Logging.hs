{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Bridge.Logging (
    withReussirLogger,
) where

import Control.Exception (bracket)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Log (LogMessage (..), Logger, mkLogger)
import Log.Internal.Logger (withLogger)
import UnliftIO (MonadUnliftIO, withRunInIO)

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Log qualified

import Reussir.Bridge.Types (LogLevel (..), logLevelToC)

data ReussirLogger

foreign import capi "Reussir/Bridge.h reussir_bridge_create_stdout_logger"
    c_reussir_bridge_create_stdout_logger ::
        CInt -> CString -> IO (Ptr ReussirLogger)

foreign import capi "Reussir/Bridge.h reussir_bridge_destroy_logger"
    c_reussir_bridge_destroy_logger :: Ptr ReussirLogger -> IO ()

foreign import capi "Reussir/Bridge.h reussir_bridge_log_with_level"
    c_reussir_bridge_log_with_level :: Ptr ReussirLogger -> CInt -> CString -> IO ()

{- | Create a logger that forwards messages to the Reussir C bridge logger.
The logger is automatically destroyed when the action finishes.
-}
withReussirLogger ::
    (MonadUnliftIO m) => LogLevel -> String -> (Logger -> m r) -> m r
withReussirLogger level name act = withRunInIO $ \unlift -> do
    withCString name $ \c_name -> do
        bracket
            (c_reussir_bridge_create_stdout_logger (logLevelToC level) c_name)
            c_reussir_bridge_destroy_logger
            ( \c_logger -> do
                withCString ("[haskell] reussir logger active: " <> name) $ \c_msg ->
                    c_reussir_bridge_log_with_level c_logger (logLevelToC LogInfo) c_msg
                logger <- mkLogger "reussir" $ \msg -> do
                    let rLevel = case lmLevel msg of
                            Log.LogTrace -> LogTrace
                            Log.LogInfo -> LogInfo
                            Log.LogAttention -> LogError
                    let textMsg = showLogMessage' msg
                    BS.useAsCString (T.encodeUtf8 textMsg) $ \c_msg ->
                        c_reussir_bridge_log_with_level c_logger (logLevelToC rLevel) c_msg
                withLogger logger $ unlift . act
            )
  where
    showLogMessage' ::
        LogMessage ->
        -- \^ The actual message.
        T.Text
    showLogMessage' msg =
        T.concat $
            [ T.intercalate "/" $ lmComponent msg : lmDomain msg
            , ": "
            , lmMessage msg
            ]
