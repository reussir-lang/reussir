{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context (
    TargetSpec (..),
    Context (..),
    CodegenT (..),
    Codegen,
    genState,
    emptyContext,
    runCodegen,
    emitModule,
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
    incIndentation,
    Path (..),
    pathSingleton,
    pathList,
    logDebug,
    logInfo,
    logWarning,
    logError,
    addTypeInstance,
)
where

import Control.Monad.State.Strict qualified as S
import Reussir.Codegen.Context.Codegen (
    Codegen,
    CodegenT (..),
    Context (..),
    TargetSpec (..),
    addTypeInstance,
    emptyContext,
    genState,
    incIndentation,
 )
import Reussir.Codegen.Context.Emission (
    Emission (emit),
    emitBuilder,
    emitCG,
    emitIndentation,
    emitLine,
    emitSpace,
 )
import Reussir.Codegen.Context.Module (
    emitModule,
    runCodegen,
 )
import Reussir.Codegen.Context.Path (Path (..), pathList, pathSingleton)
import System.Log (Priority (..))
import System.Log.Logger (getLogger, logL)

logWithPriority :: Priority -> String -> Codegen ()
logWithPriority priority msg = do
    logger <- S.liftIO $ getLogger "Reussir.Codegen"
    S.liftIO $ logL logger priority msg

logDebug :: String -> Codegen ()
logDebug msg = logWithPriority DEBUG msg

logInfo :: String -> Codegen ()
logInfo msg = logWithPriority INFO msg

logWarning :: String -> Codegen ()
logWarning msg = logWithPriority WARNING msg

logError :: String -> Codegen ()
logError msg = logWithPriority ERROR msg
