{-# LANGUAGE RecordWildCards #-}

module Reussir.Core.Full.Context (
    withSpan,
    withGenericMap,
    withFreshLocalContext,
    addError,
    emptyFullContext,
    reportAllErrors,
) where

import Control.Monad (forM_)
import Reussir.Core.Uitls.HashTable qualified as HU
import Data.Int (Int64)
import Data.IntMap qualified as IntMap
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (runState)
import Effectful.State.Static.Local qualified as State
import Reussir.Core.Data.Full.Context (
    FullContext (..),
    FullEff,
    GlobalFullEff,
    LocalFullContext (..),
 )
import Reussir.Core.Data.Full.Error (Error)
import Reussir.Core.Data.Full.Type (GenericMap)
import Reussir.Core.Data.String (StringUniqifier (StringUniqifier))
import Reussir.Core.Full.Error (errorToReport)
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (Repository)
import System.IO (Handle, hPutStrLn)

withSpan :: (Int64, Int64) -> FullEff a -> FullEff a
withSpan span' cont = do
    oldSpan <- State.gets currentSpan
    State.modify $ \s -> s{currentSpan = Just span'}
    result <- cont
    State.modify $ \s -> s{currentSpan = oldSpan}
    return result

withGenericMap :: GenericMap -> FullEff a -> FullEff a
withGenericMap newMap cont = do
    oldMap <- State.gets genericMap
    State.modify $ \s -> s{genericMap = IntMap.union newMap oldMap}
    result <- cont
    State.modify $ \s -> s{genericMap = oldMap}
    return result

withFreshLocalContext :: FullEff a -> GlobalFullEff a
withFreshLocalContext cont = do
    let localCtx =
            LocalFullContext
                { currentSpan = Nothing
                , genericMap = IntMap.empty
                , exprCounter = 0
                }
    (res, _) <- runState localCtx $ inject cont
    return res

addError :: Error -> GlobalFullEff ()
addError err = State.modify $ \st -> st{ctxErrors = err : ctxErrors st}

emptyFullContext :: (IOE :> es, Prim :> es) => FilePath -> Eff es FullContext
emptyFullContext ctxFilePath = do
    ctxFunctions <- HU.new
    ctxRecords <- HU.new
    ctxSemiRecords <- HU.new
    table <- HU.new
    let ctxStringUniqifier = StringUniqifier table
    let ctxErrors = []
    return FullContext{..}

reportAllErrors :: (IOE :> es, Prim :> es) => FullContext -> Repository -> Handle -> Eff es ()
reportAllErrors ctx repo handle = do
    forM_ (ctxErrors ctx) $ \err -> do
        displayReport (errorToReport err (ctxFilePath ctx)) repo 0 handle
        liftIO $ hPutStrLn handle ""
