{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Module (
    runCodegenToBackend,
    emitOutlineLocs,
    emitModuleEnv,
)
where

-- Import Emission instances for Type

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.String (fromString)
import Effectful as E
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, takeFileName)

import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Effectful.Log qualified as L
import Effectful.Reader.Static qualified as E
import Effectful.State.Static.Local qualified as E
import Reussir.Bridge qualified as B

import Reussir.Codegen.Context.Codegen (
    Codegen,
    Context (..),
    RecordEmissionState (..),
    TargetSpec (..),
    emptyContext,
    getRecordEmissionState,
    incIndentation,
    setRecordEmissionState,
 )
import Reussir.Codegen.Context.Emission (
    Emission (emit),
    emitBuilder,
 )
import Reussir.Codegen.Context.Symbol (symbolBuilder)
import Reussir.Codegen.Type.Emission (emitRecord)

-- | Run a Codegen action with an initial context and compile the result.
runCodegenToBackend ::
    (E.IOE :> es, L.Log :> es) => TargetSpec -> Codegen () -> Eff es ()
runCodegenToBackend spec codegen = do
    initCtx <- emptyContext
    finalCtx <- E.inject $ E.runReader spec $ E.execState initCtx $ codegen
    let mlirModule = TB.runBuilderBS $ builder finalCtx
    E.liftIO $
        B.compileForNativeMachine
            mlirModule
            (T.unpack (programName spec))
            (outputPath spec)
            (outputTarget spec)
            (optimization spec)
            (logLevel spec)
    pure ()

-- | Emit type alias definitions (placeholder for future implementation).
emitTypeAlias :: Codegen ()
emitTypeAlias = do
    instances <- E.gets typeInstances
    instances' <- E.liftIO $ H.toList instances
    -- Set all to pending first
    forM_ instances' $ flip setRecordEmissionState RecordEmissionPending . fst
    -- Now emit each one
    forM_ instances' $ \(sym, record) -> do
        status <- getRecordEmissionState sym
        case status of
            RecordEmissionComplete -> pure ()
            _ -> do
                -- Emit the record (which will set state to Incomplete when it encounters recursion)
                record' <- emitRecord True (Just sym) record
                emitBuilder $ "!" <> symbolBuilder sym <> " = " <> record' <> "\n"
                setRecordEmissionState sym RecordEmissionComplete

emitOutlineLocs :: Codegen ()
emitOutlineLocs = do
    ctx <- E.get
    locs <- E.liftIO $ H.toList (outlineLocs ctx)
    forM_ locs $ \(l, loc) -> do
        loc' <- emit loc
        emitBuilder $ "#loc" <> TB.fromDec l <> " = " <> loc' <> "\n"

-- | Emit a complete MLIR module with the given body.
emitModuleEnv :: Codegen () -> Codegen ()
emitModuleEnv body = do
    emitTypeAlias
    name <- fmap (fromString . show) $ E.asks programName
    filePath <- E.asks moduleFilePath
    (moduleDirectory, moduleBaseName) <- E.liftIO $ do
        result <- try @SomeException $ canonicalizePath filePath
        case result of
            Left _ -> pure (mempty, mempty)
            Right path -> pure (T.pack $ takeDirectory path, T.pack $ takeFileName path)
    let attributes =
            " attributes { reussir.dbg.file_basename = "
                <> TB.fromText (T.show moduleBaseName)
                <> ", reussir.dbg.file_directory = "
                <> TB.fromText (T.show moduleDirectory)
                <> "}"
    emitBuilder $ "module @" <> name <> attributes <> " {\n"
    incIndentation body
    emitBuilder "}\n"
    emitOutlineLocs
