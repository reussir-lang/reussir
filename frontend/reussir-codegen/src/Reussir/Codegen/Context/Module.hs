{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Module (
    runCodegenToBackend,
    emitModule,
)
where

-- Import Emission instances for Type
import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Effectful as E
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
import Reussir.Codegen.Type.Data (Type (TypeExpr))
import Reussir.Codegen.Type.Emission (emitRecord)
import Reussir.Codegen.Type.Mangle (mangleTypeWithPrefix)

-- | Run a Codegen action with an initial context and compile the result.
runCodegenToBackend :: (E.IOE :> es, L.Log :> es) => TargetSpec -> Codegen () -> Eff es ()
runCodegenToBackend spec codegen = do
    initCtx <- emptyContext
    finalCtx <- E.inject $ E.runReader spec $ E.execState initCtx $ codegen
    let mlirModule = TB.toLazyText (builder finalCtx)
    E.liftIO $
        B.compileForNativeMachine
            (T.unpack mlirModule)
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
    forM_ instances' $ \(expr, _) -> do
        let mangled = mangleTypeWithPrefix (TypeExpr expr)
        let mangled' = TB.toLazyText mangled
        setRecordEmissionState mangled' RecordEmissionPending
    -- Now emit each one
    forM_ instances' $ \(expr, record) -> do
        let mangled = mangleTypeWithPrefix (TypeExpr expr)
        let mangled' = TB.toLazyText mangled
        -- Check if already complete
        status <- getRecordEmissionState mangled'
        case status of
            RecordEmissionComplete -> pure ()
            _ -> do
                -- Emit the record (which will set state to Incomplete when it encounters recursion)
                record' <- emitRecord True (Just mangled') record
                emitBuilder $ "!" <> mangled <> " = " <> record' <> "\n"
                setRecordEmissionState mangled' RecordEmissionComplete

emitOutlineLocs :: Codegen ()
emitOutlineLocs = do
    ctx <- E.get
    locs <- E.liftIO $ H.toList (outlineLocs ctx)
    forM_ locs $ \(l, loc) -> do
        loc' <- emit loc
        emitBuilder $ "#loc" <> TB.fromString (show l) <> " = " <> loc' <> "\n"

-- | Emit a complete MLIR module with the given body.
emitModule :: Codegen () -> Codegen ()
emitModule body = do
    emitTypeAlias
    name <- fmap (TB.fromString . show) $ E.asks programName
    emitBuilder $ "module @" <> name <> " {\n"
    incIndentation body
    emitBuilder "}\n"
    emitOutlineLocs
