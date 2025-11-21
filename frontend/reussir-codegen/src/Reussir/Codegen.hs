module Reussir.Codegen (
    Module (..),
    RecordInstance (..),
    PolymorphicFFI (..),
    PolymorphicFFIAttr (..),
    moduleCodegen,
    emitModuleToText,
    emitModuleToBackend,
) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Log qualified as L
import Effectful.Reader.Static qualified as E
import Effectful.State.Static.Local qualified as E
import Reussir.Codegen.Context (Codegen, Context (builder), TargetSpec, addTypeInstance, emitModuleEnv, runCodegenToBackend)
import Reussir.Codegen.Context.Codegen (emptyContext)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.IR (Function, functionCodegen)
import Reussir.Codegen.PolymorphicFFI (PolymorphicFFI (..), PolymorphicFFIAttr (..), polyFFICodegen)
import Reussir.Codegen.Type.Record (Record)

newtype RecordInstance = RecordInstance {unRecordInstance :: (Symbol, Record)}
data Module = Module
    { moduleFunctions :: [Function]
    , moduleSpec :: TargetSpec
    , recordInstances :: [RecordInstance]
    , polymorphicFFIs :: [PolymorphicFFI]
    }

-- | Emit a complete MLIR module with the given body.
moduleCodegen :: Module -> Codegen ()
moduleCodegen m = do
    forM_ (recordInstances m) $ (uncurry addTypeInstance) . unRecordInstance
    emitModuleEnv $ do
        forM_ (polymorphicFFIs m) polyFFICodegen
        forM_ (moduleFunctions m) functionCodegen

-- | Helper function to emit module to a Text
emitModuleToText :: (IOE :> es, L.Log :> es) => Module -> Eff es T.Text
emitModuleToText m = do
    initCtx <- emptyContext
    res <- inject $ E.runReader (moduleSpec m) $ E.execState initCtx $ moduleCodegen m
    pure $ TB.runBuilder (builder res)

-- | Helper function to emit module to backend
emitModuleToBackend :: (IOE :> es, L.Log :> es) => Module -> Eff es ()
emitModuleToBackend m = runCodegenToBackend (moduleSpec m) $ moduleCodegen m
