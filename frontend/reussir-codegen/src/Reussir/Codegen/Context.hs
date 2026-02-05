{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context (
    TargetSpec (..),
    Context (..),
    Codegen,
    emptyContext,
    runCodegen,
    emitModuleEnv,
    emitOutlineLocs,
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
    incIndentation,
    addTypeInstance,
    runCodegenToBackend,
)
where

import Effectful ((:>))

import Effectful qualified as E
import Effectful.Log qualified as L
import Effectful.Reader.Static qualified as E
import Effectful.State.Static.Local qualified as E

import Reussir.Codegen.Context.Codegen (
    Codegen,
    Context (..),
    TargetSpec (..),
    addTypeInstance,
    emptyContext,
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
    emitModuleEnv,
    emitOutlineLocs,
    runCodegenToBackend,
 )

runCodegen ::
    (E.IOE :> es, L.Log :> es) => TargetSpec -> Codegen a -> E.Eff es a
runCodegen spec codegen = do
    initCtx <- emptyContext
    E.inject $ E.runReader spec $ E.evalState initCtx $ codegen
