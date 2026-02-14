{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Module where

import Control.Monad (forM_)
import Effectful (liftIO)
import Reussir.Codegen.Context.Symbol (verifiedSymbol)

import Data.HashTable.IO qualified as H
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Global qualified as IR

import Reussir.Core.Data.Lowering.Context (
    GlobalLoweringEff,
    LoweringContext (..),
 )
import Reussir.Core.Lowering.Function (lowerFunction)
import Reussir.Core.Lowering.Record (lowerRecord)
import Reussir.Core.String (getAllStrings, mangleStringToken)
import qualified Data.HashMap.Strict as HashMap
import qualified Reussir.Codegen.Trampoline as IR

lowerModule :: GlobalLoweringEff ()
lowerModule = do
    ctx <- Reader.ask

    -- Lower functions
    functionList <- liftIO $ H.toList (functionInstances ctx)
    forM_ functionList $ \(_, func) -> lowerFunction func

    -- Lower records
    recordList <- liftIO $ H.toList (recordInstances ctx)
    forM_ recordList $ \(_, record) -> lowerRecord record

    -- Lower strings
    strList <- getAllStrings (stringUniqifier ctx)
    forM_ strList $ \(strVal, token) -> do
        let symbol = verifiedSymbol $ mangleStringToken token
        mod' <- State.get
        let global = IR.GlobalString symbol strVal
        let updatedMod = mod'{IR.globals = global : IR.globals mod'}
        State.put updatedMod

    -- Lower trampolines
    forM_ (HashMap.toList (trampolines ctx)) $ \(name, (abi, target)) -> do
        let trampoline' = IR.Trampoline name target abi
        mod' <- State.get
        let updatedMod = mod'{IR.trampolines = trampoline' : IR.trampolines mod'}
        State.put updatedMod
