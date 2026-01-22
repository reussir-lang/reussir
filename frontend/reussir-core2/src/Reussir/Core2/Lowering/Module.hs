{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Lowering.Module where

import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Effectful (liftIO)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Global qualified as IR
import Reussir.Core2.Data.Lowering.Context (GlobalLoweringEff, LoweringContext (..))
import Reussir.Core2.Lowering.Function (lowerFunction)
import Reussir.Core2.Lowering.Record (lowerRecord)
import Reussir.Core2.String (getAllStrings, mangleStringToken)

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
