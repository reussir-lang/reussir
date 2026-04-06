{-# LANGUAGE OverloadedStrings #-}
module Reussir.Core.Semi.Trampoline where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful.State.Static.Local qualified as State

import Reussir.Core.Data.Semi.Context (SemiContext (..), GlobalSemiEff)
import Reussir.Core.Data.Semi.Function (FunctionProto(funcGenerics))
import Reussir.Core.Semi.Context (addErrReportMsg, evalType, resolveFunctionPath, withFreshLocalContext)

import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Type qualified as Syn

resolveTrampoline ::
    Identifier ->
    T.Text ->
    Path ->
    [Syn.Type] ->
    GlobalSemiEff ()
resolveTrampoline name abi target tyArgs = withFreshLocalContext $ do
    -- 1. Evaluate type arguments

    tyArgs' <- mapM evalType tyArgs

    -- 2. Lookup target function via module-aware resolution
    resolveFunctionPath target >>= \case
        Nothing ->
             addErrReportMsg $ "Trampoline target function not found: " <> T.pack (show target)
        Just (resolvedTarget, proto) -> do
            let generics = funcGenerics proto

            -- 3. Check type argument count
            if length generics /= length tyArgs'
                then
                    addErrReportMsg $
                        "Trampoline type argument count mismatch. Expected "
                            <> T.pack (show (length generics))
                            <> ", got "
                            <> T.pack (show (length tyArgs'))
                else do
                    -- 4. Register trampoline with the resolved target path
                    State.modify $ \ctx ->
                        ctx { trampolines = HashMap.insert name (resolvedTarget, abi, tyArgs') (trampolines ctx) }
