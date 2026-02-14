{-# LANGUAGE OverloadedStrings #-}
module Reussir.Core.Semi.Trampoline where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful.State.Static.Local qualified as State

import Reussir.Core.Data.Semi.Context (SemiContext (..), GlobalSemiEff)
import Reussir.Core.Data.Semi.Function (FunctionProto (..))
import Reussir.Core.Semi.Context (addErrReportMsg, evalType, withFreshLocalContext)
import Reussir.Core.Semi.Function (getFunctionProto)

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

    -- 2. Lookup target function
    funcTable <- State.gets functions
    getFunctionProto target funcTable >>= \case
        Nothing ->
             addErrReportMsg $ "Trampoline target function not found: " <> T.pack (show target)
        Just proto -> do
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
                    -- 4. Register trampoline
                    State.modify $ \ctx ->
                        ctx { trampolines = HashMap.insert name (target, abi, tyArgs') (trampolines ctx) }
