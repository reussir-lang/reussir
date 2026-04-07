{-# LANGUAGE OverloadedStrings #-}
module Reussir.Core.Semi.Trampoline (resolveFFI) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful.State.Static.Local qualified as State

import Reussir.Core.Data.Semi.Context (SemiContext (..), FFIImportInfo (..), GlobalSemiEff)
import Reussir.Core.Data.Semi.Function (FunctionProto(funcGenerics))
import Reussir.Core.Semi.Context (addErrReportMsg, evalType, resolveFunctionPath, withFreshLocalContext)

import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Reussir.Parser.Types.Stmt (FFIDirection(..), FFIBody(..))
import Reussir.Parser.Types.Type qualified as Syn
import Reussir.Parser.Types.Type qualified as Syn

-- | Resolve a unified FFI declaration (both import and export).
resolveFFI ::
    Identifier ->
    T.Text ->
    FFIDirection ->
    [(Identifier, [Path])] ->
    [(Identifier, Syn.Type)] ->
    Maybe Syn.Type ->
    FFIBody ->
    GlobalSemiEff ()
resolveFFI name abi FFIExport generics _params _retType (FFIAlias target tyArgsSyn) =
    resolveExportTrampoline name abi target tyArgsSyn
resolveFFI name abi FFIExport _generics _params _retType body = do
    withFreshLocalContext $
        addErrReportMsg $ "Export FFI '" <> unIdentifier name <> "' must use alias syntax (= target<args>), got: "
            <> T.pack (show body)
resolveFFI name abi FFIImport generics params retTypeSyn body =
    resolveImportFFI name abi generics params retTypeSyn body

-- | Resolve an export trampoline (backward-compatible with the old syntax).
-- Wraps a Reussir function for C ABI export.
resolveExportTrampoline ::
    Identifier ->
    T.Text ->
    Path ->
    [Syn.Type] ->
    GlobalSemiEff ()
resolveExportTrampoline name abi target tyArgs = withFreshLocalContext $ do
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

-- | Resolve an import FFI declaration.
-- The function prototype was already registered during the scan phase.
-- This just stores the FFI metadata (ABI, template) for later lowering.
resolveImportFFI ::
    Identifier ->
    T.Text ->
    [(Identifier, [Path])] ->
    [(Identifier, Syn.Type)] ->
    Maybe Syn.Type ->
    FFIBody ->
    GlobalSemiEff ()
resolveImportFFI name abi _generics _params _retTypeSyn body = do
    moduleSegs <- State.gets currentModulePath
    let qualifiedPath = Path name moduleSegs
    let template = case body of
            FFITemplate t -> Just t
            _ -> Nothing
    let importInfo = FFIImportInfo
            { ffiImportABI = abi
            , ffiImportFuncPath = qualifiedPath
            , ffiImportTemplate = template
            }
    State.modify $ \ctx ->
        ctx { ffiImports = HashMap.insert name importInfo (ffiImports ctx) }
