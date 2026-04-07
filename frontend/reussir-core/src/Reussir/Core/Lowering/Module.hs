{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Module where

import Control.Monad (forM_)
import Effectful (liftIO)
import Reussir.Codegen.Context.Symbol (verifiedSymbol, symbolText)

import Data.HashTable.IO qualified as H
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Global qualified as IR
import Reussir.Codegen.Type qualified as IR

import Reussir.Core.Data.Full.Context (FullFFIImport(..))
import Reussir.Core.Data.Full.Function qualified as Full
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Lowering.Context (
    GlobalLoweringEff,
    LoweringContext (..),
 )
import Reussir.Core.Lowering.Function (lowerFunction)
import Reussir.Core.Lowering.Record (lowerRecord)
import Reussir.Core.Lowering.Type (convertType)
import Reussir.Core.String (getAllStrings, mangleStringToken)
import qualified Data.HashMap.Strict as HashMap
import Reussir.Codegen.Trampoline qualified as IR
import qualified Data.Text as T

-- | Convert ${key} template syntax to [:key:] syntax for the MLIR polyffi backend.
convertTemplateSyntax :: T.Text -> T.Text
convertTemplateSyntax = go
  where
    go t = case T.breakOn "${" t of
        (before, rest)
            | T.null rest -> before
            | otherwise ->
                let afterDollar = T.drop 2 rest  -- drop "${"
                in case T.breakOn "}" afterDollar of
                    (key, rest')
                        | T.null rest' -> before <> rest  -- malformed, keep as-is
                        | otherwise ->
                            before <> "[:" <> key <> ":]" <> go (T.drop 1 rest')

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

    -- Lower export trampolines
    forM_ (HashMap.toList (trampolines ctx)) $ \(name, (abi, target)) -> do
        let trampoline' = IR.Trampoline name target abi
        mod' <- State.get
        let updatedMod = mod'{IR.trampolines = trampoline' : IR.trampolines mod'}
        State.put updatedMod

    -- Lower import FFI template instantiations.
    -- For each function instance that corresponds to an FFI import with a template,
    -- emit a reussir.polyffi op with concrete type substitutions.
    let ffiImportsByPath = HashMap.fromList
            [ (fullFFIImportFuncPath info, info)
            | (_, info) <- HashMap.toList (ffiImports ctx)
            ]
    forM_ functionList $ \(_, func) -> do
        case HashMap.lookup (Full.funcRawPath func) ffiImportsByPath of
            Just info | Just template <- fullFFIImportTemplate info -> do
                let converted = convertTemplateSyntax template
                let genericNames = fullFFIImportGenericNames info
                let tyArgs = Full.funcInstantiatedTyArgs func
                -- Convert Full types to IR types for MLIR substitution
                convertedTyArgs <- mapM convertType tyArgs
                let attrs = zipWith (\gn ty -> IR.PolyFFITypeParam gn ty) genericNames convertedTyArgs
                let polyffi = IR.PolymorphicFFI converted attrs
                mod' <- State.get
                let updatedMod = mod'{IR.polymorphicFFIs = polyffi : IR.polymorphicFFIs mod'}
                State.put updatedMod
            _ -> pure ()

    -- Lower extern struct destructor polyffi ops.
    -- For each extern struct instantiation, emit a polyffi that generates a drop
    -- function. We get the concrete Rust type name from convertType.
    forM_ recordList $ \(_, record) -> do
        case Full.recordKind record of
            Full.ExternStructKind -> do
                irType <- convertType (Full.TypeRecord (Full.recordName record))
                case irType of
                    IR.TypeFFIObject ffiName dtorSym -> do
                        let dtorTemplate =
                                "#![feature(linkage)]\nextern crate reussir_rt;\n" <>
                                "use " <> ffiName <> ";\n" <>
                                "#[linkage = \"weak_odr\"]\n" <>
                                "#[unsafe(no_mangle)]\n" <>
                                "pub unsafe extern \"C\" fn " <> symbolText dtorSym <>
                                "(_: " <> ffiName <> ") {}\n"
                        let polyffi = IR.PolymorphicFFI dtorTemplate []
                        mod' <- State.get
                        let updatedMod = mod'{IR.polymorphicFFIs = polyffi : IR.polymorphicFFIs mod'}
                        State.put updatedMod
                    _ -> pure ()
            _ -> pure ()
