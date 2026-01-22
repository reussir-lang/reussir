{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Reussir.Core2.Full.Function where

import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Effectful (inject, liftIO)
import Effectful.Prim.IORef.Strict (readIORef')
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Core2.Data.Full.Context (FullContext (..), GlobalFullEff)
import Reussir.Core2.Data.Full.Function (Function (..))
import Reussir.Core2.Data.Full.Type (GenericMap, Type (..))
import Reussir.Core2.Data.Generic (GenericSolution)
import Reussir.Core2.Data.Semi.Function qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID (..))
import Reussir.Core2.Full.Context (addError, withFreshLocalContext, withGenericMap, withSpan)
import Reussir.Core2.Full.Expr (convertSemiExpr)
import Reussir.Core2.Full.Type (convertSemiType)
import Reussir.Core2.Semi.Mangle (mangleABIName)
import Reussir.Parser.Types.Lexer (Path (..))
import Prelude hiding (span)

convertSemiFunction ::
    Semi.FunctionProto ->
    GenericMap ->
    [Semi.Type] ->
    GlobalFullEff (Maybe Function)
convertSemiFunction proto genericMap typeArgs = do
    let span = fromMaybe (0, 0) (Semi.funcSpan proto)
    let funcPath = Path (Semi.funcName proto) [] -- TODO: Module handling
    let mangledName = mangleABIName (Semi.TypeRecord funcPath typeArgs Semi.Irrelevant) -- Abuse Record mangle for now as it contains Path and Args
    let symbol = verifiedSymbol mangledName

    semiRecords <- State.gets ctxSemiRecords

    withFreshLocalContext $ withGenericMap genericMap $ withSpan span $ do
        -- Convert Parameters
        paramsResult <- for (Semi.funcParams proto) \(name, ty) -> do
            tyResult <- convertSemiType span genericMap semiRecords ty
            case tyResult of
                Left errs -> do
                    mapM_ (inject . addError) errs
                    pure Nothing
                Right t -> pure $ Just (name, t)

        -- Convert Instantiated Type Args
        instantiatedTyArgs <- for typeArgs \ty -> do
            tyResult <- convertSemiType span genericMap semiRecords ty
            case tyResult of
                Left errs -> do
                    mapM_ (inject . addError) errs
                    pure TypeBottom
                Right t -> pure t

        -- Convert Return Type
        retTypeResult <- convertSemiType span genericMap semiRecords (Semi.funcReturnType proto)
        retType <- case retTypeResult of
            Left errs -> do
                mapM_ (inject . addError) errs
                pure TypeBottom
            Right t -> pure t

        let params = sequence paramsResult

        -- Convert Body
        bodyRef <- readIORef' (Semi.funcBody proto)
        body <- case bodyRef of
            Nothing -> pure Nothing
            Just expr -> Just <$> convertSemiExpr expr

        case params of
            Nothing -> pure Nothing
            Just ps ->
                pure $
                    Just
                        Function
                            { funcVisibility = Semi.funcVisibility proto
                            , funcName = symbol
                            , funcRawPath = funcPath
                            , funcInstantiatedTyArgs = instantiatedTyArgs
                            , funcParams = ps
                            , funcReturnType = retType
                            , funcIsRegional = Semi.funcIsRegional proto
                            , funcBody = body
                            , funcSpan = Semi.funcSpan proto
                            }

convertAllSemiFunctions ::
    [Semi.FunctionProto] ->
    GenericSolution ->
    GlobalFullEff ()
convertAllSemiFunctions protos genericSolution = do
    fullTable <- State.gets ctxFunctions

    forM_ protos \proto -> do
        let tyParams = Semi.funcGenerics proto

        argCombinations <-
            if null tyParams
                then pure [[]]
                else do
                    paramSolutions <- for tyParams \(_, GenericID gid) -> do
                        solutions <- liftIO $ H.lookup genericSolution (GenericID gid)
                        pure $ fromMaybe [] solutions
                    pure $ sequence paramSolutions
        -- when no generic solution is given, the function is naturally not instantiated
        forM_ argCombinations \args -> do
            let genericMap =
                    IntMap.fromList $
                        zipWith (\tyArg (_, GenericID gid) -> (fromIntegral gid, tyArg)) args tyParams
            res <- convertSemiFunction proto genericMap args
            case res of
                Just func -> liftIO $ H.insert fullTable (funcName func) func
                Nothing -> pure ()
