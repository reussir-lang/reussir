{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Function where

import Control.Monad (forM)
import Data.Maybe (catMaybes, maybeToList)
import Effectful (inject, liftIO)
import Reussir.Parser.Types.Lexer (Identifier (..))
import System.Info (os)

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Effectful.Log qualified as L
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Location qualified as DBG
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type qualified as IR

import Reussir.Core.Data.Lowering.Context (
    GlobalLoweringEff,
    LoweringContext (..),
    LocalLoweringContext (..),
 )
import Reussir.Core.Lowering.Context (
    addIRInstr,
    lookupLocation,
    nextValue,
    withFreshLocalContext,
    withLocationSpan,
 )
import Reussir.Core.Lowering.Debug (typeAsDbgType, unmangledPath)
import Reussir.Core.Lowering.Expr (lowerExprAsBlock)
import Reussir.Core.Lowering.Type (convertType)
import Reussir.Core.Ownership (analyzeFunction)

import Reussir.Core.Data.Full.Expr qualified as Full
import Reussir.Core.Data.Full.Function qualified as Full

lowerFunction :: Full.Function -> GlobalLoweringEff ()
lowerFunction func = do
    -- Run ownership analysis before lowering
    recordTable <- Reader.asks recordInstances
    annotations <- liftIO $ analyzeFunction recordTable func
    Reader.local (\ctx -> ctx{ownershipAnnotations = annotations}) $ lowerFunctionInner func

lowerFunctionInner :: Full.Function -> GlobalLoweringEff ()
lowerFunctionInner func = do
    let path = Full.funcRawPath func
    let tyArgs = Full.funcInstantiatedTyArgs func
    let symbol = Full.funcName func
    let mBody = Full.funcBody func
    let span' = Full.funcSpan func

    let preferredLinkage =
            if os == "mingw32" || os == "darwin"
                then IR.LnkExternal
                else IR.LnkWeakODR

    let (linkage, llvmVis, mlirVis) = case mBody of
            Nothing -> (IR.LnkExternal, IR.LLVMVisDefault, IR.MLIRVisPrivate)
            Just _ -> (preferredLinkage, IR.LLVMVisDefault, IR.MLIRVisPublic)

    -- Handle Location and Debug Info for Function
    loc <- case span' of
        Nothing -> pure Nothing
        Just (s, e) -> lookupLocation (s, e)

    loc' <- case loc of
        Nothing -> pure Nothing
        Just l -> do
            mDbgTyArgs <- mapM typeAsDbgType tyArgs
            let dbgTyArgs = sequence mDbgTyArgs

            case dbgTyArgs of
                Nothing -> pure $ Just $ IR.FusedLoc Nothing [l]
                Just args -> do
                    let functionMeta =
                            IR.DBGFunction
                                { DBG.dbgFuncRawName = unmangledPath path
                                , DBG.dbgFuncTyParams = args
                                }
                    pure $ Just $ IR.FusedLoc (Just functionMeta) [l]

    retTy <- convertType (Full.funcReturnType func)

    (bodyBlock, args) <- withFreshLocalContext $ do
        -- Generate parameters (region + normal)
        regionParam <-
            if Full.funcIsRegional func
                then do
                    handle <- nextValue
                    pure $ Just (handle, IR.TypeRegion)
                else pure Nothing

        let params = Full.funcParams func
        paramValues <- forM params $ \(_, ty) -> do
            irTy <- inject $ convertType ty
            val <- nextValue
            pure (val, irTy)

        -- Update varMap assuming implicit numbering 0..N-1 for parameters
        -- regionParam is NOT in varMap (handled via regionHandle state)
        let varMapUpdates = zip [0 ..] paramValues
        State.modify $ \s ->
            s
                { varMap = IntMap.fromList [(i, v) | (i, v) <- varMapUpdates]
                , regionHandle = regionParam
                }

        let argValues = maybeToList regionParam ++ paramValues

        case mBody of
            Nothing -> do
                L.logTrace_ $ "Lowering function " <> T.show symbol <> " with no body"
                -- Return argValues so the declaration has the correct signature
                pure (Nothing, argValues)
            Just bodyExpr -> do
                L.logTrace_ $ "Lowering function " <> T.show symbol <> " with body"

                block <- lowerExprAsBlock bodyExpr argValues $ \bodyRes -> do
                    let retInstr = IR.Return bodyRes
                    case Full.exprSpan bodyExpr of
                        Just lastSpan -> withLocationSpan lastSpan $ addIRInstr retInstr
                        Nothing -> addIRInstr retInstr

                pure (Just block, argValues)

    -- Generate debug info for function parameters
    dbgArgs <- case mBody of
        Nothing -> pure []
        Just _ -> do
            let params = Full.funcParams func
            -- Filter out unnamed params?
            -- funcParams :: [(Maybe Identifier, Type)]
            dbgParams <- forM (zip [1 ..] params) $ \(idx, (name, ty)) -> do
                mDbgTy <- typeAsDbgType ty
                pure $ fmap (\dbgTy -> DBG.DBGFuncArg dbgTy (unIdentifier name) idx) mDbgTy
            pure $ catMaybes dbgParams

    let irFunc =
            IR.Function
                { IR.funcLinkage = linkage
                , IR.funcLLVMVisibility = llvmVis
                , IR.funcMLIRVisibility = mlirVis
                , IR.funcBody = bodyBlock
                , IR.funcArgs = args
                , IR.funcDbgArgs = dbgArgs
                , IR.funcLoc = loc'
                , IR.funcResult = retTy
                , IR.funcSymbol = symbol
                }

    mod' <- State.get
    let updatedMod = mod'{IR.moduleFunctions = irFunc : IR.moduleFunctions mod'}
    State.put updatedMod
