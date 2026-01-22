{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Lowering.Function where

import Control.Monad (forM)

import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes, maybeToList)
import Effectful (inject)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Location qualified as DBG
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Type qualified as IR
import Reussir.Core2.Data.Full.Expr qualified as Full
import Reussir.Core2.Data.Full.Function qualified as Full
import Reussir.Core2.Data.Lowering.Context (GlobalLoweringEff, LocalLoweringContext (..))
import Reussir.Core2.Lowering.Context (addIRInstr, lookupLocation, nextValue, withFreshLocalContext, withLocationSpan)
import Reussir.Core2.Lowering.Debug (typeAsDbgType, unmangledPath)
import Reussir.Core2.Lowering.Expr (lowerExprAsBlock)
import Reussir.Core2.Lowering.Type (convertType)
import Reussir.Parser.Types.Lexer (Identifier (..))

lowerFunction :: Full.Function -> GlobalLoweringEff ()
lowerFunction func = do
    let path = Full.funcRawPath func
    let tyArgs = Full.funcInstantiatedTyArgs func
    let symbol = Full.funcName func
    let mBody = Full.funcBody func
    let span' = Full.funcSpan func

    let (linkage, llvmVis, mlirVis) = case mBody of
            Nothing -> (IR.LnkAvailableExternally, IR.LLVMVisDefault, IR.MLIRVisPrivate)
            Just _ -> (IR.LnkWeakODR, IR.LLVMVisDefault, IR.MLIRVisPublic)

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
        case mBody of
            Nothing -> pure (Nothing, [])
            Just bodyExpr -> do
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

                block <- lowerExprAsBlock bodyExpr argValues $ \(retVal, _) -> do
                    let retInstr = IR.Return (Just (retVal, retTy))
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
