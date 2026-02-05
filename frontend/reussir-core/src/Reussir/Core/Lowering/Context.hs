{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Context where

import Control.Exception (SomeException, try)
import Data.Foldable (toList)
import Data.Int (Int64)
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Log (Log, logTrace_)
import Effectful.Prim.IORef.Strict (Prim)
import GHC.Stack (HasCallStack)
import Reussir.Diagnostic.Repository (Repository, lookupRepositoryAsRange)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, takeFileName)

import Data.IntMap.Strict qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Location qualified as IR
import Reussir.Codegen.Value qualified as IR

import Reussir.Core.Data.Lowering.Context (
    ExprResult,
    GlobalLoweringEff,
    LocalLoweringContext (..),
    LoweringContext (..),
    LoweringEff,
    LoweringSpan (..),
 )
import Reussir.Core.Data.String (StringUniqifier)
import Reussir.Core.Data.UniqueID (VarID (VarID))

import Reussir.Core.Data.Full.Function qualified as Full
import Reussir.Core.Data.Full.Record qualified as Full

{-
, srcRepository :: Repository
    , functionInstances :: Full.FunctionTable
    , recordInstances :: Full.FullRecordTable
    , stringUniqifier :: StringUniqifier
    , targetSpec :: IR.TargetSpec
-}

createLoweringContext ::
    (IOE :> es, Log :> es, Prim :> es) =>
    Repository ->
    Full.FunctionTable ->
    Full.FullRecordTable ->
    StringUniqifier ->
    IR.TargetSpec ->
    Eff es LoweringContext
createLoweringContext repo functions records stringUniqifier targetSpec = do
    (dir, base) <- liftIO $ do
        result <- try @SomeException $ canonicalizePath (IR.moduleFilePath targetSpec)
        case result of
            Left _ -> pure ("<unknown>", "<unknown>")
            Right path -> pure (T.pack $ takeDirectory path, T.pack $ takeFileName path)
    pure
        LoweringContext
            { moduleBasename = base
            , moduleDirectory = dir
            , srcRepository = repo
            , functionInstances = functions
            , recordInstances = records
            , stringUniqifier = stringUniqifier
            , targetSpec = targetSpec
            }

runLoweringToModule ::
    (IOE :> es, Log :> es, Prim :> es) =>
    LoweringContext ->
    GlobalLoweringEff () ->
    Eff es IR.Module
runLoweringToModule ctx lowering = Reader.runReader ctx $ do
    moduleFullPath <- IR.moduleFilePath <$> Reader.asks targetSpec
    logTrace_ $ "Start lowering to module for " <> T.pack moduleFullPath
    targetSpec <- Reader.asks targetSpec
    State.execState (IR.emptyModule targetSpec) $ inject lowering

withFreshLocalContext :: LoweringEff a -> GlobalLoweringEff a
withFreshLocalContext lowering = do
    let freshLocalContext =
            LocalLoweringContext
                { valueCounter = 0
                , varMap = mempty
                , regionHandle = Nothing
                , currentSpan = LoweringSpan{spanInfo = Nothing, metaInfo = Nothing}
                , currentBlock = mempty
                }
    State.evalState freshLocalContext $ inject lowering

lookupLocation :: (Int64, Int64) -> GlobalLoweringEff (Maybe IR.Location)
lookupLocation (start, end) = do
    base <- Reader.asks moduleBasename
    path <- IR.moduleFilePath <$> Reader.asks targetSpec
    repo <- Reader.asks srcRepository
    case lookupRepositoryAsRange repo (path, start, end) of
        Nothing -> pure Nothing
        Just (a, b, c, d) -> pure $ Just $ IR.FileLineColRange base a b c d

-- span to location
assocLocation :: IR.Instr -> LoweringEff IR.Instr
assocLocation instr = do
    LoweringSpan{spanInfo, metaInfo} <- State.gets currentSpan
    case (spanInfo, metaInfo) of
        (Nothing, _) -> pure instr
        (Just (start, end), Nothing) -> do
            loc <- inject $ lookupLocation (start, end)
            case loc of
                Nothing -> pure instr
                Just l -> pure $ IR.WithLoc l instr
        (Just (start, end), Just dbgMeta) -> do
            loc <- inject $ lookupLocation (start, end)
            case loc of
                Nothing -> pure instr
                Just l -> pure $ IR.WithLoc (IR.FusedLoc (Just dbgMeta) [l]) instr

withLocationSpan :: (Int64, Int64) -> LoweringEff a -> LoweringEff a
withLocationSpan spanInfo action = do
    span' <- State.gets currentSpan
    State.modify $ \s -> s{currentSpan = span'{spanInfo = Just spanInfo}}
    result <- action
    State.modify $ \s -> s{currentSpan = span'}
    pure result

withLocationMetaData :: IR.DBGMetaInfo -> LoweringEff a -> LoweringEff a
withLocationMetaData metaInfo action = do
    span' <- State.gets currentSpan
    State.modify $ \s -> s{currentSpan = span'{metaInfo = Just metaInfo}}
    result <- action
    State.modify $ \s -> s{currentSpan = span'}
    pure result

addIRInstr :: IR.Instr -> LoweringEff ()
addIRInstr instr = do
    logTrace_ $ "Adding IR instruction: " <> T.pack (show instr)
    instr' <- assocLocation instr
    State.modify $ \s -> s{currentBlock = currentBlock s Seq.|> instr'}

nextValue :: LoweringEff IR.Value
nextValue = do
    next <- State.gets valueCounter
    State.modify $ \s -> s{valueCounter = next + 1}
    pure $ IR.Value $ fromIntegral next

materializeCurrentBlock :: [IR.TypedValue] -> LoweringEff IR.Block
materializeCurrentBlock blkArgs = do
    blockInstrs <- State.gets currentBlock
    State.modify $ \s -> s{currentBlock = Seq.empty}
    pure $ IR.Block blkArgs (toList blockInstrs)

withVar :: VarID -> IR.TypedValue -> LoweringEff a -> LoweringEff a
withVar (VarID vid) val action = do
    backup <- State.gets varMap
    State.modify $ \s -> s{varMap = IntMap.insert (fromIntegral vid) val (varMap s)}
    res <- action
    State.modify $ \s -> s{varMap = backup}
    pure res

tyValOrICE :: (HasCallStack) => ExprResult -> IR.TypedValue
tyValOrICE Nothing = error "Expected a typed value but got Nothing"
tyValOrICE (Just val) = val
