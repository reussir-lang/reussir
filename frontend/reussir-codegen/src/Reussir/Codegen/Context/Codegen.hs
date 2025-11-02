{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Codegen (
    CodegenT (..),
    Codegen,
    Context (..),
    TargetSpec (..),
    TypeInstances,
    RecordEmissions,
    RecordEmissionState (..),
    genState,
    emptyContext,
    incIndentation,
    addTypeInstance,
    setRecordEmissionState,
    getRecordEmissionState,
    getRecord,
)
where

import Control.Monad.State.Strict qualified as S
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B
import Reussir.Codegen.Type.Data qualified as TT
import Reussir.Codegen.Type.Record (Record)

data TargetSpec = TargetSpec
    { programName :: T.Text
    , outputPath :: FilePath
    , optimization :: B.OptOption
    , outputTarget :: B.OutputTarget
    , logLevel :: B.LogLevel
    }
    deriving (Eq, Show)

data Context = MkCtx
    { targetSpec :: TargetSpec
    , indentation :: Int64
    , builder :: TB.Builder
    , typeInstances :: TypeInstances
    , recordEmissions :: RecordEmissions
    }

newtype CodegenT m a = Codegen {genStateT :: S.StateT Context m a}
    deriving (Functor, Applicative, Monad, S.MonadState Context, S.MonadIO, S.MonadTrans)

type Codegen = CodegenT IO
type TypeInstances = H.CuckooHashTable TT.Expr Record

data RecordEmissionState
    = RecordEmissionComplete -- An alias has been emitted
    | RecordEmissionIncomplete -- Emission in progress
    | RecordEmissionPending -- Emission not started yet
    deriving (Eq, Show)
type RecordEmissions = H.CuckooHashTable T.Text RecordEmissionState

genState :: Codegen a -> S.StateT Context IO a
genState = genStateT

emptyContext :: TargetSpec -> IO Context
emptyContext spec = do
    table <- H.new
    recordEmissions <- H.new
    return
        MkCtx
            { targetSpec = spec
            , indentation = 0
            , builder = mempty
            , typeInstances = table
            , recordEmissions = recordEmissions
            }

-- | Increment indentation level for a block of code.
incIndentation :: Codegen a -> Codegen a
incIndentation codegen = do
    S.modify' $ \ctx ->
        ctx{indentation = indentation ctx + 1}
    res <- codegen
    S.modify' $ \ctx ->
        ctx{indentation = indentation ctx - 1}
    return res

-- | Add a type instance to the context.
addTypeInstance :: TT.Expr -> Record -> Codegen ()
addTypeInstance expr record = do
    ctx <- S.get
    S.liftIO $ H.insert (typeInstances ctx) expr record

setRecordEmissionState :: T.Text -> RecordEmissionState -> Codegen ()
setRecordEmissionState key state = do
    ctx <- S.get
    S.liftIO $ H.insert (recordEmissions ctx) key state

getRecordEmissionState :: T.Text -> Codegen RecordEmissionState
getRecordEmissionState key = do
    ctx <- S.get
    S.liftIO $
        H.lookup (recordEmissions ctx) key >>= \case
            Just state -> pure state
            Nothing -> pure RecordEmissionPending

getRecord :: TT.Expr -> Codegen (Maybe Record)
getRecord expr = do
    ctx <- S.get
    S.liftIO $
        H.lookup (typeInstances ctx) expr >>= \case
            Just record -> pure $ Just record
            Nothing -> pure Nothing
