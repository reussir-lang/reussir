{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Codegen (
    Codegen,
    Context (..),
    TargetSpec (..),
    TypeInstances,
    RecordEmissions,
    RecordEmissionState (..),
    emptyContext,
    incIndentation,
    incIndentationBy,
    addTypeInstance,
    setRecordEmissionState,
    getRecordEmissionState,
    getRecord,
    withLocation,
    getNewBlockId,
    startOverBlockCounter,
    withoutLocation,
)
where

import Data.Int (Int64)
import Effectful as E (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)

import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Effectful.State.Static.Local qualified as E
import Reussir.Bridge qualified as B

import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Location (Location)
import Reussir.Codegen.Type.Record (Record)

data TargetSpec = TargetSpec
    { programName :: T.Text
    , outputPath :: FilePath
    , optimization :: B.OptOption
    , outputTarget :: B.OutputTarget
    , logLevel :: B.LogLevel
    , moduleFilePath :: FilePath
    , targetTriple :: Maybe T.Text
    , targetCPU :: Maybe T.Text
    , targetFeatures :: Maybe T.Text
    }
    deriving (Eq, Show)

data Context = MkCtx
    { indentation :: Int64
    , builder :: TB.Builder
    , typeInstances :: TypeInstances
    , recordEmissions :: RecordEmissions
    , locForLine :: Maybe Int64
    , outlineLocs :: OutlineLocs
    , allocatedLocs :: Int64
    , allocatedBlocks :: Int64
    }

type Codegen = Eff '[State Context, Reader TargetSpec, IOE, Log]
type TypeInstances = H.CuckooHashTable Symbol Record

data RecordEmissionState
    = RecordEmissionComplete -- An alias has been emitted
    | RecordEmissionIncomplete -- Emission in progress
    | RecordEmissionPending -- Emission not started yet
    deriving (Eq, Show)

type OutlineLocs = H.CuckooHashTable Int64 Location
type RecordEmissions = H.CuckooHashTable Symbol RecordEmissionState

emptyContext :: (IOE :> es) => Eff es Context
emptyContext = do
    table <- liftIO H.new
    recordEmissions <- liftIO H.new
    outlineLocs <- liftIO H.new
    return
        MkCtx
            { indentation = 0
            , builder = mempty
            , typeInstances = table
            , recordEmissions = recordEmissions
            , locForLine = Nothing
            , outlineLocs = outlineLocs
            , allocatedLocs = 0
            , allocatedBlocks = 0
            }

-- | Increment indentation level for a block of code.
incIndentation :: Codegen a -> Codegen a
incIndentation codegen = do
    E.modify $ \ctx -> ctx{indentation = indentation ctx + 1}
    res <- codegen
    E.modify $ \ctx -> ctx{indentation = indentation ctx - 1}
    return res

-- | Increment indentation level for a block of code.
incIndentationBy :: Int64 -> Codegen a -> Codegen a
incIndentationBy n codegen = do
    E.modify $ \ctx -> ctx{indentation = indentation ctx + n}
    res <- codegen
    E.modify $ \ctx -> ctx{indentation = indentation ctx - n}
    return res

-- | Add a type instance to the context.
addTypeInstance :: Symbol -> Record -> Codegen ()
addTypeInstance expr record = do
    hashTable <- E.gets typeInstances
    E.liftIO $ H.insert hashTable expr record

-- | Set the emission state for a record.
setRecordEmissionState :: Symbol -> RecordEmissionState -> Codegen ()
setRecordEmissionState key state = do
    hashTable <- E.gets recordEmissions
    E.liftIO $ H.insert hashTable key state

-- | Get the emission state for a record.
getRecordEmissionState :: Symbol -> Codegen RecordEmissionState
getRecordEmissionState key = do
    hashTable <- E.gets recordEmissions
    E.liftIO $
        H.lookup hashTable key >>= \case
            Just state -> pure state
            Nothing -> pure RecordEmissionPending

-- | Get a record from the context.
getRecord :: Symbol -> Codegen (Maybe Record)
getRecord expr = do
    hashTable <- E.gets typeInstances
    E.liftIO $
        H.lookup hashTable expr >>= \case
            Just record -> pure $ Just record
            Nothing -> pure Nothing

-- | Allocate a new location in the context.
allocLocation :: Location -> Codegen Int64
allocLocation loc = do
    allocatedLocs <- E.gets allocatedLocs
    E.modify $ \ctx -> ctx{allocatedLocs = allocatedLocs + 1}
    locs <- E.gets outlineLocs
    E.liftIO $ H.insert locs allocatedLocs loc
    pure allocatedLocs

-- | With a new location, execute a block of code and restore the previous location.
withLocation :: Location -> Codegen () -> Codegen ()
withLocation loc codegen = do
    l <- allocLocation loc
    backup <- E.gets locForLine
    E.modify $ \ctx -> ctx{locForLine = Just l}
    codegen
    E.modify $ \ctx -> ctx{locForLine = backup}

withoutLocation :: Codegen () -> Codegen ()
withoutLocation codegen = do
    backup <- E.gets locForLine
    E.modify $ \ctx -> ctx{locForLine = Nothing}
    codegen
    E.modify $ \ctx -> ctx{locForLine = backup}

getNewBlockId :: Codegen Int64
getNewBlockId = do
    allocatedBlocks <- E.gets allocatedBlocks
    E.modify $ \ctx -> ctx{allocatedBlocks = allocatedBlocks + 1}
    pure allocatedBlocks

startOverBlockCounter :: Codegen ()
startOverBlockCounter = E.modify $ \ctx -> ctx{allocatedBlocks = 0}
