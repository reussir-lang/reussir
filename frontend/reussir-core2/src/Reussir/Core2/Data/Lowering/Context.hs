module Reussir.Core2.Data.Lowering.Context (
    BlockBuilder,
    LoweringContext (..),
    LoweringSpan (..),
    LocalLoweringContext (..),
    LoweringEff,
    GlobalLoweringEff,
) where

import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE)
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Location (DBGMetaInfo)
import Reussir.Codegen.Value (TypedValue)
import Reussir.Core2.Data.Full.Function qualified as Full
import Reussir.Core2.Data.Full.Record qualified as Full
import Reussir.Core2.String
import Reussir.Diagnostic.Repository (Repository)

type BlockBuilder = Seq.Seq IR.Instr

data LoweringContext = LoweringContext
    { moduleBasename :: T.Text
    , moduleDirectory :: T.Text
    , srcRepository :: Repository
    , functionInstances :: Full.FunctionTable
    , recordInstances :: Full.FullRecordTable
    , stringUniqifier :: StringUniqifier
    , targetSpec :: IR.TargetSpec
    }

data LoweringSpan = LoweringSpan
    { spanInfo :: Maybe (Int64, Int64)
    , metaInfo :: Maybe DBGMetaInfo
    }

data LocalLoweringContext = LocalLoweringContext
    { valueCounter :: Int64
    , varMap :: IntMap.IntMap TypedValue
    , regionHandle :: Maybe TypedValue
    , currentSpan :: LoweringSpan
    , currentBlock :: BlockBuilder
    }

type LoweringEff =
    Eff
        '[ IOE
         , Prim
         , Log
         , Reader.Reader LoweringContext
         , State.State IR.Module
         , State.State LocalLoweringContext
         ]

type GlobalLoweringEff =
    Eff
        '[ IOE
         , Prim
         , Log
         , Reader.Reader LoweringContext
         , State.State IR.Module
         ]
