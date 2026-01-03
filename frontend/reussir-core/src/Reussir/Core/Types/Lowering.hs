module Reussir.Core.Types.Lowering where

import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Effectful (Eff, IOE)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Value (TypedValue)
import Reussir.Diagnostic.Repository (Repository)

type BlockBuilder = Seq IR.Instr

data LoweringState = LoweringState
    { currentBlock :: BlockBuilder
    , moduleFile :: Maybe FilePath
    , srcRepository :: Repository
    , valueCounter :: Int64
    , varMap :: IntMap.IntMap TypedValue
    }

type Lowering = Eff '[IOE, Prim, State.State LoweringState]
