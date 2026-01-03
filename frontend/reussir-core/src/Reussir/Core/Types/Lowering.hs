module Reussir.Core.Types.Lowering (
    BlockBuilder,
    GenericAssignment,
    LoweringState (..),
    Lowering,
) where

import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Effectful (Eff, IOE)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Value (TypedValue)
import Reussir.Core.Types.Translation (TranslationState)
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic.Repository (Repository)

type BlockBuilder = Seq IR.Instr
type GenericAssignment = IntMap.IntMap Sem.Type

data LoweringState = LoweringState
    { currentBlock :: BlockBuilder
    , moduleFile :: Maybe FilePath
    , srcRepository :: Repository
    , valueCounter :: Int64
    , varMap :: IntMap.IntMap TypedValue
    , translationState :: TranslationState
    , currentModule :: IR.Module
    , genericAssignment :: GenericAssignment
    }

type Lowering = Eff '[IOE, Prim, State.State LoweringState]
