module Reussir.Core.Types.Lowering (
    BlockBuilder,
    GenericAssignment,
    LoweringState (..),
    Lowering,
    LoweringSpan (..),
) where

import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Text qualified as T
import Effectful (Eff, IOE)
import Effectful.Log qualified as L
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Location (DBGMetaInfo)
import Reussir.Codegen.Value (TypedValue)
import Reussir.Core.Types.Translation (TranslationState)
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic.Repository (Repository)

type BlockBuilder = Seq IR.Instr
type GenericAssignment = IntMap.IntMap Sem.Type

data LoweringState = LoweringState
    { currentBlock :: BlockBuilder
    , moduleBasename :: T.Text
    , moduleDirectory :: T.Text
    , moduleFullPath :: FilePath
    , srcRepository :: Repository
    , valueCounter :: Int64
    , varMap :: IntMap.IntMap TypedValue
    , translationState :: TranslationState
    , genericAssignment :: GenericAssignment
    , currentModule :: IR.Module
    , regionHandle :: Maybe TypedValue
    }

type Lowering = Eff '[IOE, Prim, L.Log, State.State LoweringState]

data LoweringSpan
    = NoSpan
    | LineSpan (Int64, Int64)
    | FusedSpan (Int64, Int64) DBGMetaInfo
