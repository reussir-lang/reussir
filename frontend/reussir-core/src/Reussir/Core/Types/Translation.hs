module Reussir.Core.Types.Translation where

import Data.HashMap.Strict (HashMap)
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Bridge qualified as B
import Reussir.Core.Types.Class (ClassDAG, TypeBound)
import Reussir.Core.Types.Expr (VarID)
import Reussir.Core.Types.Function (FunctionTable)
import Reussir.Core.Types.Generic (GenericState)
import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Core.Types.Record (Record)
import Reussir.Core.Types.String (StringUniqifier)
import Reussir.Core.Types.Type (HoleID, Type, TypeClassTable)
import Reussir.Diagnostic (Report)
import Reussir.Parser.Types.Lexer (Identifier, Path)

data UnificationState
    = UnSolvedUFRoot {-# UNPACK #-} !Int TypeBound
    | SolvedUFRoot !Int Type
    | UFNode {-# UNPACK #-} !HoleID

data HoleState = HoleState
    { holeName :: Maybe T.Text
    , holeSpan :: Maybe (Int64, Int64)
    , holeUnification :: IORef' UnificationState
    }

data VarDef = VarDef
    { varName :: Identifier
    , varSpan :: Maybe (Int64, Int64)
    , varType :: Type
    }

data TranslationState = TranslationState
    { currentSpan :: Maybe (Int64, Int64)
    , currentFile :: FilePath
    , translationLogLevel :: B.LogLevel
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    , typeClassDAG :: ClassDAG
    , typeClassTable :: TypeClassTable
    , holes :: Seq.Seq HoleState
    , variableStates :: Seq.Seq VarDef
    , variableNameMap :: H.CuckooHashTable Identifier VarID
    , genericNameMap :: HashMap Identifier GenericID
    , knownRecords :: H.CuckooHashTable Path Record
    , functions :: FunctionTable
    , generics :: GenericState
    }

type GenericSolution = H.CuckooHashTable GenericID [Type]
