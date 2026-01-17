module Reussir.Core2.Data.Semi.Context (
    SemiContext (..),
    SemiEff,
) where

import Data.HashMap.Internal.Strict (HashMap)
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Effectful (Eff, IOE)
import Effectful.Log (Log)
import Effectful.Prim (Prim)
import Effectful.State.Static.Local (State)
import Reussir.Bridge qualified as B
import Reussir.Core2.Data.Class (ClassDAG)
import Reussir.Core2.Data.Function (FunctionTable)
import Reussir.Core2.Data.Generic (GenericState)
import Reussir.Core2.Data.Semi.Record (Record)
import Reussir.Core2.Data.Semi.Type (TypeClassTable)
import Reussir.Core2.Data.Semi.Unification (HoleTable)
import Reussir.Core2.Data.Semi.Variable (VarTable)
import Reussir.Core2.Data.UniqueID (GenericID)
import Reussir.Core2.String (StringUniqifier)
import Reussir.Diagnostic.Report (Report)
import Reussir.Parser.Types.Lexer (Identifier, Path)

{- | The context required for semi-elaboration from surface syntax to generic
   constrained semi-abstract syntax.
-}
data SemiContext = SemiContext
    { currentSpan :: Maybe (Int64, Int64)
    , currentFile :: FilePath
    , translationLogLevel :: B.LogLevel
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    , translationHasFailed :: Bool
    , typeClassDAG :: ClassDAG
    , typeClassTable :: TypeClassTable
    , holeTable :: HoleTable
    , varTable :: VarTable
    , genericNameMap :: HashMap Identifier GenericID
    , knownRecords :: H.CuckooHashTable Path Record
    , functions :: FunctionTable
    , generics :: GenericState
    , insideRegion :: Bool
    , exprCounter :: Int
    }

type SemiEff = Eff '[IOE, Prim, Log, State SemiContext]
