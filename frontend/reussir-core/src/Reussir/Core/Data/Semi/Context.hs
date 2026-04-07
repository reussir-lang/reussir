module Reussir.Core.Data.Semi.Context (
    SemiContext (..),
    LocalSemiContext (..),
    FFIImportInfo (..),
    GlobalSemiEff,
    SemiEff,
) where

import Data.HashMap.Internal.Strict (HashMap)
import Data.Int (Int64)
import Effectful (Eff, IOE)
import Effectful.Log (Log)
import Effectful.Prim (Prim)
import Effectful.State.Static.Local (State)
import Reussir.Diagnostic.Report (Report)
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.HashTable.IO qualified as H
import Reussir.Bridge qualified as B

import Reussir.Core.Data.Class (ClassDAG)
import Reussir.Core.Data.Generic (GenericState)
import Reussir.Core.Data.Semi.Function (FunctionTable)
import Reussir.Core.Data.Semi.Record (Record)
import Reussir.Core.Data.Semi.Type (TypeClassTable, Type)
import Reussir.Core.Data.Semi.Unification (HoleTable)
import Reussir.Core.Data.Semi.Variable (VarTable)
import Reussir.Core.Data.UniqueID (GenericID)
import Reussir.Core.String (StringUniqifier)
import qualified Data.Text as T

{- | The local context for semi-elaboration. This is separated from the global
context to indicate that it requires change per function
-}
data LocalSemiContext = LocalSemiContext
    { currentSpan :: Maybe (Int64, Int64)
    , holeTable :: HoleTable
    , varTable :: VarTable
    , genericNameMap :: HashMap Identifier GenericID
    , insideRegion :: Bool
    , exprCounter :: Int
    }

-- | Information about an imported FFI function.
data FFIImportInfo = FFIImportInfo
    { ffiImportABI :: T.Text
    -- | The resolved function path (in the function table)
    , ffiImportFuncPath :: Path
    -- | Optional template for polymorphic FFI code generation.
    -- Uses @${T}@ syntax for type parameter substitution.
    , ffiImportTemplate :: Maybe T.Text
    }

{- | The context required for semi-elaboration from surface syntax to generic
   constrained semi-abstract syntax.
-}
data SemiContext = SemiContext
    { currentFile :: FilePath
    , currentModulePath :: [Identifier]
    , translationLogLevel :: B.LogLevel
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    , translationHasFailed :: Bool
    , typeClassDAG :: ClassDAG
    , typeClassTable :: TypeClassTable
    , knownRecords :: H.CuckooHashTable Path Record
    , functions :: FunctionTable
    , generics :: GenericState
    -- | Export trampolines: maps exported symbol → (target func path, ABI, type args)
    , trampolines :: HashMap Identifier (Path, T.Text, [Type])
    -- | Import FFI declarations: maps function name → import info
    , ffiImports :: HashMap Identifier FFIImportInfo
    -- | Extern struct declarations: maps record path → foreign type template
    , externStructs :: HashMap Path T.Text
    }

type GlobalSemiEff = Eff '[IOE, Prim, Log, State SemiContext]
type SemiEff = Eff '[IOE, Prim, Log, State SemiContext, State LocalSemiContext]
