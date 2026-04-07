module Reussir.Core.Data.Full.Context where

import Data.Int (Int64)
import Effectful (Eff, IOE)
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (State)

import Reussir.Core.Data.Full.Error (Error)
import Reussir.Core.Data.Full.Function (FunctionTable)
import Reussir.Core.Data.Full.Record (FullRecordTable, SemiRecordTable)
import Reussir.Core.Data.Full.Type (GenericMap)
import Reussir.Core.Data.String (StringUniqifier)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Parser.Types.Lexer (Path)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

-- | Information about an imported FFI function after full conversion.
data FullFFIImport = FullFFIImport
    { fullFFIImportABI :: T.Text
    , fullFFIImportSymbol :: Symbol
    -- | The raw function path (for matching against function instances)
    , fullFFIImportFuncPath :: Path
    -- | Generic parameter names for template substitution
    , fullFFIImportGenericNames :: [T.Text]
    -- | Optional Rust/C template for polymorphic FFI code generation
    , fullFFIImportTemplate :: Maybe T.Text
    }

data FullContext = FullContext
    { ctxFunctions :: FunctionTable
    , ctxRecords :: FullRecordTable
    , ctxSemiRecords :: SemiRecordTable
    , ctxErrors :: [Error]
    , ctxStringUniqifier :: StringUniqifier
    , ctxFilePath :: FilePath
    , ctxFlexible :: Bool
    , ctxTrampolines :: HashMap.HashMap Symbol (T.Text, Symbol)
    -- | Import FFI declarations: maps mangled function symbol → import info
    , ctxFFIImports :: HashMap.HashMap Symbol FullFFIImport
    -- | Extern struct declarations: maps record path → foreign type template
    , ctxExternStructs :: HashMap.HashMap Path T.Text
    }

data LocalFullContext = LocalFullContext
    { currentSpan :: Maybe (Int64, Int64)
    , genericMap :: GenericMap
    , exprCounter :: Int
    }

type FullEff = Eff '[IOE, Prim, Log, State FullContext, State LocalFullContext]
type GlobalFullEff = Eff '[IOE, Prim, Log, State FullContext]
