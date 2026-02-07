module Reussir.Core.Data.Full.Record where

import Data.Int (Int64)
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Type.Data (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path)

import Data.HashTable.IO qualified as H
import Data.Vector.Strict qualified as V

import Reussir.Core.Data.Full.Type (Type)

import Reussir.Core.Data.Semi.Record qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as Semi

type FieldFlag = Bool

data RecordFields
    = Components (V.Vector (Maybe Identifier, Type, FieldFlag))
    | Variants (V.Vector Symbol)
    deriving (Show, Eq)

data RecordKind
    = StructKind
    | EnumKind
    | EnumVariant {variantParent :: Symbol, variantIdx :: Int}
    deriving (Show, Eq)

data Record = Record
    { recordName :: Symbol -- Actualy name after mangle
    , recordRawPath :: Path -- The path before mangle
    , recordSemiTyParams :: [Semi.Type] -- The type parameter used in instantiation
    , recordFields :: RecordFields -- Instantiated fields
    , recordKind :: RecordKind -- Instantiated kind
    , recordDefaultCap :: Capability -- The default capability
    , recordSpan :: Maybe (Int64, Int64)
    }
    deriving (Show, Eq)

type FullRecordTable = H.CuckooHashTable Symbol Record
type SemiRecordTable = H.CuckooHashTable Path Semi.Record
