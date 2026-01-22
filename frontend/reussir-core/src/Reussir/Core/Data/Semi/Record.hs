module Reussir.Core.Data.Semi.Record where

import Data.Int (Int64)
import Data.Vector.Strict qualified as V
import Reussir.Core.Data.Semi.Type (Type)
import Reussir.Core.Data.UniqueID (GenericID)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path, WithSpan)
import Reussir.Parser.Types.Stmt (Visibility)

type FieldFlag = Bool

data RecordFields
    = Named (V.Vector (WithSpan (Identifier, Type, FieldFlag)))
    | Unnamed (V.Vector (WithSpan (Type, FieldFlag)))
    | Variants (V.Vector (WithSpan Identifier))
    deriving (Show, Eq)

data RecordKind
    = StructKind
    | EnumKind
    | EnumVariant {variantParent :: Path, variantIdx :: Int}
    deriving (Show, Eq)

data Record = Record
    { recordName :: Path
    , recordTyParams :: [(Identifier, GenericID)]
    , recordFields :: RecordFields
    , recordKind :: RecordKind
    , recordVisibility :: Visibility
    , recordDefaultCap :: Capability
    , recordSpan :: Maybe (Int64, Int64)
    }
    deriving (Show, Eq)
