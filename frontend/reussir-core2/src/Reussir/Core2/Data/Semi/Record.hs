module Reussir.Core2.Data.Semi.Record where

import Data.Vector.Strict qualified as V
import Reussir.Core2.Data.Semi.Type (Type)
import Reussir.Core2.Data.UniqueID (GenericID)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Stmt (Visibility)

type FieldFlag = Bool

data RecordFields
    = Named (V.Vector (Identifier, Type, FieldFlag))
    | Unnamed (V.Vector (Type, FieldFlag))
    | Variants (V.Vector Identifier)
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
    }
    deriving (Show, Eq)
