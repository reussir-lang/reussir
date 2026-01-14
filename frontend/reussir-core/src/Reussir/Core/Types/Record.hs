module Reussir.Core.Types.Record where

import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Lexer (Identifier, Path)
import Reussir.Parser.Types.Stmt (Visibility)

type FieldFlag = Bool

data RecordFields
    = Named [(Identifier, Type, FieldFlag)]
    | Unnamed [(Type, FieldFlag)]
    | Variants [Identifier]
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
