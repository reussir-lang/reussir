module Reussir.Codegen.Type.Record (
    RecordField,
    Record (..),
    RecordKind (..),
) where

import Reussir.Codegen.Type.Data (Capability, Type)

type RecordField = (Type, Capability)

data RecordKind = Compound | Variant
    deriving (Eq, Show)

data Record = Record
    { defaultCapability :: Capability
    , fields :: [RecordField]
    , kind :: RecordKind
    }
    deriving (Eq, Show)
