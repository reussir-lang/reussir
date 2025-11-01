module Reussir.Codegen.Type.Record(
    RecordField,
    Record (..),
    InstantiatedName (..)
) where

import Reussir.Codegen.Type.Data (Type, Capability)
import Reussir.Codegen.Context (Path)

type RecordField = (Type, Capability)

data RecordKind = Compound | Variant
  deriving (Eq, Show)

data Record = Record
  { defaultCapability :: Capability,
    fields :: [RecordField],
    kind :: RecordKind
  }
  deriving (Eq, Show)

data InstantiatedName = InstantiatedName Path [InstantiatedName]