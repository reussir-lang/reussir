module Reussir.Codegen.Type.Record(
    RecordField,
    Record (..),
) where

import Reussir.Codegen.Type.Data (Type, Capability)

type RecordField = (Type, Capability)

data RecordKind = Compound | Variant
  deriving (Eq, Show)

data Record = Record
  { defaultCapability :: Capability,
    fields :: [RecordField],
    kind :: RecordKind
  }
  deriving (Eq, Show)