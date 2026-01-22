module Reussir.Codegen.Type.Record (
    RecordField (..),
    Record (..),
    RecordKind (..),
) where

import Data.Vector.Strict qualified as V
import Reussir.Codegen.Type.Data (Capability, Type)

data RecordField = RecordField
    { fieldType :: Type
    , fieldIsMutable :: Bool
    }
    deriving (Eq, Show)

data RecordKind = Compound | Variant
    deriving (Eq, Show)

data Record = Record
    { defaultCapability :: Capability
    , fields :: V.Vector RecordField
    , kind :: RecordKind
    }
    deriving (Eq, Show)
