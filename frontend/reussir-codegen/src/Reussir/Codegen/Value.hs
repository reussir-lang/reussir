module Reussir.Codegen.Value (Value, TypedValue) where

import Reussir.Codegen.Type (Type)

newtype Value = Value Int
    deriving (Eq, Show)

type TypedValue = (Value, Type)