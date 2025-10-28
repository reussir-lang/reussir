{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Value (Value (..), TypedValue) where

import Data.Int (Int64)
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context (Emission (emit))
import Reussir.Codegen.Type (Type)

newtype Value = Value Int64
  deriving (Eq, Show)

type TypedValue = (Value, Type)

instance Emission Value where
  emit (Value v) = "%" <> TB.fromString (show v)
