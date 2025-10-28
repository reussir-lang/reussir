{-# LANGUAGE OverloadedStrings #-}
module Reussir.Codegen.Value (Value(..), TypedValue) where

import Reussir.Codegen.Type (Type)
import Reussir.Codegen.Context (Emission(emit))
import qualified Data.Text.Lazy.Builder as TB
import Data.Int (Int64)

newtype Value = Value Int64
    deriving (Eq, Show)

type TypedValue = (Value, Type)

instance Emission Value where
    emit (Value v) = "%" <> TB.fromString (show v)