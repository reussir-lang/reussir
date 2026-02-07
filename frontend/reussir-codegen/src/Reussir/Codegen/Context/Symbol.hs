{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Symbol (
    Symbol,
    symbolText,
    symbolBuilder,
    verifiedSymbol,
) where

import Data.Hashable (Hashable)
import Data.Interned (Uninternable (unintern), intern)
import Data.Interned.Text (InternedText)
import Effectful.Dispatch.Dynamic (HasCallStack)
import Unicode.Char.Identifiers (isXIDContinue, isXIDStart)

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB

newtype Symbol = Symbol InternedText
    deriving (Eq, Show, Hashable)

symbolText :: Symbol -> T.Text
symbolText (Symbol s) = unintern s

symbolBuilder :: Symbol -> TB.Builder
symbolBuilder (Symbol s) = TB.fromText (unintern s)

verifiedSymbol :: (HasCallStack) => T.Text -> Symbol
verifiedSymbol txt = case T.uncons txt of
    Just (c, t) | isValidStart c && T.all isValidChar t -> Symbol (intern txt)
    _ -> error $ "Invalid symbol: " <> T.unpack txt
  where
    isValidStart ch = isXIDStart ch || ch == '_'
    isValidChar ch = isXIDContinue ch || ch == '$'
