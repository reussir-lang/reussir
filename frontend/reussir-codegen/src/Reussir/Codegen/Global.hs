{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Global (
    Global (..),
    globalCodegen,
) where

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Codegen.Context (Codegen, Emission (emit))
import Reussir.Codegen.Context.Emission (emitBuilderLineM)
import Reussir.Codegen.Context.Symbol (Symbol)

-- Currently only support string globals
data Global
    = GlobalString Symbol T.Text
    deriving (Eq, Show)

globalCodegen :: Global -> Codegen ()
globalCodegen (GlobalString sym content) = emitBuilderLineM $ do
    let opName = "reussir.str.global"
    sym' <- emit sym
    return $ opName <> " @" <> sym' <> " = " <> (TB.fromText $ T.show content)
