{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Context.Emission (
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
)
where

import Control.Monad.State.Strict qualified as S
import Data.Interned (Uninternable (unintern))
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context.Codegen (Codegen, Context (..))
import Reussir.Codegen.Context.Path (Path (..))

{- | The Emission class provides a way to convert values to Text.Builder
  within the Codegen monad. This is used for emitting MLIR text.
-}
class Emission a where
    emit :: a -> Codegen TB.Builder

instance Emission TB.Builder where
    emit = pure

-- | Emit a value that implements Emission and append it to the builder.
emitCG :: (Emission a) => a -> Codegen ()
emitCG item = do
    change <- emit item
    S.modify' $ \ctx ->
        ctx{builder = builder ctx <> change}

-- | Emit a Text.Builder directly.
emitBuilder :: TB.Builder -> Codegen ()
emitBuilder = emitCG

-- | Emit a single space.
emitSpace :: Codegen ()
emitSpace = emitBuilder " "

-- | Emit indentation based on the current indentation level.
emitIndentation :: Codegen ()
emitIndentation = do
    indentLevel <- S.gets indentation
    emitBuilder $ TB.fromLazyText $ T.replicate indentLevel "\t"

{- | Emit code with indentation and a newline.
  This is used to emit complete lines of code.
-}
emitLine :: Codegen a -> Codegen a
emitLine codegen = do
    emitIndentation
    a <- codegen
    emitBuilder "\n"
    pure a

{- | Emission instance for Path.
  This is defined here (not in Context.Path) to avoid cyclic dependencies.
-}
instance Emission Path where
    emit (Path segments) =
        pure $
            TB.fromLazyText $
                T.intercalate "::" (map (T.fromStrict . unintern) segments)
