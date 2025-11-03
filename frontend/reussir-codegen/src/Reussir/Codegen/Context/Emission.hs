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

import Data.Foldable (for_)
import Data.Interned (Uninternable (unintern))
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Effectful.State.Static.Local qualified as E
import Reussir.Codegen.Context.Codegen (Codegen, Context (..))
import Reussir.Codegen.Context.Path (Path (..))
import Reussir.Codegen.Location (Location (..))

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
    E.modify $ \ctx ->
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
    indentLevel <- E.gets indentation
    emitBuilder $ TB.fromLazyText $ T.replicate indentLevel "\t"

{- | Emit code with indentation and a newline.
  This is used to emit complete lines of code.
-}
emitLine :: Codegen a -> Codegen a
emitLine codegen = do
    emitIndentation
    a <- codegen
    loc <- E.gets locForLine
    for_ loc $ \l -> do
        emitBuilder $ " loc(" <> "#loc" <> TB.fromString (show l) <> ")"
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

-- callsite-location ::= `callsite` `(` location `at` location `)`
-- filelinecol-location ::= string-literal `:` integer-literal `:`
--                         integer-literal
--                         (`to` (integer-literal ?) `:` integer-literal ?)
--    A single file line location: file:line;
--    A single file line col location: file:line:column;
--    A single line range: file:line:column to :column;
--    A single file range: file:line:column to line:column;
-- fusion-metadata ::= `<` attribute-value `>`
-- fused-location ::= `fused` fusion-metadata? `[` (location (`,` location)* )? `]`
-- name-location ::= string-literal (`(` location `)`)?
-- unknown-location ::= `?`
instance Emission Location where
    emit loc = wrapLoc <$> emitInner loc
      where
        wrapLoc inner = "loc(" <> inner <> ")"
        emitInner (CallSiteLoc callee' caller') = do
            calleeInner <- emitInner callee'
            callerInner <- emitInner caller'
            pure $ "callsite(" <> calleeInner <> " at " <> callerInner <> ")"
        emitInner (FileLineColRange fname startL startC endL endC) = do
            let filePart = TB.fromString (show fname) <> ":" <> TB.fromString (show startL) <> ":" <> TB.fromString (show startC)
            if startL == endL && startC == endC
                then pure filePart
                else pure $ filePart <> " to " <> TB.fromString (show endL) <> ":" <> TB.fromString (show endC)
        emitInner (FusedLoc metadata' locations') = do
            locationsInner <- mapM emitInner locations'
            let locsPart = TB.fromLazyText $ T.intercalate ", " (map TB.toLazyText locationsInner)
            case metadata' of
                Just meta -> pure $ "fused<" <> TB.fromString (show meta) <> ">[" <> locsPart <> "]"
                Nothing -> pure $ "fused[" <> locsPart <> "]"
        emitInner UnknownLoc = pure "?"
        emitInner (NameLoc locName' childLoc') = do
            let namePart = TB.fromString (show locName')
            case childLoc' of
                Just child -> do
                    childInner <- emitInner child
                    pure $ namePart <> "(" <> childInner <> ")"
                Nothing -> pure namePart
