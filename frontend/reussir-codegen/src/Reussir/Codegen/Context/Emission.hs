{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Context.Emission (
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
    emitBuilderLineM,
    emitBuilderLine,
    intercalate,
    emitLocIfPresent,
)
where

import Data.Foldable (for_)
import Data.String (fromString)
import Data.Text.Builder.Linear qualified as TB
import Data.Text.Builder.Linear.Buffer qualified as TBB
import Effectful.State.Static.Local qualified as E
import Reussir.Codegen.Context.Codegen (Codegen, Context (..))
import Reussir.Codegen.Context.Symbol (Symbol, symbolText)
import Reussir.Codegen.Location (DGBMetaInfo (..), Location (..))

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
    emitBuilder $ TB.Builder $ TBB.appendChars (fromIntegral indentLevel) '\t'

emitLocIfPresent :: Codegen ()
emitLocIfPresent = do
    loc <- E.gets locForLine
    for_ loc $ \l -> do
        emitBuilder $ " loc(" <> "#loc" <> TB.fromDec l <> ")"

{- | Emit code with indentation and a newline.
  This is used to emit complete lines of code.
-}
emitLine :: Codegen a -> Codegen a
emitLine codegen = do
    emitIndentation
    a <- codegen
    emitLocIfPresent
    emitBuilder "\n"
    pure a

{- | Emit code with indentation and a newline.
  This is used to emit complete lines of code.
-}
emitBuilderLineM :: Codegen TB.Builder -> Codegen ()
emitBuilderLineM codegen = emitLine $ codegen >>= emitBuilder

{- | Emit code with indentation and a newline.
  This is used to emit complete lines of code.
-}
emitBuilderLine :: TB.Builder -> Codegen ()
emitBuilderLine builder = emitBuilderLineM (pure builder)

{- | Intercalate a list of builders with a separator.
  This is used to emit a list of builders with a separator.
-}
intercalate :: TB.Builder -> [TB.Builder] -> TB.Builder
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate sep (x : xs) = let !rest = intercalate sep xs in x <> sep <> rest

{- | Emission instance for Path.
  This is defined here (not in Context.Path) to avoid cyclic dependencies.
-}
instance Emission Symbol where
    emit = pure . TB.fromText . symbolText

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
            let filePart = fromString (show fname) <> ":" <> TB.fromDec startL <> ":" <> TB.fromDec startC
            if startL == endL && startC == endC
                then pure filePart
                else pure $ filePart <> " to " <> TB.fromDec endL <> ":" <> TB.fromDec endC
        emitInner (FusedLoc metadata' locations') = do
            locationsInner <- mapM emitInner locations'
            let locsPart = intercalate ", " locationsInner
            case metadata' of
                Just meta -> pure $ "fused<" <> fromString (show meta) <> ">[" <> locsPart <> "]"
                Nothing -> pure $ "fused[" <> locsPart <> "]"
        emitInner UnknownLoc = pure "?"
        emitInner (NameLoc locName' childLoc') = do
            let namePart = fromString (show locName')
            case childLoc' of
                Just child -> do
                    childInner <- emitInner child
                    pure $ namePart <> "(" <> childInner <> ")"
                Nothing -> pure namePart

instance Emission DGBMetaInfo where
    emit (DBGRawMeta text) = pure $ fromString (show text)
    emit _ = pure "<not-yet-implemented>"
