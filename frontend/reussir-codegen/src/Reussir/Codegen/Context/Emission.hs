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
import Data.Text qualified as T
import Data.Text.Builder.Linear (fromText)
import Data.Text.Builder.Linear qualified as TB
import Data.Text.Builder.Linear.Buffer qualified as TBB
import Effectful.State.Static.Local qualified as E
import Reussir.Codegen.Context.Codegen (Codegen, Context (..))
import Reussir.Codegen.Context.Symbol (Symbol, symbolText)
import Reussir.Codegen.Location (DBGMetaInfo (..), DBGType (..), Location (..))
import Reussir.Codegen.Type.Data (PrimitiveFloat (..), PrimitiveInt (..))

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
intercalate :: (Foldable f) => TB.Builder -> f TB.Builder -> TB.Builder
intercalate sep =
    snd . foldl' step (True, mempty)
  where
    step (first, acc) x
        | first = (False, x)
        | otherwise = (False, acc <> sep <> x)

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
                Just meta -> do
                    meta' <- emit meta
                    pure $ "fused<" <> meta' <> ">[" <> locsPart <> "]"
                Nothing -> pure $ "fused[" <> locsPart <> "]"
        emitInner UnknownLoc = pure "?"
        emitInner (NameLoc locName' childLoc') = do
            let namePart = fromString (show locName')
            case childLoc' of
                Just child -> do
                    childInner <- emitInner child
                    pure $ namePart <> "(" <> childInner <> ")"
                Nothing -> pure namePart

instance Emission DBGMetaInfo where
    emit (DBGRawMeta text) = pure $ fromString (show text)
    emit (DBGLocalVar ty name) = do
        ty' <- emit ty
        pure $ "#reussir.dbg_localvar<type: " <> ty' <> ", name: " <> fromString (show name) <> ">"
    emit (DBGFunction rawName tyParams) = do
        tyParams' <- mapM emit tyParams
        let paramsBuilder = intercalate ", " tyParams'
        pure $ "#reussir.dbg_subprogram<raw_name: " <> fromString (show rawName) <> ", type_params: [" <> paramsBuilder <> "]>"
    emit (DBGFuncArg ty name idx) = do
        ty' <- emit ty
        pure $ "#reussir.dbg_func_arg<type: " <> ty' <> ", name: " <> fromString (show name) <> ", index: " <> TB.fromDec idx <> ">"

emitDbgInteger :: TB.Builder -> PrimitiveInt -> T.Text -> Codegen TB.Builder
emitDbgInteger signness signlessTy name = do
    let ty = case signlessTy of
            PrimInt8 -> "i8"
            PrimInt16 -> "i16"
            PrimInt32 -> "i32"
            PrimInt64 -> "i64"
            PrimInt128 -> "i128"
            PrimIndex -> "index"
    pure $ "#reussir.dbg_inttype<signed: " <> signness <> " , " <> ty <> ", name : " <> fromText (T.show name) <> ">"

instance Emission DBGType where
    emit (Signed primInt name) = emitDbgInteger "true" primInt name
    emit (Unsigned primInt name) = emitDbgInteger "false" primInt name
    emit (FP primFloat name) = do
        let ty = case primFloat of
                PrimFloat8 -> "f8"
                PrimFloat16 -> "f16"
                PrimBFloat16 -> "bf16"
                PrimFloat32 -> "f32"
                PrimFloat64 -> "f64"
                PrimFloat128 -> "f128"
        pure $ "#reussir.dbg_fptype<" <> ty <> ", name : " <> fromText (T.show name) <> ">"
    emit (Record name members rep isVariant) = do
        let rep_reference = "!" <> TB.fromText (symbolText rep)
        members' <- mapM emitMember members
        let memberBuilders = intercalate ", " members'
        let isVariant' = if isVariant then "true" else "false"
        pure $ "#reussir.dbg_recordtype<members: [" <> memberBuilders <> "], is_variant: " <> isVariant' <> ", underlying_type: " <> rep_reference <> ", dbg_name: " <> fromText (T.show name) <> ">"
      where
        emitMember :: (T.Text, DBGType) -> Codegen TB.Builder
        emitMember (memberName, memberType) = do
            memberType' <- emit memberType
            pure $ "#reussir.dbg_record_member<name: " <> fromText (T.show memberName) <> ", type: " <> memberType' <> ">"
