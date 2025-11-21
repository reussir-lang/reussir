{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.PolymorphicFFI (
    PolymorphicFFI (..),
    PolymorphicFFIAttr (..),
    polyFFICodegen,
) where

import Data.Int (Int64)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Codegen.Context.Codegen (Codegen)
import Reussir.Codegen.Context.Emission (Emission (emit), emitBuilderLineM, intercalate)
import Reussir.Codegen.Type.Data (Type)
import Reussir.Codegen.Type.Emission ()

data PolymorphicFFIAttr
    = PolyFFITypeParam T.Text Type
    | PolyFFIStrParam T.Text T.Text
    | PolyFFIIntParam T.Text Int64

data PolymorphicFFI = PolymorphicFFI
    { polyFFITemplate :: T.Text
    , polyFFIAttrs :: [PolymorphicFFIAttr]
    }

{- | Emit a polymorphic FFI operation.
Format: reussir.polyffi texture("...") substitutions({key = value, ...})
-}
polyFFICodegen :: PolymorphicFFI -> Codegen ()
polyFFICodegen polyFFI = emitBuilderLineM $ do
    -- Emit the operation name
    let opName = "reussir.polyffi"

    -- Emit texture attribute
    let texture = polyFFITemplate polyFFI
    let texturePart = "texture(" <> (fromString $ show texture) <> ")"

    -- Emit substitutions if present
    let attrs = polyFFIAttrs polyFFI
    if null attrs
        then return $ opName <> " " <> texturePart
        else do
            substParts <- mapM emitSubstitution attrs
            let substPart = "substitutions({" <> intercalate ", " substParts <> "})"
            return $ opName <> " " <> texturePart <> " " <> substPart

{- | Emit a single substitution entry.
Format: key = value
where value can be:
  - A string: "value"
  - A type: !type
-}
emitSubstitution :: PolymorphicFFIAttr -> Codegen TB.Builder
emitSubstitution (PolyFFITypeParam key ty) = do
    let key' = TB.fromText key
    ty' <- emit ty
    return $ key' <> " = " <> ty'
emitSubstitution (PolyFFIStrParam key strVal) = do
    let key' = TB.fromText key
    let strVal' = fromString $ show strVal
    return $ key' <> " = " <> strVal'
emitSubstitution (PolyFFIIntParam key intVal) = do
    let key' = TB.fromText key
    let intVal' = TB.fromDec intVal
    return $ key' <> " = " <> intVal'
