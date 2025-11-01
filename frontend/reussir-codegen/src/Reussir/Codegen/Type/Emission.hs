{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Type.Emission where

import Control.Monad.State.Strict qualified as S
import Data.HashTable.IO qualified as H
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context (Codegen, Emission (emit))
import Reussir.Codegen.Type.Data
import Reussir.Codegen.Type.Record (InstantiatedName (..))
import qualified Data.Text.Lazy.Builder as T

-- For type emission, we may run into recursive types. In this case, we need to keep track of what's already being emitted.
newtype TypeEmissionState = TypeEmissionState (H.CuckooHashTable InstantiatedName ())

newtype TypeEmissionT m a = TypeEmissionT {runTypeEmission :: S.StateT TypeEmissionState m a}
  deriving (Functor, Applicative, Monad, S.MonadState TypeEmissionState, S.MonadIO, S.MonadTrans)

startTypeEmission :: S.MonadIO m =>TypeEmissionT m a -> m a
startTypeEmission te = S.liftIO H.new >>= \e -> fst <$> S.runStateT (runTypeEmission te) (TypeEmissionState e)

type TypeEmission = TypeEmissionT Codegen

statelessEmit :: Emission a => a -> TypeEmission T.Builder
statelessEmit a = S.lift (emit a)

instance Emission PrimitiveInt where
  emit PrimInt8 = pure "i8"
  emit PrimInt16 = pure "i16"
  emit PrimInt32 = pure "i32"
  emit PrimInt64 = pure "i64"
  emit PrimInt128 = pure "i128"
  emit PrimIndex = pure "index"

instance Emission PrimitiveFloat where
  emit PrimFloat8 = pure "f8"
  emit PrimFloat16 = pure "f16"
  emit PrimBFloat16 = pure "bf16"
  emit PrimFloat32 = pure "f32"
  emit PrimFloat64 = pure "f64"
  emit PrimFloat128 = pure "f128"

instance Emission Primitive where
  emit (PrimInt bits) = emit bits
  emit (PrimFloat pft) = emit pft
  emit PrimBool = pure "i1"
  emit PrimUnit = pure "none"

instance Emission Type where
  emit (TypePrim prim) = emit prim
  emit _ = error "Emission for non-primitive types not implemented yet"

instance Emission InstantiatedName where
  emit (InstantiatedName path args) = do
    path' <- emit path
    args' <- mapM emit args
    if null args'
      then pure $ path'
      else pure $ path' <> "<" <> TB.fromLazyText (T.intercalate "," (map TB.toLazyText args')) <> ">"