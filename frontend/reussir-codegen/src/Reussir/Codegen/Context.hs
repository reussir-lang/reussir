{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context
  ( TargetSpec (..),
    Context (..),
    Codegen,
    emptyContext,
    runCodegen,
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.Int (Int64)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B

data TargetSpec = TargetSpec
  { programName :: T.Text,
    outputPath :: FilePath,
    optimization :: B.OptOption,
    outputTarget :: B.OutputTarget,
    logLevel :: B.LogLevel
  }
  deriving (Eq, Show)

data Context = MkCtx
  { targetSpec :: TargetSpec,
    indentation :: Int64,
    builder :: TB.Builder
  }

type Codegen a = S.State Context a

emptyContext :: TargetSpec -> Context
emptyContext spec =
  MkCtx
    { targetSpec = spec,
      indentation = 0,
      builder = mempty
    }

runCodegen :: Context -> Codegen a -> IO a
runCodegen initCtx codegen = do
  let (result, finalCtx) = S.runState codegen initCtx
  let mlirModule = TB.toLazyText (builder finalCtx)
  let spec = targetSpec finalCtx
  B.compileForNativeMachine
    (T.unpack mlirModule)
    (T.unpack (programName spec))
    (outputPath spec)
    (outputTarget spec)
    (optimization spec)
    (logLevel spec)
  pure result

class Emission a where
  emit :: a -> TB.Builder

instance Emission TB.Builder where
  emit = id

emitCG :: (Emission a) => a -> Codegen ()
emitCG item = S.modify' $ \ctx ->
  ctx {builder = builder ctx <> emit item}

emitBuilder :: TB.Builder -> Codegen ()
emitBuilder = emitCG

emitSpace :: Codegen ()
emitSpace = emitBuilder " "

emitIndentation :: Codegen ()
emitIndentation = do
  indentLevel <- S.gets indentation
  emitBuilder $ TB.fromLazyText $ T.replicate indentLevel "\t"

emitLine :: Codegen a -> Codegen a
emitLine codegen = do
  emitIndentation
  a <- codegen
  emitBuilder "\n"
  pure a
