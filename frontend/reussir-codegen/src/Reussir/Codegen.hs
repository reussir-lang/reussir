{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen where

import Control.Monad.State.Strict qualified as S
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B

data Context = MkCtx
  { programName :: T.Text,
    outputPath :: FilePath,
    optimization :: B.OptOption,
    outputTarget :: B.OutputTarget,
    logLevel :: B.LogLevel,
    builder :: TB.Builder
  }

type Codegen a = S.State Context a

emptyContext :: T.Text -> FilePath -> B.OptOption -> B.OutputTarget -> B.LogLevel -> Context
emptyContext name outPath optOpt outTarget logLvl =
  MkCtx
    { programName = name,
      outputPath = outPath,
      optimization = optOpt,
      outputTarget = outTarget,
      logLevel = logLvl,
      builder = mempty
    }

runCodegen :: Context -> Codegen a -> IO a
runCodegen initCtx codegen = do
  let (result, finalCtx) = S.runState codegen initCtx
  let mlirModule = TB.toLazyText (builder finalCtx)
  B.compileForNativeMachine
    (T.unpack mlirModule)
    (T.unpack (programName finalCtx))
    (outputPath finalCtx)
    (outputTarget finalCtx)
    (optimization finalCtx)
    (logLevel finalCtx)
  pure result

class Emission a where
  emit :: a -> TB.Builder

emitCG :: Emission a => a -> Codegen ()
emitCG item = S.modify' $ \ctx ->
  ctx {builder = builder ctx <> emit item}