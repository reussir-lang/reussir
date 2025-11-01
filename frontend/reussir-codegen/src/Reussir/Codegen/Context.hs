{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context
  ( TargetSpec (..),
    Context (..),
    CodegenT (..),
    Codegen,
    genState,
    emptyContext,
    runCodegen,
    Emission (emit),
    emitCG,
    emitBuilder,
    emitSpace,
    emitIndentation,
    emitLine,
    incIndentation,
    Path (..),
    pathSingleton,
    pathList,
    logDebug,
    logInfo,
    logWarning,
    logError,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.Int (Int64)
import Data.Interned (Uninternable (unintern), intern)
import Data.Interned.Text (InternedText)
import Data.Text qualified as ST
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B
import System.Log (Priority (..))
import System.Log.Logger (getLogger, logL)

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

newtype CodegenT m a = Codegen {genStateT :: S.StateT Context m a}
  deriving (Functor, Applicative, Monad, S.MonadState Context, S.MonadIO, S.MonadTrans)

type Codegen = CodegenT IO

genState :: Codegen a -> S.StateT Context IO a
genState = genStateT

emptyContext :: TargetSpec -> Context
emptyContext spec =
  MkCtx
    { targetSpec = spec,
      indentation = 0,
      builder = mempty
    }

runCodegen :: Context -> Codegen a -> IO a
runCodegen initCtx codegen = do
  (result, finalCtx) <- S.runStateT (genState codegen) initCtx
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

newtype Path = Path [InternedText]
  deriving (Eq, Show)

instance Emission Path where
  emit (Path segments) =
    pure $
      TB.fromLazyText $
        T.intercalate "::" (map (T.fromStrict . unintern) segments)

pathSingleton :: (Show a) => a -> Path
pathSingleton x = Path [intern (ST.pack (show x))]

pathList :: (Show a) => [a] -> Path
pathList xs = Path (map (intern . ST.pack . show) xs)

class Emission a where
  emit :: a -> Codegen TB.Builder

instance Emission TB.Builder where
  emit = pure

emitCG :: (Emission a) => a -> Codegen ()
emitCG item = do
  change <- emit item
  S.modify' $ \ctx ->
    ctx {builder = builder ctx <> change}

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

incIndentation :: Codegen a -> Codegen a
incIndentation codegen = do
  S.modify' $ \ctx ->
    ctx {indentation = indentation ctx + 1}
  res <- codegen
  S.modify' $ \ctx ->
    ctx {indentation = indentation ctx - 1}
  return res

logWithPriority :: Priority -> String -> Codegen ()
logWithPriority priority msg = do
  logger <- S.liftIO $ getLogger "Reussir.Codegen"
  S.liftIO $ logL logger priority msg

logDebug :: String -> Codegen ()
logDebug msg = logWithPriority DEBUG msg

logInfo :: String -> Codegen ()
logInfo msg = logWithPriority INFO msg

logWarning :: String -> Codegen ()
logWarning msg = logWithPriority WARNING msg

logError :: String -> Codegen ()
logError msg = logWithPriority ERROR msg