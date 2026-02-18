{-# LANGUAGE OverloadedStrings #-}

module Main where

import Effectful (liftIO, runEff)
import Effectful.Prim (runPrim)
import Log (LogLevel (..))
import Options.Applicative
import Reussir.Codegen.Context (TargetSpec (..))
import Reussir.Parser.Prog (parseProg)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, runParser)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful.Log qualified as L
import Reussir.Bridge qualified as B
import Reussir.Codegen qualified as C

import Reussir.Core (translateProgToModule)

data Args = Args
    { argInputFile :: FilePath
    , argOutputFile :: FilePath
    , argOptLevel :: B.OptOption
    , argOutputTarget :: OutputTarget
    , argLogLevel :: B.LogLevel
    , argModuleName :: String
    , argTargetTriple :: Maybe String
    , argTargetCPU :: Maybe String
    , argTargetFeatures :: Maybe String
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> strArgument (metavar "INPUT" <> help "Input file")
        <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")
        <*> option
            parseOptLevel
            ( long "opt-level"
                <> short 'O'
                <> value B.OptDefault
                <> help "Optimization level (none, default, aggressive, size, tpde)"
            )
        <*> option
            parseOutputTarget
            ( long "target"
                <> short 't'
                <> value (Backend B.OutputObject)
                <> help "Output target (llvm-ir, asm, object)"
            )
        <*> option
            parseLogLevel
            ( long "log-level"
                <> short 'l'
                <> value B.LogWarning
                <> help "Log level (error, warning, info, debug, trace)"
            )
        <*> strOption
            (long "module-name" <> short 'm' <> value "main" <> help "Module name")
        <*> optional
            (strOption (long "target-triple" <> metavar "TRIPLE" <> help "Target triple (default: native)"))
        <*> optional
            (strOption (long "target-cpu" <> metavar "CPU" <> help "Target CPU (default: native)"))
        <*> optional
            (strOption (long "target-features" <> metavar "FEATURES" <> help "Target features (default: native)"))

parseOptLevel :: ReadM B.OptOption
parseOptLevel = eitherReader $ \s -> case s of
    "none" -> Right B.OptNone
    "default" -> Right B.OptDefault
    "aggressive" -> Right B.OptAggressive
    "size" -> Right B.OptSize
    "tpde" -> Right B.OptTPDE
    _ -> Left $ "Unknown optimization level: " ++ s

data OutputTarget = Backend B.OutputTarget | MLIR

parseOutputTarget :: ReadM OutputTarget
parseOutputTarget = eitherReader $ \s -> case s of
    "llvm-ir" -> Right $ Backend B.OutputLLVMIR
    "asm" -> Right $ Backend B.OutputASM
    "object" -> Right $ Backend B.OutputObject
    "mlir" -> Right MLIR
    _ -> Left $ "Unknown output target: " ++ s

parseLogLevel :: ReadM B.LogLevel
parseLogLevel = eitherReader $ \s -> case s of
    "error" -> Right B.LogError
    "warning" -> Right B.LogWarning
    "info" -> Right B.LogInfo
    "debug" -> Right B.LogDebug
    "trace" -> Right B.LogTrace
    _ -> Left $ "Unknown log level: " ++ s

main :: IO ()
main = do
    args <- execParser opts
    content <- TIO.readFile (argInputFile args)
    case runParser parseProg (argInputFile args) content of
        Left err -> do
            putStrLn (errorBundlePretty err)
            exitFailure
        Right prog -> do
            let outputTarget = argOutputTarget args
            let outputTarget' = case outputTarget of
                    MLIR -> B.OutputObject
                    Backend t -> t
            let spec =
                    TargetSpec
                        { programName = T.pack (argModuleName args)
                        , outputPath = argOutputFile args
                        , optimization = argOptLevel args
                        , outputTarget = outputTarget'
                        , logLevel = argLogLevel args
                        , moduleFilePath = argInputFile args
                        , targetTriple = T.pack <$> argTargetTriple args
                        , targetCPU = T.pack <$> argTargetCPU args
                        , targetFeatures = T.pack <$> argTargetFeatures args
                        }

            B.withReussirLogger (argLogLevel args) "reussir-compiler" $ \logger -> do
                runEff $ L.runLog "reussir-compiler" logger (toEffLogLevel (argLogLevel args)) $ do
                    irModule <- runPrim $ translateProgToModule spec prog
                    case irModule of
                        Nothing -> liftIO exitFailure
                        Just module' -> case outputTarget of
                            MLIR -> do
                                mlirText <- C.emitModuleToText module'
                                liftIO $ TIO.writeFile (argOutputFile args) mlirText
                            Backend _ ->
                                C.emitModuleToBackend module'
  where
    toEffLogLevel :: B.LogLevel -> LogLevel
    toEffLogLevel = \case
        B.LogError -> LogAttention
        B.LogWarning -> LogAttention
        B.LogInfo -> LogInfo
        B.LogDebug -> LogTrace
        B.LogTrace -> LogTrace
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Compile a Reussir file"
                <> header "reussir-compiler - Reussir Compiler"
            )
