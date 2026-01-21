module Main where

import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (inject, liftIO, runEff)
import Effectful.Log qualified as L
import Effectful.Prim (runPrim)
import Effectful.State.Static.Local (runState)
import GHC.IO.Handle.FD (stderr)
import Log (LogLevel (..))
import Log.Backend.StandardOutput qualified as L
import Options.Applicative
import Prettyprinter (hardline, unAnnotate)
import Prettyprinter.Render.Terminal (putDoc)
import Reussir.Bridge qualified as B
import Reussir.Core2.Data.Full.Context (FullContext (..))
import Reussir.Core2.Data.Semi.Context (SemiContext (..))
import Reussir.Core2.Full.Context (reportAllErrors)
import Reussir.Core2.Full.Conversion (convertCtx)
import Reussir.Core2.Full.Pretty (prettyColored)
import Reussir.Core2.Semi.Context (emptySemiContext, scanStmt)
import Reussir.Core2.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core2.Semi.Tyck (checkFuncType)
import Reussir.Diagnostic (createRepository, displayReport)
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stdout)
import Text.Megaparsec (errorBundlePretty, runParser)

data Args = Args
    { inputFile :: FilePath
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> strArgument (metavar "FILE" <> help "Input file")

tyckBridgeLogLevel :: B.LogLevel
tyckBridgeLogLevel = B.LogWarning

toEffLogLevel :: B.LogLevel -> LogLevel
toEffLogLevel = \case
    B.LogError -> LogAttention
    B.LogWarning -> LogAttention
    B.LogInfo -> LogInfo
    B.LogDebug -> LogTrace
    B.LogTrace -> LogTrace

unspanStmt :: Syn.Stmt -> Syn.Stmt
unspanStmt (Syn.SpannedStmt (WithSpan s _ _)) = unspanStmt s
unspanStmt stmt = stmt

main :: IO ()
main = do
    args <- execParser opts
    content <- TIO.readFile (inputFile args)
    case runParser parseProg (inputFile args) content of
        Left err -> do
            putStrLn (errorBundlePretty err)
            exitFailure
        Right prog -> do
            -- Setup translation state
            repository <- runEff $ createRepository [inputFile args]
            (result, finalSemiCtx) <- L.withStdOutLogger $ \logger -> do
                runEff $ L.runLog (T.pack "reussir-full-elab") logger (toEffLogLevel tyckBridgeLogLevel) $ do
                    -- 1. Semi Elab
                    initSemiState <- runPrim $ emptySemiContext tyckBridgeLogLevel (inputFile args)
                    ((slns), finalSemiState) <- runPrim $ runState initSemiState $ do
                        -- Scan all statements
                        forM_ prog $ \stmt -> inject $ scanStmt stmt

                        -- Elaborate all functions
                        forM_ prog $ \stmt -> do
                            case unspanStmt stmt of
                                Syn.FunctionStmt f -> do
                                    _ <- inject $ checkFuncType f
                                    return ()
                                _ -> return ()

                        -- Solve generics
                        inject solveAllGenerics

                    case slns of
                        Nothing -> return (Nothing, finalSemiState)
                        Just solutions -> do
                            -- 2. Full Conversion
                            fullCtx <- runPrim $ convertCtx finalSemiState solutions
                            return (Just fullCtx, finalSemiState)

            -- Report errors from Semi Elab
            forM_ (translationReports finalSemiCtx) $ \report -> do
                runEff $ displayReport report repository 0 stderr

            let putDoc' doc = do
                    isTTy <- liftIO $ hIsTerminalDevice stdout
                    if isTTy
                        then liftIO $ putDoc (doc <> hardline)
                        else liftIO $ putDoc (unAnnotate doc <> hardline)

            case result of
                Nothing -> do
                    hPutStrLn stderr "Generic solution failed or other errors in Semi Elab."
                    exitFailure
                Just finalFullCtx -> do
                    -- Report errors from Full Elab
                    runEff $ runPrim $ reportAllErrors finalFullCtx repository stderr

                    case ctxErrors finalFullCtx of
                        [] | null (translationReports finalSemiCtx) -> do
                            -- Pretty Print if no errors
                            putStrLn ";; Elaborated Full Records"
                            recs <- H.toList (ctxRecords finalFullCtx)
                            runEff $ runPrim $ forM_ recs $ \(_, record) -> do
                                doc <- prettyColored record
                                liftIO $ putDoc' (doc <> hardline)

                            putStrLn "\n;; Elaborated Full Functions"
                            funcs <- H.toList (ctxFunctions finalFullCtx)
                            runEff $ runPrim $ forM_ funcs $ \(_, func) -> do
                                doc <- prettyColored func
                                liftIO $ putDoc' (doc <> hardline)

                            exitSuccess
                        _ -> exitFailure
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Fully elaborate a Reussir file"
                <> header "reussir-full-elab - Reussir Full Elaborator"
            )
