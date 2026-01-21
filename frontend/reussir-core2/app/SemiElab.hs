module Main where

import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (inject, liftIO, runEff)
import Effectful.Log qualified as L
import Effectful.Prim (runPrim)
import Effectful.State.Static.Local (runState)
import Effectful.State.Static.Local qualified as State
import GHC.IO.Handle.FD (stderr)
import Log (LogLevel (..))
import Log.Backend.StandardOutput qualified as L
import Options.Applicative
import Prettyprinter (hardline, unAnnotate)
import Prettyprinter.Render.Terminal (putDoc)
import Reussir.Bridge qualified as B
import Reussir.Core2.Data.Semi.Context (SemiContext (..))
import Reussir.Core2.Data.Semi.Function (FunctionTable (..))
import Reussir.Core2.Semi.Context (emptySemiContext, scanStmt)
import Reussir.Core2.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core2.Semi.Pretty (prettyColored)
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
            ((), finalState) <- L.withStdOutLogger $ \logger -> do
                runEff $ L.runLog (T.pack "reussir-tyck") logger (toEffLogLevel tyckBridgeLogLevel) $ do
                    initState <- runPrim $ emptySemiContext tyckBridgeLogLevel (inputFile args)
                    runPrim $ runState initState $ do
                        -- Scan all statements
                        forM_ prog $ \stmt -> inject $ scanStmt stmt

                        -- Elaborate all functions
                        forM_ prog $ \stmt -> do
                            case unspanStmt stmt of
                                Syn.FunctionStmt f -> do
                                    _ <- inject $ checkFuncType f
                                    return ()
                                _ -> return ()

                        let putDoc' doc = do
                                isTTy <- liftIO $ hIsTerminalDevice stdout
                                if isTTy
                                    then liftIO $ putDoc (doc <> hardline)
                                    else liftIO $ putDoc (unAnnotate doc <> hardline)

                        -- Solve generics
                        slns <- inject solveAllGenerics
                        case slns of
                            Just instances -> do
                                liftIO $ putStrLn "\n;; Instantiated Generics"
                                instList <- liftIO $ H.toList instances
                                forM_ instList $ \(gid, types) -> do
                                    liftIO $ putStrLn $ "Generic " ++ show gid ++ " should be instantiated to:"
                                    forM_ types $ \ty -> do
                                        doc <- prettyColored ty
                                        liftIO $ putDoc' (doc <> hardline)
                            Nothing -> pure ()

                        liftIO $ putStrLn ";; Elaborated Records"
                        knownRecs <- State.gets knownRecords
                        recs <- liftIO $ H.toList knownRecs
                        forM_ recs $ \(_, record) -> do
                            doc <- prettyColored record
                            liftIO $ putDoc' (doc <> hardline)

                        liftIO $ putStrLn "\n;; Elaborated Functions"
                        funcsTbl <- State.gets functions
                        funcs <- liftIO $ H.toList (functionProtos funcsTbl)
                        forM_ funcs $ \(_, proto) -> do
                            doc <- prettyColored proto
                            liftIO $ putDoc' (doc <> hardline)

            forM_ (translationReports finalState) $ \report -> do
                runEff $ displayReport report repository 0 stderr
                hPutStrLn stderr ""

            if null (translationReports finalState)
                then exitSuccess
                else exitFailure
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Type check a function in a Reussir file"
                <> header "reussir-tyck - Reussir Type Checker"
            )
