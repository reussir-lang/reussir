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
import Reussir.Core.Data.Full.Context (FullContext (..))
import Reussir.Core.Data.Semi.Context (SemiContext (..))
import Reussir.Core.Data.Semi.Function (FunctionTable (..))
import Reussir.Core.Full.Context (reportAllErrors)
import Reussir.Core.Full.Conversion (convertCtx)
import Reussir.Core.Full.Pretty qualified as Full
import Reussir.Core.Semi.Context (emptySemiContext, scanStmt)
import Reussir.Core.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core.Semi.Pretty qualified as Semi
import Reussir.Core.Semi.Tyck (checkFuncType)
import Reussir.Diagnostic (createRepository, displayReport)
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stdout)
import Text.Megaparsec (errorBundlePretty, runParser)

data ElabMode = SemiMode | FullMode
    deriving (Eq, Show)

data Args = Args
    { elabMode :: ElabMode
    , inputFile :: FilePath
    }

elabModeReader :: ReadM ElabMode
elabModeReader = eitherReader $ \s ->
    case s of
        "semi" -> Right SemiMode
        "full" -> Right FullMode
        _ -> Left $ "Invalid mode '" ++ s ++ "'. Expected 'semi' or 'full'."

argsParser :: Parser Args
argsParser =
    Args
        <$> option
            elabModeReader
            ( long "mode"
                <> metavar "MODE"
                <> help "Elaboration mode: 'semi' or 'full'"
            )
        <*> strArgument (metavar "FILE" <> help "Input file")

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
        Right prog -> case elabMode args of
            SemiMode -> runSemiElab args prog
            FullMode -> runFullElab args prog
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Elaborate a Reussir file"
                <> header "reussir-elab - Reussir Elaborator"
            )

runSemiElab :: Args -> [Syn.Stmt] -> IO ()
runSemiElab args prog = do
    repository <- runEff $ createRepository [inputFile args]
    ((), finalState) <- L.withStdOutLogger $ \logger -> do
        runEff $ L.runLog (T.pack "reussir-elab") logger (toEffLogLevel tyckBridgeLogLevel) $ do
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
                                doc <- Semi.prettyColored ty
                                liftIO $ putDoc' (doc <> hardline)
                    Nothing -> pure ()

                liftIO $ putStrLn ";; Elaborated Records"
                knownRecs <- State.gets knownRecords
                recs <- liftIO $ H.toList knownRecs
                forM_ recs $ \(_, record) -> do
                    doc <- Semi.prettyColored record
                    liftIO $ putDoc' (doc <> hardline)

                liftIO $ putStrLn "\n;; Elaborated Functions"
                funcsTbl <- State.gets functions
                funcs <- liftIO $ H.toList (functionProtos funcsTbl)
                forM_ funcs $ \(_, proto) -> do
                    doc <- Semi.prettyColored proto
                    liftIO $ putDoc' (doc <> hardline)

    forM_ (translationReports finalState) $ \report -> do
        runEff $ displayReport report repository 0 stderr
        hPutStrLn stderr ""

    if null (translationReports finalState)
        then exitSuccess
        else exitFailure

runFullElab :: Args -> [Syn.Stmt] -> IO ()
runFullElab args prog = do
    repository <- runEff $ createRepository [inputFile args]
    (result, finalSemiCtx) <- L.withStdOutLogger $ \logger -> do
        runEff $ L.runLog (T.pack "reussir-elab") logger (toEffLogLevel tyckBridgeLogLevel) $ do
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
                        doc <- Full.prettyColored record
                        liftIO $ putDoc' (doc <> hardline)

                    putStrLn "\n;; Elaborated Full Functions"
                    funcs <- H.toList (ctxFunctions finalFullCtx)
                    runEff $ runPrim $ forM_ funcs $ \(_, func) -> do
                        doc <- Full.prettyColored func
                        liftIO $ putDoc' (doc <> hardline)

                    exitSuccess
                _ -> exitFailure
