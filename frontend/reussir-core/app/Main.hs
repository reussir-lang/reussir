module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (inject, liftIO, runEff)
import Effectful.Log qualified as L
import Effectful.Prim (runPrim)
import Effectful.State.Static.Local (runState)
import Log (LogLevel (..))
import Log.Backend.StandardOutput qualified as L
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (errorBundlePretty, runParser)

import Data.HashTable.IO qualified as H
import GHC.IO.Handle.FD (stderr)
import Prettyprinter (hardline)
import Prettyprinter.Render.Terminal (putDoc)
import Reussir.Bridge qualified as B
import Reussir.Core.Pretty (prettyColored)
import Reussir.Core.Translation (emptyTranslationState, scanStmt, solveAllGenerics)
import Reussir.Core.Tyck (checkFuncType)
import Reussir.Core.Types.Expr (Expr)
import Reussir.Core.Types.GenericID (GenericID)
import Reussir.Core.Types.Translation (TranslationState (..))
import Reussir.Core.Types.Type (Type)
import Reussir.Diagnostic (createRepository, displayReport)
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (Identifier (..), WithSpan (..), unIdentifier)
import Reussir.Parser.Types.Stmt qualified as Syn
import System.IO (hPutStrLn)

data Args = Args
    { inputFile :: FilePath
    , funcName :: Maybe String
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> strArgument (metavar "FILE" <> help "Input file")
        <*> optional (strArgument (metavar "FUNCTION" <> help "Function name to check"))

data Result = SingleSuccess Expr | ModuleSuccess [(GenericID, [Type])] | Failed

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
unspanStmt (Syn.SpannedStmt s) = unspanStmt (spanValue s)
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
            (result, finalState) <- L.withStdOutLogger $ \logger -> do
                runEff $ L.runLog (T.pack "reussir-tyck") logger (toEffLogLevel tyckBridgeLogLevel) $ do
                    initState <- runPrim $ emptyTranslationState tyckBridgeLogLevel (inputFile args)
                    runPrim $ runState initState $ do
                        -- Scan all statements
                        forM_ prog $ \stmt -> inject $ scanStmt stmt
                        -- Find the function
                        case funcName args of
                            Nothing -> do
                                forM_ prog $ \stmt -> do
                                    case unspanStmt stmt of
                                        Syn.FunctionStmt f -> do
                                            expr <- inject $ checkFuncType f
                                            liftIO $ putStrLn $ "Function " ++ T.unpack (unIdentifier (Syn.funcName f)) ++ " type checked successfully:"
                                            liftIO $ putDoc (prettyColored expr <> hardline)
                                            liftIO $ putStrLn ""
                                            pure ()
                                        _ -> return ()
                                slns <- inject solveAllGenerics
                                case slns of
                                    Just instances -> ModuleSuccess <$> liftIO (H.toList instances)
                                    Nothing -> pure Failed
                            Just name -> do
                                let targetName = T.pack name
                                let findFunc (Syn.FunctionStmt f) | unIdentifier (Syn.funcName f) == targetName = Just f
                                    findFunc (Syn.SpannedStmt s) = findFunc (spanValue s)
                                    findFunc _ = Nothing

                                let targetFunc = foldr (\stmt acc -> acc <|> findFunc stmt) Nothing prog

                                case targetFunc of
                                    Just f -> do
                                        expr <- inject $ checkFuncType f
                                        return (SingleSuccess expr)
                                    Nothing -> do
                                        liftIO $ putStrLn "Function not found"
                                        return Failed
            forM_ (translationReports finalState) $ \report -> do
                runEff $ displayReport report repository 0 stderr
                hPutStrLn stderr ""
            case result of
                SingleSuccess expr | null (translationReports finalState) -> do
                    putStrLn "Type check succeeded without errors."
                    putDoc (prettyColored expr <> hardline)
                    exitSuccess
                ModuleSuccess instances | null (translationReports finalState) -> do
                    forM_ instances $ \(gid, types) -> do
                        putStrLn $ "Generic " ++ show gid ++ " should be instantiated to:"
                        forM_ types $ \ty ->
                            putDoc (prettyColored ty <> hardline)
                    exitSuccess
                _ -> do
                    exitFailure
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Type check a function in a Reussir file"
                <> header "reussir-tyck - Reussir Type Checker"
            )
