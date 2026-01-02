module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (inject, liftIO, runEff)
import Effectful.Prim (runPrim)
import Effectful.State.Static.Local (runState)
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (errorBundlePretty, runParser)

import GHC.IO.Handle.FD (stderr)
import Reussir.Core.Translation (emptyTranslationState, scanStmt)
import Reussir.Core.Tyck (checkFuncType)
import Reussir.Core.Types.Translation (TranslationState (..))
import Reussir.Diagnostic (createRepository, displayReport)
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (Identifier (..), WithSpan (..), unIdentifier)
import Reussir.Parser.Types.Stmt qualified as Syn
import System.IO (hPutStrLn)

data Args = Args
    { inputFile :: FilePath
    , funcName :: String
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> strArgument (metavar "FILE" <> help "Input file")
        <*> strArgument (metavar "FUNCTION" <> help "Function name to check")

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
            initState <- runEff $ runPrim $ emptyTranslationState (inputFile args)
            repository <- runEff $ createRepository [inputFile args]
            (result, finalState) <- runEff $ runPrim $ runState initState $ do
                -- Scan all statements
                forM_ prog $ \stmt -> inject $ scanStmt stmt

                -- Find the function
                let targetName = T.pack (funcName args)
                let findFunc (Syn.FunctionStmt f) | unIdentifier (Syn.funcName f) == targetName = Just f
                    findFunc (Syn.SpannedStmt s) = findFunc (spanValue s)
                    findFunc _ = Nothing

                let targetFunc = foldr (\stmt acc -> acc <|> findFunc stmt) Nothing prog

                case targetFunc of
                    Just f -> do
                        expr <- inject $ checkFuncType f
                        return (Just expr)
                    Nothing -> do
                        liftIO $ putStrLn "Function not found"
                        return Nothing

            mapM_ print (translationReports finalState)
            case result of
                Just expr | null (translationReports finalState) -> do
                    putStrLn "Type check succeeded without errors."
                    putStrLn $ show expr
                    exitSuccess
                _ -> do
                    forM_ (translationReports finalState) $ \report -> do
                        runEff $ displayReport report repository 0 stderr
                        hPutStrLn stderr ""
                    exitFailure
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Type check a function in a Reussir file"
                <> header "reussir-tyck - Reussir Type Checker"
            )
