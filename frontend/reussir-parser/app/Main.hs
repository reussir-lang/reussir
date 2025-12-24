module Main where

import Reussir.Parser.Prog
import Reussir.Parser.Types

import Data.Text.IO qualified as T
import System.Environment
import System.Exit

main :: IO ()
main =
    getArgs >>= \case
        [infile] -> do
            contents <- T.readFile infile
            case parse parseProg infile contents of
                Left err -> putStrLn (errorBundlePretty err)
                Right p -> mapM_ print p
        _ -> do
            putStrLn "Usage: frontend <infile>"
            exitFailure
