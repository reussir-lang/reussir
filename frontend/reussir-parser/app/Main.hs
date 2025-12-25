module Main where

import Reussir.Parser.Prog
import Reussir.Parser.Types
import Reussir.Parser.Pretty (PrettyColored (..))

import Data.Text.IO qualified as T
import System.Environment
import System.Exit
import Prettyprinter
import Prettyprinter.Render.Terminal

main :: IO ()
main =
    getArgs >>= \case
        [infile] -> do
            contents <- T.readFile infile
            case parse parseProg infile contents of
                Left err -> putStrLn (errorBundlePretty err)
                Right p -> putDoc (vsep (map prettyColored p) <> line)
        _ -> do
            putStrLn "Usage: frontend <infile>"
            exitFailure
