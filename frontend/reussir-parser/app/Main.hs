module Main where

import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Environment
import System.Exit
import System.IO (hIsTerminalDevice, stdout)

import Data.Text.IO qualified as T

import Reussir.Parser.Pretty (PrettyColored (..))
import Reussir.Parser.Prog
import Reussir.Parser.Types

main :: IO ()
main =
    getArgs >>= \case
        [infile] -> do
            contents <- T.readFile infile
            case parse parseProg infile contents of
                Left err -> putStrLn (errorBundlePretty err)
                Right p -> do
                    isTTy <- hIsTerminalDevice stdout
                    let doc = vsep (map prettyColored p) <> line
                    if isTTy
                        then putDoc doc
                        else putDoc (unAnnotate doc)
        _ -> do
            putStrLn "Usage: frontend <infile>"
            exitFailure
