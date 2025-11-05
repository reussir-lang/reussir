module Main where

import Parser.Prog
import Parser.Types

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \case 
    [infile] -> do 
        contents <- readFile infile
        case parse parseProg infile contents of 
            Left err -> putStrLn (errorBundlePretty err)
            Right p  -> mapM_ print p
    _ -> do 
        putStrLn "Usage: frontend <infile>"
        exitFailure
