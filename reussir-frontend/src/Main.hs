module Main where

import Parser.Expr
import Parser.Types

import System.IO

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering   
    putStr "Enter expr > "

    input <- getLine

    case parse parseExpr "<stdin>" input of 
        Left err -> putStrLn (errorBundlePretty err)
        Right es -> print es

