{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitSuccess)
import System.Info (os)
import Test.REPL.Expression qualified as Expression
import Test.REPL.Integration qualified as Integration
import Test.REPL.State qualified as State
import Test.REPL.Statement qualified as Statement
import Test.REPL.TypeResolution qualified as TypeResolution
import Test.Tasty

main :: IO ()
main =
    if os == "mingw32"
        then exitSuccess -- skip on windows
        else defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Reussir.REPL"
        [ State.tests
        , Expression.tests
        , Statement.tests
        , TypeResolution.tests
        , Integration.tests
        ]
