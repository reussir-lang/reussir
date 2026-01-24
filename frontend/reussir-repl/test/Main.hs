{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.REPL.State qualified as State
import Test.REPL.Expression qualified as Expression
import Test.REPL.Statement qualified as Statement
import Test.REPL.TypeResolution qualified as TypeResolution
import Test.REPL.Integration qualified as Integration
import Test.Tasty

main :: IO ()
main = defaultMain tests

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
