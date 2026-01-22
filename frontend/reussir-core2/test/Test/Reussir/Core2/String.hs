{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core2.String where

import Data.HashTable.IO qualified as H
import Effectful (runEff)
import Reussir.Core2.Data.String (StringUniqifier (..))
import Reussir.Core2.String (allocateStrToken, mangleStringToken)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "String"
        [ testCase "Mangle String Token" $ do
            table <- H.new
            let uniqifier = StringUniqifier table
            token <- runEff $ allocateStrToken "hello world" uniqifier
            let mangled = mangleStringToken token
            mangled @?= "_RNvC22REUSSIR_STRING_LITERAL43wgb3YCIRgEErjlggbY3ZJCbeJRhIPIkNJeDnhWdtw0o"
            token' <- runEff $ allocateStrToken "hello world" uniqifier
            token' @?= token
            token'' <- runEff $ allocateStrToken "hello, world" uniqifier
            let mangled' = mangleStringToken token''
            mangled' @=? "_RNvC22REUSSIR_STRING_LITERAL43JqNctOZ6XzAD8ucTcG1jHb7jElJJNxsSINKQYrdEoXC"
        ]
