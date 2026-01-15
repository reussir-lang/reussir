{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core2.String where

import Data.HashTable.IO qualified as H
import Effectful (runEff)
import Reussir.Core2.String (allocateStrToken, mangleStringToToken)
import Reussir.Core2.Types.String (StringUniqifier (..))
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
            let mangled = mangleStringToToken token
            mangled @?= "_RNvC22REUSSIR_STRING_LITERAL43_6ePC0crWWyKBhtr8ziHr7FQSE1YdqqtkbZ0Na5dOXec"
        ]
