module Main where

import Reussir.Core2 (hello)
import Test.Reussir.Core2.Class qualified as Class
import Test.Reussir.Core2.Generic qualified as Generic
import Test.Reussir.Core2.String qualified as String
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Reussir.Core2"
        [ testCase "Hello World" $
            hello @?= "Hello from Reussir.Core2"
        , Generic.tests
        , Class.tests
        , String.tests
        ]
