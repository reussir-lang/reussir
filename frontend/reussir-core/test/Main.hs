module Main (main) where

import Test.Reussir.Core.Generic qualified as Generic
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup
            "Unit tests"
            [ testCase "placeholder" $ True @?= True
            ]
        , Generic.tests
        ]
