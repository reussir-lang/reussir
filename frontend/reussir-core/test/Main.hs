module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Reussir.Core.Meta qualified as Meta

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
        , Meta.tests
        ]
