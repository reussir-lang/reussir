module Main (main) where

import Test.Reussir.Core.Class qualified as Class
import Test.Reussir.Core.Generic qualified as Generic
import Test.Reussir.Core.Tyck qualified as Tyck
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
        , Tyck.tests
        , Class.tests
        ]
