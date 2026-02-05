module Main where

import Test.Tasty

import Test.Reussir.Core.Class qualified as Class
import Test.Reussir.Core.Generic qualified as Generic
import Test.Reussir.Core.Semi.Mangle qualified as Mangle
import Test.Reussir.Core.Semi.PatternMatch qualified as PatternMatch
import Test.Reussir.Core.String qualified as String

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Reussir.Core"
        [ Generic.tests
        , Class.tests
        , String.tests
        , Mangle.tests
        , PatternMatch.tests
        ]
