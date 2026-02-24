module Main where

import Test.Tasty
import Test.Tasty.Hspec (testSpec)

import Test.Reussir.Core.Class qualified as Class
import Test.Reussir.Core.Generic qualified as Generic
import Test.Reussir.Core.Semi.Mangle qualified as Mangle
import Test.Reussir.Core.Semi.PatternMatch qualified as PatternMatch
import Test.Reussir.Core.Semi.TyckSpec qualified as TyckSpec
import Test.Reussir.Core.String qualified as String

main :: IO ()
main = do
    tyckSpec <- testSpec "Reussir.Core.Semi.Tyck" TyckSpec.spec
    defaultMain (tests tyckSpec)

tests :: TestTree -> TestTree
tests tyckSpec =
    testGroup
        "Reussir.Core"
        [ Generic.tests
        , Class.tests
        , String.tests
        , Mangle.tests
        , PatternMatch.tests
        , tyckSpec
        ]
