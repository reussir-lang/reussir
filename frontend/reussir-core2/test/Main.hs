module Main where

import Test.Reussir.Core2.Class qualified as Class
import Test.Reussir.Core2.Generic qualified as Generic
import Test.Reussir.Core2.Semi.Mangle qualified as Mangle
import Test.Reussir.Core2.String qualified as String
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Reussir.Core2"
        [ Generic.tests
        , Class.tests
        , String.tests
        , Mangle.tests
        ]
