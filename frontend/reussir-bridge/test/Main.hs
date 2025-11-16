{-# LANGUAGE OverloadedStrings #-}

import Test.Bridge qualified as Bridge
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ Bridge.bridgeTests
        ]
