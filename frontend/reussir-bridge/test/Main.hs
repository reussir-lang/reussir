{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import Test.Bridge qualified as Bridge

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ Bridge.bridgeTests
        ]
