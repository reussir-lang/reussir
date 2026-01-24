{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.State (tests) where

import Reussir.Bridge (LogLevel (..))
import Reussir.Core.REPL
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "REPL State"
        [ testCase "Initialize REPL state" testInitState
        , testCase "Counter starts at 0" testCounterInit
        , testCase "Log level is preserved" testLogLevel
        , testCase "File path is preserved" testFilePath
        ]

testInitState :: Assertion
testInitState = do
    state <- initReplState LogWarning "<test>"
    -- If we get here without exception, initialization succeeded
    assertBool "REPL state initialized" True

testCounterInit :: Assertion
testCounterInit = do
    state <- initReplState LogWarning "<test>"
    replCounter state @?= 0

testLogLevel :: Assertion
testLogLevel = do
    stateWarn <- initReplState LogWarning "<test>"
    replLogLevel stateWarn @?= LogWarning

    stateDebug <- initReplState LogDebug "<test2>"
    replLogLevel stateDebug @?= LogDebug

    stateError <- initReplState LogError "<test3>"
    replLogLevel stateError @?= LogError

testFilePath :: Assertion
testFilePath = do
    state1 <- initReplState LogWarning "<repl>"
    replFilePath state1 @?= "<repl>"

    state2 <- initReplState LogWarning "/tmp/test.rss"
    replFilePath state2 @?= "/tmp/test.rss"
