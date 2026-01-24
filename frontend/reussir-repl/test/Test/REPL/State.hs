{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.State (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Reussir.Bridge (LogLevel (..))
import Reussir.Core.REPL

tests :: TestTree
tests =
    testGroup
        "REPL State"
        [ testCase "Initialize REPL state" testInitState
        , testCase "Initial mode is StmtMode" testInitialMode
        , testCase "Set and get mode - StmtMode" testSetModeStmt
        , testCase "Set and get mode - EvalMode" testSetModeEval
        , testCase "Mode toggle" testModeToggle
        , testCase "Counter starts at 0" testCounterInit
        , testCase "Log level is preserved" testLogLevel
        , testCase "File path is preserved" testFilePath
        ]

testInitState :: Assertion
testInitState = do
    state <- initReplState LogWarning "<test>"
    -- If we get here without exception, initialization succeeded
    assertBool "REPL state initialized" True
    -- Verify the mode
    getReplMode state @?= StmtMode

testInitialMode :: Assertion
testInitialMode = do
    state <- initReplState LogWarning "<test>"
    getReplMode state @?= StmtMode

testSetModeStmt :: Assertion
testSetModeStmt = do
    state <- initReplState LogWarning "<test>"
    let state' = setReplMode StmtMode state
    getReplMode state' @?= StmtMode

testSetModeEval :: Assertion
testSetModeEval = do
    state <- initReplState LogWarning "<test>"
    let state' = setReplMode EvalMode state
    getReplMode state' @?= EvalMode

testModeToggle :: Assertion
testModeToggle = do
    state <- initReplState LogWarning "<test>"
    -- Initial mode
    getReplMode state @?= StmtMode
    -- Switch to EvalMode
    let state1 = setReplMode EvalMode state
    getReplMode state1 @?= EvalMode
    -- Switch back to StmtMode
    let state2 = setReplMode StmtMode state1
    getReplMode state2 @?= StmtMode
    -- Switch to EvalMode again
    let state3 = setReplMode EvalMode state2
    getReplMode state3 @?= EvalMode

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
