{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.Expression (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text qualified as T
import Text.Megaparsec (runParser)
import Reussir.Bridge (LogLevel (..))
import Reussir.Core.REPL
import Reussir.Parser.Expr (parseExpr)

tests :: TestTree
tests =
    testGroup
        "Expression Compilation"
        [ testGroup "Literal Expressions"
            [ testCase "Integer literal" testIntLiteral
            , testCase "Float literal" testFloatLiteral
            , testCase "Boolean true" testBoolTrue
            , testCase "Boolean false" testBoolFalse
            ]
        , testGroup "Arithmetic Expressions"
            [ testCase "Addition" testAddition
            , testCase "Subtraction" testSubtraction
            , testCase "Multiplication" testMultiplication
            , testCase "Division" testDivision
            , testCase "Complex expression" testComplexExpr
            ]
        , testGroup "Floating Point Expressions"
            [ testCase "Float addition" testFloatAddition
            , testCase "Float multiplication" testFloatMultiplication
            ]
        , testGroup "Result Kind"
            [ testCase "Integer literal resolves to i64" testIntResultKind
            , testCase "Float literal resolves to f64" testFloatResultKind
            ]
        , testGroup "Counter Increment"
            [ testCase "Counter increments on compile" testCounterIncrement
            , testCase "Counter increments multiple times" testCounterMultiple
            ]
        ]

-- Helper to parse and compile an expression
parseAndCompile :: ReplState -> T.Text -> IO (Either ReplError (ReplState, ResultKind))
parseAndCompile state input =
    case runParser parseExpr "<test>" input of
        Left _err -> return $ Left $ ParseError "Parse failed"
        Right expr -> do
            result <- compileExpression state input expr
            case result of
                Left err -> return $ Left err
                Right (_, state', kind) -> return $ Right (state', kind)

testIntLiteral :: Assertion
testIntLiteral = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "42"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testFloatLiteral :: Assertion
testFloatLiteral = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "3.14"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultF64

testBoolTrue :: Assertion
testBoolTrue = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "true"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultBool

testBoolFalse :: Assertion
testBoolFalse = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "false"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultBool

testAddition :: Assertion
testAddition = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "1 + 2"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testSubtraction :: Assertion
testSubtraction = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "10 - 3"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testMultiplication :: Assertion
testMultiplication = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "6 * 7"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testDivision :: Assertion
testDivision = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "100 / 5"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testComplexExpr :: Assertion
testComplexExpr = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "(1 + 2) * (3 + 4)"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testFloatAddition :: Assertion
testFloatAddition = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "1.5 + 2.5"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultF64

testFloatMultiplication :: Assertion
testFloatMultiplication = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "3.14 * 2.0"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultF64

testIntResultKind :: Assertion
testIntResultKind = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "123"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultI64

testFloatResultKind :: Assertion
testFloatResultKind = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndCompile state "1.23"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (_, kind) -> kind @?= ResultF64

testCounterIncrement :: Assertion
testCounterIncrement = do
    state <- initReplState LogWarning "<test>"
    replCounter state @?= 0
    result <- parseAndCompile state "42"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right (state', _) -> replCounter state' @?= 1

testCounterMultiple :: Assertion
testCounterMultiple = do
    state <- initReplState LogWarning "<test>"
    replCounter state @?= 0
    
    -- First expression
    result1 <- parseAndCompile state "1"
    state1 <- case result1 of
        Left err -> assertFailure ("First expr failed: " ++ show err) >> return state
        Right (s, _) -> return s
    replCounter state1 @?= 1
    
    -- Second expression
    result2 <- parseAndCompile state1 "2"
    state2 <- case result2 of
        Left err -> assertFailure ("Second expr failed: " ++ show err) >> return state1
        Right (s, _) -> return s
    replCounter state2 @?= 2
    
    -- Third expression
    result3 <- parseAndCompile state2 "3"
    state3 <- case result3 of
        Left err -> assertFailure ("Third expr failed: " ++ show err) >> return state2
        Right (s, _) -> return s
    replCounter state3 @?= 3
