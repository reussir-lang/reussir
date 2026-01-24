{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.Integration (tests) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Foreign (FunPtr, nullPtr)
import Foreign.Ptr (castPtrToFunPtr)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (runParser)
import Reussir.Bridge (LogLevel (..), OptOption (..), addModule, lookupSymbol, withJIT)
import Reussir.Core.REPL
import Reussir.Parser.Expr (parseExpr)

-- Foreign import for calling JIT-compiled functions
foreign import ccall "dynamic"
    callI64Func :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "dynamic"
    callF64Func :: FunPtr (IO Double) -> IO Double

tests :: TestTree
tests =
    testGroup
        "Integration Tests"
        [ testGroup "JIT Execution - Integers"
            [ testCase "Execute integer literal" testExecIntLiteral
            , testCase "Execute integer addition" testExecIntAddition
            , testCase "Execute integer multiplication" testExecIntMultiplication
            , testCase "Execute complex integer expression" testExecComplexInt
            , testCase "Execute negative result" testExecNegative
            ]
        , testGroup "JIT Execution - Floating Point"
            [ testCase "Execute float literal" testExecFloatLiteral
            , testCase "Execute float addition" testExecFloatAddition
            , testCase "Execute float multiplication" testExecFloatMultiplication
            ]
        , testGroup "State Persistence"
            [ testCase "Multiple expressions share state" testMultipleExpressions
            , testCase "Counter persists across expressions" testCounterPersistence
            ]
        ]

-- Placeholder callback for JIT
placeholderCallback :: () -> IO ByteString
placeholderCallback _ = return ""

-- Helper to compile and execute an integer expression
compileAndExecInt :: T.Text -> IO (Either String Int64)
compileAndExecInt input = do
    catch (compileAndExecInt' input) handleErr
  where
    handleErr :: SomeException -> IO (Either String Int64)
    handleErr e = return $ Left $ "Exception: " ++ show e

compileAndExecInt' :: T.Text -> IO (Either String Int64)
compileAndExecInt' input = do
    state <- initReplState LogWarning "<test>"
    case runParser parseExpr "<test>" input of
        Left err -> return $ Left $ "Parse error: " ++ show err
        Right expr -> do
            result <- compileExpression state input expr
            case result of
                Left err -> return $ Left $ "Compile error: " ++ show err
                Right (moduleBytes, state', _) -> do
                    let counter = replCounter state' - 1
                    let funcName = "__repl_expr_" ++ show counter
                    withJIT placeholderCallback OptTPDE LogInfo $ \jit -> do
                        flag <- addModule jit moduleBytes
                        if flag
                            then do
                                sym <- lookupSymbol jit (fromString funcName) False
                                if sym /= nullPtr
                                    then do
                                        val <- callI64Func (castPtrToFunPtr sym)
                                        return $ Right val
                                    else return $ Left $ "Symbol not found: " ++ funcName
                            else return $ Left "Failed to add module"

-- Helper to compile and execute a float expression
compileAndExecFloat :: T.Text -> IO (Either String Double)
compileAndExecFloat input = do
    catch (compileAndExecFloat' input) handleErr
  where
    handleErr :: SomeException -> IO (Either String Double)
    handleErr e = return $ Left $ "Exception: " ++ show e

compileAndExecFloat' :: T.Text -> IO (Either String Double)
compileAndExecFloat' input = do
    state <- initReplState LogWarning "<test>"
    case runParser parseExpr "<test>" input of
        Left err -> return $ Left $ "Parse error: " ++ show err
        Right expr -> do
            result <- compileExpression state input expr
            case result of
                Left err -> return $ Left $ "Compile error: " ++ show err
                Right (moduleBytes, state', _) -> do
                    let counter = replCounter state' - 1
                    let funcName = "__repl_expr_" ++ show counter
                    withJIT placeholderCallback OptTPDE LogInfo $ \jit -> do
                        flag <- addModule jit moduleBytes
                        if flag
                            then do
                                sym <- lookupSymbol jit (fromString funcName) False
                                if sym /= nullPtr
                                    then do
                                        val <- callF64Func (castPtrToFunPtr sym)
                                        return $ Right val
                                    else return $ Left $ "Symbol not found: " ++ funcName
                            else return $ Left "Failed to add module"

testExecIntLiteral :: Assertion
testExecIntLiteral = do
    result <- compileAndExecInt "42"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testExecIntAddition :: Assertion
testExecIntAddition = do
    result <- compileAndExecInt "1 + 2"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 3

testExecIntMultiplication :: Assertion
testExecIntMultiplication = do
    result <- compileAndExecInt "6 * 7"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testExecComplexInt :: Assertion
testExecComplexInt = do
    result <- compileAndExecInt "(1 + 2) * (3 + 4)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 21

testExecNegative :: Assertion
testExecNegative = do
    result <- compileAndExecInt "5 - 10"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= (-5)

testExecFloatLiteral :: Assertion
testExecFloatLiteral = do
    result <- compileAndExecFloat "3.14"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float value close to 3.14" (abs (val - 3.14) < 0.001)

testExecFloatAddition :: Assertion
testExecFloatAddition = do
    result <- compileAndExecFloat "1.5 + 2.5"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float sum close to 4.0" (abs (val - 4.0) < 0.001)

testExecFloatMultiplication :: Assertion
testExecFloatMultiplication = do
    result <- compileAndExecFloat "3.14 * 2.0"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float product close to 6.28" (abs (val - 6.28) < 0.001)

testMultipleExpressions :: Assertion
testMultipleExpressions = do
    -- Execute multiple expressions in sequence
    result1 <- compileAndExecInt "10"
    case result1 of
        Left err -> assertFailure $ "First expression failed: " ++ err
        Right val -> val @?= 10
    
    result2 <- compileAndExecInt "20 + 5"
    case result2 of
        Left err -> assertFailure $ "Second expression failed: " ++ err
        Right val -> val @?= 25
    
    result3 <- compileAndExecInt "3 * 3 * 3"
    case result3 of
        Left err -> assertFailure $ "Third expression failed: " ++ err
        Right val -> val @?= 27

testCounterPersistence :: Assertion
testCounterPersistence = do
    state <- initReplState LogWarning "<test>"
    replCounter state @?= 0
    
    -- First expression
    case runParser parseExpr "<test>" ("1" :: T.Text) of
        Left _ -> assertFailure "Parse failed"
        Right expr1 -> do
            result1 <- compileExpression state "1" expr1
            case result1 of
                Left err -> assertFailure $ show err
                Right (_, state1, _) -> do
                    replCounter state1 @?= 1
                    
                    -- Second expression using updated state
                    case runParser parseExpr "<test>" ("2" :: T.Text) of
                        Left _ -> assertFailure "Parse failed"
                        Right expr2 -> do
                            result2 <- compileExpression state1 "2" expr2
                            case result2 of
                                Left err -> assertFailure $ show err
                                Right (_, state2, _) -> do
                                    replCounter state2 @?= 2
