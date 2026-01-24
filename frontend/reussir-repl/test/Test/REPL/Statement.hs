{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.Statement (tests) where

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
import Reussir.Parser.Stmt (parseStmt)

-- Foreign import for calling JIT-compiled functions
foreign import ccall "dynamic"
    callI64Func :: FunPtr (IO Int64) -> IO Int64

tests :: TestTree
tests =
    testGroup
        "Statement Processing"
        [ testGroup "Function Definitions"
            [ testCase "Simple function" testSimpleFunction
            , testCase "Function with parameters" testFunctionWithParams
            , testCase "Function with multiple parameters" testFunctionMultiParams
            , testCase "Recursive function" testRecursiveFunction
            , testCase "Multiple functions" testMultipleFunctions
            ]
        , testGroup "Parse Errors"
            [ testCase "Invalid syntax returns error" testInvalidSyntax
            ]
        , testGroup "Define and Evaluate"
            [ testCase "Define function and call with literal" testDefineAndCallLiteral
            , testCase "Define function and call with expression" testDefineAndCallExpr
            , testCase "Define multiple functions and call" testDefineMultipleAndCall
            , testCase "Define add function and evaluate" testDefineAddAndEvaluate
            , testCase "Define square function and evaluate" testDefineSquareAndEvaluate
            ]
        ]

-- Helper to parse and add a statement
parseAndAdd :: ReplState -> T.Text -> IO (Either ReplError ReplState)
parseAndAdd state input =
    case runParser parseStmt "<test>" input of
        Left _err -> return $ Left $ ParseError "Parse failed"
        Right stmt -> addDefinition state stmt

testSimpleFunction :: Assertion
testSimpleFunction = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndAdd state "fn foo() -> i64 { 42 }"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right state' -> do
            -- Counter should still be 0 (only incremented on expression eval)
            replCounter state' @?= 0
            -- Mode should be unchanged
            getReplMode state' @?= StmtMode

testFunctionWithParams :: Assertion
testFunctionWithParams = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndAdd state "fn double(x: i64) -> i64 { x * 2 }"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right _ -> return ()

testFunctionMultiParams :: Assertion
testFunctionMultiParams = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndAdd state "fn add(x: i64, y: i64) -> i64 { x + y }"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right _ -> return ()

testRecursiveFunction :: Assertion
testRecursiveFunction = do
    state <- initReplState LogWarning "<test>"
    result <- parseAndAdd state "fn factorial(n: i64) -> i64 { if n <= 1 { 1 } else { n * factorial(n - 1) } }"
    case result of
        Left err -> assertFailure $ "Expected success, got: " ++ show err
        Right _ -> return ()

testMultipleFunctions :: Assertion
testMultipleFunctions = do
    state <- initReplState LogWarning "<test>"
    
    -- Add first function
    result1 <- parseAndAdd state "fn helper(x: i64) -> i64 { x + 1 }"
    state1 <- case result1 of
        Left err -> assertFailure ("First function failed: " ++ show err) >> return state
        Right s -> return s
    
    -- Add second function
    result2 <- parseAndAdd state1 "fn main_func(x: i64) -> i64 { helper(x) * 2 }"
    case result2 of
        Left err -> assertFailure $ "Second function failed: " ++ show err
        Right _ -> return ()

testInvalidSyntax :: Assertion
testInvalidSyntax = do
    state <- initReplState LogWarning "<test>"
    -- This should fail to parse
    case runParser parseStmt "<test>" ("fn incomplete(" :: T.Text) of
        Left _err -> return ()  -- Expected parse failure
        Right stmt -> do
            -- If it somehow parses, it should fail in elaboration
            result <- addDefinition state stmt
            case result of
                Left _ -> return ()  -- Expected elaboration error
                Right _ -> assertFailure "Expected error for invalid syntax"

--------------------------------------------------------------------------------
-- Define and Evaluate Tests
--------------------------------------------------------------------------------

-- Placeholder callback for JIT
placeholderCallback :: () -> IO ByteString
placeholderCallback _ = return ""

-- Helper to define a function and then evaluate an expression
defineAndEval :: T.Text -> T.Text -> IO (Either String Int64)
defineAndEval funcDef exprText = do
    catch (defineAndEval' funcDef exprText) handleErr
  where
    handleErr :: SomeException -> IO (Either String Int64)
    handleErr e = return $ Left $ "Exception: " ++ show e

defineAndEval' :: T.Text -> T.Text -> IO (Either String Int64)
defineAndEval' funcDef exprText = do
    state <- initReplState LogWarning "<test>"
    
    -- First, parse and add the function definition
    case runParser parseStmt "<test>" funcDef of
        Left err -> return $ Left $ "Parse error (stmt): " ++ show err
        Right stmt -> do
            defResult <- addDefinition state stmt
            case defResult of
                Left err -> return $ Left $ "Definition error: " ++ show err
                Right stateWithFunc -> do
                    -- Now parse and compile the expression
                    case runParser parseExpr "<test>" exprText of
                        Left err -> return $ Left $ "Parse error (expr): " ++ show err
                        Right expr -> do
                            compileResult <- compileExpression stateWithFunc exprText expr
                            case compileResult of
                                Left err -> return $ Left $ "Compile error: " ++ show err
                                Right (moduleBytes, state', _) -> do
                                    let counter = replCounter state' - 1
                                    let funcName = "__repl_expr_" ++ show counter
                                    withJIT placeholderCallback OptDefault $ \jit -> do
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

-- Helper to define multiple functions and then evaluate
defineMultipleAndEval :: [T.Text] -> T.Text -> IO (Either String Int64)
defineMultipleAndEval funcDefs exprText = do
    catch (defineMultipleAndEval' funcDefs exprText) handleErr
  where
    handleErr :: SomeException -> IO (Either String Int64)
    handleErr e = return $ Left $ "Exception: " ++ show e

defineMultipleAndEval' :: [T.Text] -> T.Text -> IO (Either String Int64)
defineMultipleAndEval' funcDefs exprText = do
    state <- initReplState LogWarning "<test>"
    
    -- Add all function definitions
    finalState <- addAllDefs state funcDefs
    case finalState of
        Left err -> return $ Left err
        Right stateWithFuncs -> do
            -- Parse and compile the expression
            case runParser parseExpr "<test>" exprText of
                Left err -> return $ Left $ "Parse error (expr): " ++ show err
                Right expr -> do
                    compileResult <- compileExpression stateWithFuncs exprText expr
                    case compileResult of
                        Left err -> return $ Left $ "Compile error: " ++ show err
                        Right (moduleBytes, state', _) -> do
                            let counter = replCounter state' - 1
                            let funcName = "__repl_expr_" ++ show counter
                            withJIT placeholderCallback OptDefault $ \jit -> do
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
  where
    addAllDefs s [] = return $ Right s
    addAllDefs s (def:defs) = 
        case runParser parseStmt "<test>" def of
            Left err -> return $ Left $ "Parse error: " ++ show err
            Right stmt -> do
                result <- addDefinition s stmt
                case result of
                    Left err -> return $ Left $ "Definition error: " ++ show err
                    Right s' -> addAllDefs s' defs

testDefineAndCallLiteral :: Assertion
testDefineAndCallLiteral = do
    result <- defineAndEval 
        "fn constant() -> i64 { 42 }" 
        "constant()"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testDefineAndCallExpr :: Assertion
testDefineAndCallExpr = do
    result <- defineAndEval 
        "fn double(x: i64) -> i64 { x * 2 }" 
        "double(21)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testDefineMultipleAndCall :: Assertion
testDefineMultipleAndCall = do
    result <- defineMultipleAndEval 
        [ "fn helper(x: i64) -> i64 { x + 1 }"
        , "fn main_fn(x: i64) -> i64 { helper(x) * 2 }"
        ]
        "main_fn(20)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42  -- (20 + 1) * 2 = 42

testDefineAddAndEvaluate :: Assertion
testDefineAddAndEvaluate = do
    result <- defineAndEval 
        "fn add(x: i64, y: i64) -> i64 { x + y }" 
        "add(17, 25)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42  -- 17 + 25 = 42

testDefineSquareAndEvaluate :: Assertion
testDefineSquareAndEvaluate = do
    result <- defineAndEval 
        "fn square(x: i64) -> i64 { x * x }" 
        "square(7)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 49  -- 7 * 7 = 49
