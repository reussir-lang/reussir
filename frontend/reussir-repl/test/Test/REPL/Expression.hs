{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.Expression (tests) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Word (Word8, Word64)
import Foreign (FunPtr, Ptr, Storable (..), alloca, nullPtr)
import Foreign.C.String (peekCStringLen)
import Foreign.Ptr (castPtrToFunPtr, wordPtrToPtr)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (runParser)
import Reussir.Bridge (LogLevel (..), OptOption (..), addModule, lookupSymbol, withJIT)
import Reussir.Core.REPL
import Reussir.Parser.Expr (parseExpr)

-- Foreign imports for calling JIT-compiled functions
foreign import ccall "dynamic"
    callI64Func :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "dynamic"
    callF64Func :: FunPtr (IO Double) -> IO Double

foreign import ccall "dynamic"
    callBoolFunc :: FunPtr (IO Word8) -> IO Word8

-- | Representation of the str type from Reussir (ptr, len pair)
data StrResult = StrResult
    { strResultPtr :: {-# UNPACK #-} !Word64
    , strResultLen :: {-# UNPACK #-} !Word64
    }

instance Storable StrResult where
    sizeOf _ = 16
    alignment _ = 8
    peek p = do
        ptr <- peekByteOff p 0
        len <- peekByteOff p 8
        return $ StrResult ptr len
    poke p (StrResult ptr len) = do
        pokeByteOff p 0 ptr
        pokeByteOff p 8 len

-- | C helper function to call a JIT function returning a str type
foreign import capi "Reussir/Bridge.h reussir_bridge_call_str_func"
    c_reussir_bridge_call_str_func :: Ptr () -> Ptr StrResult -> IO ()

-- Placeholder callback for JIT
placeholderCallback :: () -> IO ByteString
placeholderCallback _ = return ""

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
        , testGroup "String Literal Expressions"
            [ testCase "Simple string literal" testSimpleStringLiteral
            , testCase "Empty string literal" testEmptyStringLiteral
            , testCase "String with spaces" testStringWithSpaces
            , testCase "String with special chars" testStringWithSpecialChars
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

-- Helper to compile and execute a boolean expression
compileAndExecBool :: T.Text -> IO (Either String Bool)
compileAndExecBool input = do
    catch (compileAndExecBool' input) handleErr
  where
    handleErr :: SomeException -> IO (Either String Bool)
    handleErr e = return $ Left $ "Exception: " ++ show e

compileAndExecBool' :: T.Text -> IO (Either String Bool)
compileAndExecBool' input = do
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
                                        val <- callBoolFunc (castPtrToFunPtr sym)
                                        return $ Right (val /= 0)
                                    else return $ Left $ "Symbol not found: " ++ funcName
                            else return $ Left "Failed to add module"

-- Helper to compile and execute a string expression, returning the actual string value
compileAndExecStr :: T.Text -> IO (Either String String)
compileAndExecStr input = do
    catch (compileAndExecStr' input) handleErr
  where
    handleErr :: SomeException -> IO (Either String String)
    handleErr e = return $ Left $ "Exception: " ++ show e

compileAndExecStr' :: T.Text -> IO (Either String String)
compileAndExecStr' input = do
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
                                        strVal <- alloca $ \resultPtr -> do
                                            c_reussir_bridge_call_str_func sym resultPtr
                                            StrResult ptrWord lenWord <- peek resultPtr
                                            let strPtrVal = wordPtrToPtr (fromIntegral ptrWord)
                                            let strLenVal = fromIntegral lenWord
                                            peekCStringLen (strPtrVal, strLenVal)
                                        return $ Right strVal
                                    else return $ Left $ "Symbol not found: " ++ funcName
                            else return $ Left "Failed to add module"

testIntLiteral :: Assertion
testIntLiteral = do
    result <- compileAndExecInt "42"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testFloatLiteral :: Assertion
testFloatLiteral = do
    result <- compileAndExecFloat "3.14"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float close to 3.14" (abs (val - 3.14) < 0.001)

testBoolTrue :: Assertion
testBoolTrue = do
    result <- compileAndExecBool "true"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= True

testBoolFalse :: Assertion
testBoolFalse = do
    result <- compileAndExecBool "false"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= False

testAddition :: Assertion
testAddition = do
    result <- compileAndExecInt "1 + 2"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 3

testSubtraction :: Assertion
testSubtraction = do
    result <- compileAndExecInt "10 - 3"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 7

testMultiplication :: Assertion
testMultiplication = do
    result <- compileAndExecInt "6 * 7"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 42

testDivision :: Assertion
testDivision = do
    result <- compileAndExecInt "100 / 5"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 20

testComplexExpr :: Assertion
testComplexExpr = do
    result <- compileAndExecInt "(1 + 2) * (3 + 4)"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 21

testFloatAddition :: Assertion
testFloatAddition = do
    result <- compileAndExecFloat "1.5 + 2.5"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float sum close to 4.0" (abs (val - 4.0) < 0.001)

testFloatMultiplication :: Assertion
testFloatMultiplication = do
    result <- compileAndExecFloat "3.14 * 2.0"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float product close to 6.28" (abs (val - 6.28) < 0.001)

testIntResultKind :: Assertion
testIntResultKind = do
    result <- compileAndExecInt "123"
    case result of
        Left err -> assertFailure err
        Right val -> val @?= 123

testFloatResultKind :: Assertion
testFloatResultKind = do
    result <- compileAndExecFloat "1.23"
    case result of
        Left err -> assertFailure err
        Right val -> assertBool "Float close to 1.23" (abs (val - 1.23) < 0.001)

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

-- String literal tests

testSimpleStringLiteral :: Assertion
testSimpleStringLiteral = do
    result <- compileAndExecStr "\"hello\""
    case result of
        Left err -> assertFailure err
        Right val -> val @?= "hello"

testEmptyStringLiteral :: Assertion
testEmptyStringLiteral = do
    result <- compileAndExecStr "\"\""
    case result of
        Left err -> assertFailure err
        Right val -> val @?= ""

testStringWithSpaces :: Assertion
testStringWithSpaces = do
    result <- compileAndExecStr "\"hello world\""
    case result of
        Left err -> assertFailure err
        Right val -> val @?= "hello world"

testStringWithSpecialChars :: Assertion
testStringWithSpecialChars = do
    result <- compileAndExecStr "\"hello\\nworld\""
    case result of
        Left err -> assertFailure err
        Right val -> val @?= "hello\nworld"