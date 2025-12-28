{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Reussir.Core.Tyck where

import Control.Monad (forM_)
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem.IO (stderr)
import Effectful.State.Static.Local (runState)
import Reussir.Core.Tyck (TranslationState (translationReports), Tyck, emptyTranslationState, inferType)
import Reussir.Core.Types.Expr (Expr (exprType))
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Repository, addDummyFile, createRepository, displayReport)
import Reussir.Parser.Expr (parseExpr)
import Reussir.Parser.Types.Expr qualified as Syn
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Tyck"
        [ testCase "Simple Literal Type Inference" testSimpleLiteral
        , testCase "Float Literal Type Inference" testFloatLiteral
        , testCase "Bool Literal Type Inference" testBoolLiteral
        , testCase "String Literal Type Inference" testStringLiteral
        , testCase "Unary Operation Type Inference" testUnaryOp
        , testCase "Binary Add Type Inference" testBinaryAdd
        , testCase "Binary Sub Type Inference" testBinarySub
        , testCase "Binary Mul Type Inference" testBinaryMul
        , testCase "Binary Div Type Inference" testBinaryDiv
        , testCase "Binary Rem Type Inference" testBinaryRem
        , testCase "Comparison Type Inference" testComparison
        , testCase "Logical And Type Inference" testLogicalAnd
        , testCase "Logical Or Type Inference" testLogicalOr
        ]

runTyck :: Repository -> Tyck a -> IO a
runTyck repo tyck = runEff $ do
    state <- emptyTranslationState "<dummy input>"
    (res, s) <- runState state $ inject tyck
    forM_ (translationReports s) $ \report -> do
        liftIO $ putStrLn ""
        displayReport report repo 0 stderr
        liftIO $ putStrLn ""
    pure res

parseToExpr :: T.Text -> IO Syn.Expr
parseToExpr input = case parse parseExpr "<dummy input>" input of
    Left err -> do
        putStrLn (errorBundlePretty err)
        error $ "Parse error"
    Right expr -> pure expr

parseAndInferType :: T.Text -> IO Sem.Expr
parseAndInferType input = do
    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input
    expr <- parseToExpr input
    runTyck repo $ inferType expr

testSimpleLiteral :: Assertion
testSimpleLiteral = do
    expr <- parseAndInferType "42"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)

testFloatLiteral :: Assertion
testFloatLiteral = do
    expr <- parseAndInferType "3.14"
    exprType expr @?= (Sem.TypeFP $ Sem.IEEEFloat 64)

testBoolLiteral :: Assertion
testBoolLiteral = do
    expr <- parseAndInferType "true"
    exprType expr @?= Sem.TypeBool
    expr2 <- parseAndInferType "false"
    exprType expr2 @?= Sem.TypeBool

testStringLiteral :: Assertion
testStringLiteral = do
    expr <- parseAndInferType "\"hello\""
    exprType expr @?= Sem.TypeStr

testUnaryOp :: Assertion
testUnaryOp = do
    expr <- parseAndInferType "-42"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)
    expr2 <- parseAndInferType "!true"
    exprType expr2 @?= Sem.TypeBool

testBinaryAdd :: Assertion
testBinaryAdd = do
    expr <- parseAndInferType "1 + 2"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)
    expr2 <- parseAndInferType "1.0 + 2.0"
    exprType expr2 @?= (Sem.TypeFP $ Sem.IEEEFloat 64)

testBinarySub :: Assertion
testBinarySub = do
    expr <- parseAndInferType "1 - 2"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)
    expr2 <- parseAndInferType "1.0 - 2.0"
    exprType expr2 @?= (Sem.TypeFP $ Sem.IEEEFloat 64)

testBinaryMul :: Assertion
testBinaryMul = do
    expr <- parseAndInferType "1 * 2"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)
    expr2 <- parseAndInferType "1.0 * 2.0"
    exprType expr2 @?= (Sem.TypeFP $ Sem.IEEEFloat 64)

testBinaryDiv :: Assertion
testBinaryDiv = do
    expr <- parseAndInferType "1 / 2"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)
    expr2 <- parseAndInferType "1.0 / 2.0"
    exprType expr2 @?= (Sem.TypeFP $ Sem.IEEEFloat 64)

testBinaryRem :: Assertion
testBinaryRem = do
    expr <- parseAndInferType "1 % 2"
    exprType expr @?= (Sem.TypeIntegral $ Sem.Signed 64)

testComparison :: Assertion
testComparison = do
    expr <- parseAndInferType "1 < 2"
    exprType expr @?= Sem.TypeBool
    expr2 <- parseAndInferType "1.0 > 2.0"
    exprType expr2 @?= Sem.TypeBool
    expr3 <- parseAndInferType "1 == 2"
    exprType expr3 @?= Sem.TypeBool
    expr4 <- parseAndInferType "1.0 != 2.0"
    exprType expr4 @?= Sem.TypeBool

testLogicalAnd :: Assertion
testLogicalAnd = do
    expr <- parseAndInferType "true && false"
    exprType expr @?= Sem.TypeBool

testLogicalOr :: Assertion
testLogicalOr = do
    expr <- parseAndInferType "true || ( 1 + 1 > 5 )"
    exprType expr @?= Sem.TypeBool
