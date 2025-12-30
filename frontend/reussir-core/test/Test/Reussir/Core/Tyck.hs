{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Reussir.Core.Tyck where

import Control.Monad (forM_)
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem.IO (stderr)
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (readIORef')
import Effectful.State.Static.Local (runState)
import Effectful.State.Static.Local qualified as State
import Reussir.Core.Class (subsumeBound)
import Reussir.Core.Tyck (TranslationState (translationReports), Tyck, emptyTranslationState, inferType)
import Reussir.Core.Tyck qualified as Sem
import Reussir.Core.Tyck qualified as Tyck
import Reussir.Core.Types.Class (Class (..))
import Reussir.Core.Types.Class qualified as Sem
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Repository, addDummyFile, createRepository, displayReport)
import Reussir.Parser.Expr (parseExpr)
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path (..))
import System.IO (hPutStrLn)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
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
runTyck repo tyck = runEff . runPrim $ do
    state <- emptyTranslationState "<dummy input>"
    (res, s) <- runState state $ inject tyck
    forM_ (translationReports s) $ \report -> do
        liftIO $ hPutStrLn stderr ""
        displayReport report repo 0 stderr
        liftIO $ hPutStrLn stderr ""
    pure res

parseToExpr :: T.Text -> IO Syn.Expr
parseToExpr input = case parse parseExpr "<dummy input>" input of
    Left err -> do
        putStrLn (errorBundlePretty err)
        error $ "Parse error"
    Right expr -> pure expr

data TypedExprAndBounds = TypedExprAndBounds
    { typedExpr :: Sem.Expr
    , typeBound :: Sem.TypeBound
    }
    deriving (Show)

parseAndInferType :: T.Text -> (TypedExprAndBounds -> Tyck.Tyck a) -> IO a
parseAndInferType input action = do
    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input
    expr <- parseToExpr input
    runTyck repo $ do
        typedExpr <- inferType expr
        ty <- Tyck.force $ Sem.exprType typedExpr
        typeBound <- case ty of
            Sem.TypeHole holeID -> do
                (_, state) <- Tyck.findHoleUnifState holeID
                state' <- readIORef' state
                case state' of
                    Sem.UnSolvedUFRoot _ bound -> pure bound
                    _ -> mempty
            _ -> mempty
        action $ TypedExprAndBounds typedExpr typeBound

shouldSubsumeBound :: Sem.TypeBound -> Sem.TypeBound -> Tyck.Tyck ()
shouldSubsumeBound a b = do
    dag <- State.gets Tyck.typeClassDAG
    subsume' <- subsumeBound dag a b
    liftIO $ assertBool (show a ++ " should subsume " ++ show b) subsume'

testSimpleLiteral :: Assertion
testSimpleLiteral = parseAndInferType "42" $ \expr -> do
    typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]

testFloatLiteral :: Assertion
testFloatLiteral = parseAndInferType "3.1415926" $ \expr -> do
    typeBound expr `shouldSubsumeBound` [Class (Path "FloatingPoint" [])]

testBoolLiteral :: Assertion
testBoolLiteral = do
    parseAndInferType "true" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool
    parseAndInferType "false" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool

testStringLiteral :: Assertion
testStringLiteral = parseAndInferType "\"hello\"" $ \expr ->
    liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeStr

testUnaryOp :: Assertion
testUnaryOp = do
    parseAndInferType "-42" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]
    parseAndInferType "!true" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool

testBinaryAdd :: Assertion
testBinaryAdd = do
    parseAndInferType "1 + 2" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]
    parseAndInferType "1.0 + 2.0" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "FloatingPoint" [])]

testBinarySub :: Assertion
testBinarySub = do
    parseAndInferType "1 - 2" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]
    parseAndInferType "1.0 - 2.0" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "FloatingPoint" [])]

testBinaryMul :: Assertion
testBinaryMul = do
    parseAndInferType "1 * 2" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]
    parseAndInferType "1.0 * 2.0" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "FloatingPoint" [])]

testBinaryDiv :: Assertion
testBinaryDiv = do
    parseAndInferType "1 / 2" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]
    parseAndInferType "1.0 / 2.0" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "FloatingPoint" [])]

testBinaryRem :: Assertion
testBinaryRem = do
    parseAndInferType "1 % 2" $ \expr ->
        typeBound expr `shouldSubsumeBound` [Class (Path "Integral" [])]

testComparison :: Assertion
testComparison = do
    parseAndInferType "1 < 2" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool
    parseAndInferType "1.0 > 2.0" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool
    parseAndInferType "1 == 2" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool
    parseAndInferType "1.0 != 2.0" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool

testLogicalAnd :: Assertion
testLogicalAnd = do
    parseAndInferType "true && false" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool

testLogicalOr :: Assertion
testLogicalOr = do
    parseAndInferType "true || ( 1 + 1 > 5 )" $ \expr ->
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeBool
