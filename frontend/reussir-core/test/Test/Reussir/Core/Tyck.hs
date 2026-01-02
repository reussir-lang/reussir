{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Reussir.Core.Tyck where

import Control.Monad (forM_)
import Data.HashTable.IO qualified as H
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem.IO (stderr)
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (newIORef', readIORef')
import Effectful.State.Static.Local (runState)
import Effectful.State.Static.Local qualified as State
import Reussir.Core.Class (subsumeBound)
import Reussir.Core.Generic (newGenericVar)
import Reussir.Core.Translation (Tyck, emptyTranslationState, wellTypedExpr)
import Reussir.Core.Translation qualified as Tyck
import Reussir.Core.Tyck (checkType, inferType)
import Reussir.Core.Types.Class (Class (..))
import Reussir.Core.Types.Class qualified as Sem
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core.Types.GenericID (GenericID (..))
import Reussir.Core.Types.Record qualified as Sem
import Reussir.Core.Types.Translation (TranslationState (translationReports))
import Reussir.Core.Types.Translation qualified as Sem
import Reussir.Core.Types.Translation qualified as Tyck
import Reussir.Core.Types.Type (IntegralType (Signed))
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Repository, addDummyFile, createRepository, displayReport)
import Reussir.Parser.Expr (parseExpr)
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path (..))
import Reussir.Parser.Types.Stmt (Visibility (..))
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
        , testCase "Casting Type Inference" testCasting
        , testCase "Let-In Without Type Annotation" testLetInWithoutType
        , testCase "Let-In With Type Annotation" testLetInWithType
        , testCase "Record Access" testRecordAccess
        , testCase "Generic Record Access" testGenericRecordAccess
        , testCase "Function Call" testFuncCall
        , testCase "Function Call with Generic Hole" testGenericFuncCall
        ]

runTyck :: Repository -> (a -> Tyck.Tyck b) -> Tyck a -> IO b
runTyck repo conti tyck = runEff . runPrim $ do
    state <- emptyTranslationState "<dummy input>"
    (out, _) <- runState state $ do
        res <- inject tyck
        s <- State.get
        forM_ (translationReports s) $ \report -> do
            liftIO $ hPutStrLn stderr ""
            displayReport report repo 0 stderr
            liftIO $ hPutStrLn stderr ""
        inject $ conti res
    return out

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
    runTyck repo action $ do
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
        return $ TypedExprAndBounds typedExpr typeBound

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

testCasting :: Assertion
testCasting = do
    parseAndInferType "( 1 + 1 ) as f64" $ \expr -> do
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeFP (Sem.IEEEFloat 64)

testLetInWithoutType :: Assertion
testLetInWithoutType = do
    parseAndInferType "let x = 42 as i32; x + 1" $ \expr -> do
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeIntegral (Sem.Signed 32)

testLetInWithType :: Assertion
testLetInWithType = do
    parseAndInferType "let x : i32 = 42; x + 1" $ \expr -> do
        liftIO $ Sem.exprType (typedExpr expr) @?= Sem.TypeIntegral (Sem.Signed 32)

testRecordAccess :: Assertion
testRecordAccess = do
    let recordName = "Point"
    let recordPath = Path recordName []
    let record =
            Sem.Record
                { Sem.recordName = recordName
                , Sem.recordTyParams = []
                , Sem.recordFields = Sem.Named [("x", Sem.TypeIntegral (Sem.Signed 32), False), ("y", Sem.TypeIntegral (Sem.Signed 32), False)]
                , Sem.recordKind = Sem.StructKind
                , Sem.recordVisibility = Public
                , Sem.recordDefaultCap = Value
                }

    let input = "p.x"
    expr <- parseToExpr input

    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input

    let check res = liftIO $ do
            Sem.exprType res @?= Sem.TypeIntegral (Sem.Signed 32)

    runTyck repo check $ do
        Tyck.addRecordDefinition recordPath record
        let pointType = Sem.TypeRecord recordPath []
        Tyck.withVariable "p" Nothing pointType $ \_ -> do
            inferType expr

testGenericRecordAccess :: Assertion
testGenericRecordAccess = do
    let recordName = "Box"
    let recordPath = Path recordName []
    let gid = GenericID 0
    let record =
            Sem.Record
                { Sem.recordName = recordName
                , Sem.recordTyParams = [("T", gid)]
                , Sem.recordFields = Sem.Named [("inner", Sem.TypeGeneric gid, False)]
                , Sem.recordKind = Sem.StructKind
                , Sem.recordVisibility = Public
                , Sem.recordDefaultCap = Value
                }

    let input = "b.inner"
    expr <- parseToExpr input

    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input

    runTyck repo (\res -> liftIO $ Sem.exprType res @?= Sem.TypeIntegral (Sem.Signed 32)) $ do
        Tyck.addRecordDefinition recordPath record
        -- Box<i32>
        let boxType = Sem.TypeRecord recordPath [Sem.TypeIntegral (Sem.Signed 32)]
        Tyck.withVariable "b" Nothing boxType $ \_ -> do
            inferType expr

addFunctionDefinition :: Path -> FunctionProto -> Tyck.Tyck ()
addFunctionDefinition path proto = do
    functions <- State.gets Tyck.functions
    liftIO $ H.insert (functionProtos functions) path proto

testFuncCall :: Assertion
testFuncCall = do
    let funcName = "add"
    let funcPath = Path funcName []
    let input = "add(1, 2)"
    expr <- parseToExpr input

    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input
    let check res = liftIO $ do
            Sem.exprType res @?= Sem.TypeIntegral (Sem.Signed 32)

    runTyck repo check $ do
        funcBodyRef <- newIORef' Nothing
        let proto =
                FunctionProto
                    { funcVisibility = Public
                    , funcName = funcName
                    , funcGenerics = []
                    , funcParams = [("a", Sem.TypeIntegral (Sem.Signed 32)), ("b", Sem.TypeIntegral (Sem.Signed 32))]
                    , funcReturnType = Sem.TypeIntegral (Sem.Signed 32)
                    , funcIsRegional = False
                    , funcBody = funcBodyRef
                    , funcSpan = Nothing
                    }
        addFunctionDefinition funcPath proto
        inferType expr

testGenericFuncCall :: Assertion
testGenericFuncCall = do
    let funcName = "add"
    let funcPath = Path funcName []
    let input = "add<_>(1, 2)"
    expr <- parseToExpr input

    repository <- runEff $ createRepository []
    let repo = addDummyFile repository "<dummy input>" input
    let check res = liftIO $ do
            Sem.exprType res @?= Sem.TypeIntegral (Sem.Signed 32)

    runTyck repo check $ do
        funcBodyRef <- newIORef' Nothing
        state <- State.gets Tyck.generics
        generic <- newGenericVar "T" Nothing [Path "Num" []] state
        let proto =
                FunctionProto
                    { funcVisibility = Public
                    , funcName = funcName
                    , funcGenerics = [("T", generic)]
                    , funcParams = [("a", Sem.TypeGeneric generic), ("b", Sem.TypeGeneric generic)]
                    , funcReturnType = Sem.TypeGeneric generic
                    , funcIsRegional = False
                    , funcBody = funcBodyRef
                    , funcSpan = Nothing
                    }
        addFunctionDefinition funcPath proto
        checkType expr (Sem.TypeIntegral $ Signed 32) >>= wellTypedExpr
