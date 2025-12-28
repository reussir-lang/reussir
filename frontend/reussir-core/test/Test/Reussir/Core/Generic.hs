{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.Reussir.Core.Generic where

import Data.HashTable.IO qualified as H
import Data.List (sortOn)
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Effectful
import Effectful.Prim
import Reussir.Core.Generic
import Reussir.Core.Types.Type
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Generic"
        [ testCase "detectGrowingCycles - Graph 1 (Growing Cycle)" testGraph1
        , testCase "detectGrowingCycles - Graph 2 (Non-Growing Cycle)" testGraph2
        , testCase "detectGrowingCycles - Graph 3 (Complex Growing Cycle)" testGraph3
        , testCase "solveGeneric - Graph 4 (Simple Flow)" testGraph4
        , testCase "solveGeneric - Graph 5 (Ctor Flow)" testGraph5
        , testCase "solveGeneric - Graph 6 (Cycle)" testGraph6
        , testCase "solveGeneric - Graph 7 (Multiple Subs)" testGraph7
        , testCase "solveGeneric - Graph 8 (Growing Cycle)" testGraph8
        , testCase "solveGeneric - Graph 9 (Consistent Instantiation)" testGraph9
        , testCase "solveGeneric - Graph 10 (Nested Pairs)" testGraph10
        ]

runGeneric :: Eff '[Prim, IOE] a -> IO a
runGeneric = runEff . runPrim

mkPath :: String -> Path
mkPath s = Path (Identifier (T.pack s)) []

mkTypeExpr :: String -> [Type] -> Type
mkTypeExpr s args = TypeExpr (mkPath s) args

mkBasic :: String -> Type
mkBasic s = mkTypeExpr s []

testGraph1 :: Assertion
testGraph1 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B", "C", "D"]
    let [a, b, c, d] = vars

    addDirectLink a b state
    addCtorLink b c (mkTypeExpr "R" [TypeGeneric b]) state
    addDirectLink c d state
    addDirectLink d b state

    res <- detectGrowingCycles state
    liftIO $ assertBool "Should detect growing cycle" (isJust res)

testGraph2 :: Assertion
testGraph2 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["X", "Y", "Z"]
    let [x, y, z] = vars

    addCtorLink x y (TypeClosure [mkBasic "Int"] (TypeGeneric x)) state
    addDirectLink y z state
    addDirectLink z y state

    res <- detectGrowingCycles state
    liftIO $ assertBool "Should NOT detect growing cycle" (not (isJust res))

testGraph3 :: Assertion
testGraph3 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["1", "2", "3", "4"]
    let [v1, v2, v3, v4] = vars

    addDirectLink v1 v2 state
    addDirectLink v2 v3 state
    addCtorLink v3 v4 (mkTypeExpr "S" [TypeGeneric v3]) state
    addDirectLink v4 v2 state

    res <- detectGrowingCycles state
    liftIO $ assertBool "Should detect growing cycle" (isJust res)

testGraph4 :: Assertion
testGraph4 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B"]
    let [a, b] = vars

    addDirectLink a b state
    addConcreteFlow a (mkBasic "Int") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solA <- liftIO $ H.lookup sol a
    solB <- liftIO $ H.lookup sol b
    liftIO $ solA @?= Just [mkBasic "Int"]
    liftIO $ solB @?= Just [mkBasic "Int"]

testGraph5 :: Assertion
testGraph5 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B"]
    let [a, b] = vars

    addCtorLink a b (mkTypeExpr "List" [TypeGeneric a]) state
    addConcreteFlow a (mkBasic "Int") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solA <- liftIO $ H.lookup sol a
    solB <- liftIO $ H.lookup sol b
    liftIO $ solA @?= Just [mkBasic "Int"]
    liftIO $ solB @?= Just [mkTypeExpr "List" [mkBasic "Int"]]

testGraph6 :: Assertion
testGraph6 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B"]
    let [a, b] = vars

    addDirectLink a b state
    addDirectLink b a state
    addConcreteFlow a (mkBasic "Int") state
    addConcreteFlow b (mkBasic "Bool") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solA <- liftIO $ H.lookup sol a
    solB <- liftIO $ H.lookup sol b
    let expected = sortOn show [mkBasic "Int", mkBasic "Bool"]
    liftIO $ fmap (sortOn show) solA @?= Just expected
    liftIO $ fmap (sortOn show) solB @?= Just expected

testGraph7 :: Assertion
testGraph7 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B"]
    let [a, b] = vars

    addCtorLink a b (mkTypeExpr "Pair" [TypeGeneric a, TypeGeneric a]) state
    addConcreteFlow a (mkBasic "Int") state
    addConcreteFlow a (mkBasic "Bool") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solB <- liftIO $ H.lookup sol b
    let expected = (sortOn show) [mkTypeExpr "Pair" [mkBasic "Int", mkBasic "Int"], mkTypeExpr "Pair" [mkBasic "Bool", mkBasic "Bool"]]
    liftIO $ fmap (sortOn show) solB @?= Just expected

testGraph8 :: Assertion
testGraph8 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A"]
    let [a] = vars

    addCtorLink a a (mkTypeExpr "List" [TypeGeneric a]) state
    addConcreteFlow a (mkBasic "Int") state

    res <- solveGeneric state
    liftIO $ assertBool "Should NOT have solution (growing cycle)" (not (isJust res))

testGraph9 :: Assertion
testGraph9 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B", "C"]
    let [a, b, c] = vars

    addDirectLink b a state
    addCtorLink a c (mkTypeExpr "Pair" [TypeGeneric a, TypeGeneric b]) state
    addCtorLink b c (mkTypeExpr "Pair" [TypeGeneric a, TypeGeneric b]) state

    addConcreteFlow a (mkBasic "Int") state
    addConcreteFlow b (mkBasic "Bool") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solC <- liftIO $ H.lookup sol c
    let expected = (sortOn show) [mkTypeExpr "Pair" [mkBasic "Int", mkBasic "Bool"], mkTypeExpr "Pair" [mkBasic "Bool", mkBasic "Bool"]]
    liftIO $ fmap (sortOn show) solC @?= Just expected

testGraph10 :: Assertion
testGraph10 = runGeneric $ do
    state <- emptyGenericState
    vars <- mapM (\n -> newGenericVar n Nothing [] state) ["A", "B", "C", "D"]
    let [a, b, c, d] = vars

    addDirectLink b a state
    addCtorLink a c (mkTypeExpr "Pair" [TypeGeneric a, TypeGeneric b]) state
    addCtorLink b c (mkTypeExpr "Pair" [TypeGeneric a, TypeGeneric b]) state
    addCtorLink c d (mkTypeExpr "Pair" [TypeGeneric c, TypeGeneric a]) state
    addCtorLink a d (mkTypeExpr "Pair" [TypeGeneric c, TypeGeneric a]) state

    addConcreteFlow a (mkBasic "Int") state
    addConcreteFlow b (mkBasic "Bool") state

    res <- solveGeneric state
    liftIO $ assertBool "Should have solution" (isJust res)
    let sol = fromJust res
    solD <- liftIO $ H.lookup sol d
    let c1 = mkTypeExpr "Pair" [mkBasic "Int", mkBasic "Bool"]
        c2 = mkTypeExpr "Pair" [mkBasic "Bool", mkBasic "Bool"]
        expected =
            sortOn
                show
                [ mkTypeExpr "Pair" [c1, mkBasic "Int"]
                , mkTypeExpr "Pair" [c1, mkBasic "Bool"]
                , mkTypeExpr "Pair" [c2, mkBasic "Int"]
                , mkTypeExpr "Pair" [c2, mkBasic "Bool"]
                ]
    liftIO $ fmap (sortOn show) solD @?= Just expected
