module Test.Reussir.Core.Class (tests) where

import Data.Text (pack)
import Effectful (liftIO, runEff)
import Reussir.Core.Class
import Reussir.Core.Types.Class
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Test.Tasty
import Test.Tasty.HUnit

mkClass :: String -> Class
mkClass name = Class $ Path (Identifier $ pack name) []

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Class"
        [ testCase "Single Inheritance Chain" testSingleInheritance
        , testCase "Multiple Inheritance" testMultipleInheritance
        , testCase "Diamond Inheritance" testDiamondInheritance
        ]

testSingleInheritance :: Assertion
testSingleInheritance = runEff $ do
    dag <- newDAG
    let cA = mkClass "A"
        cB = mkClass "B"
        cC = mkClass "C"

    addClass cA [] dag
    addClass cB [cA] dag
    addClass cC [cB] dag

    populateDAG dag

    resAA <- isSuperClass dag cA cA
    liftIO $ assertBool "A is super of A" resAA

    resAB <- isSuperClass dag cA cB
    liftIO $ assertBool "A is super of B" resAB

    resAC <- isSuperClass dag cA cC
    liftIO $ assertBool "A is super of C" resAC

    resBC <- isSuperClass dag cB cC
    liftIO $ assertBool "B is super of C" resBC

    resBA <- isSuperClass dag cB cA
    liftIO $ assertBool "B not super of A" (not resBA)

    resCB <- isSuperClass dag cC cB
    liftIO $ assertBool "C not super of B" (not resCB)

testMultipleInheritance :: Assertion
testMultipleInheritance = runEff $ do
    dag <- newDAG
    let cA = mkClass "A"
        cB = mkClass "B"
        cC = mkClass "C"
        cD = mkClass "D"

    addClass cA [] dag
    addClass cB [] dag
    addClass cC [cA, cB] dag
    addClass cD [cC] dag

    populateDAG dag

    resAC <- isSuperClass dag cA cC
    liftIO $ assertBool "A is super of C" resAC

    resBC <- isSuperClass dag cB cC
    liftIO $ assertBool "B is super of C" resBC

    resAD <- isSuperClass dag cA cD
    liftIO $ assertBool "A is super of D" resAD

    resBD <- isSuperClass dag cB cD
    liftIO $ assertBool "B is super of D" resBD

testDiamondInheritance :: Assertion
testDiamondInheritance = runEff $ do
    dag <- newDAG
    let cA = mkClass "A"
        cB = mkClass "B"
        cC = mkClass "C"
        cD = mkClass "D"

    addClass cA [] dag
    addClass cB [cA] dag
    addClass cC [cA] dag
    addClass cD [cB, cC] dag

    populateDAG dag

    resAD <- isSuperClass dag cA cD
    liftIO $ assertBool "A is super of D" resAD

    resBD <- isSuperClass dag cB cD
    liftIO $ assertBool "B is super of D" resBD

    resCD <- isSuperClass dag cC cD
    liftIO $ assertBool "C is super of D" resCD
