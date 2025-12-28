module Test.Reussir.Core.Class (tests) where

import Data.Text (pack)
import Data.Set qualified as Set
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
        , testCase "Meet Class" testMeetClass
        , testCase "Meet Bound" testMeetBound
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

testMeetClass :: Assertion
testMeetClass = runEff $ do
    dag <- newDAG
    let cA = mkClass "A"
        cB = mkClass "B"
        cC = mkClass "C"
    
    addClass cA [] dag
    addClass cB [cA] dag -- B <: A
    addClass cC [] dag   -- C unrelated
    
    populateDAG dag
    
    -- A meet B -> B (since B <: A)
    resAB <- meetClass dag cA cB
    liftIO $ resAB @?= [cB]
    
    -- B meet A -> B
    resBA <- meetClass dag cB cA
    liftIO $ resBA @?= [cB]
    
    -- A meet C -> [A, C] (or [C, A])
    resAC <- meetClass dag cA cC
    liftIO $ Set.fromList resAC @?= Set.fromList [cA, cC]

testMeetBound :: Assertion
testMeetBound = runEff $ do
    dag <- newDAG
    let cA = mkClass "A"
        cB = mkClass "B"
        cC = mkClass "C"
        cD = mkClass "D"
        cE = mkClass "E"
    
    addClass cA [] dag
    addClass cB [cA] dag -- B <: A
    addClass cC [cB] dag -- C <: B <: A
    addClass cD [] dag   -- D unrelated
    addClass cE [cD] dag -- E <: D
    
    populateDAG dag
    
    -- {A} meet {B} -> {B}
    res1 <- meetBound dag [cA] [cB]
    liftIO $ res1 @?= [cB]
    
    -- {A} meet {C} -> {C}
    res2 <- meetBound dag [cA] [cC]
    liftIO $ res2 @?= [cC]
    
    -- {B} meet {C} -> {C}
    res3 <- meetBound dag [cB] [cC]
    liftIO $ res3 @?= [cC]
    
    -- {A, D} meet {B} -> {B, D} (A is super of B, D unrelated)
    res4 <- meetBound dag [cA, cD] [cB]
    liftIO $ Set.fromList res4 @?= Set.fromList [cB, cD]
    
    -- {A} meet {D} -> {A, D}
    res5 <- meetBound dag [cA] [cD]
    liftIO $ Set.fromList res5 @?= Set.fromList [cA, cD]
    
    -- {A, D} meet {B, E} -> {B, E} (A super of B, D super of E)
    res6 <- meetBound dag [cA, cD] [cB, cE]
    liftIO $ Set.fromList res6 @?= Set.fromList [cB, cE]
