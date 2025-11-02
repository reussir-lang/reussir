{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Location (
    locationTests,
)
where

import Control.Monad.State.Strict qualified as S
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Location
import Test.Tasty
import Test.Tasty.HUnit

-- Helper to run codegen and extract the builder as text
runCodegenAsText :: C.Codegen TB.Builder -> IO T.Text
runCodegenAsText codegen = do
    let spec = C.TargetSpec "test_module" "output.mlir" B.OptDefault B.OutputObject B.LogInfo
    ctx <- C.emptyContext spec
    (result, _) <- S.runStateT (C.genState codegen) ctx
    return $ TB.toLazyText result

-- Test: UnknownLoc
testUnknownLoc :: TestTree
testUnknownLoc = testCase "UnknownLoc emission" $ do
    result <- runCodegenAsText $ C.emit UnknownLoc
    T.unpack result @?= "loc(?)"

-- Test: FileLineColRange - single point
testFileLineColSinglePoint :: TestTree
testFileLineColSinglePoint = testCase "FileLineColRange single point" $ do
    let loc = FileLineColRange "test.rs" 10 5 10 5
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(\"test.rs\":10:5)"

-- Test: FileLineColRange - range
testFileLineColRange :: TestTree
testFileLineColRange = testCase "FileLineColRange range" $ do
    let loc = FileLineColRange "example.rs" 1 1 5 10
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(\"example.rs\":1:1 to 5:10)"

-- Test: NameLoc without child
testNameLocWithoutChild :: TestTree
testNameLocWithoutChild = testCase "NameLoc without child" $ do
    let loc = NameLoc "MyLoc" Nothing
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(\"MyLoc\")"

-- Test: NameLoc with child
testNameLocWithChild :: TestTree
testNameLocWithChild = testCase "NameLoc with child" $ do
    let childLoc = FileLineColRange "test.rs" 5 3 5 3
    let loc = NameLoc "MyLoc" (Just childLoc)
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(\"MyLoc\"(\"test.rs\":5:3))"

-- Test: FusedLoc without metadata
testFusedLocWithoutMetadata :: TestTree
testFusedLocWithoutMetadata = testCase "FusedLoc without metadata" $ do
    let loc1 = FileLineColRange "file1.rs" 1 1 1 1
    let loc2 = FileLineColRange "file2.rs" 2 2 2 2
    let loc = FusedLoc Nothing [loc1, loc2]
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(fused[\"file1.rs\":1:1, \"file2.rs\":2:2])"

-- Test: FusedLoc with metadata
testFusedLocWithMetadata :: TestTree
testFusedLocWithMetadata = testCase "FusedLoc with metadata" $ do
    let loc1 = UnknownLoc
    let loc2 = NameLoc "test" Nothing
    let loc = FusedLoc (Just "info") [loc1, loc2]
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(fused<\"info\">[?, \"test\"])"

-- Test: CallSiteLoc
testCallSiteLoc :: TestTree
testCallSiteLoc = testCase "CallSiteLoc emission" $ do
    let calleeLoc = FileLineColRange "callee.rs" 10 1 10 1
    let callerLoc = FileLineColRange "caller.rs" 20 5 20 5
    let loc = CallSiteLoc calleeLoc callerLoc
    result <- runCodegenAsText $ C.emit loc
    T.unpack result @?= "loc(callsite(\"callee.rs\":10:1 at \"caller.rs\":20:5))"

-- Test: Nested CallSiteLoc
testNestedCallSite :: TestTree
testNestedCallSite = testCase "Nested CallSiteLoc" $ do
    let innerLoc = CallSiteLoc UnknownLoc UnknownLoc
    let outerLoc = CallSiteLoc innerLoc UnknownLoc
    result <- runCodegenAsText $ C.emit outerLoc
    T.unpack result @?= "loc(callsite(callsite(? at ?) at ?))"

-- Test: Complex nested location
testComplexNested :: TestTree
testComplexNested = testCase "Complex nested location" $ do
    let inner1 = FileLineColRange "file.rs" 5 10 5 10
    let inner2 = FileLineColRange "file.rs" 10 20 10 20
    let fused = FusedLoc (Just "meta") [inner1, inner2]
    let named = NameLoc "outer" (Just fused)
    result <- runCodegenAsText $ C.emit named
    T.unpack result
        @?= "loc(\"outer\"(fused<\"meta\">[\"file.rs\":5:10, \"file.rs\":10:20]))"

locationTests :: TestTree
locationTests =
    testGroup
        "Location Emission Tests"
        [ testUnknownLoc
        , testFileLineColSinglePoint
        , testFileLineColRange
        , testNameLocWithoutChild
        , testNameLocWithChild
        , testFusedLocWithoutMetadata
        , testFusedLocWithMetadata
        , testCallSiteLoc
        , testNestedCallSite
        , testComplexNested
        ]
