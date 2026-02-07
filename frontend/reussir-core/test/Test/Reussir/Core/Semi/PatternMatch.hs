{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core.Semi.PatternMatch where

import Data.Maybe (fromJust, isJust, isNothing)
import Effectful (inject, liftIO, runEff)
import Effectful.Log (runLog)
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (newIORef', writeIORef')
import Log (LogLevel (..))
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Stmt (Visibility (..))
import Test.Tasty
import Test.Tasty.HUnit

import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.IO qualified as H
import Data.RRBVector qualified as RRB
import Data.Sequence qualified as Seq
import Data.Text qualified
import Data.Vector.Strict qualified as V
import Effectful.State.Static.Local qualified as State
import Reussir.Bridge qualified as B
import Reussir.Parser.Types.Capability qualified as Cap
import Reussir.Parser.Types.Expr qualified as Syn

import Reussir.Core.Data.Semi.Context (
    LocalSemiContext (..),
    SemiContext (..),
    SemiEff,
    knownRecords,
 )
import Reussir.Core.Data.Semi.Expr (PatternVarRef (..))
import Reussir.Core.Data.Semi.Record (
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core.Semi.Context (emptyLocalSemiContext, emptySemiContext)
import Reussir.Core.Semi.PatternMatch (
    PMMatrix (..),
    PMRow (..),
    SplitResult (..),
    normalizeCtorPattern,
    normalizeVarRefLevel,
    splitAtFirstWildcard,
 )

import Reussir.Core.Data.Semi.Type qualified as SemiType

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Semi.PatternMatch"
        [ testCase "normalize named record" (testNormalizeNamed "testNormalizeNamed")
        , testCase
            "normalize named record (unknown field)"
            (testNormalizeNamedUnknown "testNormalizeNamedUnknown")
        , testCase
            "normalize named record (positional arg)"
            (testNormalizeNamedPositional "testNormalizeNamedPositional")
        , testCase
            "normalize named record (missing field)"
            (testNormalizeNamedMissing "testNormalizeNamedMissing")
        , testCase
            "normalize positional record"
            (testNormalizePositional "testNormalizePositional")
        , testCase
            "normalize positional record (too many)"
            (testNormalizePositionalTooMany "testNormalizePositionalTooMany")
        , testCase
            "normalize positional record (named arg)"
            (testNormalizePositionalNamed "testNormalizePositionalNamed")
        , testCase
            "normalize var ref level"
            (testNormalizeVarRefLevel "testNormalizeVarRefLevel")
        , testCase
            "split at first wildcard"
            (testSplitAtFirstWildcard "testSplitAtFirstWildcard")
        ]

runSemi :: String -> SemiEff a -> IO a
runSemi name action = do
    B.withReussirLogger B.LogError name $ \logger -> do
        runEff $ runPrim $ runLog (Data.Text.pack name) logger LogAttention $ do
            semiCtx <- emptySemiContext B.LogError "test.reussir"
            locCtx <- emptyLocalSemiContext
            State.evalState semiCtx $ State.evalState locCtx $ inject action

mkSpan :: a -> WithSpan a
mkSpan x = WithSpan x 0 0

mkPath :: String -> Path
mkPath s = Path (Identifier (Data.Text.pack s)) []

mkId :: String -> Identifier
mkId s = Identifier (Data.Text.pack s)

testNormalizeNamed :: String -> Assertion
testNormalizeNamed name = runSemi name $ do
    -- Setup
    let recName = mkPath "Point"
    fieldsRef <- newIORef' Nothing
    -- Dummy record
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }

    -- Add to knownRecords
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record

    -- Populate fields
    -- Named: x, y
    let fields =
            V.fromList
                [ mkSpan (mkId "x", SemiType.TypeUnit, False)
                , mkSpan (mkId "y", SemiType.TypeUnit, False)
                ]
    writeIORef' fieldsRef (Just (Named fields))

    -- Case 1: All fields present { x: _, y: _ }
    let args1 =
            V.fromList
                [ Syn.PatternCtorArg (Just (mkId "x")) Syn.WildcardPat
                , Syn.PatternCtorArg (Just (mkId "y")) Syn.WildcardPat
                ]
    res1 <- normalizeCtorPattern recName args1 False
    liftIO $ assertBool "Should succeed" (isJust res1)
    let v1 = fromJust res1
    liftIO $ V.length v1 @?= 2

    -- Case 2: Missing field { x: _ } -> Fail (no ellipsis)
    let args2 =
            V.fromList
                [ Syn.PatternCtorArg (Just (mkId "x")) Syn.WildcardPat
                ]
    res2 <- normalizeCtorPattern recName args2 False
    liftIO $ assertBool "Should fail (missing y)" (isNothing res2)

    -- Case 3: Missing field with ellipsis { x: _, .. } -> Succeed
    res3 <- normalizeCtorPattern recName args2 True
    liftIO $ assertBool "Should succeed with ellipsis" (isJust res3)

testNormalizeNamedUnknown :: String -> Assertion
testNormalizeNamedUnknown name = runSemi name $ do
    let recName = mkPath "Point"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record
    let fields = V.fromList [mkSpan (mkId "x", SemiType.TypeUnit, False)]
    writeIORef' fieldsRef (Just (Named fields))

    -- Unknown field 'z'
    let args = V.fromList [Syn.PatternCtorArg (Just (mkId "z")) Syn.WildcardPat]
    res <- normalizeCtorPattern recName args False
    liftIO $ assertBool "Should fail (unknown field)" (isNothing res)

testNormalizeNamedPositional :: String -> Assertion
testNormalizeNamedPositional name = runSemi name $ do
    let recName = mkPath "Point"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record
    let fields = V.fromList [mkSpan (mkId "x", SemiType.TypeUnit, False)]
    writeIORef' fieldsRef (Just (Named fields))

    -- Positional arg in named record
    let args = V.fromList [Syn.PatternCtorArg Nothing Syn.WildcardPat]
    res <- normalizeCtorPattern recName args False
    liftIO $ assertBool "Should fail (positional arg)" (isNothing res)

testNormalizeNamedMissing :: String -> Assertion
testNormalizeNamedMissing name = runSemi name $ do
    let recName = mkPath "Point"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record
    let fields = V.fromList [mkSpan (mkId "x", SemiType.TypeUnit, False)]
    writeIORef' fieldsRef (Just (Named fields))

    let args = V.empty
    res <- normalizeCtorPattern recName args False
    liftIO $ assertBool "Should fail (missing field)" (isNothing res)

testNormalizePositional :: String -> Assertion
testNormalizePositional name = runSemi name $ do
    let recName = mkPath "Tuple"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record

    -- Unnamed fields (Tuple-like)
    let fields =
            V.fromList
                [ mkSpan (SemiType.TypeUnit, False)
                , mkSpan (SemiType.TypeUnit, False)
                ]
    writeIORef' fieldsRef (Just (Unnamed fields))

    -- Exact match
    let args1 =
            V.fromList
                [ Syn.PatternCtorArg Nothing Syn.WildcardPat
                , Syn.PatternCtorArg Nothing Syn.WildcardPat
                ]
    res1 <- normalizeCtorPattern recName args1 False
    liftIO $ assertBool "Should succeed" (isJust res1)

    -- Ellipsis match
    let args2 = V.fromList [Syn.PatternCtorArg Nothing Syn.WildcardPat]
    res2 <- normalizeCtorPattern recName args2 True -- hasEllipsis
    liftIO $ assertBool "Should succeed with ellipsis" (isJust res2)
    let v2 = fromJust res2
    liftIO $ V.length v2 @?= 2 -- filled with wildcard

testNormalizePositionalTooMany :: String -> Assertion
testNormalizePositionalTooMany name = runSemi name $ do
    let recName = mkPath "Tuple"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record
    let fields = V.fromList [mkSpan (SemiType.TypeUnit, False)]
    writeIORef' fieldsRef (Just (Unnamed fields))

    let args =
            V.fromList
                [ Syn.PatternCtorArg Nothing Syn.WildcardPat
                , Syn.PatternCtorArg Nothing Syn.WildcardPat
                ]
    res <- normalizeCtorPattern recName args False
    liftIO $ assertBool "Should fail (too many)" (isNothing res)

testNormalizePositionalNamed :: String -> Assertion
testNormalizePositionalNamed name = runSemi name $ do
    let recName = mkPath "Tuple"
    fieldsRef <- newIORef' Nothing
    span' <- State.gets currentSpan
    let record =
            Record
                { recordName = recName
                , recordTyParams = []
                , recordFields = fieldsRef
                , recordKind = StructKind
                , recordVisibility = Public
                , recordDefaultCap = Cap.Value
                , recordSpan = span'
                }
    records <- State.gets knownRecords
    liftIO $ H.insert records recName record
    let fields = V.fromList [mkSpan (SemiType.TypeUnit, False)]
    writeIORef' fieldsRef (Just (Unnamed fields))

    -- Named arg in positional record
    let args = V.fromList [Syn.PatternCtorArg (Just (mkId "x")) Syn.WildcardPat]
    res <- normalizeCtorPattern recName args False
    liftIO $ assertBool "Should fail (named arg)" (isNothing res)

mkVarRef :: Int -> PatternVarRef
mkVarRef i = PatternVarRef (Seq.singleton i)

testNormalizeVarRefLevel :: String -> Assertion
testNormalizeVarRefLevel name = runSemi name $ do
    let ref0 = mkVarRef 0
    let ref1 = mkVarRef 1
    let ref2 = mkVarRef 2

    -- Case 1: Empty matrix -> should remain unchanged
    let matEmpty = PMMatrix ref0 RRB.empty HashMap.empty
    let resEmpty = normalizeVarRefLevel matEmpty
    liftIO $ matrixCursor resEmpty @?= ref0

    -- Case 2: Matrix with rows having different leading refs [1], [0], [2]
    -- Row 1: [1] ...
    let row1 =
            PMRow
                (RRB.fromList [(ref1, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))
    -- Row 2: [0] ...
    let row2 =
            PMRow
                (RRB.fromList [(ref0, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))
    -- Row 3: [2] ...
    let row3 =
            PMRow
                (RRB.fromList [(ref2, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))

    let matMixed = PMMatrix ref1 (RRB.fromList [row1, row2, row3]) HashMap.empty
    let resMixed = normalizeVarRefLevel matMixed
    -- Should pick min(1, 0, 2) = 0
    liftIO $ matrixCursor resMixed @?= ref0

    -- Case 3: Matrix with rows all having same leading ref [2]
    let rowA =
            PMRow
                (RRB.fromList [(ref2, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))
    let rowB =
            PMRow
                (RRB.fromList [(ref2, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))
    let matSame = PMMatrix ref0 (RRB.fromList [rowA, rowB]) HashMap.empty
    let resSame = normalizeVarRefLevel matSame
    liftIO $ matrixCursor resSame @?= ref2

    -- Case 4: Row with empty patterns (wildcard) -> ignored by normalizeVarRefLevel logic?
    let rowEmpty = PMRow RRB.empty HashMap.empty Nothing (Syn.ConstExpr (Syn.ConstInt 0))
    let matPartialWild = PMMatrix ref0 (RRB.fromList [row1, rowEmpty]) HashMap.empty
    let resPartialWild = normalizeVarRefLevel matPartialWild
    liftIO $ matrixCursor resPartialWild @?= ref1

testSplitAtFirstWildcard :: String -> Assertion
testSplitAtFirstWildcard name = runSemi name $ do
    let ref0 = mkVarRef 0
    let ref1 = mkVarRef 1

    -- Specific row (matches at ref0)
    let rowSpecific =
            PMRow
                (RRB.fromList [(ref0, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 0))

    -- Wildcard row (empty patterns)
    let rowWildEmpty =
            PMRow
                RRB.empty
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 1))

    -- Wildcard row (matches at ref1 > ref0)
    let rowWildFuture =
            PMRow
                (RRB.fromList [(ref1, Syn.WildcardPat)])
                HashMap.empty
                Nothing
                (Syn.ConstExpr (Syn.ConstInt 2))

    -- Case 1: No wildcards
    let rows1 = RRB.fromList [rowSpecific, rowSpecific]
    let mat1 = PMMatrix ref0 rows1 HashMap.empty
    let SplitResult l1 w1 t1 = splitAtFirstWildcard mat1
    liftIO $ length l1 @?= 2
    liftIO $ length w1 @?= 0
    liftIO $ length t1 @?= 0

    -- Case 2: Wildcard at start [Wild, Specific]
    let rows2 = RRB.fromList [rowWildEmpty, rowSpecific]
    let mat2 = PMMatrix ref0 rows2 HashMap.empty
    let SplitResult l2 w2 t2 = splitAtFirstWildcard mat2
    liftIO $ length l2 @?= 0
    liftIO $ length w2 @?= 1
    liftIO $ length t2 @?= 1

    -- Case 3: Wildcard block in middle [Specific, Wild, Wild, Specific]
    let rows3 = RRB.fromList [rowSpecific, rowWildEmpty, rowWildFuture, rowSpecific]
    let mat3 = PMMatrix ref0 rows3 HashMap.empty
    let SplitResult l3 w3 t3 = splitAtFirstWildcard mat3
    liftIO $ length l3 @?= 1
    liftIO $ length w3 @?= 2 -- Both empty and future match as wildcard for ref0
    liftIO $ length t3 @?= 1

    -- Case 4: Consecutive wildcards at end [Specific, Wild]
    let rows4 = RRB.fromList [rowSpecific, rowWildEmpty]
    let mat4 = PMMatrix ref0 rows4 HashMap.empty
    let SplitResult l4 w4 t4 = splitAtFirstWildcard mat4
    liftIO $ length l4 @?= 1
    liftIO $ length w4 @?= 1
    liftIO $ length t4 @?= 0
