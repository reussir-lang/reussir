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

import Data.HashTable.IO qualified as H
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
import Reussir.Core.Data.Semi.Record (
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core.Semi.Context (emptyLocalSemiContext, emptySemiContext)
import Reussir.Core.Semi.PatternMatch (normalizeCtorPattern)

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
