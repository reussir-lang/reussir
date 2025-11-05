{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.IR (
    irTests,
)
where

import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Effectful qualified as E
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as E
import Log (defaultLogLevel)
import Log.Backend.StandardOutput qualified as L
import Reussir.Bridge qualified as B
import Reussir.Codegen.Context (runCodegen)
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Context.Path (pathSingleton)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Location qualified as Loc
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V
import Test.Tasty
import Test.Tasty.HUnit

runCodegenAsText :: C.Codegen () -> IO T.Text
runCodegenAsText codegen = do
    let spec = C.TargetSpec "test_module" "output.mlir" B.OptDefault B.OutputObject B.LogInfo
    L.withStdOutLogger $ \logger -> do
        E.runEff $ L.runLog "Test.Codegen.IR" logger defaultLogLevel $ runCodegen spec $ do
            codegen
            C.emitBuilder "========= trailing locs =========\n"
            C.emitOutlineLocs
            E.gets (TB.runBuilder . C.builder)

runCodegenForInstr :: IR.Instr -> IO T.Text
runCodegenForInstr instr =
    runCodegenAsText (IR.instrCodegen instr)

primitiveI32 :: TT.Type
primitiveI32 = TT.TypePrim (TT.PrimInt TT.PrimInt32)

primitiveBool :: TT.Type
primitiveBool = TT.TypePrim TT.PrimBool

primitiveI64 :: TT.Type
primitiveI64 = TT.TypePrim (TT.PrimInt TT.PrimInt64)

-- RC I32 type (non-nullable)
rcI32 :: TT.Type
rcI32 = TT.TypeRc (TT.Rc primitiveI32 TT.NonAtomic TT.Shared)

-- Nullable RC I32 type
nullableRcI32 :: TT.Type
nullableRcI32 = TT.TypeNullable rcI32

-- Helper functions for creating test values
val :: Int64 -> V.Value
val = V.Value

typedVal :: Int64 -> TT.Type -> V.TypedValue
typedVal v t = (val v, t)

-- Helper function for creating blocks
emptyBlock :: IR.Block
emptyBlock = IR.Block{blkArgs = [], blkBody = []}

blockWithArgs :: [V.TypedValue] -> [IR.Instr] -> IR.Block
blockWithArgs args body = IR.Block{blkArgs = args, blkBody = body}

-- Helper function for creating intrinsic calls
intrinsicCall :: I.IntrinsicCall -> IR.Instr
intrinsicCall = IR.ICall

-- Helper function for creating function calls
funcCall :: C.Path -> [V.TypedValue] -> Maybe V.TypedValue -> IR.Instr
funcCall target args result = IR.FCall (IR.FuncCall target args result)

-- Helper function for creating nullable operations
nullableCheck :: V.TypedValue -> V.TypedValue -> IR.Instr
nullableCheck val' res = IR.NullableCheck val' res

nullableCreate :: Maybe V.TypedValue -> V.TypedValue -> IR.Instr
nullableCreate val' res = IR.NullableCreate val' res

nullableDispatch :: V.TypedValue -> IR.Block -> IR.Block -> Maybe V.TypedValue -> IR.Instr
nullableDispatch val' nonnull nullBlock result = IR.NullableDispatch val' nonnull nullBlock result

-- Helper function for creating location
fileLocation :: T.Text -> Int64 -> Int64 -> Int64 -> Int64 -> Loc.Location
fileLocation filename startLine startCol endLine endCol =
    Loc.FileLineColRange filename startLine startCol endLine endCol

-- Helper to check if a string is present in the output
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

-- Helper to check relative positions - ensures needle2 comes after needle1
comesAfter :: String -> String -> String -> Bool
comesAfter needle1 needle2 haystack =
    isInfixOf needle1 haystack
        && isInfixOf needle2 haystack
        && case (T.breakOn (T.pack needle1) (T.pack haystack), T.breakOn (T.pack needle2) (T.pack haystack)) of
            ((before1, _), (before2, _)) -> T.length before2 > T.length before1

irTests :: TestTree
irTests =
    testGroup
        "IR.Codegen"
        [ testGroup
            "ICall"
            [ testCase "ICall Codegen" $ do
                result <-
                    runCodegenForInstr
                        ( intrinsicCall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addi (I.IntOFFlag 0)))
                                [typedVal 1 primitiveI32, typedVal 2 primitiveI32]
                                [typedVal 3 primitiveI32]
                            )
                        )
                let resultStr = T.unpack result
                assertBool "Should contain arith.addi" $ "arith.addi" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3" `isInfixOf` resultStr
                assertBool "Should contain args %1 and %2" $ "%1" `isInfixOf` resultStr && "%2" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ "%3 = " `isInfixOf` resultStr
            ]
        , testGroup
            "FCall"
            [ testCase "FCall Codegen without result" $ do
                result <-
                    runCodegenForInstr
                        (funcCall (pathSingleton "test_func") [typedVal 1 primitiveI32] Nothing)
                let resultStr = T.unpack result
                assertBool "Should contain func.call" $ "func.call" `isInfixOf` resultStr
                assertBool "Should contain test_func" $ "test_func" `isInfixOf` resultStr
                assertBool "Should contain arg %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Should not contain result assignment" $ not (" = func.call" `isInfixOf` resultStr)
            , testCase "FCall Codegen with result" $ do
                result <-
                    runCodegenForInstr
                        (funcCall (pathSingleton "test_func") [typedVal 1 primitiveI32] (Just (typedVal 2 primitiveI32)))
                let resultStr = T.unpack result
                assertBool "Should contain func.call" $ "func.call" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain test_func" $ "test_func" `isInfixOf` resultStr
                assertBool "Should contain arg %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain return type ->" $ " -> " `isInfixOf` resultStr
                assertBool "Result should come before func.call" $ comesAfter "%2 = " "func.call" resultStr
            , testCase "FCall Codegen with multiple args" $ do
                result <-
                    runCodegenForInstr
                        ( funcCall
                            (pathSingleton "test_func")
                            [typedVal 1 primitiveI32, typedVal 2 primitiveI64]
                            (Just (typedVal 3 primitiveBool))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain func.call" $ "func.call" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3 = " `isInfixOf` resultStr
                assertBool "Should contain test_func" $ "test_func" `isInfixOf` resultStr
                assertBool "Should contain both args" $ "%1" `isInfixOf` resultStr && "%2" `isInfixOf` resultStr
                assertBool "Should contain i32 and i64 types" $ "i32" `isInfixOf` resultStr && "i64" `isInfixOf` resultStr
                assertBool "Should contain return type" $ " -> " `isInfixOf` resultStr
            ]
        , testGroup
            "Panic"
            [ testCase "Panic Codegen" $ do
                result <- runCodegenForInstr (IR.Panic "error message")
                let resultStr = T.unpack result
                assertBool "Should contain reussir.panic" $ "reussir.panic" `isInfixOf` resultStr
                assertBool "Should contain error message" $ "error message" `isInfixOf` resultStr
            , testCase "Panic Codegen with empty message" $ do
                result <- runCodegenForInstr (IR.Panic "")
                let resultStr = T.unpack result
                assertBool "Should contain reussir.panic" $ "reussir.panic" `isInfixOf` resultStr
            ]
        , testGroup
            "Return"
            [ testCase "Return Codegen without value" $ do
                result <- runCodegenForInstr (IR.Return Nothing)
                let resultStr = T.unpack result
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
            , testCase "Return Codegen with value" $ do
                result <- runCodegenForInstr (IR.Return (Just (typedVal 1 primitiveI32)))
                let resultStr = T.unpack result
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Value should come after return" $ comesAfter "func.return" "%1" resultStr
            ]
        , testGroup
            "NullableCheck"
            [ testCase "NullableCheck Codegen" $ do
                result <-
                    runCodegenForInstr
                        (nullableCheck (typedVal 1 nullableRcI32) (typedVal 2 primitiveBool))
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.check" $ "reussir.nullable.check" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain nullable type" $ "nullable" `isInfixOf` resultStr || "!reussir.nullable" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain boolean type or i1" $ "i1" `isInfixOf` resultStr || "bool" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.nullable.check" resultStr
            ]
        , testGroup
            "NullableCreate"
            [ testCase "NullableCreate Codegen without value" $ do
                result <-
                    runCodegenForInstr
                        (nullableCreate Nothing (typedVal 1 nullableRcI32))
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.create" $ "reussir.nullable.create" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1 = " `isInfixOf` resultStr
                assertBool "Should contain nullable type" $ "nullable" `isInfixOf` resultStr || "!reussir.nullable" `isInfixOf` resultStr
                assertBool "Should contain type annotation" $ " : " `isInfixOf` resultStr
                assertBool "Should not contain value argument" $ not ("(%2" `isInfixOf` resultStr)
            , testCase "NullableCreate Codegen with value" $ do
                result <-
                    runCodegenForInstr
                        (nullableCreate (Just (typedVal 2 rcI32)) (typedVal 1 nullableRcI32))
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.create" $ "reussir.nullable.create" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1 = " `isInfixOf` resultStr
                assertBool "Should contain value %2" $ "%2" `isInfixOf` resultStr
                assertBool "Should contain nullable type" $ "nullable" `isInfixOf` resultStr || "!reussir.nullable" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Value should come before nullable type" $ comesAfter "%2" "nullable" resultStr || comesAfter "%2" "!reussir.nullable" resultStr
            ]
        , testGroup
            "NullableDispatch"
            [ testCase "NullableDispatch Codegen without result" $ do
                result <-
                    runCodegenForInstr
                        ( nullableDispatch
                            (typedVal 1 nullableRcI32)
                            (blockWithArgs [typedVal 2 rcI32] [])
                            emptyBlock
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.dispatch" $ "reussir.nullable.dispatch" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain nullable type" $ "nullable" `isInfixOf` resultStr || "!reussir.nullable" `isInfixOf` resultStr
                assertBool "Should contain nonnull region" $ "nonnull" `isInfixOf` resultStr
                assertBool "Should contain null region" $ "null" `isInfixOf` resultStr
                assertBool "Should contain block arguments" $ "%2" `isInfixOf` resultStr
                assertBool "nonnull should come before null" $ comesAfter "nonnull ->" "null ->" resultStr
            , testCase "NullableDispatch Codegen with result" $ do
                result <-
                    runCodegenForInstr
                        ( nullableDispatch
                            (typedVal 1 nullableRcI32)
                            (blockWithArgs [typedVal 2 rcI32] [])
                            emptyBlock
                            (Just (typedVal 3 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.dispatch" $ "reussir.nullable.dispatch" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3" `isInfixOf` resultStr
                assertBool "Should contain return type ->" $ " -> " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain nonnull region" $ "nonnull" `isInfixOf` resultStr
                assertBool "Should contain null region" $ "null" `isInfixOf` resultStr
                assertBool "Should contain i32 return type" $ "i32" `isInfixOf` resultStr
            , testCase "NullableDispatch Codegen with instructions in blocks" $ do
                result <-
                    runCodegenForInstr
                        ( nullableDispatch
                            (typedVal 1 nullableRcI32)
                            (blockWithArgs [typedVal 2 rcI32] [IR.Return (Just (typedVal 2 primitiveI32))])
                            (blockWithArgs [] [IR.Return Nothing])
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.nullable.dispatch" $ "reussir.nullable.dispatch" `isInfixOf` resultStr
                assertBool "Should contain func.return in nonnull block" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain %2 in return" $ "%2" `isInfixOf` resultStr
                assertBool "Should contain nonnull region" $ "nonnull" `isInfixOf` resultStr
                assertBool "Should contain null region" $ "null" `isInfixOf` resultStr
                assertBool "nonnull should come before null" $ comesAfter "nonnull ->" "null ->" resultStr
            ]
        , testGroup
            "WithLoc"
            [ testCase "WithLoc Codegen with FileLineColRange" $ do
                result <-
                    runCodegenForInstr
                        ( IR.WithLoc
                            (fileLocation "test.hs" 10 5 10 15)
                            (IR.Return (Just (typedVal 1 primitiveI32)))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain location reference" $ "loc(" `isInfixOf` resultStr
                assertBool "Should contain #loc0" $ "#loc0" `isInfixOf` resultStr
                assertBool "Location should come after instruction" $ comesAfter "func.return" "loc" resultStr
                assertBool "Should contain trailing locs marker" $ "========= trailing locs =========" `isInfixOf` resultStr
                assertBool "Should contain outline location definition" $ "#loc0 = " `isInfixOf` resultStr
                assertBool "Outline location should come after marker" $ comesAfter "========= trailing locs =========" "#loc0 = " resultStr
                assertBool "Should contain file location in outline" $ "test.hs" `isInfixOf` resultStr
                assertBool "Should contain line/column info in outline" $ "10:5" `isInfixOf` resultStr || "10:5 to 10:15" `isInfixOf` resultStr
            , testCase "WithLoc Codegen with UnknownLoc" $ do
                result <-
                    runCodegenForInstr
                        ( IR.WithLoc
                            Loc.UnknownLoc
                            (IR.Panic "error")
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.panic" $ "reussir.panic" `isInfixOf` resultStr
                assertBool "Should contain error" $ "error" `isInfixOf` resultStr
                assertBool "Should contain location reference" $ "loc(" `isInfixOf` resultStr
                assertBool "Should contain #loc0" $ "#loc0" `isInfixOf` resultStr
                assertBool "Location should come after instruction" $ comesAfter "reussir.panic" "loc" resultStr
                assertBool "Should contain trailing locs marker" $ "========= trailing locs =========" `isInfixOf` resultStr
                assertBool "Should contain outline location definition" $ "#loc0 = " `isInfixOf` resultStr
                assertBool "Outline location should come after marker" $ comesAfter "========= trailing locs =========" "#loc0 = " resultStr
                assertBool "Should contain unknown location in outline" $ "?" `isInfixOf` resultStr || "loc(?)" `isInfixOf` resultStr
            ]
        , testGroup
            "RcInc"
            [ testCase "RcInc Codegen" $ do
                result <- runCodegenForInstr (IR.RcInc (typedVal 1 rcI32))
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.inc" $ "reussir.rc.inc" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Input should come in parentheses" $ "(%1" `isInfixOf` resultStr
            ]
        , testGroup
            "RcCreate"
            [ testCase "RcCreate Codegen without region" $ do
                result <-
                    runCodegenForInstr
                        ( IR.RcCreate
                            (typedVal 1 primitiveI32)
                            Nothing
                            (typedVal 2 rcI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.create" $ "reussir.rc.create" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain value parameter" $ "value(" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.rc.create" resultStr
                assertBool "Should not contain region parameter" $ not ("region(" `isInfixOf` resultStr)
            , testCase "RcCreate Codegen with region" $ do
                let regionType = TT.TypeRegion
                result <-
                    runCodegenForInstr
                        ( IR.RcCreate
                            (typedVal 1 primitiveI32)
                            (Just (typedVal 3 regionType))
                            (typedVal 2 rcI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.create" $ "reussir.rc.create" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain value parameter" $ "value(" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain region parameter" $ "region(" `isInfixOf` resultStr
                assertBool "Should contain region value %3" $ "%3" `isInfixOf` resultStr
                assertBool "Should contain region type" $ "reussir.region" `isInfixOf` resultStr || "region" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "value should come before region" $ comesAfter "value(" "region(" resultStr
            ]
        , testGroup
            "RcFreeze"
            [ testCase "RcFreeze Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RcFreeze
                            (typedVal 1 rcI32)
                            (typedVal 2 refType)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.freeze" $ "reussir.rc.freeze" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Should contain ref type" $ "reussir.ref" `isInfixOf` resultStr || "ref" `isInfixOf` resultStr
                assertBool "Input should come in parentheses" $ "(%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.rc.freeze" resultStr
            ]
        , testGroup
            "RcBorrow"
            [ testCase "RcBorrow Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RcBorrow
                            (typedVal 1 rcI32)
                            (typedVal 2 refType)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.borrow" $ "reussir.rc.borrow" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Should contain ref type" $ "reussir.ref" `isInfixOf` resultStr || "ref" `isInfixOf` resultStr
                assertBool "Input should come in parentheses" $ "(%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.rc.borrow" resultStr
            ]
        , testGroup
            "RcIsUnique"
            [ testCase "RcIsUnique Codegen" $ do
                result <-
                    runCodegenForInstr
                        ( IR.RcIsUnique
                            (typedVal 1 rcI32)
                            (typedVal 2 primitiveBool)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.rc.is_unique" $ "reussir.rc.is_unique" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Should contain boolean type or i1" $ "i1" `isInfixOf` resultStr || "bool" `isInfixOf` resultStr
                assertBool "Input should come in parentheses" $ "(%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.rc.is_unique" resultStr
            ]
        , testGroup
            "CompoundCreate"
            [ testCase "CompoundCreate Codegen with single field" $ do
                result <-
                    runCodegenForInstr
                        ( IR.CompoundCreate
                            [typedVal 1 primitiveI32]
                            (typedVal 2 primitiveI64)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.compound" $ "reussir.record.compound" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain field value %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Should contain i64 type" $ "i64" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.record.compound" resultStr
            , testCase "CompoundCreate Codegen with multiple fields" $ do
                result <-
                    runCodegenForInstr
                        ( IR.CompoundCreate
                            [typedVal 1 primitiveI32, typedVal 2 primitiveI64, typedVal 3 primitiveBool]
                            (typedVal 4 rcI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.compound" $ "reussir.record.compound" `isInfixOf` resultStr
                assertBool "Should contain result %4" $ "%4 = " `isInfixOf` resultStr
                assertBool "Should contain all field values" $ "%1" `isInfixOf` resultStr && "%2" `isInfixOf` resultStr && "%3" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Should contain i64 type" $ "i64" `isInfixOf` resultStr
                assertBool "Should contain boolean type or i1" $ "i1" `isInfixOf` resultStr || "bool" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
            , testCase "CompoundCreate Codegen with empty fields" $ do
                result <-
                    runCodegenForInstr
                        ( IR.CompoundCreate
                            []
                            (typedVal 1 primitiveI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.compound" $ "reussir.record.compound" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1 = " `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
            ]
        , testGroup
            "VariantCreate"
            [ testCase "VariantCreate Codegen with tag 0" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantCreate
                            0
                            (typedVal 1 primitiveI32)
                            (typedVal 2 primitiveI64)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.variant" $ "reussir.record.variant" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain tag [0]" $ "[0]" `isInfixOf` resultStr
                assertBool "Should contain value %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
                assertBool "Should contain i64 type" $ "i64" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.record.variant" resultStr
            , testCase "VariantCreate Codegen with tag 42" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantCreate
                            42
                            (typedVal 1 primitiveBool)
                            (typedVal 2 rcI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.variant" $ "reussir.record.variant" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain tag [42]" $ "[42]" `isInfixOf` resultStr
                assertBool "Should contain value %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain boolean type or i1" $ "i1" `isInfixOf` resultStr || "bool" `isInfixOf` resultStr
                assertBool "Should contain rc type" $ "reussir.rc" `isInfixOf` resultStr || "rc" `isInfixOf` resultStr
                assertBool "Tag should come before value" $ comesAfter "[42]" "%1" resultStr
            , testCase "VariantCreate Codegen with large tag" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantCreate
                            999
                            (typedVal 1 primitiveI32)
                            (typedVal 2 primitiveI64)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.variant" $ "reussir.record.variant" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain tag [999]" $ "[999]" `isInfixOf` resultStr
                assertBool "Should contain value %1" $ "%1" `isInfixOf` resultStr
            ]
        , testGroup
            "VariantDispatch"
            [ testCase "VariantDispatch Codegen without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantDispatch
                            (typedVal 1 primitiveI64)
                            (IR.VariantDispData [([0, 1], blockWithArgs [] [IR.Return Nothing])])
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.dispatch" $ "reussir.record.dispatch" `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain case tags" $ "[0, 1]" `isInfixOf` resultStr || "[0,1]" `isInfixOf` resultStr
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Dispatch should come before cases" $ comesAfter "reussir.record.dispatch" "{" resultStr
            , testCase "VariantDispatch Codegen with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantDispatch
                            (typedVal 1 primitiveI64)
                            (IR.VariantDispData [([42], blockWithArgs [] [IR.Return (Just (typedVal 2 primitiveI32))])])
                            (Just (typedVal 3 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.dispatch" $ "reussir.record.dispatch" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3 = " `isInfixOf` resultStr
                assertBool "Should contain tag [42]" $ "[42]" `isInfixOf` resultStr
                assertBool "Should contain -> for result" $ " -> " `isInfixOf` resultStr
            , testCase "VariantDispatch Codegen with multiple cases" $ do
                result <-
                    runCodegenForInstr
                        ( IR.VariantDispatch
                            (typedVal 1 primitiveI64)
                            ( IR.VariantDispData
                                [ ([0], blockWithArgs [] [])
                                , ([1, 2], blockWithArgs [] [])
                                ]
                            )
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.record.dispatch" $ "reussir.record.dispatch" `isInfixOf` resultStr
                assertBool "Should contain multiple case tags" $ ("[0]" `isInfixOf` resultStr || "[1, 2]" `isInfixOf` resultStr || "[1,2]" `isInfixOf` resultStr)
            ]
        , testGroup
            "RefProject"
            [ testCase "RefProject Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RefProject
                            (typedVal 1 refType)
                            0
                            (typedVal 2 primitiveI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.ref.project" $ "reussir.ref.project" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain field [0]" $ "[0]" `isInfixOf` resultStr
                assertBool "Should contain ref type" $ "reussir.ref" `isInfixOf` resultStr || "ref" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.ref.project" resultStr
            , testCase "RefProject Codegen with field 5" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI64 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RefProject
                            (typedVal 1 refType)
                            5
                            (typedVal 2 primitiveI64)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain field [5]" $ "[5]" `isInfixOf` resultStr
            ]
        , testGroup
            "RefSpill"
            [ testCase "RefSpill Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RefSpill
                            (typedVal 1 refType)
                            (typedVal 2 rcI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.ref.spill" $ "reussir.ref.spill" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.ref.spill" resultStr
            ]
        , testGroup
            "RefLoad"
            [ testCase "RefLoad Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RefLoad
                            (typedVal 1 refType)
                            (typedVal 2 primitiveI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.ref.load" $ "reussir.ref.load" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.ref.load" resultStr
            ]
        , testGroup
            "RefStore"
            [ testCase "RefStore Codegen" $ do
                let refType = TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Shared)
                result <-
                    runCodegenForInstr
                        ( IR.RefStore
                            (typedVal 1 refType)
                            (typedVal 2 primitiveI32)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.ref.store" $ "reussir.ref.store" `isInfixOf` resultStr
                assertBool "Should contain target %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain value %2" $ "%2" `isInfixOf` resultStr
                assertBool "Target should come before value" $ comesAfter "%1" "%2" resultStr
            ]
        , testGroup
            "RegionRun"
            [ testCase "RegionRun Codegen without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.RegionRun
                            (blockWithArgs [] [IR.Return Nothing])
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.region.run" $ "reussir.region.run" `isInfixOf` resultStr
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain block body" $ "{" `isInfixOf` resultStr
            , testCase "RegionRun Codegen with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.RegionRun
                            (blockWithArgs [] [IR.Return (Just (typedVal 1 primitiveI32))])
                            (Just (typedVal 2 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.region.run" $ "reussir.region.run" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain -> for result" $ " -> " `isInfixOf` resultStr
                assertBool "Should contain i32 type" $ "i32" `isInfixOf` resultStr
            ]
        , testGroup
            "Yield"
            [ testCase "Yield Codegen with YieldClosure without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldClosure
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.yield" $ "reussir.closure.yield" `isInfixOf` resultStr
            , testCase "Yield Codegen with YieldClosure with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldClosure
                            (Just (typedVal 1 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.yield" $ "reussir.closure.yield" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1" `isInfixOf` resultStr
            , testCase "Yield Codegen with YieldRegion without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldRegion
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.region.yield" $ "reussir.region.yield" `isInfixOf` resultStr
            , testCase "Yield Codegen with YieldRegion with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldRegion
                            (Just (typedVal 1 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.region.yield" $ "reussir.region.yield" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1" `isInfixOf` resultStr
            , testCase "Yield Codegen with YieldScf without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldScf
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.yield" $ "scf.yield" `isInfixOf` resultStr
            , testCase "Yield Codegen with YieldScf with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.Yield
                            IR.YieldScf
                            (Just (typedVal 1 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.yield" $ "scf.yield" `isInfixOf` resultStr
                assertBool "Should contain result %1" $ "%1" `isInfixOf` resultStr
            ]
        , testGroup
            "ClosureCreate"
            [ testCase "ClosureCreate Codegen" $ do
                let closureType = TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64)
                result <-
                    runCodegenForInstr
                        ( IR.ClosureCreate
                            (blockWithArgs [typedVal 1 primitiveI32] [IR.Return (Just (typedVal 2 primitiveI64))])
                            (typedVal 3 closureType)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.create" $ "reussir.closure.create" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3 = " `isInfixOf` resultStr
                assertBool "Should contain -> for type" $ " -> " `isInfixOf` resultStr
                assertBool "Should contain body" $ "body" `isInfixOf` resultStr
                assertBool "Should contain block structure" $ "{" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%3 = " "reussir.closure.create" resultStr
            ]
        , testGroup
            "ClosureApply"
            [ testCase "ClosureApply Codegen" $ do
                let closureType = TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64)
                result <-
                    runCodegenForInstr
                        ( IR.ClosureApply
                            (typedVal 1 closureType)
                            (typedVal 2 primitiveI32)
                            (typedVal 3 primitiveI64)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.apply" $ "reussir.closure.apply" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3 = " `isInfixOf` resultStr
                assertBool "Should contain target %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain argument %2" $ "%2" `isInfixOf` resultStr
                assertBool "Should contain type annotation" $ " : " `isInfixOf` resultStr
                assertBool "Target should come before argument" $ comesAfter "%1" "%2" resultStr
            ]
        , testGroup
            "ClosureEval"
            [ testCase "ClosureEval Codegen without result" $ do
                let closureType = TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64)
                result <-
                    runCodegenForInstr
                        ( IR.ClosureEval
                            (typedVal 1 closureType)
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.eval" $ "reussir.closure.eval" `isInfixOf` resultStr
                assertBool "Should contain target %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should not contain result assignment" $ not ("%2 = " `isInfixOf` resultStr)
            , testCase "ClosureEval Codegen with result" $ do
                let closureType = TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64)
                result <-
                    runCodegenForInstr
                        ( IR.ClosureEval
                            (typedVal 1 closureType)
                            (Just (typedVal 2 primitiveI64))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.eval" $ "reussir.closure.eval" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain type annotation" $ " : " `isInfixOf` resultStr
            ]
        , testGroup
            "ClosureUniqify"
            [ testCase "ClosureUniqify Codegen" $ do
                let closureType = TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64)
                result <-
                    runCodegenForInstr
                        ( IR.ClosureUniqify
                            (typedVal 1 closureType)
                            (typedVal 2 closureType)
                        )
                let resultStr = T.unpack result
                assertBool "Should contain reussir.closure.uniqify" $ "reussir.closure.uniqify" `isInfixOf` resultStr
                assertBool "Should contain result %2" $ "%2 = " `isInfixOf` resultStr
                assertBool "Should contain input %1" $ "%1" `isInfixOf` resultStr
                assertBool "Result should come before operation" $ comesAfter "%2 = " "reussir.closure.uniqify" resultStr
            ]
        , testGroup
            "IfThenElse"
            [ testCase "IfThenElse Codegen without else and without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.IfThenElse
                            (typedVal 1 primitiveBool)
                            (blockWithArgs [] [IR.Return Nothing])
                            Nothing
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain condition %1" $ "%1" `isInfixOf` resultStr
                assertBool "Should contain then block" $ "{" `isInfixOf` resultStr
                assertBool "Should not contain else" $ not ("else" `isInfixOf` resultStr)
                assertBool "Should not contain result assignment" $ not ("%2 = " `isInfixOf` resultStr)
            , testCase "IfThenElse Codegen with else and without result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.IfThenElse
                            (typedVal 1 primitiveBool)
                            (blockWithArgs [] [IR.Return Nothing])
                            (Just (blockWithArgs [] [IR.Return Nothing]))
                            Nothing
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain else" $ "else" `isInfixOf` resultStr
                assertBool "Then should come before else" $ comesAfter "{" "else" resultStr
            , testCase "IfThenElse Codegen without else with result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.IfThenElse
                            (typedVal 1 primitiveBool)
                            (blockWithArgs [] [IR.Return (Just (typedVal 2 primitiveI32))])
                            Nothing
                            (Just (typedVal 3 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain result %3" $ "%3 = " `isInfixOf` resultStr
                assertBool "Should contain -> for result" $ " -> " `isInfixOf` resultStr
            , testCase "IfThenElse Codegen with else and result" $ do
                result <-
                    runCodegenForInstr
                        ( IR.IfThenElse
                            (typedVal 1 primitiveBool)
                            (blockWithArgs [] [IR.Return (Just (typedVal 2 primitiveI32))])
                            (Just (blockWithArgs [] [IR.Return (Just (typedVal 3 primitiveI32))]))
                            (Just (typedVal 4 primitiveI32))
                        )
                let resultStr = T.unpack result
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain result %4" $ "%4 = " `isInfixOf` resultStr
                assertBool "Should contain else" $ "else" `isInfixOf` resultStr
                assertBool "Result should come before condition" $ comesAfter "%4 = " "scf.if" resultStr
            ]
        ]
