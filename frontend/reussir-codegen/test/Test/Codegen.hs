{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen (
    codegenTests,
)
where

import Data.Int (Int64)
import Data.Text qualified as T
import Effectful qualified as E
import Effectful.Log qualified as L
import Log (defaultLogLevel)
import Log.Backend.StandardOutput qualified as L
import Reussir.Bridge qualified as B
import Reussir.Codegen qualified as C
import Reussir.Codegen.Context (TargetSpec (..))
import Reussir.Codegen.Context.Path (pathSingleton)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V
import Test.Tasty
import Test.Tasty.HUnit

-- Helper types
primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

-- Helper functions for creating test values
val :: Int64 -> V.Value
val = V.Value

typedVal :: Int64 -> TT.Type -> V.TypedValue
typedVal v t = (val v, t)

f32val :: Int64 -> V.TypedValue
f32val v = typedVal v primitiveF32

-- Helper to check if a string is present in the output
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

-- Create a simple f32 addition function: f32 a + b
createAddF32Function :: IR.Function
createAddF32Function =
    IR.Function
        { funcLinkage = IR.LnkExternal
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcPath = pathSingleton "add_f32"
        , funcArgs = [f32val 0, f32val 1]
        , funcResult = primitiveF32
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [f32val 0, f32val 1]
                    , blkBody =
                        [ IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f32val 0, f32val 1]
                                [f32val 2]
                            )
                        , IR.Return (Just (f32val 2))
                        ]
                    }
                )
        }

-- Create a simple module with the add_f32 function
createSimpleModule :: C.Module
createSimpleModule =
    C.Module
        { C.moduleFunctions = [createAddF32Function]
        , C.moduleSpec =
            TargetSpec
                "test_module"
                "/tmp/output.o"
                B.OptDefault
                B.OutputObject
                B.LogInfo
        , C.recordInstances = []
        }

codegenTests :: TestTree
codegenTests =
    testGroup
        "Codegen"
        [ testGroup
            "emitModuleToText"
            [ testCase "emitModuleToText produces MLIR output" $ do
                let module' = createSimpleModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain module declaration" $ "module" `isInfixOf` resultStr
                assertBool "Should contain func.func" $ "func.func" `isInfixOf` resultStr
                assertBool "Should contain function name add_f32" $ "add_f32" `isInfixOf` resultStr
                assertBool "Should contain f32 type" $ "f32" `isInfixOf` resultStr
                assertBool "Should contain arith.addf" $ "arith.addf" `isInfixOf` resultStr
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain function arguments" $ "%0" `isInfixOf` resultStr && "%1" `isInfixOf` resultStr
                assertBool "Should contain result" $ "%2" `isInfixOf` resultStr
            , testCase "emitModuleToText contains correct function signature" $ do
                let module' = createSimpleModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain external visibility" $ "external" `isInfixOf` resultStr
                assertBool "Should contain multiple f32 references" $ T.count "f32" (T.pack resultStr) >= 3
            ]
        , testGroup
            "emitModuleToBackend"
            [ testCase "emitModuleToBackend executes without error" $ do
                let module' = createSimpleModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                            E.liftIO $ putStrLn ""
                            C.emitModuleToBackend module'
                            pure True
                assertBool "Should complete successfully" result
            ]
        ]
