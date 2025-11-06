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
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V
import Test.Tasty
import Test.Tasty.HUnit

-- Helper types
primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

primitiveI128 :: TT.Type
primitiveI128 = TT.TypePrim (TT.PrimInt TT.PrimInt128)

primitiveBool :: TT.Type
primitiveBool = TT.TypePrim TT.PrimBool

-- Helper functions for creating test values
val :: Int64 -> V.Value
val = V.Value

typedVal :: Int64 -> TT.Type -> V.TypedValue
typedVal v t = (val v, t)

f32val :: Int64 -> V.TypedValue
f32val v = typedVal v primitiveF32

i128val :: Int64 -> V.TypedValue
i128val v = typedVal v primitiveI128

boolval :: Int64 -> V.TypedValue
boolval v = typedVal v primitiveBool

-- Helper to create integer constants
i128constant :: Int64 -> V.TypedValue
i128constant v = (val v, primitiveI128)

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
        , funcSymbol = verifiedSymbol "add_f32"
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

-- Create a naive fibonacci function: fib(n: i128) -> i128
-- Naive implementation: if n < 2 then n else fib(n-1) + fib(n-2)
createFibonacciFunction :: IR.Function
createFibonacciFunction =
    IR.Function
        { funcLinkage = IR.LnkExternal
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "fibonacci"
        , funcArgs = [i128val 0] -- n: i128
        , funcResult = primitiveI128
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [i128val 0] -- n
                    , blkBody =
                        [ -- Create constant 2
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "2")))
                                []
                                [i128constant 1]
                            )
                        , -- Create constant 0 for comparison result
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Cmpi I.CISlt))
                                [i128val 0, i128constant 1] -- n < 2
                                [boolval 2]
                            )
                        , -- If n < 2, return n; else compute fib(n-1) + fib(n-2)
                          IR.IfThenElse
                            (boolval 2)
                            -- Then block: yield n
                            ( IR.Block
                                { blkArgs = []
                                , blkBody = [IR.Yield IR.YieldScf (Just (i128val 0))]
                                }
                            )
                            -- Else block: compute fib(n-1) + fib(n-2) and yield
                            ( Just
                                ( IR.Block
                                    { blkArgs = []
                                    , blkBody =
                                        [ -- Create constant 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Constant (read "1")))
                                                []
                                                [i128constant 3]
                                            )
                                        , -- n - 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Subi (I.IntOFFlag 0)))
                                                [i128val 0, i128constant 3]
                                                [i128val 4]
                                            )
                                        , -- Create constant 2
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Constant (read "2")))
                                                []
                                                [i128constant 5]
                                            )
                                        , -- n - 2
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Subi (I.IntOFFlag 0)))
                                                [i128val 0, i128constant 5]
                                                [i128val 6]
                                            )
                                        , -- Call fib(n-1)
                                          IR.FCall
                                            ( IR.FuncCall
                                                { target = verifiedSymbol "fibonacci"
                                                , args = [i128val 4]
                                                , results = Just (i128val 7)
                                                }
                                            )
                                        , -- Call fib(n-2)
                                          IR.FCall
                                            ( IR.FuncCall
                                                { target = verifiedSymbol "fibonacci"
                                                , args = [i128val 6]
                                                , results = Just (i128val 8)
                                                }
                                            )
                                        , -- Add results: fib(n-1) + fib(n-2)
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Addi (I.IntOFFlag 0)))
                                                [i128val 7, i128val 8]
                                                [i128val 9]
                                            )
                                        , -- Yield the sum
                                          IR.Yield IR.YieldScf (Just (i128val 9))
                                        ]
                                    }
                                )
                            )
                            (Just (i128val 10)) -- scf.if returns a value
                        , -- Return the result from scf.if
                          IR.Return (Just (i128val 10))
                        ]
                    }
                )
        }

-- Create a module with the fibonacci function using aggressive optimization
createFibonacciModule :: C.Module
createFibonacciModule =
    C.Module
        { C.moduleFunctions = [createFibonacciFunction]
        , C.moduleSpec =
            TargetSpec
                "fibonacci_module"
                "/tmp/fibonacci.o"
                B.OptAggressive
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
        , testGroup
            "fibonacci"
            [ testCase "emitModuleToText produces MLIR output for naive fibonacci" $ do
                let module' = createFibonacciModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain module declaration" $ "module" `isInfixOf` resultStr
                assertBool "Should contain func.func" $ "func.func" `isInfixOf` resultStr
                assertBool "Should contain function name fibonacci" $ "fibonacci" `isInfixOf` resultStr
                assertBool "Should contain i128 type" $ "i128" `isInfixOf` resultStr
                assertBool "Should contain arith.cmpi" $ "arith.cmpi" `isInfixOf` resultStr
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain func.call" $ "func.call" `isInfixOf` resultStr
                assertBool "Should contain arith.addi" $ "arith.addi" `isInfixOf` resultStr
                assertBool "Should contain arith.subi" $ "arith.subi" `isInfixOf` resultStr
            , testCase "emitModuleToBackend executes fibonacci module with aggressive optimization" $ do
                let module' = createFibonacciModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                            E.liftIO $ putStrLn ""
                            C.emitModuleToBackend module'
                            pure True
                assertBool "Should complete successfully" result
            ]
        ]
