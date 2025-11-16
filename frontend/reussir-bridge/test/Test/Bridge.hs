{-# LANGUAGE OverloadedStrings #-}

module Test.Bridge (
    bridgeTests,
)
where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Foreign (castPtrToFunPtr)
import Foreign.Ptr (FunPtr)
import Reussir.Bridge
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- Module with a simple add function
moduleWithToAdd :: BS.ByteString
moduleWithToAdd = "module { func.func public @\"add\"(%0: i32, %1: i32) -> i32  attributes { llvm.linkage = #llvm.linkage<external>, llvm.visibility = \"default\" } { %2 = arith.addi %0, %1 : i32\n func.return %2 : i32 } }"

foreign import ccall "dynamic"
    getAddFunc :: FunPtr (Int32 -> Int32 -> IO Int32) -> Int32 -> Int32 -> IO Int32

bridgeTests :: TestTree
bridgeTests =
    testGroup
        "Bridge"
        [ testGroup
            "Compiler"
            [ testCase "Compile empty MLIR module" $ do
                withSystemTempDirectory "reussir-test" $ \tmpDir -> do
                    let outputFile = tmpDir </> "empty.o"
                    compileForNativeMachine
                        "module {}"
                        "empty.mlir"
                        outputFile
                        OutputObject
                        OptDefault
                        LogInfo
                    -- If no exception was thrown, the test passes
                    assertBool "Compilation should succeed" True
            ]
        , testGroup
            "JITEngine"
            [ testCase "Create empty JIT engine" $ do
                putStrLn ""
                withJIT (const $ pure "") OptDefault (const $ pure ())
                -- If no exception was thrown, the test passes
                assertBool "JIT creation should succeed" True
            , testCase "Add a function to the JIT engine" $ do
                putStrLn ""
                withJIT (const $ pure "") OptAggressive $ \jit -> do
                    res <- addModule jit moduleWithToAdd
                    assertBool "Failed to add module" res
                    ptr <- lookupSymbol jit "add" False
                    let funcPtr :: FunPtr (Int32 -> Int32 -> IO Int32) = castPtrToFunPtr ptr
                    let func = getAddFunc funcPtr
                    three <- func 1 2
                    assertEqual "1 + 2 should equal 3" 3 three
            , testCase "Add a function to the JIT engine lazily" $ do
                putStrLn ""
                withJIT (const $ pure moduleWithToAdd) OptTPDE $ \jit -> do
                    res <- addLazyModule jit () [("add", symbolFlagExported)]
                    assertBool "Failed to add lazy module" res
                    ptr <- lookupSymbol jit "add" True -- lazily added symbols are mangled
                    let funcPtr :: FunPtr (Int32 -> Int32 -> IO Int32) = castPtrToFunPtr ptr
                    let func = getAddFunc funcPtr
                    three <- func 1 2
                    assertEqual "1 + 2 should equal 3" 3 three
            ]
        ]
