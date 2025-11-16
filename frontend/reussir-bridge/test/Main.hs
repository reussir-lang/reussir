{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Foreign (castPtrToFunPtr)
import Foreign.Ptr (FunPtr)
import Reussir.Bridge
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

testEmptyJIT :: IO ()
testEmptyJIT = withJIT (const $ pure "") OptDefault (const $ pure ())

moduleWithToAdd :: BS.ByteString
moduleWithToAdd = "module { func.func public @\"add\"(%0: i32, %1: i32) -> i32  attributes { llvm.linkage = #llvm.linkage<external>, llvm.visibility = \"default\" } { %2 = arith.addi %0, %1 : i32\n func.return %2 : i32 } }"

foreign import ccall "dynamic"
    getAddFunc :: FunPtr (Int32 -> Int32 -> IO Int32) -> Int32 -> Int32 -> IO Int32

testJITAddFunc :: IO ()
testJITAddFunc = withJIT (const $ pure "") OptAggressive $ \jit -> do
    res <- addModule jit moduleWithToAdd
    if not res then error "Failed to add module" else pure ()
    ptr <- lookupSymbol jit "add" False
    let funcPtr :: FunPtr (Int32 -> Int32 -> IO Int32) = castPtrToFunPtr ptr
    let func = getAddFunc funcPtr
    three <- func 1 2
    if three /= 3 then error "3 != 3" else pure ()

testJITAddFuncLazily :: IO ()
testJITAddFuncLazily = withJIT (const $ pure moduleWithToAdd) OptTPDE $ \jit -> do
    res <- addLazyModule jit () [("add", symbolFlagExported)]
    if not res then error "Failed to add module" else pure ()
    ptr <- lookupSymbol jit "add" True -- lazily added symbols are mangled
    let funcPtr :: FunPtr (Int32 -> Int32 -> IO Int32) = castPtrToFunPtr ptr
    let func = getAddFunc funcPtr
    three <- func 1 2
    if three /= 3 then error "3 != 3" else pure ()

main :: IO ()
main = do
    putStrLn "Reussir Bridge Test Suite"
    putStrLn "========================="
    putStrLn ""

    -- Test 1: Compile an empty MLIR module
    putStrLn "Test 1: Compiling empty MLIR module..."
    withSystemTempDirectory "reussir-test" $ \tmpDir -> do
        let outputFile = tmpDir </> "empty.o"
        compileForNativeMachine
            "module {}"
            "empty.mlir"
            outputFile
            OutputObject
            OptDefault
            LogInfo
        putStrLn "✓ Successfully compiled empty module"

    -- Test 2: Create an empty JIT engine
    putStrLn "Test 2: Creating empty JIT engine..."
    testEmptyJIT
    putStrLn "✓ Successfully created empty JIT engine"

    -- Test 3: Add a function to the JIT engine
    putStrLn "Test 3: Adding a function to the JIT engine..."
    testJITAddFunc
    putStrLn "✓ Successfully added a function to the JIT engine"

    -- Test 4: Add a function to the JIT engine lazily
    putStrLn "Test 4: Adding a function to the JIT engine lazily..."
    testJITAddFuncLazily
    putStrLn "✓ Successfully added a function to the JIT engine lazily"

    putStrLn ""
    putStrLn "All tests passed!"
