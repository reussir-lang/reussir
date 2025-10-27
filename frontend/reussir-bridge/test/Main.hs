module Main (main) where

import Reussir.Bridge
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

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

  putStrLn ""
  putStrLn "All tests passed!"
