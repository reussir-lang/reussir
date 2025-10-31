{-# LANGUAGE OverloadedStrings #-}

import Test.Codegen.Intrinsics.Arith qualified as Arith
import Test.Codegen.Intrinsics.Math qualified as Math
import Test.Tasty
import System.Log.Logger (saveGlobalLogger, getLogger, Priority(WARNING), setLevel) 

initializeCodegenLogger :: IO ()
initializeCodegenLogger = do
  logger <- getLogger "Reussir.Codegen"
  -- Turn this to DEBUG to see codegen logs
  saveGlobalLogger $ setLevel WARNING logger 
  return ()

main :: IO ()
main = initializeCodegenLogger >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Arith.arithTests, Math.mathTests]
