{-# LANGUAGE OverloadedStrings #-}

import System.Log.Logger (Priority (WARNING), getLogger, saveGlobalLogger, setLevel)
import Test.Codegen.Context.Module qualified as Module
import Test.Codegen.Intrinsics.Arith qualified as Arith
import Test.Codegen.Intrinsics.Math qualified as Math
import Test.Codegen.Location qualified as Location
import Test.Codegen.Type.Mangle qualified as Mangle
import Test.Tasty

initializeCodegenLogger :: IO ()
initializeCodegenLogger = do
    logger <- getLogger "Reussir.Codegen"
    -- Turn this to DEBUG to see codegen logs
    saveGlobalLogger $ setLevel WARNING logger

main :: IO ()
main = initializeCodegenLogger >> defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ Arith.arithTests
        , Math.mathTests
        , Mangle.mangleTests
        , Location.locationTests
        , Module.moduleTests
        ]
