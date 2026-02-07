{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import Test.Codegen qualified as Codegen
import Test.Codegen.Context.Module qualified as Module
import Test.Codegen.IR qualified as IR
import Test.Codegen.Intrinsics.Arith qualified as Arith
import Test.Codegen.Intrinsics.Math qualified as Math
import Test.Codegen.Location qualified as Location

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ Arith.arithTests
        , Math.mathTests
        , Location.locationTests
        , Module.moduleTests
        , IR.irTests
        , Codegen.codegenTests
        ]
