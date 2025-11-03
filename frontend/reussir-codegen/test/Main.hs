{-# LANGUAGE OverloadedStrings #-}

import Test.Codegen.Context.Module qualified as Module
import Test.Codegen.Intrinsics.Arith qualified as Arith
import Test.Codegen.Intrinsics.Math qualified as Math
import Test.Codegen.Location qualified as Location
import Test.Codegen.Type.Mangle qualified as Mangle
import Test.Tasty

main :: IO ()
main = defaultMain tests

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
