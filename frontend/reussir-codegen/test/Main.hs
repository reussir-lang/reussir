{-# LANGUAGE OverloadedStrings #-}

import Test.Codegen.Intrinsic.Arith qualified as Arith
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Arith.arithTests]
