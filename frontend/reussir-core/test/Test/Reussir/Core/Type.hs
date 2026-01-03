{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core.Type (tests) where

import Reussir.Core.Type
import Reussir.Core.Types
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Lexer (Path (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Type"
        [ testCase "containsGeneric" $ do
            let gid1 = GenericID 1
                gid2 = GenericID 2
                t1 = TypeGeneric gid1
                t2 = TypeGeneric gid2
                t3 = TypeRecord (Path "Foo" []) [t1, t2]
                t4 = TypeClosure [t1] t2
                t5 = TypeRc t1 Shared
                t6 = TypeRef t1 Shared
                t7 = TypeIntegral (Signed 64)

            containsGeneric t1 gid1 @?= True
            containsGeneric t1 gid2 @?= False
            containsGeneric t3 gid1 @?= True
            containsGeneric t3 gid2 @?= True
            containsGeneric t4 gid1 @?= True
            containsGeneric t4 gid2 @?= True
            containsGeneric t5 gid1 @?= True
            containsGeneric t5 gid2 @?= False
            containsGeneric t6 gid1 @?= True
            containsGeneric t6 gid2 @?= False
            containsGeneric t7 gid1 @?= False
        ]
