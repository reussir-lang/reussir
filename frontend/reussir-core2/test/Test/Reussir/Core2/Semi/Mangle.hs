{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core2.Semi.Mangle where

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Core2.Semi.Mangle
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Reussir.Core2.Semi.Mangle"
        [ testGroup
            "B62Num encoding"
            [ testCase "0 -> _" $ mangleB62 0 @?= "_"
            , testCase "1 -> 0_" $ mangleB62 1 @?= "0_"
            , testCase "11 -> a_" $ mangleB62 11 @?= "a_"
            , testCase "62 -> Z_" $ mangleB62 62 @?= "Z_"
            , testCase "63 -> 10_" $ mangleB62 63 @?= "10_"
            , testCase "1000 -> g7_" $ mangleB62 1000 @?= "g7_"
            ]
        , testGroup
            "Identifier encoding"
            [ testCase "ASCII: foo -> 3foo" $ mangleIdent "foo" @?= "3foo"
            , testCase "ASCII: bach -> 4bach" $ mangleIdent "bach" @?= "4bach"
            , testCase "ASCII with digit: 123abc -> 6_123abc" $ mangleIdent "123abc" @?= "6_123abc"
            , testCase "Unicode: gödel -> u8gdel_5qa" $ mangleIdent "gödel" @?= "u8gdel_5qa"
            ]
        , testGroup
            "Path encoding"
            [ testCase "Root: example -> C7example" $
                manglePath (Path "example" []) @?= "C7example"
            , testCase "Nested: mycrate::example -> NvC7mycrate7example" $
                manglePath (Path "example" ["mycrate"]) @?= "NvC7mycrate7example"
            , testCase "Deeply nested: a::example::exampla -> NvNvC1a7example7exampla" $
                manglePath (Path "exampla" ["a", "example"]) @?= "NvNvC1a7example7exampla"
            ]
        ]

-- | Helper to mangle B62Num and extract the text
mangleB62 :: Int -> T.Text
mangleB62 = TB.runBuilder . mangle . B62Num

-- | Helper to mangle Identifier and extract the text
mangleIdent :: T.Text -> T.Text
mangleIdent = TB.runBuilder . mangle . Identifier

-- | Helper to mangle Path and extract the text
manglePath :: Path -> T.Text
manglePath = TB.runBuilder . mangle
