{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IntMap.Lazy qualified as IntMap
import Data.Text.Lazy qualified as Text
import Reussir.Diagnostic.LineCache
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Reussir.Diagnostic"
        [ testGroup
            "LineCache"
            [ testCase "fromFile" testFromFile
            , testCase "selectLines overlap" testSelectLinesOverlap
            , testCase "selectLines none" testSelectLinesNone
            ]
        ]

testFromFile :: Assertion
testFromFile = do
    let content = Text.unlines ["abc", "de", "fghi"]
        -- "abc\nde\nfghi\n"
        -- 0123 456 78901 2
        -- 0: abc
        -- 4: de
        -- 7: fghi
        LineCache cache = fromFile content
        expected =
            IntMap.fromList
                [ (0, "abc")
                , (4, "de")
                , (7, "fghi")
                ]
    assertEqual "LineCache should match expected offsets" expected cache

testSelectLinesOverlap :: Assertion
testSelectLinesOverlap = do
    let content = Text.unlines ["abc", "de", "fghi"]
        lc = fromFile content
        selected = selectLines lc 2 8
        expected =
            [ SelectedLine 1 "abc" 0 3 2 3
            , SelectedLine 2 "de" 4 6 0 2
            , SelectedLine 3 "fghi" 7 11 0 1
            ]
    assertEqual "selectLines should return overlapping lines with scope info" expected selected

testSelectLinesNone :: Assertion
testSelectLinesNone = do
    let content = Text.unlines ["abc", "de", "fghi"]
        lc = fromFile content
    assertEqual "selectLines should return empty when no overlap" [] (selectLines lc 12 20)
