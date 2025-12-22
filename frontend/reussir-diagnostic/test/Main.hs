{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function ((&))
import Data.IntMap.Lazy qualified as IntMap
import Data.Text.Lazy qualified as Text
import Effectful (runEff)
import Reussir.Diagnostic.Display
import Reussir.Diagnostic.LineCache
import Reussir.Diagnostic.Report
import Reussir.Diagnostic.Repository
import System.Console.ANSI.Types qualified as ANSI
import System.Directory (doesFileExist)
import System.IO (stdout)
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
        , testCase "Display" testDisplay
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
            [ SelectedLine 1 "abc" 0 3 3 3
            , SelectedLine 2 "de" 4 6 1 2
            , SelectedLine 3 "fghi" 7 11 1 1
            ]
    assertEqual "selectLines should return overlapping lines with scope info" expected selected

testSelectLinesNone :: Assertion
testSelectLinesNone = do
    let content = Text.unlines ["abc", "de", "fghi"]
        lc = fromFile content
    assertEqual "selectLines should return empty when no overlap" [] (selectLines lc 12 20)

testDisplay :: Assertion
testDisplay = do
    putStrLn "\n\n"
    let path = "/usr/include/search.h"
    exists <- doesFileExist path
    if exists
        then do
            repo <- runEff $ createRepository [path]

            let
                -- Single line reference
                ref1 =
                    defaultCodeRef path 10 20
                        & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid

                -- Multi-line reference
                ref2 =
                    defaultCodeRef path 50 150
                        & addForegroundColorToCodeRef ANSI.Blue ANSI.Vivid

                -- Annotated single line
                ref3 =
                    defaultCodeRef path 200 210
                        & addForegroundColorToCodeRef ANSI.Green ANSI.Vivid
                ann3 =
                    defaultText "This is an annotation"
                        & addForegroundColorToText ANSI.Green ANSI.Vivid

                -- Annotated multi-line
                ref4 =
                    defaultCodeRef path 300 400
                        & addForegroundColorToCodeRef ANSI.Magenta ANSI.Vivid
                ann4 =
                    defaultText "This covers multiple lines"
                        & addForegroundColorToText ANSI.Magenta ANSI.Vivid

                report =
                    Labeled Error (FormattedText [defaultText "Found some issues in search.h"])
                        <> Nested
                            ( codeRef ref1
                                <> annotatedCodeRef ref3 ann3
                                <> codeRef ref2
                                <> annotatedCodeRef ref4 ann4
                            )
            runEff $ displayReport report repo 4 stdout
            putStrLn "\n\n"
        else putStrLn $ "Skipping display test: " ++ path ++ " not found"
