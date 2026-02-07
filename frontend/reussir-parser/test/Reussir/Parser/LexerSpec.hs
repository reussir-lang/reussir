{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.LexerSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Reussir.Parser.Lexer
import Reussir.Parser.Types.Lexer

spec :: Spec
spec = do
    describe "parsePath" $ do
        it "parses a simple identifier" $ do
            parse parsePath "" "foo" `shouldParse` Path (Identifier "foo") []

        it "parses a path with one segment" $ do
            parse parsePath "" "std::io"
                `shouldParse` Path (Identifier "io") [Identifier "std"]

        it "parses a path with multiple segments" $ do
            parse parsePath "" "std::io::File"
                `shouldParse` Path (Identifier "File") [Identifier "std", Identifier "io"]

        it "fails on empty input" $ do
            parse parsePath "" "" `shouldFailWith` err 0 ueof

        it "fails on trailing double colon" $ do
            parse parsePath "" "std::" `shouldFailWith` err 5 ueof

    describe "comments" $ do
        it "skips line comments" $ do
            parse (parseIdentifier <* eof) "" "foo // this is a comment"
                `shouldParse` Identifier "foo"
