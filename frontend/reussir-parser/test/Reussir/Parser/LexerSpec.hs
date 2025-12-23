{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Parser.LexerSpec (spec) where

import Reussir.Parser.Lexer
import Reussir.Parser.Types.Lexer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Data.Either (isLeft)

spec :: Spec
spec = do
    describe "parsePath" $ do
        it "parses a simple identifier" $ do
            parse parsePath "" "foo" `shouldParse` Path (Identifier "foo") []

        it "parses a path with one segment" $ do
            parse parsePath "" "std::io" `shouldParse` Path (Identifier "io") [Identifier "std"]

        it "parses a path with multiple segments" $ do
            parse parsePath "" "std::io::File" `shouldParse` Path (Identifier "File") [Identifier "std", Identifier "io"]

        it "fails on empty input" $ do
            parse parsePath "" "" `shouldSatisfy` isLeft

        it "fails on trailing double colon" $ do
            parse parsePath "" "std::" `shouldSatisfy` isLeft
