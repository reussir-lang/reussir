{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.TypeSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Reussir.Parser.Type
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Type

spec :: Spec
spec = do
    describe "parseIntegralType" $ do
        it "parses i8" $
            parse parseIntegralType "" "i8" `shouldParse` TypeIntegral (Signed 8)
        it "parses u8" $
            parse parseIntegralType "" "u8" `shouldParse` TypeIntegral (Unsigned 8)
        it "parses i16" $
            parse parseIntegralType "" "i16" `shouldParse` TypeIntegral (Signed 16)
        it "parses u16" $
            parse parseIntegralType "" "u16" `shouldParse` TypeIntegral (Unsigned 16)
        it "parses i32" $
            parse parseIntegralType "" "i32" `shouldParse` TypeIntegral (Signed 32)
        it "parses u32" $
            parse parseIntegralType "" "u32" `shouldParse` TypeIntegral (Unsigned 32)
        it "parses i64" $
            parse parseIntegralType "" "i64" `shouldParse` TypeIntegral (Signed 64)
        it "parses u64" $
            parse parseIntegralType "" "u64" `shouldParse` TypeIntegral (Unsigned 64)

        it "fails on invalid width" $
            parse parseIntegralType "" `shouldFailOn` "i7"

        it "fails if followed by identifier char" $
            parse parseIntegralType "" `shouldFailOn` "i8x"

    describe "parseFPType" $ do
        it "parses f16" $
            parse parseFPType "" "f16" `shouldParse` TypeFP (IEEEFloat 16)
        it "parses f32" $
            parse parseFPType "" "f32" `shouldParse` TypeFP (IEEEFloat 32)
        it "parses f64" $
            parse parseFPType "" "f64" `shouldParse` TypeFP (IEEEFloat 64)
        it "parses bfloat16" $
            parse parseFPType "" "bfloat16" `shouldParse` TypeFP BFloat16
        it "parses float8" $
            parse parseFPType "" "float8" `shouldParse` TypeFP Float8

        it "fails if followed by identifier char" $
            parse parseFPType "" `shouldFailOn` "f32x"

    describe "parseTypeExpr" $ do
        it "parses simple type" $
            parse parseTypeExpr "" "MyType"
                `shouldParse` TypeExpr (Path (Identifier "MyType") []) []

        it "parses qualified type" $
            parse parseTypeExpr "" "std::vec::Vec"
                `shouldParse` TypeExpr (Path (Identifier "Vec") [Identifier "std", Identifier "vec"]) []

        it "parses generic type" $
            parse parseTypeExpr "" "Vec<i32>"
                `shouldParse` TypeExpr (Path (Identifier "Vec") []) [TypeIntegral (Signed 32)]

        it "parses nested generic type" $
            parse parseTypeExpr "" "Map<str, Vec<i32>>"
                `shouldParse` TypeExpr
                    (Path (Identifier "Map") [])
                    [ TypeStr
                    , TypeExpr (Path (Identifier "Vec") []) [TypeIntegral (Signed 32)]
                    ]

    describe "parseType" $ do
        it "parses bool" $
            parse parseType "" "bool" `shouldParse` TypeBool
        it "parses str" $
            parse parseType "" "str" `shouldParse` TypeStr
        it "parses unit" $
            parse parseType "" "unit" `shouldParse` TypeUnit

        it "parses integral type via parseType" $
            parse parseType "" "i32" `shouldParse` TypeIntegral (Signed 32)

        it "parses fp type via parseType" $
            parse parseType "" "f64" `shouldParse` TypeFP (IEEEFloat 64)

        it "parses type expr via parseType" $
            parse parseType "" "Option<bool>"
                `shouldParse` TypeExpr (Path (Identifier "Option") []) [TypeBool]

        it "returns spanned type by default" $ do
            let res = parse parseType "" "i32"
            case res of
                Right (TypeSpanned _) -> pure ()
                Right _ -> expectationFailure "Expected TypeSpanned"
                Left e -> expectationFailure (errorBundlePretty e)

    describe "parseArrowType" $ do
        it "parses simple arrow" $
            parse parseArrowType "" "i32 -> bool"
                `shouldParse` TypeArrow (TypeIntegral (Signed 32)) TypeBool

        it "parses right associative arrow" $
            parse parseArrowType "" "i32 -> i32 -> bool"
                `shouldParse` TypeArrow
                    (TypeIntegral (Signed 32))
                    (TypeArrow (TypeIntegral (Signed 32)) TypeBool)

        it "parses arrow with parens" $
            parse parseArrowType "" "(i32 -> i32) -> bool"
                `shouldParse` TypeArrow
                    (TypeArrow (TypeIntegral (Signed 32)) (TypeIntegral (Signed 32)))
                    TypeBool

    describe "parenthesized types" $ do
        it "parses parenthesized type" $
            parse parseType "" "(i32)" `shouldParse` TypeIntegral (Signed 32)

        it "parses nested parens" $
            parse parseType "" "((i32))" `shouldParse` TypeIntegral (Signed 32)

    describe "Eq Type" $ do
        it "ignores span in TypeSpanned" $ do
            let t1 = TypeBool
                t2 = TypeSpanned (WithSpan TypeBool 0 4)
                t3 = TypeSpanned (WithSpan TypeBool 10 14)
            t1 `shouldBe` t2
            t2 `shouldBe` t1
            t2 `shouldBe` t3

        it "distinguishes different types even with spans" $ do
            let t1 = TypeSpanned (WithSpan TypeBool 0 4)
                t2 = TypeSpanned (WithSpan TypeStr 0 4)
            t1 `shouldNotBe` t2
