{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.StmtSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Reussir.Parser.Stmt
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Reussir.Parser.Types.Stmt
import Reussir.Parser.Types.Type

spec :: Spec
spec = do
    describe "parseStructDec" $ do
        it "parses simple struct" $
            parse parseStructDec "" "struct Point (i32, i32)"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed [(TypeIntegral (Signed 32), Unspecified), (TypeIntegral (Signed 32), Unspecified)])
                        StructKind
                        Private
                    )

        it "parses public struct" $
            parse parseStructDec "" "pub struct Point (i32, i32)"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed [(TypeIntegral (Signed 32), Unspecified), (TypeIntegral (Signed 32), Unspecified)])
                        StructKind
                        Public
                    )

        it "parses named struct" $
            parse parseStructDec "" "struct Point { x: i32, y: i32 }"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Named [(Identifier "x", TypeIntegral (Signed 32), Unspecified), (Identifier "y", TypeIntegral (Signed 32), Unspecified)])
                        StructKind
                        Private
                    )

        it "parses named struct with capabilities" $
            parse parseStructDec "" "struct Point { x: [shared] i32, y: [value] i32 }"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Named [(Identifier "x", TypeIntegral (Signed 32), Shared), (Identifier "y", TypeIntegral (Signed 32), Value)])
                        StructKind
                        Private
                    )

        it "parses unnamed struct with capabilities" $
            parse parseStructDec "" "struct Point ([shared] i32, [value] i32)"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed [(TypeIntegral (Signed 32), Shared), (TypeIntegral (Signed 32), Value)])
                        StructKind
                        Private
                    )

    describe "parseEnumDec" $ do
        it "parses simple enum" $
            parse parseEnumDec "" "enum Option<T> { Some(T), None }"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Option")
                        [Identifier "T"]
                        ( Variants
                            [ (Identifier "Some", [TypeExpr (Path (Identifier "T") []) []])
                            , (Identifier "None", [])
                            ]
                        )
                        EnumKind
                        Private
                    )

        it "parses public enum" $
            parse parseEnumDec "" "pub enum Result<T, E> { Ok(T), Err(E) }"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Result")
                        [Identifier "T", Identifier "E"]
                        ( Variants
                            [ (Identifier "Ok", [TypeExpr (Path (Identifier "T") []) []])
                            , (Identifier "Err", [TypeExpr (Path (Identifier "E") []) []])
                            ]
                        )
                        EnumKind
                        Public
                    )
