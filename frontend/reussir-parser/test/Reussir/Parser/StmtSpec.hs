{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.StmtSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Reussir.Parser.Stmt
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Expr hiding (Named, Unnamed)
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Stmt
import Reussir.Parser.Types.Type

stripExprSpans :: Expr -> Expr
stripExprSpans (SpannedExpr (WithSpan e _ _)) = stripExprSpans e
stripExprSpans (BinOpExpr op e1 e2) = BinOpExpr op (stripExprSpans e1) (stripExprSpans e2)
stripExprSpans (UnaryOpExpr op e) = UnaryOpExpr op (stripExprSpans e)
stripExprSpans (If e1 e2 e3) = If (stripExprSpans e1) (stripExprSpans e2) (stripExprSpans e3)
stripExprSpans (Cast t e) = Cast t (stripExprSpans e)
stripExprSpans (LetIn n t e1 e2) = LetIn n t (stripExprSpans e1) (stripExprSpans e2)
stripExprSpans (FuncCallExpr (FuncCall p tys es)) = FuncCallExpr (FuncCall p tys (map stripExprSpans es))
stripExprSpans (Lambda n t e) = Lambda n t (stripExprSpans e)
stripExprSpans (Match e cases) = Match (stripExprSpans e) (map (\(p, ex) -> (p, stripExprSpans ex)) cases)
stripExprSpans (RegionalExpr e) = RegionalExpr (stripExprSpans e)
stripExprSpans (CtorCallExpr (CtorCall p v tys args)) = CtorCallExpr (CtorCall p v tys (map (\(i, e) -> (i, stripExprSpans e)) args))
stripExprSpans (AccessChain e accesses) = AccessChain (stripExprSpans e) accesses
stripExprSpans e = e

stripStmtSpans :: Stmt -> Stmt
stripStmtSpans (SpannedStmt (WithSpan s _ _)) = stripStmtSpans s
stripStmtSpans (FunctionStmt f) = FunctionStmt (f{funcBody = stripExprSpans (funcBody f)})
stripStmtSpans s = s

spec :: Spec
spec = do
    describe "parseFuncDef" $ do
        it "parses simple function" $
            (stripStmtSpans <$> parse parseFuncDef "" "fn foo() { 0 }")
                `shouldParse` FunctionStmt
                    ( Function
                        Private
                        (Identifier "foo")
                        []
                        []
                        Nothing
                        False
                        (ConstExpr (ConstInt 0))
                    )

        it "parses function with capabilities" $
            (stripStmtSpans <$> parse parseFuncDef "" "fn foo(x: [shared] i32) -> [value] i32 { 0 }")
                `shouldParse` FunctionStmt
                    ( Function
                        Private
                        (Identifier "foo")
                        []
                        [(Identifier "x", TypeIntegral (Signed 32), Shared)]
                        (Just (TypeIntegral (Signed 32), Value))
                        False
                        (ConstExpr (ConstInt 0))
                    )

        it "parses regional function" $
            (stripStmtSpans <$> parse parseFuncDef "" "pub regional fn foo() { 0 }")
                `shouldParse` FunctionStmt
                    ( Function
                        Public
                        (Identifier "foo")
                        []
                        []
                        Nothing
                        True
                        (ConstExpr (ConstInt 0))
                    )

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
                        Unspecified
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
                        Unspecified
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
                        Unspecified
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
                        Unspecified
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
                        Unspecified
                    )

        it "parses struct with default capability" $
            parse parseStructDec "" "struct [value] Point (i32, i32)"
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed [(TypeIntegral (Signed 32), Unspecified), (TypeIntegral (Signed 32), Unspecified)])
                        StructKind
                        Private
                        Value
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
                        Unspecified
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
                        Unspecified
                    )
