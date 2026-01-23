{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.StmtSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Data.Vector.Strict qualified as V
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
stripExprSpans (Let n t e) = Let n t (stripExprSpans e)
stripExprSpans (FuncCallExpr (FuncCall p tys es)) = FuncCallExpr (FuncCall p tys (map stripExprSpans es))
stripExprSpans (Lambda n t e) = Lambda n t (stripExprSpans e)
stripExprSpans (Match e cases) = Match (stripExprSpans e) (fmap (\(p, ex) -> (p, stripExprSpans ex)) cases)
stripExprSpans (RegionalExpr e) = RegionalExpr (stripExprSpans e)
stripExprSpans (CtorCallExpr (CtorCall p tys args)) = CtorCallExpr (CtorCall p tys (map (\(i, e) -> (i, stripExprSpans e)) args))
stripExprSpans (AccessChain e accesses) = AccessChain (stripExprSpans e) accesses
stripExprSpans (ExprSeq es) = ExprSeq (map stripExprSpans es)
stripExprSpans e = e

stripStmtSpans :: Stmt -> Stmt
stripStmtSpans (SpannedStmt (WithSpan s _ _)) = stripStmtSpans s
stripStmtSpans (FunctionStmt f) = FunctionStmt (f{funcBody = fmap stripExprSpans (funcBody f)})
stripStmtSpans (RecordStmt r) = RecordStmt (r{recordFields = stripFields (recordFields r)})
  where
    stripFields (Named fs) = Named (V.map (\(WithSpan (n, t, f) _ _) -> WithSpan (n, t, f) 0 0) fs)
    stripFields (Unnamed fs) = Unnamed (V.map (\(WithSpan (t, f) _ _) -> WithSpan (t, f) 0 0) fs)
    stripFields (Variants vs) = Variants (V.map (\(WithSpan (n, ts) _ _) -> WithSpan (n, ts) 0 0) vs)

dummyWithSpan :: a -> WithSpan a
dummyWithSpan x = WithSpan x 0 0

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
                        (Just (ExprSeq [ConstExpr (ConstInt 0)]))
                    )

        it "parses function with capabilities" $
            (stripStmtSpans <$> parse parseFuncDef "" "fn foo(x: [flex] i32) -> [flex] i32 { 0 }")
                `shouldParse` FunctionStmt
                    ( Function
                        Private
                        (Identifier "foo")
                        []
                        [(Identifier "x", TypeIntegral (Signed 32), True)]
                        (Just (TypeIntegral (Signed 32), True))
                        False
                        (Just (ExprSeq [ConstExpr (ConstInt 0)]))
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
                        (Just (ExprSeq [ConstExpr (ConstInt 0)]))
                    )

        it "parses extern function" $
            (stripStmtSpans <$> parse parseFuncDef "" "fn foo();")
                `shouldParse` FunctionStmt
                    ( Function
                        Private
                        (Identifier "foo")
                        []
                        []
                        Nothing
                        False
                        Nothing
                    )

    describe "parseStructDec" $ do
        it "parses simple struct" $
            (stripStmtSpans <$> parse parseStructDec "" "struct Point (i32, i32)")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed $ V.fromList [dummyWithSpan (TypeIntegral (Signed 32), False), dummyWithSpan (TypeIntegral (Signed 32), False)])
                        StructKind
                        Private
                        Shared
                    )

        it "parses public struct" $
            (stripStmtSpans <$> parse parseStructDec "" "pub struct Point (i32, i32)")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed $ V.fromList [dummyWithSpan (TypeIntegral (Signed 32), False), dummyWithSpan (TypeIntegral (Signed 32), False)])
                        StructKind
                        Public
                        Shared
                    )

        it "parses named struct" $
            (stripStmtSpans <$> parse parseStructDec "" "struct Point { x: i32, y: i32 }")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Named $ V.fromList [dummyWithSpan (Identifier "x", TypeIntegral (Signed 32), False), dummyWithSpan (Identifier "y", TypeIntegral (Signed 32), False)])
                        StructKind
                        Private
                        Shared
                    )

        it "parses named struct with capabilities" $
            (stripStmtSpans <$> parse parseStructDec "" "struct Point { x: [field] i32, y: [field] i32 }")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Named $ V.fromList [dummyWithSpan (Identifier "x", TypeIntegral (Signed 32), True), dummyWithSpan (Identifier "y", TypeIntegral (Signed 32), True)])
                        StructKind
                        Private
                        Shared
                    )

        it "parses unnamed struct with capabilities" $
            (stripStmtSpans <$> parse parseStructDec "" "struct Point ([field] i32, [field] i32)")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed $ V.fromList [dummyWithSpan (TypeIntegral (Signed 32), True), dummyWithSpan (TypeIntegral (Signed 32), True)])
                        StructKind
                        Private
                        Shared
                    )

        it "parses struct with default capability" $
            (stripStmtSpans <$> parse parseStructDec "" "struct [value] Point (i32, i32)")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Point")
                        []
                        (Unnamed $ V.fromList [dummyWithSpan (TypeIntegral (Signed 32), False), dummyWithSpan (TypeIntegral (Signed 32), False)])
                        StructKind
                        Private
                        Value
                    )

    describe "parseEnumDec" $ do
        it "parses simple enum" $
            (stripStmtSpans <$> parse parseEnumDec "" "enum Option<T> { Some(T), None }")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Option")
                        [(Identifier "T", [])]
                        ( Variants $
                            V.fromList
                                [ dummyWithSpan (Identifier "Some", V.fromList [TypeExpr (Path (Identifier "T") []) []])
                                , dummyWithSpan (Identifier "None", V.empty)
                                ]
                        )
                        EnumKind
                        Private
                        Shared
                    )

        it "parses public enum" $
            (stripStmtSpans <$> parse parseEnumDec "" "pub enum Result<T, E> { Ok(T), Err(E) }")
                `shouldParse` RecordStmt
                    ( Record
                        (Identifier "Result")
                        [(Identifier "T", []), (Identifier "E", [])]
                        ( Variants $
                            V.fromList
                                [ dummyWithSpan (Identifier "Ok", V.fromList [TypeExpr (Path (Identifier "T") []) []])
                                , dummyWithSpan (Identifier "Err", V.fromList [TypeExpr (Path (Identifier "E") []) []])
                                ]
                        )
                        EnumKind
                        Public
                        Shared
                    )
