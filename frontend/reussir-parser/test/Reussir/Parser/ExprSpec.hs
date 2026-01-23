{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.ExprSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Data.Vector.Strict (fromList)
import Reussir.Parser.Expr
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer (Path (..), WithSpan (..))
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

spec :: Spec
spec = do
    describe "parseLet" $ do
        it "parses simple let" $
            (stripExprSpans <$> parse parseLetIn "" "let x = 1")
                `shouldParse` Let
                    (WithSpan "x" 4 6)
                    Nothing
                    (ConstExpr (ConstInt 1))

        it "parses let with type" $
            (stripExprSpans <$> parse parseLetIn "" "let x: i32 = 1")
                `shouldParse` Let
                    (WithSpan "x" 4 5)
                    (Just (TypeIntegral (Signed 32), False))
                    (ConstExpr (ConstInt 1))

        it "parses let with type and capability" $
            (stripExprSpans <$> parse parseLetIn "" "let x: [flex] i32 = 1")
                `shouldParse` Let
                    (WithSpan "x" 4 5)
                    (Just (TypeIntegral (Signed 32), True))
                    (ConstExpr (ConstInt 1))

    describe "parseRegionalExpr" $ do
        it "parses regional block" $
            (stripExprSpans <$> parse parseRegionalExpr "" "regional { 1 }")
                `shouldParse` RegionalExpr (ExprSeq [ConstExpr (ConstInt 1)])

    describe "parseFuncCallExpr" $ do
        it "parses function call" $
            (stripExprSpans <$> parse parsePathBasedExpr "" "foo(1, 2)")
                `shouldParse` FuncCallExpr (FuncCall (Path "foo" []) [] [ConstExpr (ConstInt 1), ConstExpr (ConstInt 2)])
        it "parses function call with type args" $
            (stripExprSpans <$> parse parsePathBasedExpr "" "foo<i32, _>(1)")
                `shouldParse` FuncCallExpr (FuncCall (Path "foo" []) [Just (TypeIntegral (Signed 32)), Nothing] [ConstExpr (ConstInt 1)])

    describe "parseCtorCallExpr" $ do
        it "parses struct constructor" $
            (stripExprSpans <$> parse parsePathBasedExpr "" "Foo { x: 1 }")
                `shouldParse` CtorCallExpr (CtorCall (Path "Foo" []) [] [(Just "x", ConstExpr (ConstInt 1))])

        it "parses struct constructor with type args" $
            (stripExprSpans <$> parse parsePathBasedExpr "" "Foo<i32> { 1 }")
                `shouldParse` CtorCallExpr (CtorCall (Path "Foo" []) [Just (TypeIntegral (Signed 32))] [(Nothing, ConstExpr (ConstInt 1))])

    {-
    it "parses enum variant constructor" $
        (stripExprSpans <$> parse parsePathBasedExpr "" "List<i32>::Cons(1, xs)")
            `shouldParse` CtorCallExpr (CtorCall (Path "List" []) (Just "Cons") [Just (TypeIntegral (Signed 32))] [(Nothing, ConstExpr (ConstInt 1)), (Nothing, Var (Path "xs" []))])

    it "parses unit variant" $
        (stripExprSpans <$> parse parsePathBasedExpr "" "List<i32>::Nil")
            `shouldParse` CtorCallExpr (CtorCall (Path "List" []) (Just "Nil") [Just (TypeIntegral (Signed 32))] [])
    -}

    describe "parseAccessChain" $ do
        it "parses field access" $
            (stripExprSpans <$> parse parseExpr "" "foo.bar")
                `shouldParse` AccessChain (Var (Path "foo" [])) (fromList [Named "bar"])

        it "parses nested field access" $
            (stripExprSpans <$> parse parseExpr "" "foo.bar.baz")
                `shouldParse` AccessChain (Var (Path "foo" [])) (fromList [Named "bar", Named "baz"])

        it "parses tuple access" $
            (stripExprSpans <$> parse parseExpr "" "foo.0")
                `shouldParse` AccessChain (Var (Path "foo" [])) (fromList [Unnamed 0])

        it "parses mixed access" $
            (stripExprSpans <$> parse parseExpr "" "foo.bar.0.baz")
                `shouldParse` AccessChain (Var (Path "foo" [])) (fromList [Named "bar", Unnamed 0, Named "baz"])

    describe "parseExprSeq" $ do
        it "parses simple sequence" $
            (stripExprSpans <$> parse parseExprSeq "" "{ 1; 2 }")
                `shouldParse` ExprSeq [ConstExpr (ConstInt 1), ConstExpr (ConstInt 2)]

        it "parses sequence with trailing semicolon" $
            (stripExprSpans <$> parse parseExprSeq "" "{ 1; 2; }")
                `shouldParse` ExprSeq [ConstExpr (ConstInt 1), ConstExpr (ConstInt 2)]

        it "parses sequence with let" $
            (stripExprSpans <$> parse parseExprSeq "" "{ let x = 1; x }")
                `shouldParse` ExprSeq
                    [ Let
                        (WithSpan "x" 6 8) -- dummy span; span information is stripped by stripExprSpans, but the Let constructor still requires it
                        Nothing
                        (ConstExpr (ConstInt 1))
                    , Var (Path "x" [])
                    ]
        -- In this grammar, "{ let x = 1; x }" is parsed as a sequence of two expressions: a let-binding and a
        -- subsequent reference to x, which is why the expected value is ExprSeq [Let ..., Var (Path "x" [])].

        it "parses longer sequence" $
            (stripExprSpans <$> parse parseExprSeq "" "{ 1; 2; 3 }")
                `shouldParse` ExprSeq [ConstExpr (ConstInt 1), ConstExpr (ConstInt 2), ConstExpr (ConstInt 3)]

    describe "parseExpr" $ do
        it "parses less than or equal with potential type arg ambiguity" $
            (stripExprSpans <$> parse parseExpr "" "n <= 1")
                `shouldParse` BinOpExpr Lte (Var (Path "n" [])) (ConstExpr (ConstInt 1))
