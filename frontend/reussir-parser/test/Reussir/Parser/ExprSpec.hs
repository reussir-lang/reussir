{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.ExprSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

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
stripExprSpans (LetIn n t e1 e2) = LetIn n t (stripExprSpans e1) (stripExprSpans e2)
stripExprSpans (FuncCallExpr (FuncCall p tys es)) = FuncCallExpr (FuncCall p tys (map stripExprSpans es))
stripExprSpans (Lambda n t e) = Lambda n t (stripExprSpans e)
stripExprSpans (Match e cases) = Match (stripExprSpans e) (map (\(p, ex) -> (p, stripExprSpans ex)) cases)
stripExprSpans (RegionalExpr e) = RegionalExpr (stripExprSpans e)
stripExprSpans (CtorCallExpr (CtorCall p tys args)) = CtorCallExpr (CtorCall p tys (map (\(i, e) -> (i, stripExprSpans e)) args))
stripExprSpans (AccessChain e accesses) = AccessChain (stripExprSpans e) accesses
-- use access chain for testing for now. will need to remove this later
stripExprSpans (AccessExpr e a) =
    case stripExprSpans e of
        AccessChain base accs -> AccessChain base (a : accs)
        stripped -> AccessChain stripped [a]
stripExprSpans e = e

spec :: Spec
spec = do
    describe "parseLetIn" $ do
        it "parses simple let" $
            (stripExprSpans <$> parse parseLetIn "" "let x = 1; x")
                `shouldParse` LetIn
                    (WithSpan "x" 4 6)
                    Nothing
                    (ConstExpr (ConstInt 1))
                    (Var (Path "x" []))

        it "parses let with type" $
            (stripExprSpans <$> parse parseLetIn "" "let x: i32 = 1; x")
                `shouldParse` LetIn
                    (WithSpan "x" 4 5)
                    (Just (TypeIntegral (Signed 32), False))
                    (ConstExpr (ConstInt 1))
                    (Var (Path "x" []))

        it "parses let with type and capability" $
            (stripExprSpans <$> parse parseLetIn "" "let x: [flex] i32 = 1; x")
                `shouldParse` LetIn
                    (WithSpan "x" 4 5)
                    (Just (TypeIntegral (Signed 32), True))
                    (ConstExpr (ConstInt 1))
                    (Var (Path "x" []))

    describe "parseRegionalExpr" $ do
        it "parses regional block" $
            (stripExprSpans <$> parse parseRegionalExpr "" "regional { 1 }")
                `shouldParse` RegionalExpr (ConstExpr (ConstInt 1))

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
                `shouldParse` AccessChain (Var (Path "foo" [])) [Named "bar"]

        it "parses nested field access" $
            (stripExprSpans <$> parse parseExpr "" "foo.bar.baz")
                `shouldParse` AccessChain (Var (Path "foo" [])) [Named "baz", Named "bar"]

        it "parses tuple access" $
            (stripExprSpans <$> parse parseExpr "" "foo.0")
                `shouldParse` AccessChain (Var (Path "foo" [])) [Unnamed 0]

        it "parses mixed access" $
            (stripExprSpans <$> parse parseExpr "" "foo.bar.0.baz")
                `shouldParse` AccessChain (Var (Path "foo" [])) [Named "baz", Unnamed 0, Named "bar"]

    describe "parseExpr" $ do
        it "parses less than or equal with potential type arg ambiguity" $
            (stripExprSpans <$> parse parseExpr "" "n <= 1")
                `shouldParse` BinOpExpr Lte (Var (Path "n" [])) (ConstExpr (ConstInt 1))
