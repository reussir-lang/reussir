{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core.Semi.TyckSpec (spec) where

import Test.Hspec

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as UV

import Reussir.Core.Data.Semi.Expr (
    ClosureExpr (..),
    DecisionTree (..),
    Expr (..),
    ExprKind (..),
    PatternVarRef (..),
 )
import Reussir.Core.Data.Semi.Type (Type (TypeUnit))
import Reussir.Core.Data.UniqueID (ExprID (..), VarID (..))
import Reussir.Core.Semi.Tyck (collectExprFreeVars)

spec :: Spec
spec = do
    describe "collectExprFreeVars" $ do
        it "collects free vars in regular expression trees" $ do
            let expr =
                    mkExpr $
                        ClosureCall
                            (mkVar 1)
                            [ mkExpr $ Proj (mkVar 2) (UV.fromList [0, 1])
                            , mkExpr $ NullableCall (Just (mkVar 3))
                            ]
            collectExprFreeVars expr `shouldBe` IntSet.fromList [1, 2, 3]

        it "respects let-scope within sequence expressions" $ do
            let expr =
                    mkExpr $
                        Sequence
                            [ mkLet 1 (mkVar 0)
                            , mkVar 1
                            , mkExpr $ Sequence [mkVar 1, mkVar 2]
                            ]
            collectExprFreeVars expr `shouldBe` IntSet.fromList [0, 2]

        it "treats closure args/captures as inner scope but keeps captures as free vars" $ do
            let expr =
                    mkClosure
                        [3, 4]
                        [1, 2]
                        (mkExpr $ Sequence [mkVar 1, mkVar 2, mkVar 3, mkVar 5])
            collectExprFreeVars expr `shouldBe` IntSet.fromList [3, 4, 5]

        it "lets nested closures contribute free vars through their captures" $ do
            let inner =
                    mkClosure
                        [7]
                        [8]
                        (mkExpr $ Sequence [mkVar 7, mkVar 8])
                outer = mkClosure [] [] (mkExpr $ Sequence [inner, mkVar 9])
            collectExprFreeVars outer `shouldBe` IntSet.fromList [7, 9]

        it "does not leak free vars when nested closure captures are bound by outer closure" $ do
            let inner = mkClosure [7] [] (mkVar 7)
                outer = mkClosure [] [7] inner
            collectExprFreeVars outer `shouldBe` IntSet.empty

        it "respects decision-tree leaf bindings" $ do
            let dt = DTLeaf (mkExpr $ Sequence [mkVar 1, mkVar 2]) (mkBindings [1])
                expr = mkExpr $ Match (mkVar 0) dt
            collectExprFreeVars expr `shouldBe` IntSet.fromList [0, 2]

        it "does not apply DTGuard bindings to the false branch" $ do
            let dt =
                    DTGuard
                        (mkBindings [1])
                        (mkVar 1)
                        (DTLeaf (mkVar 1) (mkBindings [1]))
                        (DTLeaf (mkVar 1) IntMap.empty)
                expr = mkExpr $ Match (mkVar 0) dt
            collectExprFreeVars expr `shouldBe` IntSet.fromList [0, 1]

mkExpr :: ExprKind -> Expr
mkExpr kind = Expr{exprKind = kind, exprSpan = Nothing, exprType = TypeUnit, exprID = ExprID 0}

mkVar :: Int -> Expr
mkVar vid = mkExpr $ Var (VarID vid)

mkLet :: Int -> Expr -> Expr
mkLet vid rhs =
    mkExpr $
        Let
            { letVarSpan = Nothing
            , letVarID = VarID vid
            , letVarName = "v"
            , letVarExpr = rhs
            }

mkClosure :: [Int] -> [Int] -> Expr -> Expr
mkClosure captures args body =
    mkExpr $
        Closure $
            ClosureExpr
                { closureExprCaptures = map (\vid -> (VarID vid, TypeUnit)) captures
                , closureExprArgs = map (\vid -> (VarID vid, TypeUnit)) args
                , closureExprBody = body
                }

mkBindings :: [Int] -> IntMap.IntMap PatternVarRef
mkBindings vids =
    IntMap.fromList $
        map (\vid -> (vid, PatternVarRef Seq.empty)) vids
