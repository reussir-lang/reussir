{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import qualified Data.Text as T
import Reussir.Parser.Types.Lexer
import Reussir.Parser.Types.Type

-- Helper to construct deep types
deepArrow :: Int -> Type
deepArrow 0 = TypeBool
deepArrow n = TypeArrow TypeBool (deepArrow (n - 1))

-- Orphan instances for NFData to support benchmarking

instance NFData IntegralType where
    rnf (Signed _) = ()
    rnf (Unsigned _) = ()

instance NFData FloatingPointType where
    rnf (IEEEFloat _) = ()
    rnf BFloat16 = ()
    rnf Float8 = ()

instance NFData Identifier where
    rnf (Identifier t) = rnf t

instance NFData Path where
    rnf (Path base segs) = rnf base `seq` rnf segs

instance NFData a => NFData (WithSpan a) where
    rnf (WithSpan val start end) = rnf val `seq` rnf start `seq` rnf end

instance NFData Type where
    rnf (TypeExpr p args) = rnf p `seq` rnf args
    rnf (TypeIntegral i) = rnf i
    rnf (TypeFP f) = rnf f
    rnf TypeBool = ()
    rnf TypeStr = ()
    rnf TypeUnit = ()
    rnf (TypeArrow a b) = rnf a `seq` rnf b
    rnf TypeBottom = ()
    rnf (TypeSpanned ws) = rnf ws

setupEnv :: Int -> Type
setupEnv n = deepArrow n

main :: IO ()
main = defaultMain
    [ bgroup "show"
        [ bench "depth-10"   $ nf (show . setupEnv) 10
        , bench "depth-100"  $ nf (show . setupEnv) 100
        , bench "depth-1000" $ nf (show . setupEnv) 1000
        ]
    ]
