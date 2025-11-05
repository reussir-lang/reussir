{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsics (
    IntOFFlag (..),
    FastMathFlag (..),
    CmpIPredicate (..),
    CmpFPredicate (..),
    RoundingMode (..),
    Arith (..),
    Math (..),
    Intrinsic (..),
    IntrinsicCall (..),
    arithCodegen,
    mathCodegen,
    intrinsicCallCodegen,
)
where

import Log qualified as L
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Intrinsics.Arith (
    Arith (..),
    CmpFPredicate (..),
    CmpIPredicate (..),
    FastMathFlag (..),
    IntOFFlag (..),
    RoundingMode (..),
    arithCodegen,
 )
import Reussir.Codegen.Intrinsics.Math (Math (..), mathCodegen)
import Reussir.Codegen.Value (TypedValue)

data Intrinsic
    = Arith Arith
    | Math Math
    deriving (Eq, Show)

data IntrinsicCall
    = IntrinsicCall
    { target :: Intrinsic
    , args :: [TypedValue]
    , results :: [TypedValue]
    }
    deriving (Eq, Show)

intrinsicCallCodegen :: IntrinsicCall -> C.Codegen ()
intrinsicCallCodegen (IntrinsicCall (Arith arith) args rets) = do
    L.logTrace_ "Generating code for arithmetic intrinsic"
    arithCodegen arith args rets
intrinsicCallCodegen (IntrinsicCall (Math math) args rets) = do
    L.logTrace_ "Generating code for math intrinsic"
    mathCodegen math args rets
