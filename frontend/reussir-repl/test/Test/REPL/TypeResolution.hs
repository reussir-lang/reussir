{-# LANGUAGE OverloadedStrings #-}

module Test.REPL.TypeResolution (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Reussir.Core.REPL
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Data.FP (FloatingPointType (..))
import Reussir.Core.Data.Integral (IntegralType (..))

tests :: TestTree
tests =
    testGroup
        "Type Resolution"
        [ testGroup "isPrimitiveType"
            [ testCase "i8 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Signed 8)) @?= True
            , testCase "i16 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Signed 16)) @?= True
            , testCase "i32 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Signed 32)) @?= True
            , testCase "i64 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Signed 64)) @?= True
            , testCase "u8 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Unsigned 8)) @?= True
            , testCase "u16 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Unsigned 16)) @?= True
            , testCase "u32 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Unsigned 32)) @?= True
            , testCase "u64 is primitive" $ isPrimitiveType (Semi.TypeIntegral (Unsigned 64)) @?= True
            , testCase "f16 is primitive" $ isPrimitiveType (Semi.TypeFP (IEEEFloat 16)) @?= True
            , testCase "f32 is primitive" $ isPrimitiveType (Semi.TypeFP (IEEEFloat 32)) @?= True
            , testCase "f64 is primitive" $ isPrimitiveType (Semi.TypeFP (IEEEFloat 64)) @?= True
            , testCase "bool is primitive" $ isPrimitiveType Semi.TypeBool @?= True
            , testCase "unit is primitive" $ isPrimitiveType Semi.TypeUnit @?= True
            ]
        , testGroup "typeToResultKind - Signed Integers"
            [ testCase "i8 -> ResultI8" $ typeToResultKind (Semi.TypeIntegral (Signed 8)) @?= ResultI8
            , testCase "i16 -> ResultI16" $ typeToResultKind (Semi.TypeIntegral (Signed 16)) @?= ResultI16
            , testCase "i32 -> ResultI32" $ typeToResultKind (Semi.TypeIntegral (Signed 32)) @?= ResultI32
            , testCase "i64 -> ResultI64" $ typeToResultKind (Semi.TypeIntegral (Signed 64)) @?= ResultI64
            ]
        , testGroup "typeToResultKind - Unsigned Integers"
            [ testCase "u8 -> ResultU8" $ typeToResultKind (Semi.TypeIntegral (Unsigned 8)) @?= ResultU8
            , testCase "u16 -> ResultU16" $ typeToResultKind (Semi.TypeIntegral (Unsigned 16)) @?= ResultU16
            , testCase "u32 -> ResultU32" $ typeToResultKind (Semi.TypeIntegral (Unsigned 32)) @?= ResultU32
            , testCase "u64 -> ResultU64" $ typeToResultKind (Semi.TypeIntegral (Unsigned 64)) @?= ResultU64
            ]
        , testGroup "typeToResultKind - Floating Point"
            [ testCase "f16 -> ResultF16" $ typeToResultKind (Semi.TypeFP (IEEEFloat 16)) @?= ResultF16
            , testCase "f32 -> ResultF32" $ typeToResultKind (Semi.TypeFP (IEEEFloat 32)) @?= ResultF32
            , testCase "f64 -> ResultF64" $ typeToResultKind (Semi.TypeFP (IEEEFloat 64)) @?= ResultF64
            ]
        , testGroup "typeToResultKind - Other Types"
            [ testCase "bool -> ResultBool" $ typeToResultKind Semi.TypeBool @?= ResultBool
            , testCase "unit -> ResultUnit" $ typeToResultKind Semi.TypeUnit @?= ResultUnit
            ]
        ]
