{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen (
    codegenTests,
)
where

import Data.Int (Int64)
import Log (defaultLogLevel)
import System.Exit (ExitCode (ExitSuccess))
import System.IO ()
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful qualified as E
import Effectful.Log qualified as L
import Log.Backend.StandardOutput qualified as L
import Reussir.Bridge qualified as B
import System.IO qualified as IO

import Reussir.Codegen (RecordInstance (..), emptyModule)
import Reussir.Codegen.Context (TargetSpec (..))
import Reussir.Codegen.Context.Symbol (Symbol, verifiedSymbol)
import Reussir.Codegen.Type.Record (
    Record (..),
    RecordField (..),
    RecordKind (..),
 )

import Reussir.Codegen qualified as C
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V

-- Helper types
primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

primitiveF64 :: TT.Type
primitiveF64 = TT.TypePrim (TT.PrimFloat TT.PrimFloat64)

primitiveI64 :: TT.Type
primitiveI64 = TT.TypePrim (TT.PrimInt TT.PrimInt64)

primitiveI128 :: TT.Type
primitiveI128 = TT.TypePrim (TT.PrimInt TT.PrimInt128)

primitiveBool :: TT.Type
primitiveBool = TT.TypePrim TT.PrimBool

normalField :: TT.Type -> TT.RecordField
normalField fieldType = RecordField{fieldType, fieldIsMutable = False}

-- Helper functions for creating test values
val :: Int64 -> V.Value
val = V.Value

typedVal :: Int64 -> TT.Type -> V.TypedValue
typedVal v t = (val v, t)

f32val :: Int64 -> V.TypedValue
f32val v = typedVal v primitiveF32

i128val :: Int64 -> V.TypedValue
i128val v = typedVal v primitiveI128

boolval :: Int64 -> V.TypedValue
boolval v = typedVal v primitiveBool

-- Helper to create integer constants
i128constant :: Int64 -> V.TypedValue
i128constant v = (val v, primitiveI128)

-- Helper to check if a string is present in the output
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

-- Create a simple f32 addition function: f32 a + b
createAddF32Function :: IR.Function
createAddF32Function =
    IR.Function
        { funcLinkage = IR.LnkExternal
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "add_f32"
        , funcArgs = [f32val 0, f32val 1]
        , funcResult = primitiveF32
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [f32val 0, f32val 1]
                    , blkBody =
                        [ IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f32val 0, f32val 1]
                                [f32val 2]
                            )
                        , IR.Return (Just (f32val 2))
                        ]
                    }
                )
        }

-- Create a simple module with the add_f32 function
createSimpleModule :: C.Module
createSimpleModule =
    (emptyModule spec){C.moduleFunctions = [createAddF32Function]}
  where
    spec =
        TargetSpec
            "test_module"
            "output.o"
            B.OptDefault
            B.OutputObject
            B.LogWarning
            "test.rr"

-- Create the Tensor2x2 record type symbol
tensor2x2Symbol :: Symbol
tensor2x2Symbol = verifiedSymbol "_Z9Tensor2x2I3f64E"

-- Create the Tensor2x2 TypeExpr type
tensor2x2Type :: TT.Type
tensor2x2Type = TT.TypeExpr tensor2x2Symbol

-- Helper to create typed values with TypeExpr
tensor2x2val :: Int64 -> V.TypedValue
tensor2x2val v = typedVal v tensor2x2Type

-- Helper to create f64 typed values
f64val :: Int64 -> V.TypedValue
f64val v = typedVal v primitiveF64

-- Helper to create i64 typed values
i64val :: Int64 -> V.TypedValue
i64val v = typedVal v primitiveI64

-- Helper to create default ref type: non-atomic, unspecified capability
defaultRef :: TT.Type -> TT.Type
defaultRef t = TT.TypeRef (TT.Ref t TT.NonAtomic TT.Unspecified)

-- Create a module with a Tensor2x2 record instance
-- Record: _Z9Tensor2x2I3f64E with defaultCapability as Unspecified
-- Four f64 fields, all with Value capability
createTensor2x2Module :: C.Module
createTensor2x2Module =
    (emptyModule spec)
        { C.moduleFunctions =
            [ createMatmulFunction
            , createPowImplFunction
            , createPowFunction
            , createFibonacciFastFunction
            ]
        , C.recordInstances =
            [ RecordInstance
                ( tensor2x2Symbol
                , Record
                    { defaultCapability = TT.Value
                    , fields =
                        V.fromList $
                            map
                                normalField
                                [ primitiveF64
                                , primitiveF64
                                , primitiveF64
                                , primitiveF64
                                ]
                    , kind = Compound
                    }
                )
            ]
        }
  where
    spec =
        TargetSpec
            "tensor_module"
            "tensor.o"
            B.OptAggressive
            B.OutputObject
            B.LogWarning
            "test.rr"

-- Create matmul function: _ZN9Tensor2x2I3f64E6matmulE
-- Takes two Tensor2x2, returns Tensor2x2
-- Performs 2x2 matrix multiplication
createMatmulFunction :: IR.Function
createMatmulFunction =
    IR.Function
        { funcLinkage = IR.LnkLinkOnceODR
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "_ZN9Tensor2x2I3f64E6matmulE"
        , funcArgs = [tensor2x2val 0, tensor2x2val 1] -- a: Tensor2x2, b: Tensor2x2
        , funcResult = tensor2x2Type
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [tensor2x2val 0, tensor2x2val 1] -- a, b
                    , blkBody =
                        [ -- Spill argument a (producing ref type)
                          IR.RefSpill
                            (tensor2x2val 0)
                            (typedVal 2 (defaultRef tensor2x2Type))
                        , -- Spill argument b
                          IR.RefSpill
                            (tensor2x2val 1)
                            (typedVal 3 (defaultRef tensor2x2Type))
                        , -- Project a's fields (0-3): a00, a01, a10, a11
                          IR.RefProject
                            (typedVal 2 (defaultRef tensor2x2Type))
                            0
                            (typedVal 4 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 2 (defaultRef tensor2x2Type))
                            1
                            (typedVal 5 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 2 (defaultRef tensor2x2Type))
                            2
                            (typedVal 6 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 2 (defaultRef tensor2x2Type))
                            3
                            (typedVal 7 (defaultRef primitiveF64))
                        , -- Project b's fields (0-3): b00, b01, b10, b11
                          IR.RefProject
                            (typedVal 3 (defaultRef tensor2x2Type))
                            0
                            (typedVal 8 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 3 (defaultRef tensor2x2Type))
                            1
                            (typedVal 9 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 3 (defaultRef tensor2x2Type))
                            2
                            (typedVal 10 (defaultRef primitiveF64))
                        , IR.RefProject
                            (typedVal 3 (defaultRef tensor2x2Type))
                            3
                            (typedVal 11 (defaultRef primitiveF64))
                        , -- Load all 8 field values
                          -- Load a's fields
                          IR.RefLoad
                            (typedVal 4 (defaultRef primitiveF64))
                            (f64val 12)
                        , IR.RefLoad
                            (typedVal 5 (defaultRef primitiveF64))
                            (f64val 13)
                        , IR.RefLoad
                            (typedVal 6 (defaultRef primitiveF64))
                            (f64val 14)
                        , IR.RefLoad
                            (typedVal 7 (defaultRef primitiveF64))
                            (f64val 15)
                        , -- Load b's fields
                          IR.RefLoad
                            (typedVal 8 (defaultRef primitiveF64))
                            (f64val 16)
                        , IR.RefLoad
                            (typedVal 9 (defaultRef primitiveF64))
                            (f64val 17)
                        , IR.RefLoad
                            (typedVal 10 (defaultRef primitiveF64))
                            (f64val 18)
                        , IR.RefLoad
                            (typedVal 11 (defaultRef primitiveF64))
                            (f64val 19)
                        , -- Matrix multiplication: result[0][0] = a00*b00 + a01*b10
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 12, f64val 16] -- a00 * b00
                                [f64val 20]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 13, f64val 18] -- a01 * b10
                                [f64val 21]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f64val 20, f64val 21] -- a00*b00 + a01*b10
                                [f64val 22]
                            )
                        , -- result[0][1] = a00*b01 + a01*b11
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 12, f64val 17] -- a00 * b01
                                [f64val 23]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 13, f64val 19] -- a01 * b11
                                [f64val 24]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f64val 23, f64val 24] -- a00*b01 + a01*b11
                                [f64val 25]
                            )
                        , -- result[1][0] = a10*b00 + a11*b10
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 14, f64val 16] -- a10 * b00
                                [f64val 26]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 15, f64val 18] -- a11 * b10
                                [f64val 27]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f64val 26, f64val 27] -- a10*b00 + a11*b10
                                [f64val 28]
                            )
                        , -- result[1][1] = a10*b01 + a11*b11
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 14, f64val 17] -- a10 * b01
                                [f64val 29]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Mulf (I.FastMathFlag 0)))
                                [f64val 15, f64val 19] -- a11 * b11
                                [f64val 30]
                            )
                        , IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Addf (I.FastMathFlag 0)))
                                [f64val 29, f64val 30] -- a10*b01 + a11*b11
                                [f64val 31]
                            )
                        , -- Create result tensor using CompoundCreate
                          IR.CompoundCreate
                            [f64val 22, f64val 25, f64val 28, f64val 31] -- r00, r01, r10, r11
                            (tensor2x2val 32)
                        , -- Return the result
                          IR.Return (Just (tensor2x2val 32))
                        ]
                    }
                )
        }

-- Create pow_impl function: _ZN9Tensor2x2I3f64E8pow_implI3u64EE
-- Private linkage, recursive fast powering
-- base: Tensor2x2, exp: i64, acc: Tensor2x2 -> Tensor2x2
createPowImplFunction :: IR.Function
createPowImplFunction =
    IR.Function
        { funcLinkage = IR.LnkPrivate
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPrivate
        , funcSymbol = verifiedSymbol "_ZN9Tensor2x2I3f64E8pow_implI3u64EE"
        , funcArgs = [tensor2x2val 0, i64val 1, tensor2x2val 2] -- base, exp, acc
        , funcResult = tensor2x2Type
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [tensor2x2val 0, i64val 1, tensor2x2val 2] -- base, exp, acc
                    , blkBody =
                        [ -- Create constant 0 for comparison
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "0")))
                                []
                                [i64val 3]
                            )
                        , -- Check if exp == 0
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Cmpi I.CIEq))
                                [i64val 1, i64val 3] -- exp == 0
                                [boolval 4]
                            )
                        , -- Outer if: if exp == 0, return acc; else check exp & 1
                          IR.IfThenElse
                            (boolval 4)
                            -- Then: return acc
                            ( IR.Block
                                { blkArgs = []
                                , blkBody = [IR.Yield IR.YieldScf (Just (tensor2x2val 2))]
                                }
                            )
                            -- Else: check if (exp & 1) != 0
                            ( Just
                                ( IR.Block
                                    { blkArgs = []
                                    , blkBody =
                                        [ -- Create constant 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Constant (read "1")))
                                                []
                                                [i64val 5]
                                            )
                                        , -- exp & 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith I.Andi)
                                                [i64val 1, i64val 5]
                                                [i64val 6]
                                            )
                                        , -- Check if (exp & 1) != 0
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Cmpi I.CINe))
                                                [i64val 6, i64val 3] -- (exp & 1) != 0
                                                [boolval 7]
                                            )
                                        , -- Inner if: if (exp & 1) != 0, call with (base*base, exp>>1, acc*base); else call with (base*base, exp>>1, acc)
                                          IR.IfThenElse
                                            (boolval 7)
                                            -- Then: (exp & 1) != 0, call with (base*base, exp>>1, acc*base)
                                            ( IR.Block
                                                { blkArgs = []
                                                , blkBody =
                                                    [ -- base * base
                                                      IR.FCall
                                                        ( IR.FuncCall
                                                            { target = verifiedSymbol "_ZN9Tensor2x2I3f64E6matmulE"
                                                            , args = [tensor2x2val 0, tensor2x2val 0]
                                                            , results = Just (tensor2x2val 8)
                                                            }
                                                        )
                                                    , -- exp >> 1
                                                      IR.ICall
                                                        ( I.IntrinsicCall
                                                            (I.Arith I.Shrsi)
                                                            [i64val 1, i64val 5] -- exp >> 1
                                                            [i64val 9]
                                                        )
                                                    , -- acc * base
                                                      IR.FCall
                                                        ( IR.FuncCall
                                                            { target = verifiedSymbol "_ZN9Tensor2x2I3f64E6matmulE"
                                                            , args = [tensor2x2val 2, tensor2x2val 0]
                                                            , results = Just (tensor2x2val 10)
                                                            }
                                                        )
                                                    , -- Recursive call: pow_impl(base*base, exp>>1, acc*base)
                                                      IR.FCall
                                                        ( IR.FuncCall
                                                            { target = verifiedSymbol "_ZN9Tensor2x2I3f64E8pow_implI3u64EE"
                                                            , args = [tensor2x2val 8, i64val 9, tensor2x2val 10]
                                                            , results = Just (tensor2x2val 11)
                                                            }
                                                        )
                                                    , IR.Yield IR.YieldScf (Just (tensor2x2val 11))
                                                    ]
                                                }
                                            )
                                            -- Else: (exp & 1) == 0, call with (base*base, exp>>1, acc)
                                            ( Just
                                                ( IR.Block
                                                    { blkArgs = []
                                                    , blkBody =
                                                        [ -- base * base
                                                          IR.FCall
                                                            ( IR.FuncCall
                                                                { target = verifiedSymbol "_ZN9Tensor2x2I3f64E6matmulE"
                                                                , args = [tensor2x2val 0, tensor2x2val 0]
                                                                , results = Just (tensor2x2val 12)
                                                                }
                                                            )
                                                        , -- exp >> 1
                                                          IR.ICall
                                                            ( I.IntrinsicCall
                                                                (I.Arith I.Shrsi)
                                                                [i64val 1, i64val 5] -- exp >> 1
                                                                [i64val 13]
                                                            )
                                                        , -- Recursive call: pow_impl(base*base, exp>>1, acc)
                                                          IR.FCall
                                                            ( IR.FuncCall
                                                                { target = verifiedSymbol "_ZN9Tensor2x2I3f64E8pow_implI3u64EE"
                                                                , args = [tensor2x2val 12, i64val 13, tensor2x2val 2]
                                                                , results = Just (tensor2x2val 14)
                                                                }
                                                            )
                                                        , IR.Yield IR.YieldScf (Just (tensor2x2val 14))
                                                        ]
                                                    }
                                                )
                                            )
                                            (Just (tensor2x2val 15)) -- Inner if result
                                        , IR.Yield IR.YieldScf (Just (tensor2x2val 15))
                                        ]
                                    }
                                )
                            )
                            (Just (tensor2x2val 16)) -- Outer if result
                        , -- Return the result from outer if
                          IR.Return (Just (tensor2x2val 16))
                        ]
                    }
                )
        }

-- Create pow function: _ZN9Tensor2x2I3f64E3powI3u64EE
-- WeakODR linkage, wrapper that calls pow_impl with identity matrix
-- base: Tensor2x2, exp: i64 -> Tensor2x2
createPowFunction :: IR.Function
createPowFunction =
    IR.Function
        { funcLinkage = IR.LnkLinkOnceODR
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "_ZN9Tensor2x2I3f64E3powI3u64EE"
        , funcArgs = [tensor2x2val 0, i64val 1] -- base, exp
        , funcResult = tensor2x2Type
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [tensor2x2val 0, i64val 1] -- base, exp
                    , blkBody =
                        [ -- Create identity matrix: [1.0, 0.0, 0.0, 1.0]
                          -- Create constant 1.0
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "1.0")))
                                []
                                [f64val 2]
                            )
                        , -- Create constant 0.0
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "0.0")))
                                []
                                [f64val 3]
                            )
                        , -- Create identity matrix using CompoundCreate
                          IR.CompoundCreate
                            [f64val 2, f64val 3, f64val 3, f64val 2] -- [1.0, 0.0, 0.0, 1.0]
                            (tensor2x2val 4)
                        , -- Call pow_impl with identity matrix as acc
                          IR.FCall
                            ( IR.FuncCall
                                { target = verifiedSymbol "_ZN9Tensor2x2I3f64E8pow_implI3u64EE"
                                , args = [tensor2x2val 0, i64val 1, tensor2x2val 4]
                                , results = Just (tensor2x2val 5)
                                }
                            )
                        , -- Return the result
                          IR.Return (Just (tensor2x2val 5))
                        ]
                    }
                )
        }

-- Create fibonacci_fast function: _Z13fibnacci_fast
-- Computes fibonacci using transition matrix [0, 1; 1, 1] raised to nth power
-- Takes n: i64, returns i64 (the nth fibonacci number)
createFibonacciFastFunction :: IR.Function
createFibonacciFastFunction =
    IR.Function
        { funcLinkage = IR.LnkExternal
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "_Z13fibnacci_fast"
        , funcArgs = [i64val 0] -- n: i64
        , funcResult = primitiveF64
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [i64val 0] -- n
                    , blkBody =
                        [ -- Create transition matrix [0, 1; 1, 1] using CompoundCreate
                          -- Create constant 0.0
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "0.0")))
                                []
                                [f64val 1]
                            )
                        , -- Create constant 1.0
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "1.0")))
                                []
                                [f64val 2]
                            )
                        , -- Create transition matrix [0, 1; 1, 1] = [0.0, 1.0, 1.0, 1.0]
                          IR.CompoundCreate
                            [f64val 1, f64val 2, f64val 2, f64val 2] -- [0.0, 1.0, 1.0, 1.0]
                            (tensor2x2val 3)
                        , -- Raise transition matrix to nth power using pow function
                          IR.FCall
                            ( IR.FuncCall
                                { target = verifiedSymbol "_ZN9Tensor2x2I3f64E3powI3u64EE"
                                , args = [tensor2x2val 3, i64val 0]
                                , results = Just (tensor2x2val 4)
                                }
                            )
                        , -- Extract the result from the matrix
                          -- The result matrix T^n will have F(n) in the top-right or bottom-right
                          -- We need to spill to get a ref, then project and load
                          IR.RefSpill
                            (tensor2x2val 4)
                            (typedVal 5 (defaultRef tensor2x2Type))
                        , -- Project to get the first element (index 0) which should give us the result
                          -- Actually, in the standard Fibonacci matrix exponentiation:
                          -- [F(n+1), F(n); F(n), F(n-1)] = [0, 1; 1, 1]^n
                          -- So we want index 1 (top-right) for F(n), or we can use index 0
                          -- Let's use index 1 for the top-right element which is F(n)
                          IR.RefProject
                            (typedVal 5 (defaultRef tensor2x2Type))
                            1
                            (typedVal 6 (defaultRef primitiveF64))
                        , -- Load the f64 value
                          IR.RefLoad
                            (typedVal 6 (defaultRef primitiveF64))
                            (f64val 7)
                        , -- Return the result (f64 directly)
                          IR.Return (Just (f64val 7))
                        ]
                    }
                )
        }

-- Create a naive fibonacci function: fib(n: i128) -> i128
-- Naive implementation: if n < 2 then n else fib(n-1) + fib(n-2)
createFibonacciFunction :: IR.Function
createFibonacciFunction =
    IR.Function
        { funcLinkage = IR.LnkExternal
        , funcLLVMVisibility = IR.LLVMVisDefault
        , funcMLIRVisibility = IR.MLIRVisPublic
        , funcSymbol = verifiedSymbol "fibonacci"
        , funcArgs = [i128val 0] -- n: i128
        , funcResult = primitiveI128
        , funcDbgArgs = []
        , funcLoc = Nothing
        , funcBody =
            Just
                ( IR.Block
                    { blkArgs = [i128val 0] -- n
                    , blkBody =
                        [ -- Create constant 2
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Constant (read "2")))
                                []
                                [i128constant 1]
                            )
                        , -- Create constant 0 for comparison result
                          IR.ICall
                            ( I.IntrinsicCall
                                (I.Arith (I.Cmpi I.CISlt))
                                [i128val 0, i128constant 1] -- n < 2
                                [boolval 2]
                            )
                        , -- If n < 2, return n; else compute fib(n-1) + fib(n-2)
                          IR.IfThenElse
                            (boolval 2)
                            -- Then block: yield n
                            ( IR.Block
                                { blkArgs = []
                                , blkBody = [IR.Yield IR.YieldScf (Just (i128val 0))]
                                }
                            )
                            -- Else block: compute fib(n-1) + fib(n-2) and yield
                            ( Just
                                ( IR.Block
                                    { blkArgs = []
                                    , blkBody =
                                        [ -- Create constant 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Constant (read "1")))
                                                []
                                                [i128constant 3]
                                            )
                                        , -- n - 1
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Subi (I.IntOFFlag 0)))
                                                [i128val 0, i128constant 3]
                                                [i128val 4]
                                            )
                                        , -- Create constant 2
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Constant (read "2")))
                                                []
                                                [i128constant 5]
                                            )
                                        , -- n - 2
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Subi (I.IntOFFlag 0)))
                                                [i128val 0, i128constant 5]
                                                [i128val 6]
                                            )
                                        , -- Call fib(n-1)
                                          IR.FCall
                                            ( IR.FuncCall
                                                { target = verifiedSymbol "fibonacci"
                                                , args = [i128val 4]
                                                , results = Just (i128val 7)
                                                }
                                            )
                                        , -- Call fib(n-2)
                                          IR.FCall
                                            ( IR.FuncCall
                                                { target = verifiedSymbol "fibonacci"
                                                , args = [i128val 6]
                                                , results = Just (i128val 8)
                                                }
                                            )
                                        , -- Add results: fib(n-1) + fib(n-2)
                                          IR.ICall
                                            ( I.IntrinsicCall
                                                (I.Arith (I.Addi (I.IntOFFlag 0)))
                                                [i128val 7, i128val 8]
                                                [i128val 9]
                                            )
                                        , -- Yield the sum
                                          IR.Yield IR.YieldScf (Just (i128val 9))
                                        ]
                                    }
                                )
                            )
                            (Just (i128val 10)) -- scf.if returns a value
                        , -- Return the result from scf.if
                          IR.Return (Just (i128val 10))
                        ]
                    }
                )
        }

-- Create a module with the fibonacci function using aggressive optimization
createFibonacciModule :: C.Module
createFibonacciModule =
    (emptyModule spec){C.moduleFunctions = [createFibonacciFunction]}
  where
    spec =
        TargetSpec
            "fibonacci_module"
            "fibonacci.o"
            B.OptAggressive
            B.OutputObject
            B.LogWarning
            "test.rr"

codegenTests :: TestTree
codegenTests =
    testGroup
        "Codegen"
        [ testGroup
            "emitModuleToText"
            [ testCase "emitModuleToText produces MLIR output" $ do
                let module' = createSimpleModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $
                            L.runLog "Test.Codegen" logger defaultLogLevel $
                                C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain module declaration" $ "module" `isInfixOf` resultStr
                assertBool "Should contain func.func" $ "func.func" `isInfixOf` resultStr
                assertBool "Should contain function name add_f32" $
                    "add_f32" `isInfixOf` resultStr
                assertBool "Should contain f32 type" $ "f32" `isInfixOf` resultStr
                assertBool "Should contain arith.addf" $ "arith.addf" `isInfixOf` resultStr
                assertBool "Should contain func.return" $ "func.return" `isInfixOf` resultStr
                assertBool "Should contain function arguments" $
                    "%0" `isInfixOf` resultStr && "%1" `isInfixOf` resultStr
                assertBool "Should contain result" $ "%2" `isInfixOf` resultStr
            , testCase "emitModuleToText contains correct function signature" $ do
                let module' = createSimpleModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $
                            L.runLog "Test.Codegen" logger defaultLogLevel $
                                C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain external visibility" $
                    "external" `isInfixOf` resultStr
                assertBool "Should contain multiple f32 references" $
                    T.count "f32" (T.pack resultStr) >= 3
            ]
        , testGroup
            "emitModuleToBackend"
            [ testCase "emitModuleToBackend executes without error" $ do
                withSystemTempDirectory "reussir-test" $ \tmpDir -> do
                    let outputPath = tmpDir ++ "/output.o"
                    let origSpec = C.moduleSpec createSimpleModule
                    let module' = createSimpleModule{C.moduleSpec = origSpec{outputPath = outputPath}}
                    result <-
                        L.withStdOutLogger $ \logger -> do
                            E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                                C.emitModuleToBackend module'
                                pure True
                    assertBool "Should complete successfully" result
            ]
        , testGroup
            "fibonacci"
            [ testCase "emitModuleToText produces MLIR output for naive fibonacci" $ do
                let module' = createFibonacciModule
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $
                            L.runLog "Test.Codegen" logger defaultLogLevel $
                                C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain module declaration" $ "module" `isInfixOf` resultStr
                assertBool "Should contain func.func" $ "func.func" `isInfixOf` resultStr
                assertBool "Should contain function name fibonacci" $
                    "fibonacci" `isInfixOf` resultStr
                assertBool "Should contain i128 type" $ "i128" `isInfixOf` resultStr
                assertBool "Should contain arith.cmpi" $ "arith.cmpi" `isInfixOf` resultStr
                assertBool "Should contain scf.if" $ "scf.if" `isInfixOf` resultStr
                assertBool "Should contain func.call" $ "func.call" `isInfixOf` resultStr
                assertBool "Should contain arith.addi" $ "arith.addi" `isInfixOf` resultStr
                assertBool "Should contain arith.subi" $ "arith.subi" `isInfixOf` resultStr
            , testCase
                "emitModuleToBackend executes fibonacci module with aggressive optimization" $ do
                withSystemTempDirectory "reussir-test" $ \tmpDir -> do
                    let fibonacciPath = tmpDir ++ "/fibonacci.o"
                    let origSpec = C.moduleSpec createFibonacciModule
                    let module' = createFibonacciModule{C.moduleSpec = origSpec{outputPath = fibonacciPath}}
                    result <-
                        L.withStdOutLogger $ \logger -> do
                            E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                                C.emitModuleToBackend module'
                                pure True
                    assertBool "Should complete successfully" result
            ]
        , testGroup
            "tensor2x2"
            [ testCase "emitModuleToText produces MLIR output for Tensor2x2 record" $ do
                let module' = createTensor2x2Module
                result <-
                    L.withStdOutLogger $ \logger -> do
                        E.runEff $
                            L.runLog "Test.Codegen" logger defaultLogLevel $
                                C.emitModuleToText module'
                let resultStr = T.unpack result
                assertBool "Should contain module declaration" $ "module" `isInfixOf` resultStr
                assertBool "Should contain record type" $
                    "!reussir.record" `isInfixOf` resultStr
                assertBool "Should contain record name _Z9Tensor2x2I3f64E" $
                    "_Z9Tensor2x2I3f64E" `isInfixOf` resultStr
                assertBool "Should contain f64 type" $ "f64" `isInfixOf` resultStr
                assertBool "Should contain value capability" $ "value" `isInfixOf` resultStr
                assertBool "Should contain four f64 fields" $
                    T.count "f64" (T.pack resultStr) >= 4
            , testCase "emitModuleToBackend executes tensor2x2 module without error" $ do
                withSystemTempDirectory "reussir-test" $ \tmpDir -> do
                    let tensorPath = tmpDir ++ "/tensor.o"
                    let origSpec = C.moduleSpec createTensor2x2Module
                    let module' = createTensor2x2Module{C.moduleSpec = origSpec{outputPath = tensorPath}}
                    result <-
                        L.withStdOutLogger $ \logger -> do
                            E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                                C.emitModuleToBackend module'
                                pure True
                    assertBool "Should complete successfully" result
            , testCase "tensor2x2 fibonacci computation produces correct result" $ do
                withSystemTempDirectory "reussir-test" $ \tmpDir -> do
                    let tensorObjPath = tmpDir ++ "/tensor.o"
                    let tensorCPath = tmpDir ++ "/tensor.c"
                    let tensorExecPath = tmpDir ++ "/tensor_exec"
                    let origSpec = C.moduleSpec createTensor2x2Module
                    let module' = createTensor2x2Module{C.moduleSpec = origSpec{outputPath = tensorObjPath}}
                    -- Emit the object file
                    _ <-
                        L.withStdOutLogger $ \logger -> do
                            E.runEff $ L.runLog "Test.Codegen" logger defaultLogLevel $ do
                                C.emitModuleToBackend module'
                    -- Write the C file
                    let cSource =
                            "extern double _Z13fibnacci_fast(unsigned long long);\nint main() {\n        double value = _Z13fibnacci_fast(42);\n        __builtin_printf(\"%.0f\\n\", value);\n        return 0;\n}\n"
                    IO.writeFile tensorCPath cSource
                    -- Compile the executable
                    (exitCode1, _, _) <-
                        readProcessWithExitCode
                            "cc"
                            [tensorCPath, tensorObjPath, "-O3", "-o", tensorExecPath]
                            ""
                    -- Check that compilation succeeded
                    assertBool "Compilation should succeed" (exitCode1 == ExitSuccess)
                    -- Run the executable and capture output
                    (exitCode2, output, _) <-
                        readProcessWithExitCode tensorExecPath [] ""
                    -- Check that execution succeeded
                    assertBool "Execution should succeed" (exitCode2 == ExitSuccess)
                    -- Check that the output is the expected value
                    let outputStr = filter (/= '\r') output -- Remove carriage returns
                    assertEqual "Fibonacci(42) should be 267914296" "267914296\n" outputStr
            ]
        ]
