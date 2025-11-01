{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Type.Mangle
  ( mangleTests,
  )
where

import Data.Interned (intern)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context (Path (..))
import Reussir.Codegen.Type
  ( Atomicity (..),
    Capability (..),
    Closure (..),
    Expr (..),
    Primitive (..),
    PrimitiveFloat (..),
    PrimitiveInt (..),
    Rc (..),
    Ref (..),
    Tensor (..),
    Type (..),
    mangleType,
    mangleTypeWithPrefix,
  )
import Test.Tasty
import Test.Tasty.HUnit

-- Helper to convert Builder to String for comparison
builderToString :: TB.Builder -> String
builderToString = T.unpack . TB.toLazyText

-- Test primitive types
primitiveTests :: TestTree
primitiveTests =
  testGroup
    "Primitive Types"
    [ testCase "i8" $
        builderToString (mangleType (TypePrim (PrimInt PrimInt8))) @?= "2i8",
      testCase "i16" $
        builderToString (mangleType (TypePrim (PrimInt PrimInt16))) @?= "3i16",
      testCase "i32" $
        builderToString (mangleType (TypePrim (PrimInt PrimInt32))) @?= "3i32",
      testCase "i64" $
        builderToString (mangleType (TypePrim (PrimInt PrimInt64))) @?= "3i64",
      testCase "i128" $
        builderToString (mangleType (TypePrim (PrimInt PrimInt128))) @?= "4i128",
      testCase "index" $
        builderToString (mangleType (TypePrim (PrimInt PrimIndex))) @?= "5index",
      testCase "f8" $
        builderToString (mangleType (TypePrim (PrimFloat PrimFloat8))) @?= "2f8",
      testCase "f16" $
        builderToString (mangleType (TypePrim (PrimFloat PrimFloat16))) @?= "3f16",
      testCase "bf16" $
        builderToString (mangleType (TypePrim (PrimFloat PrimBFloat16))) @?= "4bf16",
      testCase "f32" $
        builderToString (mangleType (TypePrim (PrimFloat PrimFloat32))) @?= "3f32",
      testCase "f64" $
        builderToString (mangleType (TypePrim (PrimFloat PrimFloat64))) @?= "3f64",
      testCase "f128" $
        builderToString (mangleType (TypePrim (PrimFloat PrimFloat128))) @?= "4f128",
      testCase "bool" $
        builderToString (mangleType (TypePrim PrimBool)) @?= "b",
      testCase "unit" $
        builderToString (mangleType (TypePrim PrimUnit)) @?= "v"
    ]

-- Test tensor types
tensorTests :: TestTree
tensorTests =
  testGroup
    "Tensor Types"
    [ testCase "int[3]" $
        builderToString
          (mangleType (TypeTensor (Tensor (TypePrim (PrimInt PrimInt32)) [3])))
          @?= "6TensorIA3_3i32E",
      testCase "float[10][20]" $
        builderToString
          ( mangleType
              ( TypeTensor
                  (Tensor (TypePrim (PrimFloat PrimFloat64)) [10, 20])
              )
          )
          @?= "6TensorIA10_A20_3f64E",
      testCase "bool[]" $
        builderToString
          (mangleType (TypeTensor (Tensor (TypePrim PrimBool) [])))
          @?= "6TensorIA_bE",
      testCase "i64[2][3][4]" $
        builderToString
          ( mangleType
              ( TypeTensor
                  (Tensor (TypePrim (PrimInt PrimInt64)) [2, 3, 4])
              )
          )
          @?= "6TensorIA2_A3_A4_3i64E"
    ]

-- Test Rc types
rcTests :: TestTree
rcTests =
  testGroup
    "Rc Types"
    [ testCase "Rc<i32>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim (PrimInt PrimInt32))
                      NonAtomic
                      Unspecified
                  )
              )
          )
          @?= "2RcI3i32E",
      testCase "AtomicRc<i64>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim (PrimInt PrimInt64))
                      Atomic
                      Unspecified
                  )
              )
          )
          @?= "8AtomicRcI3i64E",
      testCase "FlexRc<f32>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim (PrimFloat PrimFloat32))
                      NonAtomic
                      Flex
                  )
              )
          )
          @?= "6FlexRcI3f32E",
      testCase "RigidRc<f64>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim (PrimFloat PrimFloat64))
                      NonAtomic
                      Rigid
                  )
              )
          )
          @?= "7RigidRcI3f64E",
      testCase "AtomicFlexRc<bool>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim PrimBool)
                      Atomic
                      Flex
                  )
              )
          )
          @?= "12AtomicFlexRcIbE",
      testCase "AtomicRigidRc<unit>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      (TypePrim PrimUnit)
                      Atomic
                      Rigid
                  )
              )
          )
          @?= "13AtomicRigidRcIvE"
    ]

-- Test Ref types
refTests :: TestTree
refTests =
  testGroup
    "Ref Types"
    [ testCase "Ref<i32>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim (PrimInt PrimInt32))
                      NonAtomic
                      Unspecified
                  )
              )
          )
          @?= "3RefI3i32E",
      testCase "AtomicRef<i64>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim (PrimInt PrimInt64))
                      Atomic
                      Unspecified
                  )
              )
          )
          @?= "9AtomicRefI3i64E",
      testCase "FlexRef<f32>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim (PrimFloat PrimFloat32))
                      NonAtomic
                      Flex
                  )
              )
          )
          @?= "7FlexRefI3f32E",
      testCase "RigidRef<f64>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim (PrimFloat PrimFloat64))
                      NonAtomic
                      Rigid
                  )
              )
          )
          @?= "8RigidRefI3f64E",
      testCase "AtomicFlexRef<bool>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim PrimBool)
                      Atomic
                      Flex
                  )
              )
          )
          @?= "13AtomicFlexRefIbE",
      testCase "AtomicRigidRef<unit>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      (TypePrim PrimUnit)
                      Atomic
                      Rigid
                  )
              )
          )
          @?= "14AtomicRigidRefIvE"
    ]

-- Test Closure types
closureTests :: TestTree
closureTests =
  testGroup
    "Closure Types"
    [ testCase "Closure() -> i32" $
        builderToString
          ( mangleType
              ( TypeClosure
                  ( Closure
                      []
                      (TypePrim (PrimInt PrimInt32))
                  )
              )
          )
          @?= "7ClosureIF3i32vEE",
      testCase "Closure(i32) -> i64" $
        builderToString
          ( mangleType
              ( TypeClosure
                  ( Closure
                      [TypePrim (PrimInt PrimInt32)]
                      (TypePrim (PrimInt PrimInt64))
                  )
              )
          )
          @?= "7ClosureIF3i643i32EE",
      testCase "Closure(i32, f64) -> bool" $
        builderToString
          ( mangleType
              ( TypeClosure
                  ( Closure
                      [ TypePrim (PrimInt PrimInt32),
                        TypePrim (PrimFloat PrimFloat64)
                      ]
                      (TypePrim PrimBool)
                  )
              )
          )
          @?= "7ClosureIFb3i323f64EE",
      testCase "Closure(Rc<i32>) -> Ref<i64>" $
        builderToString
          ( mangleType
              ( TypeClosure
                  ( Closure
                      [ TypeRc
                          ( Rc
                              (TypePrim (PrimInt PrimInt32))
                              NonAtomic
                              Unspecified
                          )
                      ]
                      ( TypeRef
                          ( Ref
                              (TypePrim (PrimInt PrimInt64))
                              NonAtomic
                              Unspecified
                          )
                      )
                  )
              )
          )
          @?= "7ClosureIF3RefI3i64E2RcI3i32EEE"
    ]

-- Test Expr types
exprTests :: TestTree
exprTests =
  testGroup
    "Expr Types"
    [ testCase "Expr(My::Path)" $
        builderToString
          ( mangleType
              ( TypeExpr
                  ( Expr
                      (Path [intern "My", intern "Path"])
                      []
                  )
              )
          )
          @?= "N2My4PathE",
      testCase "Expr(My::Path<i32>)" $
        builderToString
          ( mangleType
              ( TypeExpr
                  ( Expr
                      (Path [intern "My", intern "Path"])
                      [TypePrim (PrimInt PrimInt32)]
                  )
              )
          )
          @?= "N2My4PathI3i32EE",
      testCase "Expr(Long::Namespace::Type<i32, f64>)" $
        builderToString
          ( mangleType
              ( TypeExpr
                  ( Expr
                      (Path [intern "Long", intern "Namespace", intern "Type"])
                      [ TypePrim (PrimInt PrimInt32),
                        TypePrim (PrimFloat PrimFloat64)
                      ]
                  )
              )
          )
          @?= "N4Long9Namespace4TypeI3i323f64EE"
    ]

-- Test mangleTypeWithPrefix
prefixTests :: TestTree
prefixTests =
  testGroup
    "Mangle Type With Prefix"
    [ testCase "mangleTypeWithPrefix for i32" $
        builderToString
          (mangleTypeWithPrefix (TypePrim (PrimInt PrimInt32)))
          @?= "_Z3i32",
      testCase "mangleTypeWithPrefix for Rc<i64>" $
        builderToString
          ( mangleTypeWithPrefix
              ( TypeRc
                  ( Rc
                      (TypePrim (PrimInt PrimInt64))
                      NonAtomic
                      Unspecified
                  )
              )
          )
          @?= "_Z2RcI3i64E",
      testCase "mangleTypeWithPrefix for Closure(i32) -> bool" $
        builderToString
          ( mangleTypeWithPrefix
              ( TypeClosure
                  ( Closure
                      [TypePrim (PrimInt PrimInt32)]
                      (TypePrim PrimBool)
                  )
              )
          )
          @?= "_Z7ClosureIFb3i32EE"
    ]

-- Test nested/complex types
nestedTests :: TestTree
nestedTests =
  testGroup
    "Nested/Complex Types"
    [ testCase "Rc<Tensor<i32[10]>>" $
        builderToString
          ( mangleType
              ( TypeRc
                  ( Rc
                      ( TypeTensor
                          (Tensor (TypePrim (PrimInt PrimInt32)) [10])
                      )
                      NonAtomic
                      Unspecified
                  )
              )
          )
          @?= "2RcI6TensorIA10_3i32EE",
      testCase "Ref<Rc<i64>>" $
        builderToString
          ( mangleType
              ( TypeRef
                  ( Ref
                      ( TypeRc
                          ( Rc
                              (TypePrim (PrimInt PrimInt64))
                              NonAtomic
                              Unspecified
                          )
                      )
                      NonAtomic
                      Unspecified
                  )
              )
          )
          @?= "3RefI2RcI3i64EE",
      testCase "Closure(Rc<i32>, Ref<f64>) -> Tensor<bool[5]>" $
        builderToString
          ( mangleType
              ( TypeClosure
                  ( Closure
                      [ TypeRc
                          ( Rc
                              (TypePrim (PrimInt PrimInt32))
                              NonAtomic
                              Unspecified
                          ),
                        TypeRef
                          ( Ref
                              (TypePrim (PrimFloat PrimFloat64))
                              NonAtomic
                              Unspecified
                          )
                      ]
                      ( TypeTensor
                          (Tensor (TypePrim PrimBool) [5])
                      )
                  )
              )
          )
          @?= "7ClosureIF6TensorIA5_bE2RcI3i32E3RefI3f64EEE"
    ]

mangleTests :: TestTree
mangleTests =
  testGroup
    "Type Mangle Tests"
    [ primitiveTests,
      tensorTests,
      rcTests,
      refTests,
      closureTests,
      exprTests,
      prefixTests,
      nestedTests
    ]
