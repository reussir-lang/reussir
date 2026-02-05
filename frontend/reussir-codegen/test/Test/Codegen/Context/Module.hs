{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Context.Module (
    moduleTests,
)
where

import Log (defaultLogLevel)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Vector.Strict qualified as V
import Effectful qualified as E
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as E
import Log.Backend.StandardOutput qualified as L
import Reussir.Bridge qualified as B

import Reussir.Codegen.Context (runCodegen)
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type.Record (
    Record (..),
    RecordField (..),
    RecordKind (..),
 )

import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Type qualified as TT

-- | Helper to run codegen and extract the builder as text
runCodegenAsText :: C.Codegen () -> IO T.Text
runCodegenAsText codegen = do
    let spec =
            C.TargetSpec
                "test_module"
                "output.mlir"
                B.OptDefault
                B.OutputObject
                B.LogInfo
                "./module.mlir"
    L.withStdOutLogger $ \logger -> do
        E.runEff $ L.runLog "Test.Codegen.Context.Module" logger defaultLogLevel $ runCodegen spec $ do
            codegen
            E.gets (TB.runBuilder . C.builder)

-- | Helper types
primitiveI32 :: TT.Type
primitiveI32 = TT.TypePrim (TT.PrimInt TT.PrimInt32)

primitiveI64 :: TT.Type
primitiveI64 = TT.TypePrim (TT.PrimInt TT.PrimInt64)

primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

primitiveUnit :: TT.Type
primitiveUnit = TT.TypePrim TT.PrimUnit

normalField :: TT.Type -> TT.RecordField
normalField fieldType = RecordField{fieldType, fieldIsMutable = False}

-- | Test: Recursive List type with Cons and Nil variants
testRecursiveListType :: TestTree
testRecursiveListType = testCase "Recursive List type with Cons and Nil" $ do
    -- Construct the recursive List type:
    -- List::Cons [value] { i32, !list }
    -- List::Nil [value] {}
    -- List { !cons, !nil }

    -- The list type (for recursive reference)
    let listSymbol = verifiedSymbol "_ZN4ListE"
    let listType = TT.TypeExpr listSymbol

    -- Cons variant: { i32, [shared] !list }
    let consRecord =
            Record
                { kind = Compound
                , fields =
                    V.fromList $
                        map
                            normalField
                            [ primitiveI32
                            , listType
                            ]
                , defaultCapability = TT.Value
                }

    -- Nil variant: {}
    let nilRecord =
            Record
                { kind = Compound
                , fields = mempty
                , defaultCapability = TT.Value
                }

    -- List type: variant with cons and nil
    let consSymbol = verifiedSymbol "_ZN4List4ConsE"
    let nilSymbol = verifiedSymbol "_ZN4List3NilE"
    let consType = TT.TypeExpr consSymbol
    let nilType = TT.TypeExpr nilSymbol
    let listRecord =
            Record
                { kind = Variant
                , fields =
                    V.fromList $
                        map
                            normalField
                            [ consType
                            , nilType
                            ]
                , defaultCapability = TT.Shared
                }

    result <- runCodegenAsText $ do
        -- Add type instances
        C.addTypeInstance consSymbol consRecord
        C.addTypeInstance nilSymbol nilRecord
        C.addTypeInstance listSymbol listRecord

        -- Emit module with empty body
        C.emitModuleEnv $ pure ()

    -- Expected output should contain type aliases
    let resultStr = T.unpack result

    -- Check that all type aliases are emitted (using Itanium mangling format)
    assertBool "Should contain List::Cons alias" $
        "!_ZN4List4ConsE =" `isInfixOf` resultStr
    assertBool "Should contain List::Nil alias" $
        "!_ZN4List3NilE =" `isInfixOf` resultStr
    assertBool "Should contain List alias" $ "!_ZN4ListE =" `isInfixOf` resultStr

    -- Check record structure
    assertBool "Should contain compound record" $
        "!reussir.record<compound" `isInfixOf` resultStr
    assertBool "Should contain variant record" $
        "!reussir.record<variant" `isInfixOf` resultStr
    assertBool "Should contain i32 field" $ "i32" `isInfixOf` resultStr
    assertBool "Should contain [shared] capability" $
        "[shared]" `isInfixOf` resultStr

    -- Check module structure
    assertBool "Should contain module declaration" $
        "module @" `isInfixOf` resultStr
    assertBool "Should contain closing brace" $ "}" `isInfixOf` resultStr

-- | Test: Simple compound type
testSimpleCompoundType :: TestTree
testSimpleCompoundType = testCase "Simple compound type" $ do
    let pointSymbol = verifiedSymbol "_ZN5PointE"
    let pointRecord =
            Record
                { kind = Compound
                , fields =
                    V.fromList $
                        map
                            normalField
                            [ primitiveI32 -- x
                            , primitiveI32 -- y
                            ]
                , defaultCapability = TT.Shared
                }

    result <- runCodegenAsText $ do
        C.addTypeInstance pointSymbol pointRecord
        C.emitModuleEnv $ pure ()

    let resultStr = T.unpack result

    assertBool "Should contain Point alias" $ "!_ZN5PointE =" `isInfixOf` resultStr
    assertBool "Should contain compound record" $
        "!reussir.record<compound" `isInfixOf` resultStr
    assertBool "Should contain i32 fields" $ "i32" `isInfixOf` resultStr

-- | Test: Variant type with capabilities
testVariantWithCapabilities :: TestTree
testVariantWithCapabilities = testCase "Variant type with different capabilities" $ do
    -- Some { [value] i32 }
    let someRecord =
            Record
                { kind = Compound
                , fields = V.fromList [normalField primitiveI32]
                , defaultCapability = TT.Value
                }

    -- None {}
    let noneRecord =
            Record
                { kind = Compound
                , fields = mempty
                , defaultCapability = TT.Value
                }

    -- Option { !some, !none }
    let someSymbol = verifiedSymbol "_ZN6Option4SomeE"
    let noneSymbol = verifiedSymbol "_ZN6Option4NoneE"
    let optionSymbol = verifiedSymbol "_ZN6OptionE"
    let someType = TT.TypeExpr someSymbol
    let noneType = TT.TypeExpr noneSymbol
    let optionRecord =
            Record
                { kind = Variant
                , fields =
                    V.fromList $
                        map
                            normalField
                            [ someType
                            , noneType
                            ]
                , defaultCapability = TT.Value
                }

    result <- runCodegenAsText $ do
        C.addTypeInstance someSymbol someRecord
        C.addTypeInstance noneSymbol noneRecord
        C.addTypeInstance optionSymbol optionRecord
        C.emitModuleEnv $ pure ()

    let resultStr = T.unpack result

    assertBool "Should contain [value] capability" $ "[value]" `isInfixOf` resultStr
    assertBool "Should contain variant record" $
        "!reussir.record<variant" `isInfixOf` resultStr

-- | Test: RC types with different capabilities and atomicity
testRcTypes :: TestTree
testRcTypes = testCase "RC types with capabilities" $ do
    result <- runCodegenAsText $ do
        C.emitBuilder "!i32_rc = "
        C.emitBuilder
            =<< emitTy' (TT.TypeRc (TT.Rc primitiveI32 TT.NonAtomic TT.Shared))
        C.emitBuilder "\n"
        C.emitBuilder "!i64_rc_atomic = "
        C.emitBuilder =<< emitTy' (TT.TypeRc (TT.Rc primitiveI64 TT.Atomic TT.Value))
        C.emitBuilder "\n"

    let resultStr = T.unpack result

    assertBool "Should contain reussir.rc" $ "!reussir.rc<" `isInfixOf` resultStr
    assertBool "Should contain shared capability" $ " shared" `isInfixOf` resultStr
    assertBool "Should contain atomic" $ " atomic" `isInfixOf` resultStr

-- | Test: Ref types with different capabilities
testRefTypes :: TestTree
testRefTypes = testCase "Ref types with capabilities" $ do
    result <- runCodegenAsText $ do
        C.emitBuilder "!i32_ref = "
        C.emitBuilder
            =<< emitTy' (TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Flex))
        C.emitBuilder "\n"
        C.emitBuilder "!f32_ref_rigid = "
        C.emitBuilder =<< emitTy' (TT.TypeRef (TT.Ref primitiveF32 TT.Atomic TT.Rigid))
        C.emitBuilder "\n"

    let resultStr = T.unpack result

    assertBool "Should contain reussir.ref" $ "!reussir.ref<" `isInfixOf` resultStr
    assertBool "Should contain flex capability" $ " flex" `isInfixOf` resultStr
    assertBool "Should contain rigid capability" $ " rigid" `isInfixOf` resultStr

-- | Test: Closure types
testClosureTypes :: TestTree
testClosureTypes = testCase "Closure types" $ do
    result <- runCodegenAsText $ do
        C.emitBuilder "!closure_no_args = "
        C.emitBuilder =<< emitTy' (TT.TypeClosure (TT.Closure [] primitiveUnit))
        C.emitBuilder "\n"
        C.emitBuilder "!closure_i32_to_i64 = "
        C.emitBuilder
            =<< emitTy' (TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64))
        C.emitBuilder "\n"
        C.emitBuilder "!closure_multi_args = "
        C.emitBuilder
            =<< emitTy' (TT.TypeClosure (TT.Closure [primitiveI32, primitiveF32] primitiveUnit))
        C.emitBuilder "\n"

    let resultStr = T.unpack result

    assertBool "Should contain reussir.closure" $
        "!reussir.closure<" `isInfixOf` resultStr
    assertBool "Should contain () for unit return" $ ")>" `isInfixOf` resultStr
    assertBool "Should contain -> for non-unit return" $
        " -> " `isInfixOf` resultStr
    assertBool "Should contain multiple arguments" $
        "i32, f32" `isInfixOf` resultStr

-- Helper to emit types
emitTy' :: TT.Type -> C.Codegen TB.Builder
emitTy' ty = C.emit ty

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

moduleTests :: TestTree
moduleTests =
    testGroup
        "Module Emission Tests"
        [ testRecursiveListType
        , testSimpleCompoundType
        , testVariantWithCapabilities
        , testRcTypes
        , testRefTypes
        , testClosureTypes
        ]
