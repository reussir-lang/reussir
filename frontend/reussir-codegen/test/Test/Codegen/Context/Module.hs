{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Context.Module (
    moduleTests,
)
where

import Control.Monad.State.Strict qualified as S
import Data.Interned (intern)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Context.Path (Path (..))
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Type.Record (Record (..), RecordKind (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to run codegen and extract the builder as text
runCodegenAsText :: C.Codegen () -> IO T.Text
runCodegenAsText codegen = do
    let spec = C.TargetSpec "test_module" "output.mlir" B.OptDefault B.OutputObject B.LogInfo
    ctx <- C.emptyContext spec
    (_, finalCtx) <- S.runStateT (C.genState codegen) ctx
    return $ TB.toLazyText (C.builder finalCtx)

-- | Helper types
primitiveI32 :: TT.Type
primitiveI32 = TT.TypePrim (TT.PrimInt TT.PrimInt32)

primitiveI64 :: TT.Type
primitiveI64 = TT.TypePrim (TT.PrimInt TT.PrimInt64)

primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

primitiveUnit :: TT.Type
primitiveUnit = TT.TypePrim TT.PrimUnit

-- | Create a path from segments
mkPath :: [String] -> Path
mkPath segments = Path (map (intern . T.toStrict . T.pack) segments)

-- | Test: Recursive List type with Cons and Nil variants
testRecursiveListType :: TestTree
testRecursiveListType = testCase "Recursive List type with Cons and Nil" $ do
    -- Construct the recursive List type:
    -- List::Cons { i32, [shared] !list }
    -- List::Nil {}
    -- List { !cons, !nil }

    let consPath = mkPath ["List", "Cons"]
    let nilPath = mkPath ["List", "Nil"]
    let listPath = mkPath ["List"]

    -- Type expressions for each variant
    let consExpr = TT.Expr{TT.exprPath = consPath, TT.exprArgs = []}
    let nilExpr = TT.Expr{TT.exprPath = nilPath, TT.exprArgs = []}
    let listExpr = TT.Expr{TT.exprPath = listPath, TT.exprArgs = []}

    -- The list type (for recursive reference)
    let listType = TT.TypeExpr listExpr

    -- Cons variant: { i32, [shared] !list }
    let consRecord =
            Record
                { kind = Compound
                , fields =
                    [ (primitiveI32, TT.Unspecified)
                    , (listType, TT.Shared)
                    ]
                , defaultCapability = TT.Unspecified
                }

    -- Nil variant: {}
    let nilRecord =
            Record
                { kind = Compound
                , fields = []
                , defaultCapability = TT.Unspecified
                }

    -- List type: variant with cons and nil
    let consType = TT.TypeExpr consExpr
    let nilType = TT.TypeExpr nilExpr
    let listRecord =
            Record
                { kind = Variant
                , fields =
                    [ (consType, TT.Unspecified)
                    , (nilType, TT.Unspecified)
                    ]
                , defaultCapability = TT.Unspecified
                }

    result <- runCodegenAsText $ do
        -- Add type instances
        C.addTypeInstance consExpr consRecord
        C.addTypeInstance nilExpr nilRecord
        C.addTypeInstance listExpr listRecord

        -- Emit module with empty body
        C.emitModule $ pure ()

    -- Expected output should contain type aliases
    let resultStr = T.unpack result

    -- Check that all type aliases are emitted (using Itanium mangling format)
    assertBool "Should contain List::Cons alias" $ "!_ZN4List4ConsE =" `isInfixOf` resultStr
    assertBool "Should contain List::Nil alias" $ "!_ZN4List3NilE =" `isInfixOf` resultStr
    assertBool "Should contain List alias" $ "!_ZN4ListE =" `isInfixOf` resultStr

    -- Check record structure
    assertBool "Should contain compound record" $ "!reussir.record<compound" `isInfixOf` resultStr
    assertBool "Should contain variant record" $ "!reussir.record<variant" `isInfixOf` resultStr
    assertBool "Should contain i32 field" $ "i32" `isInfixOf` resultStr
    assertBool "Should contain [shared] capability" $ "[shared]" `isInfixOf` resultStr

    -- Check module structure
    assertBool "Should contain module declaration" $ "module @" `isInfixOf` resultStr
    assertBool "Should contain closing brace" $ "}" `isInfixOf` resultStr

-- | Test: Simple compound type
testSimpleCompoundType :: TestTree
testSimpleCompoundType = testCase "Simple compound type" $ do
    let pointPath = mkPath ["Point"]
    let pointExpr = TT.Expr{TT.exprPath = pointPath, TT.exprArgs = []}

    let pointRecord =
            Record
                { kind = Compound
                , fields =
                    [ (primitiveI32, TT.Unspecified) -- x
                    , (primitiveI32, TT.Unspecified) -- y
                    ]
                , defaultCapability = TT.Unspecified
                }

    result <- runCodegenAsText $ do
        C.addTypeInstance pointExpr pointRecord
        C.emitModule $ pure ()

    let resultStr = T.unpack result

    assertBool "Should contain Point alias" $ "!_ZN5PointE =" `isInfixOf` resultStr
    assertBool "Should contain compound record" $ "!reussir.record<compound" `isInfixOf` resultStr
    assertBool "Should contain i32 fields" $ "i32" `isInfixOf` resultStr

-- | Test: Variant type with capabilities
testVariantWithCapabilities :: TestTree
testVariantWithCapabilities = testCase "Variant type with different capabilities" $ do
    let somePath = mkPath ["Option", "Some"]
    let nonePath = mkPath ["Option", "None"]
    let optionPath = mkPath ["Option"]

    let someExpr = TT.Expr{TT.exprPath = somePath, TT.exprArgs = []}
    let noneExpr = TT.Expr{TT.exprPath = nonePath, TT.exprArgs = []}
    let optionExpr = TT.Expr{TT.exprPath = optionPath, TT.exprArgs = []}

    -- Some { [value] i32 }
    let someRecord =
            Record
                { kind = Compound
                , fields = [(primitiveI32, TT.Value)]
                , defaultCapability = TT.Unspecified
                }

    -- None {}
    let noneRecord =
            Record
                { kind = Compound
                , fields = []
                , defaultCapability = TT.Unspecified
                }

    -- Option { !some, !none }
    let someType = TT.TypeExpr someExpr
    let noneType = TT.TypeExpr noneExpr
    let optionRecord =
            Record
                { kind = Variant
                , fields =
                    [ (someType, TT.Unspecified)
                    , (noneType, TT.Unspecified)
                    ]
                , defaultCapability = TT.Unspecified
                }

    result <- runCodegenAsText $ do
        C.addTypeInstance someExpr someRecord
        C.addTypeInstance noneExpr noneRecord
        C.addTypeInstance optionExpr optionRecord
        C.emitModule $ pure ()

    let resultStr = T.unpack result

    assertBool "Should contain [value] capability" $ "[value]" `isInfixOf` resultStr
    assertBool "Should contain variant record" $ "!reussir.record<variant" `isInfixOf` resultStr

-- | Test: RC types with different capabilities and atomicity
testRcTypes :: TestTree
testRcTypes = testCase "RC types with capabilities" $ do
    result <- runCodegenAsText $ do
        C.emitBuilder "!i32_rc = "
        C.emitBuilder =<< emitTy' (TT.TypeRc (TT.Rc primitiveI32 TT.NonAtomic TT.Shared))
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
        C.emitBuilder =<< emitTy' (TT.TypeRef (TT.Ref primitiveI32 TT.NonAtomic TT.Flex))
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
        C.emitBuilder =<< emitTy' (TT.TypeClosure (TT.Closure [primitiveI32] primitiveI64))
        C.emitBuilder "\n"
        C.emitBuilder "!closure_multi_args = "
        C.emitBuilder =<< emitTy' (TT.TypeClosure (TT.Closure [primitiveI32, primitiveF32] primitiveUnit))
        C.emitBuilder "\n"

    let resultStr = T.unpack result

    assertBool "Should contain reussir.closure" $ "!reussir.closure<" `isInfixOf` resultStr
    assertBool "Should contain () for unit return" $ ")>" `isInfixOf` resultStr
    assertBool "Should contain -> for non-unit return" $ " -> " `isInfixOf` resultStr
    assertBool "Should contain multiple arguments" $ "i32, f32" `isInfixOf` resultStr

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
