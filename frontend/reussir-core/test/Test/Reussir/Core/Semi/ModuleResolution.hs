{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core.Semi.ModuleResolution (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Reussir.Core.Module (PackageInfo (..), validatePackageInfo)
import Reussir.Core.Semi.Context (resolveSpecialRelativePath)

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Semi.ModuleResolution"
        [ testGroup
            "validatePackageInfo"
            [ testCase "accepts non-reserved package names" testValidateNormalPackage
            , testCase "rejects the reserved core package name" testValidateReservedPackage
            ]
        , testGroup
            "resolveSpecialRelativePath"
            [ testCase "ignores non-special leading segments" testNonSpecialPath
            , testCase "root resolves from any nested module to the package root" testRootPath
            , testCase "super resolves to the parent module" testSuperPath
            , testCase "multiple supers climb multiple module levels" testNestedSuperPath
            , testCase "super cannot escape the package root" testInvalidSuperPath
            , testCase "root requires a package-qualified module path" testRootWithoutPackagePath
            ]
        ]

testValidateNormalPackage :: Assertion
testValidateNormalPackage =
    case validatePackageInfo (PackageInfo "/tmp/mypkg" "mypkg") of
        Right _ -> pure ()
        Left err -> assertFailure err

testValidateReservedPackage :: Assertion
testValidateReservedPackage =
    case validatePackageInfo (PackageInfo "/tmp/core" "core") of
        Left err ->
            err @?= "Error: package name 'core' is reserved for the built-in core package"
        Right _ -> assertFailure "expected the reserved core package name to be rejected"

testNonSpecialPath :: Assertion
testNonSpecialPath =
    resolveSpecialRelativePath ["mypkg", "utils"] ["math"]
        @?= Nothing

testRootPath :: Assertion
testRootPath =
    resolveSpecialRelativePath ["mypkg", "utils", "nested"] ["root", "models"]
        @?= Just ["mypkg", "models"]

testSuperPath :: Assertion
testSuperPath =
    resolveSpecialRelativePath ["mypkg", "utils", "nested"] ["super"]
        @?= Just ["mypkg", "utils"]

testNestedSuperPath :: Assertion
testNestedSuperPath =
    resolveSpecialRelativePath ["mypkg", "utils", "nested"] ["super", "super", "models"]
        @?= Just ["mypkg", "models"]

testInvalidSuperPath :: Assertion
testInvalidSuperPath =
    resolveSpecialRelativePath ["mypkg"] ["super"]
        @?= Nothing

testRootWithoutPackagePath :: Assertion
testRootWithoutPackagePath =
    resolveSpecialRelativePath [] ["root", "models"]
        @?= Nothing
