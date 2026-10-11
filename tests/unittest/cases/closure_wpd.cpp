//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/ClosureWpd.h"
#include "Reussir/IR/ReussirDialect.h"
#include <gtest/gtest.h>
#include <mlir/AsmParser/AsmParser.h>
#include <mlir/IR/BuiltinTypes.h>
#include <set>
#include <string>

namespace reussir {
namespace {

std::string printType(mlir::Type type, bool compact) {
  std::string text;
  llvm::raw_string_ostream os(text);
  if (compact)
    printTypeWithRecordBodiesOnce(type, os);
  else
    type.print(os);
  return text;
}

std::string returnTypeId(mlir::Type type) {
  return closureWpdTypeId(ClosureType::get(type.getContext(), {}, type));
}

TEST(ClosureWpdTest, SharedRecordTowerHasBoundedText) {
  mlir::MLIRContext context;
  context.getOrLoadDialect<ReussirDialect>();
  mlir::Type type = mlir::IntegerType::get(&context, 64);
  for (unsigned depth = 0; depth <= 12; ++depth) {
    type = RecordType::get(
        &context, {type, type}, {false, false},
        mlir::StringAttr::get(&context, "D" + std::to_string(depth)),
        RecordKind::compound, Capability::shared, false);
    const auto compact = printType(type, true);
    // A deterministic size bound catches exponential expansion without a
    // timing threshold or requiring an enormous fixture.
    EXPECT_LT(compact.size(), 128u * (depth + 1));
    llvm::BLAKE3 hasher;
    hasher.update(compact);
    EXPECT_EQ(returnTypeId(type),
              "reussir.closure.wpd." + base62Digits(blake3Words(hasher)));
  }
  const auto ordinary = printType(type, false);
  EXPECT_GT(ordinary.size(), 10 * printType(type, true).size());
  // The printing mode is scoped: repeated ID generation must neither retain
  // a visited set nor change the normal, parseable MLIR representation.
  EXPECT_EQ(returnTypeId(type), returnTypeId(type));
  EXPECT_EQ(ordinary, printType(type, false));
}

TEST(ClosureWpdTest, StableAcrossContextsAndDistinctReturnTypes) {
  const char *spellings[] = {
      "i32",
      "i64",
      R"(!reussir.record<compound "A" {i32}>)",
      R"(!reussir.record<compound "B" {i32}>)",
      R"(!reussir.record<variant "A" {i32}>)",
      R"(!reussir.record<variant "A" fixed {i32}>)",
      R"(!reussir.record<compound "A" {i64}>)",
      R"(!reussir.record<compound "A" {i32, i64}>)",
      R"(!reussir.record<compound "A" {i64, i32}>)",
      R"(!reussir.record<compound "Pair" {
          !reussir.record<compound "Leaf" {i32}>,
          !reussir.record<compound "Leaf" {i32}>}>)",
      R"(!reussir.record<compound "Pair" {
          !reussir.record<compound "Leaf" {i64}>,
          !reussir.record<compound "Leaf" {i64}>}>)",
      R"(!reussir.record<compound "Pair" {
          !reussir.record<compound "Leaf" {i32}>,
          !reussir.record<variant "Leaf" {i32}>}>)",
      R"(!reussir.record<compound "Node" {
          !reussir.rc<!reussir.record<compound "Node">>}>)",
      R"(!reussir.record<compound "Node" {
          !reussir.rc<!reussir.record<compound "Node">>, i32}>)",
  };
  std::set<std::string> ids;
  for (auto spelling : spellings) {
    // Named records are uniqued by (name, kind) within a context. Use fresh
    // contexts to also distinguish the same name with different definitions.
    mlir::MLIRContext first, second;
    first.getOrLoadDialect<ReussirDialect>();
    second.getOrLoadDialect<ReussirDialect>();
    ASSERT_TRUE(mlir::parseType(
        R"(!reussir.record<compound "Unrelated" {i8}>)", &second));
    auto a = mlir::parseType(spelling, &first);
    auto b = mlir::parseType(spelling, &second);
    ASSERT_TRUE(a) << spelling;
    ASSERT_TRUE(b) << spelling;
    const auto id = returnTypeId(a);
    EXPECT_EQ(id, returnTypeId(b)) << spelling;
    EXPECT_TRUE(ids.insert(id).second) << spelling;
    // Inputs do not change a closure's return-type family.
    EXPECT_EQ(id, closureWpdTypeId(ClosureType::get(
                      &first, {mlir::IntegerType::get(&first, 8)}, a)));
    const auto ordinary = printType(a, false);
    (void)printType(a, true);
    EXPECT_EQ(ordinary, printType(a, false));
  }
}

TEST(ClosureWpdTest, UnsharedAndRecursiveTypesKeepTheirText) {
  mlir::MLIRContext context;
  context.getOrLoadDialect<ReussirDialect>();
  for (auto spelling : {
           "i32",
           R"(!reussir.record<compound {i32, i64}>)",
           R"(tuple<!reussir.record<compound {i32}>,
                    !reussir.record<compound {i32}>>)",
           R"(!reussir.record<compound "Outer" {
               !reussir.record<compound "Inner" {i32}>}>)",
           R"(!reussir.record<compound "Cycle" {
               !reussir.rc<!reussir.record<compound "Cycle">>}>)"}) {
    auto type = mlir::parseType(spelling, &context);
    ASSERT_TRUE(type);
    EXPECT_EQ(printType(type, false), printType(type, true));
  }
  EXPECT_EQ(closureWpdTypeId(ClosureType::get(&context, {}, {})),
            "reussir.closure.wpd.unit");
}

} // namespace
} // namespace reussir
