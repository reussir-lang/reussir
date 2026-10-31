//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Transformation/Passes.h"
#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Pass/PassManager.h>
import reussir.test;

namespace reussir {
namespace {
class BorrowSpecializationTest : public ReussirTest {
protected:
  // Runs the pass (and the verifier after it) on `source`.
  mlir::OwningOpRef<mlir::ModuleOp> specialize(std::string_view source) {
    auto module = parse(source);
    if (!module)
      return module;
    mlir::PassManager pm(context.get());
    pm.addPass(createReussirBorrowSpecializationPass());
    if (mlir::failed(pm.run(*module)))
      return nullptr;
    return module;
  }
};

constexpr llvm::StringLiteral kRead = R"(
  func.func private @read(%x: !reussir.rc<i64>) -> i64 {
    %b = reussir.rc.borrow(%x : !reussir.rc<i64>) : !reussir.ref<i64 shared>
    %v = reussir.ref.load(%b : !reussir.ref<i64 shared>) : i64
    reussir.rc.dec(%x : !reussir.rc<i64>)
    return %v : i64
  }
)";

// A caller that keeps the argument calls a borrowed clone that neither
// retains nor releases it, and the module still verifies.
TEST_F(BorrowSpecializationTest, LiveThroughCallBorrows) {
  auto module = specialize((kRead + R"(
    func.func @twice(%x: !reussir.rc<i64>) -> i64 {
      reussir.rc.inc(%x : !reussir.rc<i64>)
      %a = func.call @read(%x) : (!reussir.rc<i64>) -> i64
      %b = func.call @read(%x) : (!reussir.rc<i64>) -> i64
      %s = arith.addi %a, %b : i64
      return %s : i64
    }
  )")
                               .str());
  ASSERT_TRUE(module);
  auto clone = module->lookupSymbol<mlir::func::FuncOp>("read.borrowed_0");
  ASSERT_TRUE(clone);
  EXPECT_TRUE(isBorrowedArgument(clone.getArgument(0)));
  EXPECT_TRUE(clone.isPrivate());
  unsigned counts = 0;
  clone.walk([&](mlir::Operation *op) {
    counts += llvm::isa<ReussirRcIncOp, ReussirRcDecOp>(op);
  });
  EXPECT_EQ(counts, 0u);
  unsigned retains = 0;
  module->lookupSymbol<mlir::func::FuncOp>("twice").walk(
      [&](ReussirRcIncOp) { ++retains; });
  EXPECT_EQ(retains, 0u);
}
} // namespace
} // namespace reussir
