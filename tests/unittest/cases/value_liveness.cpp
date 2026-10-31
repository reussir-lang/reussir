//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "Reussir/Analysis/ValueLiveness.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/Dialect/ControlFlow/IR/ControlFlowOps.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Parser/Parser.h>

namespace reussir {
namespace {
class ValueLivenessTest : public testing::Test {
protected:
  mlir::MLIRContext context;
  ValueLivenessTest() {
    context.loadDialect<ReussirDialect, mlir::func::FuncDialect,
                        mlir::arith::ArithDialect, mlir::scf::SCFDialect,
                        mlir::cf::ControlFlowDialect>();
  }
  mlir::OwningOpRef<mlir::ModuleOp> parse(llvm::StringRef source) {
    return mlir::parseSourceString<mlir::ModuleOp>(source, &context);
  }
  // The calls of the module's first function, in order.
  static llvm::SmallVector<mlir::func::CallOp> calls(mlir::ModuleOp module) {
    llvm::SmallVector<mlir::func::CallOp> result;
    module.walk([&](mlir::func::CallOp call) { result.push_back(call); });
    return result;
  }
};

// A use after the op holding the region keeps the value live inside it,
// along the branch that reaches it; the other branch's use does not run
// after the call.
TEST_F(ValueLivenessTest, RegionsExitToTheirParent) {
  auto module = parse(R"(
    func.func private @g(%x: !reussir.rc<i64>)
    func.func private @f(%x: !reussir.rc<i64>, %y: !reussir.rc<i64>, %c: i1) {
      scf.if %c {
        func.call @g(%x) : (!reussir.rc<i64>) -> ()
      } else {
        func.call @g(%y) : (!reussir.rc<i64>) -> ()
      }
      func.call @g(%x) : (!reussir.rc<i64>) -> ()
      return
    }
  )");
  ASSERT_TRUE(module);
  auto c = calls(*module);
  ASSERT_EQ(c.size(), 3u);
  mlir::Value x = c[0].getOperand(0), y = c[1].getOperand(0);
  ValueLiveness liveness(c[0]->getParentOfType<mlir::func::FuncOp>(), {x, y});
  EXPECT_TRUE(liveness.isLiveAfter(x, c[0]));
  EXPECT_TRUE(liveness.isLiveAfter(x, c[1]));
  EXPECT_FALSE(liveness.isLiveAfter(y, c[1]));
  EXPECT_FALSE(liveness.isLiveAfter(x, c[2]));
}

// A value from outside a loop is used again by the next iteration; one
// defined in the body is a new value each time, so the back edge does not
// carry it.
TEST_F(ValueLivenessTest, LoopsCarryOnlyOuterValues) {
  auto module = parse(R"(
    func.func private @g(%x: !reussir.rc<i64>)
    func.func private @f(%x: !reussir.rc<i64>, %n: index) {
      %c0 = arith.constant 0 : index
      %c1 = arith.constant 1 : index
      scf.for %i = %c0 to %n step %c1 {
        %v = arith.constant 7 : i64
        %y = reussir.rc.create value(%v : i64) : !reussir.rc<i64>
        func.call @g(%y) : (!reussir.rc<i64>) -> ()
        func.call @g(%x) : (!reussir.rc<i64>) -> ()
      }
      return
    }
  )");
  ASSERT_TRUE(module);
  auto c = calls(*module);
  ASSERT_EQ(c.size(), 2u);
  mlir::Value y = c[0].getOperand(0), x = c[1].getOperand(0);
  ValueLiveness liveness(c[0]->getParentOfType<mlir::func::FuncOp>(), {x, y});
  EXPECT_FALSE(liveness.isLiveAfter(y, c[0]));
  EXPECT_TRUE(liveness.isLiveAfter(x, c[1]));
}

// Uses in successor blocks count, and a block argument is defined on entry.
TEST_F(ValueLivenessTest, BranchesReachSuccessorBlocks) {
  auto module = parse(R"(
    func.func private @g(%x: !reussir.rc<i64>)
    func.func private @f(%x: !reussir.rc<i64>, %c: i1) {
      func.call @g(%x) : (!reussir.rc<i64>) -> ()
      cf.cond_br %c, ^use, ^done
    ^use:
      func.call @g(%x) : (!reussir.rc<i64>) -> ()
      cf.br ^done
    ^done:
      return
    }
  )");
  ASSERT_TRUE(module);
  auto c = calls(*module);
  ASSERT_EQ(c.size(), 2u);
  mlir::Value x = c[0].getOperand(0);
  ValueLiveness liveness(c[0]->getParentOfType<mlir::func::FuncOp>(), {x});
  EXPECT_TRUE(liveness.isLiveAfter(x, c[0]));
  EXPECT_FALSE(liveness.isLiveAfter(x, c[1]));
}
} // namespace
} // namespace reussir
