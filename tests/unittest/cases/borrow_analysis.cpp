//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Parser/Parser.h>

namespace reussir {
namespace {
class BorrowAnalysisTest : public testing::Test {
protected:
  mlir::MLIRContext context;
  BorrowAnalysisTest() {
    context.loadDialect<ReussirDialect, mlir::func::FuncDialect,
                        mlir::arith::ArithDialect, mlir::scf::SCFDialect>();
  }
  mlir::OwningOpRef<mlir::ModuleOp> parse(llvm::StringRef source) {
    return mlir::parseSourceString<mlir::ModuleOp>(source, &context);
  }
  static mlir::func::FuncOp lookup(mlir::ModuleOp module,
                                   llvm::StringRef name) {
    return module.lookupSymbol<mlir::func::FuncOp>(name);
  }
};

// Mutual recursion through borrowable positions keeps both parameters
// borrowable; one consuming use anywhere in the cycle removes both.
TEST_F(BorrowAnalysisTest, FixedPointOverMutualRecursion) {
  auto module = parse(R"(
    func.func private @even(%x: !reussir.rc<i64>, %n: i64) -> i1 {
      %c0 = arith.constant 0 : i64
      %z = arith.cmpi eq, %n, %c0 : i64
      %r = scf.if %z -> (i1) {
        reussir.rc.dec(%x : !reussir.rc<i64>)
        %t = arith.constant true
        scf.yield %t : i1
      } else {
        %c1 = arith.constant 1 : i64
        %m = arith.subi %n, %c1 : i64
        %o = func.call @odd(%x, %m) : (!reussir.rc<i64>, i64) -> i1
        scf.yield %o : i1
      }
      return %r : i1
    }
    func.func private @odd(%x: !reussir.rc<i64>, %n: i64) -> i1 {
      %e = func.call @even(%x, %n) : (!reussir.rc<i64>, i64) -> i1
      return %e : i1
    }
    func.func private @leaky_even(%x: !reussir.rc<i64>) -> !reussir.rc<i64> {
      %y = func.call @leaky_odd(%x) : (!reussir.rc<i64>) -> !reussir.rc<i64>
      return %y : !reussir.rc<i64>
    }
    func.func private @leaky_odd(%x: !reussir.rc<i64>) -> !reussir.rc<i64> {
      %y = func.call @sink(%x) : (!reussir.rc<i64>) -> !reussir.rc<i64>
      return %y : !reussir.rc<i64>
    }
    func.func private @sink(!reussir.rc<i64>) -> !reussir.rc<i64>
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "even"), 0));
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "odd"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "even"), 1));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "leaky_odd"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "leaky_even"), 0));
}

// Erasing a plain release is all a borrowed clone has to do. A release that
// yields a reuse token, or destructures and so hands retains to bound
// members, consumes the parameter.
TEST_F(BorrowAnalysisTest, OnlyPlainReleasesKeepBorrowable) {
  auto module = parse(R"(
    !list_ = !reussir.record<variant "List" incomplete>
    !nil = !reussir.record<compound "List::Nil" [value] { }>
    !cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
    !list = !reussir.record<variant "List" { !nil, !cons }>
    !pair = !reussir.record<compound "Pair" { i64, !list }>
    !token = !reussir.nullable<!reussir.token<align: 8, size: 24>>
    func.func private @plain(%p: !reussir.rc<!pair>) {
      reussir.rc.dec(%p : !reussir.rc<!pair>)
      return
    }
    func.func private @token(%p: !reussir.rc<!pair>) -> !token {
      %t = reussir.rc.dec(%p : !reussir.rc<!pair>) : !token
      return %t : !token
    }
    func.func private @destructure(%p: !reussir.rc<!pair>) -> !reussir.rc<!list> {
      %r = reussir.rc.borrow(%p : !reussir.rc<!pair>) : !reussir.ref<!pair shared>
      %s = reussir.ref.project(%r : !reussir.ref<!pair shared>) [1] : !reussir.ref<!reussir.rc<!list> shared>
      %m = reussir.ref.load(%s : !reussir.ref<!reussir.rc<!list> shared>) : !reussir.rc<!list>
      reussir.rc.dec(%p : !reussir.rc<!pair>) {boundMembers = array<i64: 1>}
      return %m : !reussir.rc<!list>
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "plain"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "token"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "destructure"), 0));
}

// Inside a loop, a value defined outside it is used again by the next
// iteration; a value defined in the body is a new value each time.
TEST_F(BorrowAnalysisTest, LoopsReuseOuterValues) {
  auto module = parse(R"(
    func.func private @read(%x: !reussir.rc<i64>) -> i64 {
      reussir.rc.dec(%x : !reussir.rc<i64>)
      %z = arith.constant 0 : i64
      return %z : i64
    }
    func.func private @loop(%x: !reussir.rc<i64>, %n: index) {
      %c0 = arith.constant 0 : index
      %c1 = arith.constant 1 : index
      scf.for %i = %c0 to %n step %c1 {
        reussir.rc.inc(%x : !reussir.rc<i64>)
        %a = func.call @read(%x) : (!reussir.rc<i64>) -> i64
        %v = arith.constant 7 : i64
        %y = reussir.rc.create value(%v : i64) : !reussir.rc<i64>
        %b = func.call @read(%y) : (!reussir.rc<i64>) -> i64
      }
      reussir.rc.dec(%x : !reussir.rc<i64>)
      return
    }
  )");
  ASSERT_TRUE(module);
  llvm::SmallVector<mlir::func::CallOp> calls;
  lookup(*module, "loop").walk([&](mlir::func::CallOp c) {
    calls.push_back(c);
  });
  ASSERT_EQ(calls.size(), 2u);
  EXPECT_TRUE(isUsedAfter(calls[0].getOperand(0), calls[0]));
  EXPECT_FALSE(isUsedAfter(calls[1].getOperand(0), calls[1]));
}
} // namespace
} // namespace reussir
