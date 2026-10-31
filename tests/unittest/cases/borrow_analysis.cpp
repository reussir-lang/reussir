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
      return %x : !reussir.rc<i64>
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "even"), 0));
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "odd"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "even"), 1));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "leaky_odd"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "leaky_even"), 0));
}

// The frontend's plain release keeps the parameter borrowable. The forms
// later passes make (a release yielding a reuse token, or one with retains
// of bound members folded in) are not rewritten by the specialization, so
// such a parameter stays owned.
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

// A borrow of the parameter may only be read, through projections too;
// dropping the payload would release the fields the caller still owns.
TEST_F(BorrowAnalysisTest, BorrowsMayOnlyRead) {
  auto module = parse(R"(
    !list_ = !reussir.record<variant "List" incomplete>
    !nil = !reussir.record<compound "List::Nil" [value] { }>
    !cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
    !list = !reussir.record<variant "List" { !nil, !cons }>
    !box = !reussir.record<compound "Box" { i64, !list }>
    func.func private @read(%p: !reussir.rc<!box>) -> i64 {
      %r = reussir.rc.borrow(%p : !reussir.rc<!box>) : !reussir.ref<!box shared>
      %f = reussir.ref.project(%r : !reussir.ref<!box shared>) [0] : !reussir.ref<i64 shared>
      %v = reussir.ref.load(%f : !reussir.ref<i64 shared>) : i64
      reussir.rc.dec(%p : !reussir.rc<!box>)
      return %v : i64
    }
    func.func private @drop(%p: !reussir.rc<!box>) {
      %r = reussir.rc.borrow(%p : !reussir.rc<!box>) : !reussir.ref<!box shared>
      %f = reussir.ref.project(%r : !reussir.ref<!box shared>) [1] : !reussir.ref<!reussir.rc<!list> shared>
      reussir.ref.drop(%f : !reussir.ref<!reussir.rc<!list> shared>)
      return
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "read"), 0));
  EXPECT_FALSE(analysis.isBorrowable(lookup(*module, "drop"), 0));
}

} // namespace
} // namespace reussir
