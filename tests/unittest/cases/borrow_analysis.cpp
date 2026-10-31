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
#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
import reussir.test;

namespace reussir {
namespace {
class BorrowAnalysisTest : public ReussirTest {
protected:
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

// A call is live-through when a retain right before it feeds it and the
// caller still uses the value afterwards. The retain of a value loaded from
// a box the call consumes only takes ownership of it; a release in between
// may be the caller's last reference.
TEST_F(BorrowAnalysisTest, LiveThroughNeedsRetainAndLaterUse) {
  auto module = parse(R"(
    !list_ = !reussir.record<variant "List" incomplete>
    !nil = !reussir.record<compound "List::Nil" [value] { }>
    !cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
    !list = !reussir.record<variant "List" { !nil, !cons }>
    !box = !reussir.record<compound "Box" { !list, i64 }>
    func.func private @read(%x: !reussir.rc<!list>, %b: !reussir.rc<!box>) {
      reussir.rc.dec(%b : !reussir.rc<!box>)
      reussir.rc.dec(%x : !reussir.rc<!list>)
      return
    }
    func.func private @kept(%x: !reussir.rc<!list>, %b: !reussir.rc<!box>, %c: i1) {
      scf.if %c {
        reussir.rc.inc(%b : !reussir.rc<!box>)
        reussir.rc.inc(%x : !reussir.rc<!list>)
        func.call @read(%x, %b) : (!reussir.rc<!list>, !reussir.rc<!box>) -> ()
      }
      reussir.rc.dec(%x : !reussir.rc<!list>)
      reussir.rc.dec(%b : !reussir.rc<!box>)
      return
    }
    func.func private @loaded(%b: !reussir.rc<!box>) {
      %r = reussir.rc.borrow(%b : !reussir.rc<!box>) : !reussir.ref<!box shared>
      %f = reussir.ref.project(%r : !reussir.ref<!box shared>) [0] : !reussir.ref<!reussir.rc<!list> shared>
      %x = reussir.ref.load(%f : !reussir.ref<!reussir.rc<!list> shared>) : !reussir.rc<!list>
      reussir.rc.inc(%x : !reussir.rc<!list>)
      func.call @read(%x, %b) : (!reussir.rc<!list>, !reussir.rc<!box>) -> ()
      return
    }
    func.func private @released(%x: !reussir.rc<!list>, %y: !reussir.rc<!list>, %b: !reussir.rc<!box>) {
      reussir.rc.inc(%x : !reussir.rc<!list>)
      reussir.rc.dec(%y : !reussir.rc<!list>)
      func.call @read(%x, %b) : (!reussir.rc<!list>, !reussir.rc<!box>) -> ()
      reussir.rc.dec(%x : !reussir.rc<!list>)
      return
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  llvm::StringMap<const BorrowAnalysis::CallSite *> sites;
  for (const auto &site : analysis.getCallSites())
    sites[site.call->getParentOfType<mlir::func::FuncOp>().getSymName()] =
        &site;
  ASSERT_EQ(sites.size(), 3u);
  EXPECT_TRUE(sites["kept"]->liveThrough.test(0));
  EXPECT_TRUE(sites["kept"]->liveThrough.test(1));
  EXPECT_TRUE(sites["loaded"]->lastUse.test(0));
  EXPECT_TRUE(sites["loaded"]->lastUse.test(1));
  EXPECT_TRUE(sites["released"]->lastUse.test(0));
}

// Liveness reaches successor blocks, and a loop that repeats the call uses
// a value from outside it again; a value defined in the loop body is a new
// one each iteration.
TEST_F(BorrowAnalysisTest, LivenessFollowsBlocksAndLoops) {
  auto module = parse(R"(
    func.func private @read(%x: !reussir.rc<i64>) {
      reussir.rc.dec(%x : !reussir.rc<i64>)
      return
    }
    func.func private @blocks(%x: !reussir.rc<i64>, %c: i1) {
      reussir.rc.inc(%x : !reussir.rc<i64>)
      func.call @read(%x) : (!reussir.rc<i64>) -> ()
      cf.cond_br %c, ^use, ^done
    ^use:
      func.call @read(%x) : (!reussir.rc<i64>) -> ()
      return
    ^done:
      reussir.rc.dec(%x : !reussir.rc<i64>)
      return
    }
    func.func private @loop(%x: !reussir.rc<i64>, %n: index) {
      %c0 = arith.constant 0 : index
      %c1 = arith.constant 1 : index
      scf.for %i = %c0 to %n step %c1 {
        reussir.rc.inc(%x : !reussir.rc<i64>)
        func.call @read(%x) : (!reussir.rc<i64>) -> ()
        %v = arith.constant 7 : i64
        %y = reussir.rc.create value(%v : i64) : !reussir.rc<i64>
        reussir.rc.inc(%y : !reussir.rc<i64>)
        func.call @read(%y) : (!reussir.rc<i64>) -> ()
      }
      reussir.rc.dec(%x : !reussir.rc<i64>)
      return
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  llvm::StringMap<llvm::SmallVector<const BorrowAnalysis::CallSite *>> sites;
  for (const auto &site : analysis.getCallSites())
    sites[site.call->getParentOfType<mlir::func::FuncOp>().getSymName()]
        .push_back(&site);
  ASSERT_EQ(sites["blocks"].size(), 2u);
  ASSERT_EQ(sites["loop"].size(), 2u);
  EXPECT_TRUE(sites["blocks"][0]->liveThrough.test(0));
  EXPECT_TRUE(sites["blocks"][1]->lastUse.test(0));
  EXPECT_TRUE(sites["loop"][0]->liveThrough.test(0));
  EXPECT_TRUE(sites["loop"][1]->lastUse.test(0));
}

// A nested module is a separate symbol table: its calls are not resolved
// against the outer one.
TEST_F(BorrowAnalysisTest, NestedModulesAreLeftAlone) {
  auto module = parse(R"(
    func.func private @read(%x: !reussir.rc<i64>) {
      reussir.rc.dec(%x : !reussir.rc<i64>)
      return
    }
    module @inner {
      func.func private @read(%x: !reussir.rc<i64>)
      func.func private @use(%x: !reussir.rc<i64>) {
        reussir.rc.inc(%x : !reussir.rc<i64>)
        func.call @read(%x) : (!reussir.rc<i64>) -> ()
        reussir.rc.dec(%x : !reussir.rc<i64>)
        return
      }
    }
  )");
  ASSERT_TRUE(module);
  BorrowAnalysis analysis(*module);
  EXPECT_TRUE(analysis.isBorrowable(lookup(*module, "read"), 0));
  EXPECT_TRUE(analysis.getCallSites().empty());
}
} // namespace
} // namespace reussir
