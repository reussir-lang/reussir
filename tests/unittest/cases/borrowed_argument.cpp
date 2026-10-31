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
#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Diagnostics.h>
#include <mlir/Parser/Parser.h>

namespace reussir {
namespace {
class BorrowedArgumentTest : public testing::Test {
protected:
  mlir::MLIRContext context;
  BorrowedArgumentTest() {
    context.loadDialect<ReussirDialect, mlir::func::FuncDialect,
                        mlir::arith::ArithDialect, mlir::scf::SCFDialect>();
  }
  mlir::OwningOpRef<mlir::ModuleOp> parse(llvm::StringRef source) {
    return mlir::parseSourceString<mlir::ModuleOp>(source, &context);
  }
};

// Only an entry-block argument of the function that carries the attribute
// is borrowed; the next parameter and a region argument inside the body are
// not.
TEST_F(BorrowedArgumentTest, OnlyAttributedParameters) {
  auto module = parse(R"(
    func.func private @f(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed},
                         %y: !reussir.rc<i64>, %c: i1) -> i64 {
      %r = scf.if %c -> (i64) {
        %bx = reussir.rc.borrow(%x : !reussir.rc<i64>) : !reussir.ref<i64 shared>
        %v = reussir.ref.load(%bx : !reussir.ref<i64 shared>) : i64
        scf.yield %v : i64
      } else {
        %z = arith.constant 0 : i64
        scf.yield %z : i64
      }
      reussir.rc.dec(%y : !reussir.rc<i64>)
      return %r : i64
    }
  )");
  ASSERT_TRUE(module);
  auto function = *module->getOps<mlir::func::FuncOp>().begin();
  EXPECT_TRUE(isBorrowedArgument(function.getArgument(0)));
  EXPECT_FALSE(isBorrowedArgument(function.getArgument(1)));
  EXPECT_FALSE(isBorrowedArgument(function.getArgument(2)));
  function.walk([](ReussirRcBorrowOp borrow) {
    EXPECT_FALSE(isBorrowedArgument(borrow.getResult()));
  });
}

// The attribute is rejected where external callers would pass the argument
// owned.
TEST_F(BorrowedArgumentTest, PublicFunctionIsRejected) {
  std::string message;
  mlir::ScopedDiagnosticHandler handler(&context, [&](mlir::Diagnostic &d) {
    message = d.str();
    return mlir::success();
  });
  auto module = parse(R"(
    func.func @f(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}) {
      return
    }
  )");
  EXPECT_FALSE(module);
  EXPECT_NE(message.find("requires a non-public function"), std::string::npos);
}
} // namespace
} // namespace reussir
