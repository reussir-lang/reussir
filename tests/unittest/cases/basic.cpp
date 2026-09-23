#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>
#include <mlir/IR/Verifier.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, BasicContextTest) {
  ASSERT_NE(context, nullptr);
  // Basic test that the context is properly initialized
  EXPECT_TRUE(context->isMultithreadingEnabled() ||
              !context->isMultithreadingEnabled());
}

// Test parsing RecordType from string
TEST_F(ReussirTest, ParseRecordTypeTest) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT, R"(!reussir.record<compound "Test" {}>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {

      });
}

TEST_F(ReussirTest, TokenAcceptorBuildsFixedSize) {
  withModule(R"(
module {
  func.func @create(%init: i32) -> !reussir.rc<i32> {
    %rc = reussir.rc.create value(%init : i32) : !reussir.rc<i32>
    return %rc : !reussir.rc<i32>
  }
}
)", [](mlir::ModuleOp module) {
    module.walk([](ReussirRcCreateOp op) {
      mlir::OpBuilder builder(op);
      auto acceptor = llvm::cast<TokenAcceptor>(op.getOperation());
      auto size = acceptor.buildTokenSize(builder);
      ASSERT_TRUE(size.getType().isIndex());
      auto constant = size.getDefiningOp<mlir::arith::ConstantIndexOp>();
      ASSERT_TRUE(constant);
      EXPECT_EQ(constant.value(), 8);
    });
    EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
  });
}
} // namespace reussir
