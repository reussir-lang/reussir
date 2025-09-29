#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>
#include <llvm/Support/raw_ostream.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>

import reussir.test.value;

namespace reussir {
TEST_F(ReussirValueTransformTest, RcAcquisition) {
  testValueAcquisition("!reussir.rc<i32>", [](mlir::func::FuncOp funcOp) {
    auto &inc = funcOp.getFunctionBody().front().front();
    EXPECT_TRUE(llvm::isa<ReussirRcIncOp>(inc));
  });
}

TEST_F(ReussirValueTransformTest, RefToRcAcquisition) {
  testValueAcquisition("!reussir.ref<!reussir.rc<i32>>",
                       [](mlir::func::FuncOp funcOp) {
                         auto &block = funcOp.getFunctionBody().front();
                         auto &loadOp = block.front();
                         auto &incOp = *std::next(block.begin());

                         EXPECT_TRUE(llvm::isa<ReussirRefLoadOp>(loadOp));
                         EXPECT_TRUE(llvm::isa<ReussirRcIncOp>(incOp));
                       });
}

TEST_F(ReussirValueTransformTest, RefToCompoundRecordAcquisition) {
  testValueAcquisition(
      "!reussir.ref<!reussir.record<compound \"Point\" {i32, [shared] i64}>>",
      [](mlir::func::FuncOp funcOp) {
        auto &block = funcOp.getFunctionBody().front();

        // Count operations
        size_t opCount = 0;
        bool foundProjection = false;
        for (auto &op : block.getOperations()) {
          opCount++;
          if (llvm::isa<ReussirRefProjectOp>(op)) {
            foundProjection = true;
          }
        }

        // Should have projection operations for each field
        EXPECT_GE(opCount, 2);
        EXPECT_TRUE(foundProjection);
      });
}

TEST_F(ReussirValueTransformTest, RefToVariantRecordAcquisition) {
  testValueAcquisition(
      "!reussir.ref<!reussir.record<variant \"Option\" "
      "{!reussir.record<compound \"Option::None\" {}>, "
      "!reussir.record<compound \"Option::Some\" {[shared] i32}>}>>",
      [](mlir::func::FuncOp funcOp) {
        auto &block = funcOp.getFunctionBody().front();
        auto &ops = block.getOperations();

        // Should have at least one operation (the dispatch)
        EXPECT_GE(ops.size(), 1);

        // Check that we have a dispatch operation
        bool foundDispatch = false;
        for (auto &op : ops) {
          if (llvm::isa<ReussirRecordDispatchOp>(op)) {
            foundDispatch = true;
            break;
          }
        }
        EXPECT_TRUE(foundDispatch);
      });
}
} // namespace reussir
