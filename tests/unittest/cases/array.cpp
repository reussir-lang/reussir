#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>

import reussir.test;
import reussir.test.value;

namespace reussir {
TEST_F(ReussirTest, ParseArrayTypeTest) {
  withType<reussir::ArrayType>(
      SIMPLE_LAYOUT, R"(!reussir.array<4 x 8 x !reussir.rc<i64>>)",
      [](mlir::ModuleOp module, reussir::ArrayType type) {
        EXPECT_EQ(type.getShape().size(), 2u);
        EXPECT_EQ(type.getShape()[0], 4);
        EXPECT_EQ(type.getShape()[1], 8);
        EXPECT_TRUE(llvm::isa<RcType>(type.getElementType()));
      });
}

TEST_F(ReussirTest, ViewTypeElementTypeTest) {
  auto i8Type = mlir::IntegerType::get(context.get(), 8);
  auto arrayType = reussir::ArrayType::get(context.get(), {4, 8}, i8Type);
  auto viewType =
      reussir::ViewType::get(context.get(), /*isMutable=*/true, arrayType);

  EXPECT_EQ(viewType.getArrayType(), arrayType);
  EXPECT_EQ(viewType.getElementType(), i8Type);
}

TEST_F(ReussirValueTransformTest, RefToArrayOfRcAcquisition) {
  testValueAcquisition(
      "!reussir.ref<!reussir.array<2 x !reussir.rc<i32>>>",
      [](mlir::func::FuncOp funcOp) {
        size_t projectCount = 0;
        size_t incCount = 0;
        bool foundView = false;
        for (auto &op : funcOp.getFunctionBody().front()) {
          if (llvm::isa<ReussirArrayViewOp>(op))
            foundView = true;
          if (llvm::isa<ReussirArrayProjectOp>(op))
            ++projectCount;
          if (llvm::isa<ReussirRcIncOp>(op))
            ++incCount;
        }
        EXPECT_TRUE(foundView);
        EXPECT_EQ(projectCount, 2u);
        EXPECT_EQ(incCount, 2u);
      });
}

TEST_F(ReussirValueTransformTest, RefToNestedArrayOfRcAcquisition) {
  testValueAcquisition(
      "!reussir.ref<!reussir.array<2 x 2 x !reussir.rc<i32>>>",
      [](mlir::func::FuncOp funcOp) {
        size_t projectCount = 0;
        size_t incCount = 0;
        bool foundView = false;
        for (auto &op : funcOp.getFunctionBody().front()) {
          if (llvm::isa<ReussirArrayViewOp>(op))
            foundView = true;
          if (llvm::isa<ReussirArrayProjectOp>(op))
            ++projectCount;
          if (llvm::isa<ReussirRcIncOp>(op))
            ++incCount;
        }
        EXPECT_TRUE(foundView);
        EXPECT_EQ(projectCount, 6u);
        EXPECT_EQ(incCount, 4u);
      });
}
} // namespace reussir
