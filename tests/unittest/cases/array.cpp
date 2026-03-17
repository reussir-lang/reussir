#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, ParseArrayTypeTest) {
  withType<reussir::ArrayType>(
      SIMPLE_LAYOUT, R"(!reussir.array<4 x i32>)",
      [](mlir::ModuleOp module, reussir::ArrayType type) {
        mlir::DataLayout dataLayout(module);
        EXPECT_EQ(type.getExtent(), 4u);
        EXPECT_EQ(type.getElementType(), mlir::IntegerType::get(type.getContext(), 32));
        EXPECT_EQ(dataLayout.getTypeSize(type).getFixedValue(), 16u);
        EXPECT_EQ(dataLayout.getTypeABIAlignment(type), 4u);
      });
}

TEST_F(ReussirTest, ArrayScannerNestedRecordTest) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "WithArray" [regional] {
        !reussir.array<2 x !reussir.record<compound "Cell" [value] {[field] i64}>>
      }>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, {});
        llvm::SmallVector<int32_t> expected = {
            scanner::field(), scanner::advance(8), scanner::field(),
            scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}
} // namespace reussir
