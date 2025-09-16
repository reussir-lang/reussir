#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, SimpleRecordScanner) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "Test" {i32, i64, [field] f128}>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, std::nullopt);
        llvm::SmallVector<int32_t> expected = {
            scanner::advance(16), scanner::field(), scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}

TEST_F(ReussirTest, NestedRecordScanner) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "Test" {
        i32, 
        i64, 
        [value] !reussir.record<compound "Nested" {
          i8, 
          [field] i16
        }>,
        f128,
        [field] i32
      }>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, std::nullopt);
        llvm::SmallVector<int32_t> expected = {
            scanner::advance(16), scanner::advance(8),  scanner::field(),
            scanner::advance(8),  scanner::advance(16), scanner::field(),
            scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}
} // namespace reussir
