#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, SimpleRecordScanner) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "Test" [regional] {i32, i64, [field] f128}>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, {});
        llvm::SmallVector<int32_t> expected = {
            scanner::advance(8), scanner::field(), scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}

TEST_F(ReussirTest, NestedRecordScanner) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "Test" [regional] {
        i32, 
        i64, 
        !reussir.record<compound "Nested" [value] {
          i8, 
          !reussir.rc<i16 atomic>
        }>,
        f128,
        [field] i32
      }>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, {});
        llvm::SmallVector<int32_t> expected = {
            scanner::advance(40), scanner::field(), scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}

TEST_F(ReussirTest, VariantRecordScanner) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<variant "Test" [regional] {
        [field] i32, 
        i64, 
        !reussir.record<compound "Nested" [value]  {
          i8, 
          !reussir.rc<i16 atomic>
        }>,
        f128,
        !reussir.record<compound "Nested2" [value] {
          !reussir.rc<i16 atomic>,
          f128, 
          !reussir.rc<i16 atomic>
        }>
      }>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {
        llvm::SmallVector<int32_t> buffer;
        mlir::DataLayout dataLayout = mlir::DataLayout(module);
        type.emitScannerInstructions(buffer, dataLayout, {});
        llvm::SmallVector<int32_t> expected = {
            // Fused header: advance past the i32 count slot to the i32 tag,
            // then read the 4-byte tag.
            scanner::advance(4), scanner::variant(4),
            // Nested value records contain shared links, not regional fields.
            // Only the directly field-qualified first arm is scanned.
            scanner::skip(5), scanner::skip(8), scanner::skip(9),
            scanner::skip(10), scanner::skip(11),
            scanner::advance(12), scanner::field(), scanner::advance(32),
            scanner::skip(8),
            scanner::advance(44), scanner::skip(6),
            scanner::advance(44), scanner::skip(4),
            scanner::advance(44), scanner::skip(2),
            scanner::advance(44),
            scanner::end()};
        EXPECT_EQ(buffer, expected);
      });
}
} // namespace reussir
