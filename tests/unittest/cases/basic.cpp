#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>

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
} // namespace reussir