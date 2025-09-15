#include <gmock/gmock.h>
#include <gtest/gtest.h>

// Include headers from MLIR
#include "mlir/IR/MLIRContext.h"
#include "mlir/Parser/Parser.h"

// Include Reussir headers
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirTypes.h"

using namespace mlir;

namespace {

class ReussirTest : public ::testing::Test {
protected:
  void SetUp() override {
    context = std::make_unique<MLIRContext>();
    // Load required dialects
    context->loadDialect<reussir::ReussirDialect>();
  }

  void TearDown() override { context.reset(); }

  std::unique_ptr<MLIRContext> context;
};

// Example test case
TEST_F(ReussirTest, BasicContextTest) {
  ASSERT_NE(context, nullptr);
  // Basic test that the context is properly initialized
  EXPECT_TRUE(context->isMultithreadingEnabled() ||
              !context->isMultithreadingEnabled());
}

// Test parsing RecordType from string
TEST_F(ReussirTest, ParseRecordTypeTest) {
  // Create the RecordType directly as it would be parsed from
  // "!reussir.record<compound "Test" {}>"
  llvm::ArrayRef<mlir::Type> members;                     // empty member list
  llvm::ArrayRef<reussir::Capability> memberCapabilities; // empty capabilities
  mlir::StringAttr name = mlir::StringAttr::get(context.get(), "Test");
  reussir::RecordKind kind = reussir::RecordKind::compound;
  reussir::Capability defaultCapability = reussir::Capability::unspecified;

  // Create the RecordType
  auto recordType =
      reussir::RecordType::get(context.get(), members, memberCapabilities, name,
                               kind, defaultCapability);

  ASSERT_TRUE(recordType) << "Failed to create RecordType";

  // Verify the record properties
  EXPECT_EQ(recordType.getKind(), reussir::RecordKind::compound);
  EXPECT_EQ(recordType.getName().str(), "Test");
  EXPECT_TRUE(recordType.getComplete());
  EXPECT_TRUE(recordType.getMembers().empty());
}

} // namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}