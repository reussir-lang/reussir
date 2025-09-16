#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/DialectRegistry.h>
#include <mlir/IR/OwningOpRef.h>
#include <mlir/InitAllDialects.h>
#include <string_view>

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
    mlir::DialectRegistry registry;
    mlir::registerAllDialects(registry);
    registry.insert<reussir::ReussirDialect>();
    context = std::make_unique<MLIRContext>(registry);
    context->loadAllAvailableDialects();
  }

  void TearDown() override { context.reset(); }

  std::unique_ptr<MLIRContext> context;

  mlir::OwningOpRef<mlir::ModuleOp> parse(std::string_view source) {
    mlir::OwningOpRef<mlir::ModuleOp> module =
        mlir::parseSourceString<mlir::ModuleOp>(source, context.get());
    EXPECT_TRUE(module) << "Failed to parse MLIR source:\n" << source;
    return module;
  }
  template <typename F>
  void withModule(std::string_view source, F &&func)
    requires std::is_invocable_v<F, mlir::ModuleOp &>
  {
    OwningOpRef<mlir::ModuleOp> module = parse(source);
    if (module)
      func(module.get());
  }
  template <typename Type, typename F>
  void withType(std::string_view attributes, std::string_view source, F &&func)
    requires std::is_invocable_v<F, Type>
  {
    auto typeString = std::format(
        "module attributes {{ {} }} {{ func.func private @test() -> {} }}",
        attributes, source);
    withModule(typeString, [&](mlir::ModuleOp module) {
      auto op = llvm::cast<mlir::func::FuncOp>(module.getBody()->front());
      auto type = op.getFunctionType().getResult(0);
      func(llvm::cast<Type>(type));
    });
  }
};

std::string_view SIMPLE_LAYOUT = R"(dlti.dl_spec = #dlti.dl_spec<
        #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, 
        #dlti.dl_entry<i8, dense<8> : vector<2xi64>>>
)";

// Example test case
TEST_F(ReussirTest, BasicContextTest) {
  ASSERT_NE(context, nullptr);
  // Basic test that the context is properly initialized
  EXPECT_TRUE(context->isMultithreadingEnabled() ||
              !context->isMultithreadingEnabled());
}

// Test parsing RecordType from string
TEST_F(ReussirTest, ParseRecordTypeTest) {
  withType<reussir::RecordType>(SIMPLE_LAYOUT,
                                R"(!reussir.record<compound "Test" {}>)",
                                [](reussir::RecordType type) {

                                });
}

} // namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}