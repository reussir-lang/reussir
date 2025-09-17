module;

#include <gtest/gtest.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/DialectRegistry.h>
#include <mlir/IR/MLIRContext.h>
#include <mlir/IR/OwningOpRef.h>
#include <mlir/InitAllDialects.h>
#include <mlir/Parser/Parser.h>
#include <string_view>

// Include Reussir headers
#include "Reussir/IR/ReussirDialect.h"

export module reussir.test;

namespace reussir {

export class ReussirTest : public ::testing::Test {
protected:
  void SetUp() override {
    mlir::DialectRegistry registry;
    mlir::registerAllDialects(registry);
    registry.insert<reussir::ReussirDialect>();
    context = std::make_unique<mlir::MLIRContext>(registry);
    context->loadAllAvailableDialects();
  }

  void TearDown() override { context.reset(); }

  std::unique_ptr<mlir::MLIRContext> context;

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
    mlir::OwningOpRef<mlir::ModuleOp> module = parse(source);
    if (module)
      func(module.get());
  }
  template <typename Type, typename F>
  void withType(std::string_view attributes, std::string_view source, F &&func)
    requires std::is_invocable_v<F, mlir::ModuleOp, Type>
  {
    auto typeString = std::format(
        "module attributes {{ {} }} {{ func.func private @test() -> {} }}",
        attributes, source);
    withModule(typeString, [&](mlir::ModuleOp module) {
      auto op = llvm::cast<mlir::func::FuncOp>(module.getBody()->front());
      auto type = op.getFunctionType().getResult(0);
      func(module, llvm::cast<Type>(type));
    });
  }

  std::string_view SIMPLE_LAYOUT = R"(dlti.dl_spec = #dlti.dl_spec<
        #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, 
        #dlti.dl_entry<i8, dense<8> : vector<2xi64>>,
        #dlti.dl_entry<f128, dense<128> : vector<2xi64>>,
        #dlti.dl_entry<i32, dense<32> : vector<2xi64>>,
        #dlti.dl_entry<i16, dense<16> : vector<2xi64>>>
  )";
};
} // namespace reussir
