// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#ifdef REUSSIR_ENABLE_OPENXLA
#include "Reussir/Analysis/PjrtDonationAnalysis.h"
#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Transformation/Passes.h"
#include <gtest/gtest.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>
import reussir.test;
void registerIfrtDialectsAndPasses(mlir::DialectRegistry &registry);
namespace reussir {
namespace {
std::string program(unsigned calls) {
  std::string source = R"mlir(
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
)mlir";
  for (unsigned i = 0; i < calls; ++i) {
    source += "func.func @host" + std::to_string(i) +
              R"mlir((%a: !R) -> !R attributes {ifrt.function} {
  %v = reussir.array.to_ifrt %a : !R -> !I
  %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
  %r = reussir.array.from_ifrt %o : !I -> !R
  reussir.rc.dec(%a : !R)
  return %r : !R
}
)mlir";
  }
  source += R"mlir(
module @kernel attributes {sym_visibility = "private"} {
  func.func @main(%x: tensor<4xf32>) -> tensor<4xf32> {
    %one = stablehlo.constant dense<1.0> : tensor<4xf32>
    %r = stablehlo.add %x, %one : tensor<4xf32>
    return %r : tensor<4xf32>
  }
}
})mlir";
  return source;
}
void load(mlir::MLIRContext &context) {
  mlir::DialectRegistry registry;
  registerIfrtDialectsAndPasses(registry);
  context.appendDialectRegistry(registry);
  context.loadAllAvailableDialects();
}
mlir::LogicalResult prepare(mlir::ModuleOp module) {
  mlir::PassManager pm(module.getContext());
  pm.addPass(createReussirIFRTJustInTimeTransformPass());
  pm.addPass(mlir::createSymbolDCEPass());
  return pm.run(module);
}
} // namespace

TEST_F(ReussirTest, PjrtDonationAnalysisWorkIsBounded) {
  load(*context);
  auto module = parse(program(1000));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  PjrtDonationAnalysis analysis(*module, 64);
  module->walk([&](ReussirPJRTJitCallOp call) {
    auto view = call.getInputs()[0].getDefiningOp<ReussirArrayToIfrtOp>();
    EXPECT_EQ(analysis.analyze(view.getArray(), call),
              PjrtDonationAnalysis::Decision::Forbidden);
  });
  EXPECT_TRUE(analysis.exhausted());
  EXPECT_LE(analysis.getWork(), 64u);
}


} // namespace reussir
#endif
