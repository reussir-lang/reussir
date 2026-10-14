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
std::string program(unsigned calls, llvm::StringRef tensorType = "4xf32",
                    llvm::StringRef arrayType = "4 x f32") {
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
    %r = stablehlo.add %x, %x : tensor<4xf32>
    return %r : tensor<4xf32>
  }
}
})mlir";
  for (auto [before, after] :
       {std::pair{llvm::StringRef("4xf32"), tensorType},
        std::pair{llvm::StringRef("4 x f32"), arrayType}}) {
    size_t position = 0;
    while ((position = source.find(before.str(), position)) != std::string::npos) {
      source.replace(position, before.size(), after.str());
      position += after.size();
    }
  }
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
mlir::LogicalResult donate(mlir::ModuleOp module,
                           ReussirPjrtDonationPassOptions options = {}) {
  mlir::PassManager pm(module.getContext());
  pm.addPass(createReussirPjrtDonationPass(options));
  return pm.run(module);
}
} // namespace

TEST_F(ReussirTest, PjrtDonationSharedKernelAndIdempotence) {
  load(*context);
  auto module = parse(program(100));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 2u);
  unsigned calls = 0;
  module->walk([&](ReussirPJRTJitCallOp call) {
    ++calls;
    EXPECT_EQ(call.getDonationGuards().size(), 1u);
    EXPECT_EQ(call.getDonationVariants().size(), 1u);
  });
  EXPECT_EQ(calls, 100u);
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 2u);
}

TEST_F(ReussirTest, PjrtDonationSerializedContractAndChecksum) {
  load(*context);
  auto module = parse(program(1));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  unsigned donors = 0, baseline = 0;
  for (auto code : module->getOps<ReussirIFRTBytecodeOp>()) {
    EXPECT_TRUE(mlir::succeeded(code.verify()));
    auto kernel = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                          context.get());
    ASSERT_TRUE(kernel);
    auto entry = kernel->lookupSymbol<mlir::func::FuncOp>("main");
    if (entry.getArgAttr(0, "jax.buffer_donor"))
      ++donors;
    else
      ++baseline;
  }
  EXPECT_EQ(donors, 1u);
  EXPECT_EQ(baseline, 1u);
}

TEST_F(ReussirTest, PjrtDonationUnknownSizeIsAnOptionalHint) {
  load(*context);
  auto module = parse(program(1, "?xf32", "? x f32"));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  unsigned hinted = 0;
  module->walk([&](ReussirPJRTJitCallOp call) {
    EXPECT_EQ(call.getDonationGuards().size(), 1u);
    EXPECT_EQ(call.getDonationVariants().size(), 1u);
    EXPECT_TRUE(call.getIoAliases().empty());
  });
  for (auto code : module->getOps<ReussirIFRTBytecodeOp>()) {
    auto kernel = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                        context.get());
    ASSERT_TRUE(kernel);
    auto entry = kernel->lookupSymbol<mlir::func::FuncOp>("main");
    hinted += bool(entry.getArgAttr(0, "jax.buffer_donor"));
    EXPECT_FALSE(entry.getArgAttr(0, "tf.aliasing_output"));
  }
  EXPECT_EQ(hinted, 1u);
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 2u);
}

TEST_F(ReussirTest, PjrtDonationComplexElementSizes) {
  load(*context);
  auto module = parse(program(1, "4xcomplex<f32>", "4 x complex<f32>"));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  module->walk([&](ReussirPJRTJitCallOp call) {
    EXPECT_EQ(call.getDonationVariants().size(), 1u);
  });
}

TEST_F(ReussirTest, PjrtDonationEmptyTensorsHaveNoBenefit) {
  load(*context);
  auto module = parse(program(1, "0xf32", "0 x f32"));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  module->walk([&](ReussirPJRTJitCallOp call) {
    EXPECT_TRUE(call.getDonationGuards().empty());
    EXPECT_TRUE(call.getDonationVariants().empty());
  });
}

TEST_F(ReussirTest, PjrtDonationBudgetsFallBackWithoutGuards) {
  load(*context);
  for (unsigned kind = 0; kind < 3; ++kind) {
    auto module = parse(program(2));
    ASSERT_TRUE(module);
    ASSERT_TRUE(mlir::succeeded(prepare(*module)));
    ReussirPjrtDonationPassOptions options;
    if (kind == 0)
      options.maxExtraBytes = 0;
    if (kind == 1)
      options.maxVariants = 1;
    if (kind == 2)
      options.maxAnalysisWork = 1;
    ASSERT_TRUE(mlir::succeeded(donate(*module, options)));
    EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 1u);
    module->walk([](ReussirPJRTJitCallOp call) {
      EXPECT_TRUE(call.getDonationVariants().empty());
      EXPECT_TRUE(call.getDonationGuards().empty());
    });
  }
}

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

TEST_F(ReussirTest, PjrtDonationSolverBudgetDiscardsPartialFacts) {
  load(*context);
  auto source = program(1);
  auto view = source.find("%v = reussir.array.to_ifrt %a");
  ASSERT_NE(view, std::string::npos);
  source.replace(view, std::string("%v = reussir.array.to_ifrt %a").size(),
                 R"mlir(%condition = reussir.rc.is_unique(%a : !R) : i1
  %forwarded = scf.if %condition -> !R {
    scf.yield %a : !R
  } else {
    scf.yield %a : !R
  }
  %v = reussir.array.to_ifrt %forwarded)mlir");
  source.replace(source.find("reussir.rc.dec(%a"),
                 std::string("reussir.rc.dec(%a").size(),
                 "reussir.rc.dec(%forwarded");
  auto module = parse(source);
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  PjrtDonationAnalysis complete(*module);
  ASSERT_FALSE(complete.exhausted());
  PjrtDonationAnalysis partial(*module, complete.getWork() + 1);
  ASSERT_FALSE(partial.exhausted());
  module->walk([&](ReussirPJRTJitCallOp call) {
    auto view = call.getInputs()[0].getDefiningOp<ReussirArrayToIfrtOp>();
    EXPECT_EQ(complete.analyze(view.getArray(), call),
              PjrtDonationAnalysis::Decision::Guarded);
    EXPECT_EQ(partial.analyze(view.getArray(), call),
              PjrtDonationAnalysis::Decision::Forbidden);
  });
  EXPECT_TRUE(partial.exhausted());
}

TEST_F(ReussirTest, PjrtDonationRejectsInvalidContract) {
  load(*context);
  auto module = parse(program(1));
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(prepare(*module)));
  ASSERT_TRUE(mlir::succeeded(donate(*module)));
  module->walk([&](ReussirPJRTJitCallOp call) {
    mlir::OpBuilder builder(context.get());
    auto original =
        llvm::cast<mlir::DictionaryAttr>(call.getDonationVariants()[0]);
    auto invalid = builder.getDictionaryAttr(
        {builder.getNamedAttr("callee", original.get("callee")),
         builder.getNamedAttr("arguments", builder.getDenseI32ArrayAttr({0})),
         builder.getNamedAttr("donors", builder.getDenseI32ArrayAttr({1}))});
    call.setDonationVariantsAttr(builder.getArrayAttr({invalid}));
    mlir::ScopedDiagnosticHandler suppress(
        context.get(), [](mlir::Diagnostic &) { return mlir::success(); });
    EXPECT_TRUE(mlir::failed(call.verify()));
  });
}
TEST_F(ReussirTest, PjrtDonationExplicitContractReachesBytecode) {
  load(*context);
  for (bool alias : {false, true}) {
    auto module = parse(program(1));
    ASSERT_TRUE(module);
    ASSERT_TRUE(mlir::succeeded(prepare(*module)));
    module->walk([&](ReussirPJRTJitCallOp call) {
      mlir::OpBuilder builder(context.get());
      call.getInputs()[0].getDefiningOp<ReussirArrayToIfrtOp>().setMayDonate(
          true);
      if (alias)
        call.setIoAliasesAttr(
            builder.getArrayAttr({builder.getDenseI32ArrayAttr({0, 0})}));
      else
        call.setDonatedInputIndicesAttr(builder.getDenseI32ArrayAttr({0}));
    });
    ASSERT_TRUE(mlir::succeeded(donate(*module)));
    module->walk([&](ReussirPJRTJitCallOp call) {
      auto code =
          module->lookupSymbol<ReussirIFRTBytecodeOp>(call.getCalleeAttr());
      ASSERT_TRUE(code);
      auto kernel = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                            context.get());
      ASSERT_TRUE(kernel);
      auto entry = kernel->lookupSymbol<mlir::func::FuncOp>("main");
      EXPECT_TRUE(entry.getArgAttr(0, alias ? "tf.aliasing_output"
                                            : "jax.buffer_donor"));
      EXPECT_TRUE(call.getDonationVariants().empty());
    });
    ASSERT_TRUE(mlir::succeeded(donate(*module)));
    EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 2u);
  }
}

} // namespace reussir
#endif
