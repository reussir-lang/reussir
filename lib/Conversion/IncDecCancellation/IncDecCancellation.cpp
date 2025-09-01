#include "Reussir/Conversion/IncDecCancellation.h"
#include "Reussir/Analysis/AliasAnalysis.h"
#include "Reussir/Analysis/CancellationAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include <mlir/IR/Dominance.h>

#if LLVM_VERSION_MAJOR >= 22
#include <mlir/Analysis/DataFlow/Utils.h>
#else
#include <mlir/Analysis/DataFlow/ConstantPropagationAnalysis.h>
#include <mlir/Analysis/DataFlow/DeadCodeAnalysis.h>
#endif

#include <mlir/Pass/Pass.h>

// Polyfill for loadBaselineAnalyses
#if LLVM_VERSION_MAJOR < 22
namespace mlir {
static void loadBaselineAnalyses(DataFlowSolver &solver) {
  solver.load<dataflow::DeadCodeAnalysis>();
  solver.load<dataflow::SparseConstantPropagation>();
}
} // namespace mlir
#endif

namespace reussir {

#define GEN_PASS_DEF_REUSSIRINCDECCANCELLATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// IncDecCancellationPass
//===----------------------------------------------------------------------===//

namespace {
struct IncDecCancellationPass
    : public impl::ReussirIncDecCancellationPassBase<IncDecCancellationPass> {
  using Base::Base;
  void runOnOperation() override {
    if (failed(runIncDecCancellation(getOperation())))
      signalPassFailure();
  }
};
} // namespace

llvm::LogicalResult runIncDecCancellation(mlir::func::FuncOp func) {
  // First load alias analysis
  mlir::AliasAnalysis aliasAnalysis(func);
  mlir::DominanceInfo dominanceInfo(func);
  registerAliasAnalysisImplementations(aliasAnalysis);
  mlir::DataFlowConfig config;
  config.setInterprocedural(false);
  mlir::DataFlowSolver solver(config);
  mlir::loadBaselineAnalyses(solver);
  solver.load<CancellationAnalysis>(aliasAnalysis);
  if (failed(solver.initializeAndRun(func)))
    return mlir::failure();
  return mlir::success();
}

} // namespace reussir
