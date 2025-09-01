#include "Reussir/Conversion/IncDecCancellation.h"
#include "Reussir/Analysis/AliasAnalysis.h"
#include "Reussir/Analysis/CancellationAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/SCF/IR/SCF.h>
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
  solver.load<CancellationAnalysis>(aliasAnalysis, func);
  if (failed(solver.initializeAndRun(func)))
    return mlir::failure();
  func->walk([&](ReussirRcDecOp op) {
    auto after =
        solver.lookupState<RcIncLattice>(solver.getProgramPointAfter(op));
    auto before =
        solver.lookupState<RcIncLattice>(solver.getProgramPointBefore(op));
    llvm::errs() << "RcIncLattice at dec:\n\t";
    llvm::errs() << "after: ";
    after->print(llvm::errs());
    llvm::errs() << "\n\t";
    llvm::errs() << "before: ";
    before->print(llvm::errs());
    llvm::errs() << "\n";
  });
  return mlir::success();
}

} // namespace reussir
