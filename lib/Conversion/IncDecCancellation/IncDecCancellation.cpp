#include "Reussir/Conversion/IncDecCancellation.h"
#include "Reussir/Analysis/AliasAnalysis.h"
#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"

#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Dominance.h>
#include <mlir/IR/Value.h>
#include <mlir/Interfaces/CallInterfaces.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Pass/Pass.h>

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

ReussirRcDecOp hasShallowAliasedRcDec(mlir::TypedValue<RcType> rcPtr,
                                      mlir::AliasAnalysis &aliasAnalysis,
                                      mlir::Block *block) {
  for (const auto &op : *block)
    if (auto rcDecOp = llvm::dyn_cast<ReussirRcDecOp>(&op))
      if (aliasAnalysis.alias(rcDecOp.getRcPtr(), rcPtr) ==
          mlir::AliasResult::MustAlias)
        return rcDecOp;

  return nullptr;
}

void eraseOrReplaceDecOp(ReussirRcDecOp decOp) {
  if (decOp.use_empty())
    decOp.erase();
  else {
    mlir::OpBuilder builder(decOp);
    auto replacement = builder.create<ReussirNullableCreateOp>(
        decOp.getLoc(), decOp.getResultTypes(), nullptr);
    decOp.replaceAllUsesWith(replacement->getResults());
    decOp.erase();
  }
}

} // namespace

llvm::LogicalResult runIncDecCancellation(mlir::func::FuncOp func) {
  mlir::AliasAnalysis aliasAnalysis(func);
  registerAliasAnalysisImplementations(aliasAnalysis);
  llvm::SmallVector<ReussirRcIncOp> incOps;
  func->walk([&](ReussirRcIncOp op) { incOps.push_back(op); });
  for (auto op : incOps) {
    // iterate all succeeding operations in the same block
    mlir::Operation *next = op->getNextNode();
    while (next) {
      if (auto ifOp = llvm::dyn_cast<mlir::scf::IfOp>(next)) {
        // This is a similar situation as other aborting cases.
        if (!ifOp->hasAttr(kExpandedDecrementAttr))
          break;
        ReussirRcDecOp decOp = hasShallowAliasedRcDec(
            op.getRcPtr(), aliasAnalysis, ifOp.thenBlock());
        if (decOp) {
          op->moveBefore(ifOp.elseYield());
          eraseOrReplaceDecOp(decOp);
          break;
        }
      }

      if (auto decOp = llvm::dyn_cast<ReussirRcDecOp>(next)) {
        if (aliasAnalysis.alias(op.getRcPtr(), decOp.getRcPtr()) ==
            mlir::AliasResult::MustAlias) {
          op.erase();
          eraseOrReplaceDecOp(decOp);
        }
        // avoid nested decrement operation that may mess up the cancellation
        // maybe this should be handled more gracefully to continue the
        // cancellation if the decrement is known to be disjoint from the
        // increment.
        break;
      }
      // If encounter any of the following, there is a chance that nested
      // operation may demand rc management. Hence, we abort the cancellation.
      if (llvm::isa<mlir::CallableOpInterface>(next) ||
          llvm::isa<mlir::RegionBranchOpInterface>(next))
        break;
      next = next->getNextNode();
    }
  }
  func->walk(
      [&](mlir::scf::IfOp op) { op->removeAttr(kExpandedDecrementAttr); });
  return mlir::success();
}

} // namespace reussir
