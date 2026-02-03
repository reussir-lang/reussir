//===-- IncDecCancellation.cpp ----------------------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/IncDecCancellation.h"
#include "Reussir/Analysis/AliasAnalysis.h"
#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"

#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Dominance.h>
#include <mlir/IR/Value.h>
#include <mlir/IR/Visitors.h>
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

ReussirRcDecOp findPostDominantAliasedRcDec(
    mlir::TypedValue<RcType> rcPtr, mlir::AliasAnalysis &aliasAnalysis,
    mlir::PostDominanceInfo &postDominanceInfo, mlir::Region &dropRegion) {
  ReussirRcDecOp fusionTarget{};
  dropRegion.walk([&](ReussirRcDecOp decOp) {
    auto aliasResult = aliasAnalysis.alias(decOp.getRcPtr(), rcPtr);
    if (postDominanceInfo.postDominates(decOp->getBlock(),
                                        &dropRegion.front()) &&
        aliasResult == mlir::AliasResult::MustAlias) {
      fusionTarget = decOp;
      return mlir::WalkResult::interrupt();
    }
    return mlir::WalkResult::advance();
  });
  return fusionTarget;
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

llvm::LogicalResult runIncDecCancellation(reussir::ReussirFuncOp func) {
  mlir::AliasAnalysis aliasAnalysis(func);
  registerAliasAnalysisImplementations(aliasAnalysis);
  mlir::PostDominanceInfo postDominanceInfo(func);
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
        ReussirRcDecOp decOp = findPostDominantAliasedRcDec(
            op.getRcPtr(), aliasAnalysis, postDominanceInfo,
            ifOp.getThenRegion());
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
  return mlir::success();
}

} // namespace reussir
