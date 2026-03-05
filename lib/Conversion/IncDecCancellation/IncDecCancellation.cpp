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
#include <mlir/IR/Value.h>
#include <mlir/IR/Visitors.h>
#include <mlir/Interfaces/CallInterfaces.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Interfaces/LoopLikeInterface.h>
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

enum class DecSearchState {
  Found,
  CleanNoDec,
  Blocked,
};

struct DecSearchResult {
  DecSearchState state;
  llvm::SmallVector<ReussirRcDecOp> decOps;
};

DecSearchResult findGuaranteedAliasedRcDecsInRegion(
    mlir::TypedValue<RcType> rcPtr, mlir::AliasAnalysis &aliasAnalysis,
    mlir::Region &region);

bool isCancellationBarrier(mlir::Operation *op) {
  return llvm::isa<mlir::CallableOpInterface>(op) ||
         llvm::isa<ReussirClosureApplyOp>(op) ||
         llvm::isa<ReussirClosureEvalOp>(op) ||
         llvm::isa<mlir::LoopLikeOpInterface>(op);
}

DecSearchResult findGuaranteedAliasedRcDecsInBlock(
    mlir::TypedValue<RcType> rcPtr, mlir::AliasAnalysis &aliasAnalysis,
    mlir::Block &block) {
  for (mlir::Operation &op : block) {
    if (auto decOp = llvm::dyn_cast<ReussirRcDecOp>(&op)) {
      if (aliasAnalysis.alias(decOp.getRcPtr(), rcPtr) ==
          mlir::AliasResult::MustAlias)
        return {DecSearchState::Found, {decOp}};
      return {DecSearchState::Blocked, {}};
    }

    if (auto ifOp = llvm::dyn_cast<mlir::scf::IfOp>(&op)) {
      auto thenResult = findGuaranteedAliasedRcDecsInRegion(
          rcPtr, aliasAnalysis, ifOp.getThenRegion());
      auto elseResult = findGuaranteedAliasedRcDecsInRegion(
          rcPtr, aliasAnalysis, ifOp.getElseRegion());
      if (thenResult.state == DecSearchState::Found &&
          elseResult.state == DecSearchState::Found) {
        llvm::SmallVector<ReussirRcDecOp> decOps;
        decOps.append(thenResult.decOps.begin(), thenResult.decOps.end());
        decOps.append(elseResult.decOps.begin(), elseResult.decOps.end());
        return {DecSearchState::Found, std::move(decOps)};
      }
      if (thenResult.state == DecSearchState::CleanNoDec &&
          elseResult.state == DecSearchState::CleanNoDec)
        continue;
      return {DecSearchState::Blocked, {}};
    }

    if (isCancellationBarrier(&op) ||
        llvm::isa<mlir::RegionBranchOpInterface>(&op) ||
        op.getNumRegions() != 0) {
      return {DecSearchState::Blocked, {}};
    }
  }

  return {DecSearchState::CleanNoDec, {}};
}

DecSearchResult findGuaranteedAliasedRcDecsInRegion(
    mlir::TypedValue<RcType> rcPtr, mlir::AliasAnalysis &aliasAnalysis,
    mlir::Region &region) {
  if (region.empty())
    return {DecSearchState::CleanNoDec, {}};
  if (!region.hasOneBlock())
    return {DecSearchState::Blocked, {}};
  return findGuaranteedAliasedRcDecsInBlock(rcPtr, aliasAnalysis,
                                            region.front());
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
        if (!ifOp->hasAttr(kExpandedDecrementAttr) || !ifOp.elseBlock())
          break;

        DecSearchResult thenResult = findGuaranteedAliasedRcDecsInRegion(
            op.getRcPtr(), aliasAnalysis, ifOp.getThenRegion());
        if (thenResult.state == DecSearchState::Found) {
          op->moveBefore(ifOp.elseYield());
          for (ReussirRcDecOp decOp : thenResult.decOps)
            eraseOrReplaceDecOp(decOp);
          break;
        }
        if (thenResult.state == DecSearchState::Blocked)
          break;
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
      if (isCancellationBarrier(next) ||
          llvm::isa<mlir::RegionBranchOpInterface>(next) ||
          next->getNumRegions() != 0)
        break;
      next = next->getNextNode();
    }
  }
  return mlir::success();
}

} // namespace reussir
