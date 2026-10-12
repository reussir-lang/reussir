//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements the pipeline's SCCP: MLIR's `sccp` on the whole
/// module while its call sites are within a budget, on each function alone
/// otherwise (see `reussir-sccp` in Passes.td).
///
//===----------------------------------------------------------------------===//

#include "Reussir/Transformation/Passes.h"

#include <llvm/ADT/DenseMap.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/CallInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRSCCPPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

struct ReussirSCCPPass : public impl::ReussirSCCPPassBase<ReussirSCCPPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    // The sum over callees of the square of their call sites, counted
    // incrementally: the n-th call site of a callee adds n^2 - (n-1)^2.
    // Only callees with a body count: the analysis treats a declaration as
    // external and never walks its call sites.
    mlir::SymbolTableCollection symbols;
    llvm::DenseMap<mlir::Operation *, uint64_t> callSites;
    uint64_t pairs = 0;
    auto walk = module.walk([&](mlir::CallOpInterface call) {
      auto callable = llvm::dyn_cast_if_present<mlir::CallableOpInterface>(
          call.resolveCallableInTable(&symbols));
      if (!callable || !callable.getCallableRegion() ||
          callable.getCallableRegion()->empty())
        return mlir::WalkResult::advance();
      uint64_t &count = callSites[callable.getOperation()];
      // Compare before adding, both to stop counting once the decision is
      // known and to avoid overflow even with the largest allowed budget.
      uint64_t remaining = maxCallSitePairs - pairs;
      if (remaining == 0 || count > (remaining - 1) / 2)
        return mlir::WalkResult::interrupt();
      pairs += 2 * count + 1;
      ++count;
      return mlir::WalkResult::advance();
    });

    mlir::OpPassManager pipeline(mlir::ModuleOp::getOperationName());
    if (!walk.wasInterrupted())
      pipeline.addPass(mlir::createSCCPPass());
    else
      pipeline.addNestedPass<mlir::func::FuncOp>(mlir::createSCCPPass());
    if (mlir::failed(runPipeline(pipeline, module)))
      signalPassFailure();
  }
};

} // namespace
} // namespace reussir
