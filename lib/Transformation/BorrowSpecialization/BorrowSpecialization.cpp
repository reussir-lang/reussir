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
/// Borrowed-parameter passes. `reussir-borrow-analysis` reports what
/// `BorrowAnalysis` finds without changing the module.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/Transformation/Passes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRBORROWANALYSISPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {
struct BorrowAnalysisPass
    : public impl::ReussirBorrowAnalysisPassBase<BorrowAnalysisPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    BorrowAnalysis analysis(module);
    for (auto function : module.getOps<mlir::func::FuncOp>())
      for (unsigned index : analysis.getBorrowable(function).set_bits())
        function.emitRemark() << "parameter #" << index << " is borrowable";
    for (const BorrowAnalysis::CallSite &site : analysis.getCallSites()) {
      mlir::Operation *call = site.call;
      for (unsigned index : site.callerLive.set_bits())
        call->emitRemark() << "argument #" << index
                           << " is caller-live: may be borrowed";
      for (unsigned index : site.lastUse.set_bits())
        call->emitRemark() << "argument #" << index
                           << " is at its last use: stays owned";
    }
  }
};
} // namespace

} // namespace reussir
