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
/// `reussir-borrow-analysis-remark` reports what `BorrowAnalysis` finds
/// through MLIR's remark framework, without changing the module.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/Transformation/Passes.h"

#include <mlir/IR/Remarks.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRBORROWANALYSISREMARKPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {
struct BorrowAnalysisRemarkPass
    : public impl::ReussirBorrowAnalysisRemarkPassBase<
          BorrowAnalysisRemarkPass> {
  using Base::Base;

  static mlir::remark::RemarkOpts remarkOpts(llvm::StringRef name,
                                             mlir::func::FuncOp function) {
    return mlir::remark::RemarkOpts::name(name)
        .category("Borrow")
        .subCategory("Analysis")
        .function(function.getSymName());
  }

  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    BorrowAnalysis analysis(module);
    for (auto function : module.getOps<mlir::func::FuncOp>())
      for (unsigned index : analysis.getBorrowable(function).set_bits())
        mlir::remark::analysis(function.getLoc(),
                               remarkOpts("BorrowableParameter", function))
            << mlir::remark::metric("Parameter", index);
    for (const BorrowAnalysis::CallSite &site : analysis.getCallSites()) {
      mlir::func::CallOp call = site.call;
      mlir::func::FuncOp callee = site.callee;
      auto caller = call->getParentOfType<mlir::func::FuncOp>();
      auto report = [&](llvm::StringRef name, unsigned index) {
        mlir::remark::analysis(call.getLoc(), remarkOpts(name, caller))
            << mlir::remark::metric("Callee", callee.getSymName())
            << mlir::remark::metric("Argument", index);
      };
      for (unsigned index : site.liveThrough.set_bits())
        report("LiveThrough", index);
      for (unsigned index : site.lastUse.set_bits())
        report("LastUse", index);
    }
  }
};
} // namespace
} // namespace reussir
