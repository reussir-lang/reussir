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
/// This file implements the pass that makes the storage of scalable vectors
/// legal for SVE.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/Passes.h"

#include <mlir/Dialect/ArmSVE/IR/ArmSVEDialect.h>
#include <mlir/Dialect/ArmSVE/Transforms/Passes.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRLEGALIZEVECTORSTORAGEPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {
struct ReussirLegalizeVectorStoragePass
    : public impl::ReussirLegalizeVectorStoragePassBase<
          ReussirLegalizeVectorStoragePass> {
  void runOnOperation() override {
    mlir::RewritePatternSet patterns(&getContext());
    mlir::arm_sve::populateLegalizeVectorStoragePatterns(patterns);
    // The patterns alone reach a fixpoint; the driver's cleanup of the rest
    // of the function may not within its iteration limit.
    mlir::GreedyRewriteConfig config;
    config.enableFolding(false).enableConstantCSE(false);
    config.setRegionSimplificationLevel(
        mlir::GreedySimplifyRegionLevel::Disabled);
    if (mlir::failed(mlir::applyPatternsGreedily(getOperation(),
                                                 std::move(patterns), config)))
      return signalPassFailure();
    // The patterns give a legalized allocation its old type back through a
    // cast between memrefs, for the uses they then rewrite. One that is left
    // has a use they cannot rewrite, such as a call.
    getOperation().walk([&](mlir::UnrealizedConversionCastOp cast) {
      if (cast.getNumOperands() != 1 || cast.getNumResults() != 1 ||
          !llvm::isa<mlir::MemRefType>(cast.getOperand(0).getType()) ||
          !llvm::isa<mlir::MemRefType>(cast.getResult(0).getType()))
        return;
      cast.emitError("the storage of a scalable vector is used by an "
                     "operation that cannot be legalized");
      signalPassFailure();
    });
  }
};
} // namespace
} // namespace reussir
