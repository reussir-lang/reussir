//===-- RcCreateFusion.cpp - Reussir rc.create fusion -----------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRRCCREATEFUSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {

void eraseDeadRecordMaterialization(mlir::PatternRewriter &rewriter,
                                    mlir::Operation *op) {
  while (op && op->use_empty()) {
    mlir::Operation *next = nullptr;
    if (auto variant = llvm::dyn_cast<ReussirRecordVariantOp>(op))
      next = variant.getValue().getDefiningOp();
    rewriter.eraseOp(op);
    op = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(next) ? next
                                                                  : nullptr;
  }
}

struct FuseRcCreatePattern : public mlir::OpRewritePattern<ReussirRcCreateOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcCreateOp create,
                  mlir::PatternRewriter &rewriter) const override {
    auto variant =
        llvm::dyn_cast_if_present<ReussirRecordVariantOp>(create.getValue().getDefiningOp());
    if (variant) {
      auto compound = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(
          variant.getValue().getDefiningOp());
      auto fused = rewriter.create<ReussirRcCreateVariantOp>(
          create.getLoc(), create.getRcPtr().getType(), variant.getTagAttr(),
          compound ? mlir::Value{} : variant.getValue(),
          compound ? compound.getFields() : mlir::ValueRange{}, create.getToken(),
          create.getRegion(), create.getVtableAttr(), create.getSkipRcAttr());
      rewriter.replaceOp(create, fused.getRcPtr());
      eraseDeadRecordMaterialization(rewriter, variant.getOperation());
      return mlir::success();
    }

    auto compound =
        llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(create.getValue().getDefiningOp());
    if (!compound)
      return mlir::failure();

    auto fused = rewriter.create<ReussirRcCreateCompoundOp>(
        create.getLoc(), create.getRcPtr().getType(), compound.getFields(),
        create.getToken(), create.getRegion(), create.getVtableAttr(),
        create.getSkipRcAttr());
    rewriter.replaceOp(create, fused.getRcPtr());
    eraseDeadRecordMaterialization(rewriter, compound.getOperation());
    return mlir::success();
  }
};

struct RcCreateFusionPass
    : public impl::ReussirRcCreateFusionPassBase<RcCreateFusionPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::RewritePatternSet patterns(&getContext());
    patterns.add<FuseRcCreatePattern>(&getContext());
    if (mlir::failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace
} // namespace reussir
