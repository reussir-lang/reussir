//===-- RegionPatterns.cpp - Reussir region-related patterns -*-C++-*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <llvm/Support/Casting.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/Conversion/RegionPatterns.h"
#include "Reussir/IR/ReussirDialect.h.inc"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRREGIONPATTERNSPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {
struct AttachVTablePattern : public mlir::OpRewritePattern<ReussirRcCreateOp> {
private:
  ReussirRegionVTableOp
  createVTableIfNotExists(mlir::ModuleOp moduleOp, RecordType type,
                          mlir::func::FuncOp dropOp,
                          mlir::OpBuilder &builder) const {
    mlir::SymbolTable symbolTable(moduleOp);
    llvm::Twine vtablePrefix = "core::region::vtable<";
    llvm::Twine vtableSuffix = ">";
    std::string vtableName =
        (vtablePrefix + type.getName().strref() + vtableSuffix).str();
    if (auto existingVTable = symbolTable.lookup<ReussirRegionVTableOp>(
            builder.getStringAttr(vtableName)))
      return existingVTable;
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(moduleOp.getBody());
    mlir::FlatSymbolRefAttr dropOpRef = mlir::SymbolRefAttr::get(dropOp);
    auto vtableOp = builder.create<ReussirRegionVTableOp>(
        moduleOp.getLoc(), vtableName, type, dropOpRef);
    return vtableOp;
  }

public:
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirRcCreateOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // Nothing to do if vtable is already attached or no region
    if (!op.needsVTable() || op.getVtable())
      return mlir::failure();
    mlir::Type elementType = op.getRcPtr().getType().getElementType();
    RecordType recordType = llvm::dyn_cast<RecordType>(elementType);
    auto moduleOp = op->getParentOfType<mlir::ModuleOp>();
    mlir::func::FuncOp dropFunc =
        createDtorIfNotExists(moduleOp, recordType, rewriter);
    ReussirRegionVTableOp vtableOp =
        createVTableIfNotExists(moduleOp, recordType, dropFunc, rewriter);
    mlir::FlatSymbolRefAttr vtableRef = mlir::SymbolRefAttr::get(vtableOp);
    op.setVtableAttr(vtableRef);
    return mlir::success();
  }
};

struct RegionInlinePattern : public mlir::OpRewritePattern<ReussirRegionRunOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRegionRunOp op,
                  mlir::PatternRewriter &rewriter) const override {
    auto region = rewriter.create<ReussirRegionCreateOp>(
        op.getLoc(), RegionType::get(op->getContext()));
    auto yieldOp =
        llvm::cast<ReussirRegionYieldOp>(op.getBody().front().getTerminator());
    rewriter.inlineBlockBefore(&op.getBody().front(), op, region->getResults());
    auto yieldValue = yieldOp.getValue();
    auto rcType =
        yieldValue ? llvm::dyn_cast<RcType>(yieldValue.getType()) : nullptr;

    // Remove the yield op
    rewriter.eraseOp(yieldOp);

    // Create the final value based on the yield value
    mlir::Value finalValue;
    if (rcType && rcType.getCapability() == Capability::flex) {
      auto freezeOp = rewriter.create<ReussirRcFreezeOp>(
          op.getLoc(), op->getResult(0).getType(), yieldValue);
      finalValue = freezeOp.getResult();
    } else {
      finalValue = yieldValue;
    }

    // Create cleanup operation
    rewriter.setInsertionPoint(op);
    rewriter.create<ReussirRegionCleanupOp>(op.getLoc(), region);

    // Replace the whole RegionRunOp with the final value
    if (finalValue)
      rewriter.replaceOp(op, finalValue);
    else
      rewriter.eraseOp(op);
    return mlir::success();
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// RegionPatternsPass
//===----------------------------------------------------------------------===//

namespace {
struct RegionPatternsPass
    : public impl::ReussirRegionPatternsPassBase<RegionPatternsPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::RewritePatternSet patterns(&getContext());
    populateRegionPatternsConversionPatterns(patterns);
    if (mlir::failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateRegionPatternsConversionPatterns(
    mlir::RewritePatternSet &patterns) {
  patterns.add<AttachVTablePattern, RegionInlinePattern>(patterns.getContext());
}

} // namespace reussir
