//===-- RcDecrementExpansion.cpp -------------------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <cassert>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Twine.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/LogicalResult.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/Conversion/AcquireDropExpansion.h"
#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRRCDECREMENTEXPANSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

struct RcDecrementExpansionPattern
    : public mlir::OpRewritePattern<ReussirRcDecOp> {
  using mlir::OpRewritePattern<ReussirRcDecOp>::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcDecOp op,
                  mlir::PatternRewriter &rewriter) const override {
    RcType type = op.getRcPtr().getType();
    // No need to proceed if dec operation is applied to a rigid type.
    // Also delay the FFI object type clean up until basic ops lowering pass.
    if (type.getCapability() == Capability::rigid ||
        mlir::isa<FFIObjectType, ClosureType>(type.getElementType()))
      return mlir::failure();

    auto prevRcCount =
        rewriter.create<ReussirRcFetchDecOp>(op.getLoc(), op.getRcPtr());
    auto isOne = rewriter.create<mlir::arith::CmpIOp>(
        op.getLoc(), mlir::arith::CmpIPredicate::eq, prevRcCount.getRefCount(),
        rewriter.create<mlir::arith::ConstantIndexOp>(op.getLoc(), 1));
    auto ifOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), isOne, true, true);
    RefType borrowedRefType = rewriter.getType<RefType>(
        type.getElementType(), Capability::unspecified, type.getAtomicKind());
    TokenType tokenType = llvm::cast<TokenType>(
        llvm::cast<NullableType>(op.getNullableToken().getType()).getPtrTy());
    {
      rewriter.setInsertionPointToStart(ifOp.thenBlock());
      mlir::Value ref = rewriter.create<ReussirRcBorrowOp>(
          op.getLoc(), borrowedRefType, op.getRcPtr());
      rewriter.create<ReussirRefDropOp>(op.getLoc(), ref);
      mlir::Value token = rewriter.create<ReussirRcReinterpretOp>(
          op.getLoc(), tokenType, op.getRcPtr());
      mlir::Value nonnull = rewriter.create<ReussirNullableCreateOp>(
          op.getLoc(), op.getNullableToken().getType(), token);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), nonnull);
    }
    {
      rewriter.setInsertionPointToStart(ifOp.elseBlock());
      auto null = rewriter.create<ReussirNullableCreateOp>(
          op.getLoc(), op.getNullableToken().getType(), nullptr);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), null->getResults());
    }
    ifOp->setAttr(kExpandedDecrementAttr, rewriter.getUnitAttr());
    rewriter.replaceOp(op, ifOp);
    return mlir::success();
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// RcDecrementExpansionPass
//===----------------------------------------------------------------------===//

namespace {
struct RcDecrementExpansionPass
    : public impl::ReussirRcDecrementExpansionPassBase<
          RcDecrementExpansionPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());
    populateRcDecrementExpansionConversionPatterns(patterns);
    if (failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateRcDecrementExpansionConversionPatterns(
    mlir::RewritePatternSet &patterns) {
  patterns.add<RcDecrementExpansionPattern>(patterns.getContext());
}

} // namespace reussir
