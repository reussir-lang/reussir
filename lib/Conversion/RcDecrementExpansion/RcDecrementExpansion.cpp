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
/// This file implements Reussir reference-count decrement expansion.
///
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
#include "Reussir/Transformation/SpecialPointerTag.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRRCDECREMENTEXPANSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

mlir::LogicalResult resolveDropOrder(mlir::ModuleOp module,
                                     llvm::StringRef configured) {
  auto saved = module->getAttrOfType<mlir::StringAttr>("reussir.drop_order");
  if (module->hasAttr("reussir.drop_order") && !saved)
    return module.emitError("reussir.drop_order must be a string");
  auto valid = [](llvm::StringRef value) {
    return value == "forward" || value == "backward";
  };
  if ((!configured.empty() && !valid(configured)) ||
      (saved && !valid(saved.getValue())))
    return module.emitError("drop-order must be forward or backward");
  if (saved && !configured.empty() && saved.getValue() != configured)
    return module.emitError("drop-order conflicts with saved module policy");
  if (!saved)
    module->setAttr(
        "reussir.drop_order",
        mlir::StringAttr::get(module.getContext(),
                              configured.empty() ? "forward" : configured));
  return mlir::success();
}

bool canScheduleDecrement(ReussirRcDecOp op) {
  if (op.isDestructuring() || op->hasAttr("reussir.worklist_raw") ||
      op.getRcPtr().getType().getCapability() != Capability::shared)
    return false;
  auto token = op.getNullableToken();
  return !token || llvm::all_of(token.getUsers(), [](mlir::Operation *user) {
    return llvm::isa<ReussirTokenFreeOp>(user);
  });
}

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

// Under the special-pointer-tag scheme a nullary constructor is an immediate
// pointing at a static per-tag dummy box. Under TBI, increments of an
// immediate land on the dummy's 32-bit count (the LLVM lowering keeps rc.inc
// unguarded), so after 2^32 references the count wraps and a decrement reads
// 1. The decrement must then not take its unique branch (the dummy is not
// heap memory): no release, no token. Whether the value must be tested in
// the branch condition: not for types without immediates, not under the
// immortal encoding (rc.inc steers its store away from the dummy, so the
// count never reaches 1), and not for a destructuring decrement of an arm
// with members.
bool needsImmediateGuard(ReussirRcDecOp op) {
  RcType type = op.getRcPtr().getType();
  auto module = op->getParentOfType<mlir::ModuleOp>();
  if (!module)
    return false;
  auto encoding = module->getAttrOfType<mlir::StringAttr>(kSpecialPtrTagAttr);
  if (!encoding || encoding.getValue() == kSpecialPtrTagImmortal ||
      !type.mayCarrySpecialPointerTag())
    return false;
  auto variant = llvm::cast<RecordType>(type.getElementType());
  if (op.isVariantDestructuring())
    return variant.isNullaryArm(
        op.getDestructureTagAttr().getValue().getZExtValue());
  for (size_t tag = 0, n = variant.getMembers().size(); tag < n; ++tag)
    if (variant.isNullaryArm(tag))
      return true;
  return false;
}

struct RcDecrementExpansionPattern
    : public mlir::OpRewritePattern<ReussirRcDecOp> {
  bool deferNormal;
  RcDecrementExpansionPattern(mlir::MLIRContext *context, bool deferNormal)
      : mlir::OpRewritePattern<ReussirRcDecOp>(context),
        deferNormal(deferNormal) {}

  mlir::LogicalResult
  matchAndRewrite(ReussirRcDecOp op,
                  mlir::PatternRewriter &rewriter) const override {
    if (deferNormal && canScheduleDecrement(op))
      return mlir::failure();
    RcType type = op.getRcPtr().getType();
    // No need to proceed if dec operation is applied to a rigid type.
    // Also delay the FFI object type clean up until basic ops lowering pass.
    if (type.getCapability() == Capability::rigid ||
        mlir::isa<FFIObjectType, ClosureType>(type.getElementType()))
      return mlir::failure();

    // An atomic box decrements with one acquire-release `rc.fetch_sub`
    // (returning the previous count): the subtraction is already published,
    // so unlike the plain-load scheme below there is nothing to store on the
    // shared path, and the final decrementer's acquire half orders every
    // other thread's uses before the destruction. Destructuring decrements
    // never target atomic boxes (rejected by the `rc.dec` verifier).
    const bool atomic = type.getAtomicKind() == AtomicKind::atomic;
    mlir::Value prevRcCount =
        atomic
            ? ReussirRcFetchSubOp::create(rewriter, op.getLoc(), op.getRcPtr())
                  .getRefCount()
            : ReussirRcFetchOp::create(rewriter, op.getLoc(), op.getRcPtr())
                  .getRefCount();
    auto isOne = mlir::arith::CmpIOp::create(
        rewriter, op.getLoc(), mlir::arith::CmpIPredicate::eq, prevRcCount,
        mlir::arith::ConstantIndexOp::create(rewriter, op.getLoc(), 1));
    // An immediate whose wrapped count reads 1 must take the shared branch:
    // it is not heap memory, so it is neither freed nor turned into a token
    // (the shared branch's rc.set skips immediates). The test joins the
    // condition, so the unique branch keeps its usual shape.
    mlir::Value unique = isOne.getResult();
    // One test for all nullary arms: rc.is_immediate lowers to a single
    // top-byte check under TBI.
    if (needsImmediateGuard(op)) {
      mlir::Value isImmediate = ReussirRcIsImmediateOp::create(
          rewriter, op.getLoc(), rewriter.getI1Type(), op.getRcPtr());
      mlir::Value notImmediate = mlir::arith::XOrIOp::create(
          rewriter, op.getLoc(), isImmediate,
          mlir::arith::ConstantIntOp::create(rewriter, op.getLoc(), 1, 1));
      unique = mlir::arith::AndIOp::create(rewriter, op.getLoc(), unique,
                                           notImmediate);
    }
    auto likelyUnique =
        ReussirExpectOp::create(rewriter, op.getLoc(), unique, true);
    auto ifOp =
        mlir::scf::IfOp::create(rewriter, op.getLoc(), op->getResultTypes(),
                                likelyUnique.getLikely(), true, true);
    RefType borrowedRefType = rewriter.getType<RefType>(
        type.getElementType(), Capability::unspecified, type.getAtomicKind());
    TokenType tokenType = llvm::cast<TokenType>(
        llvm::cast<NullableType>(op.getNullableToken().getType()).getPtrTy());
    // A *destructuring* decrement (see `reussir-rc-dispatch-fusion`) knows
    // the pattern arm that consumed the box: bound members transfer with the
    // arm, so the unique path releases only the *unbound* content and the
    // shared path retains the bound members in place of the fused-away
    // per-binding retains. Everything else expands through the transitive
    // drop glue as before.
    {
      rewriter.setInsertionPointToStart(ifOp.thenBlock());
      if (op.isDestructuring()) {
        llvm::SmallDenseSet<int64_t> bound;
        for (int64_t index : op.getBoundMembersAttr().asArrayRef())
          bound.insert(index);
        auto [payload, coerced] = op.destructuredPayloadAndRef(rewriter);
        for (auto [idx, memberTy, memberIsField] : llvm::enumerate(
                 payload.getMembers(), payload.getMemberIsField())) {
          if (bound.contains(static_cast<int64_t>(idx)) || memberIsField)
            continue;
          auto projectedTy =
              getProjectedType(memberTy, memberIsField, Capability::unspecified,
                               type.getAtomicKind());
          if (isTriviallyCopyable(projectedTy))
            continue;
          // Unbound members release through `ref.drop` — the same route the
          // transitive glue took. The acquire/drop expansion later turns an
          // rc slot's drop into a *plain* member decrement, which keeps it
          // visible to the post-expansion cancellation window (`inc %m`
          // against the unique-path release moves the retain to the shared
          // branch); materializing the decrement here would see it expanded
          // immediately and hide it from that optimization.
          auto projectedRefTy = rewriter.getType<RefType>(
              projectedTy, Capability::unspecified, type.getAtomicKind());
          mlir::Value slot =
              ReussirRefProjectOp::create(rewriter, op.getLoc(), projectedRefTy,
                                          coerced, rewriter.getIndexAttr(idx));
          ReussirRefDropOp::create(rewriter, op.getLoc(), slot);
        }
      } else {
        mlir::Value ref = ReussirRcBorrowOp::create(
            rewriter, op.getLoc(), borrowedRefType, op.getRcPtr());
        ReussirRefDropOp::create(rewriter, op.getLoc(), ref);
      }
      mlir::Value token = ReussirRcReinterpretOp::create(
          rewriter, op.getLoc(), tokenType, op.getRcPtr());
      mlir::Value nonnull = ReussirNullableCreateOp::create(
          rewriter, op.getLoc(), op.getNullableToken().getType(), token);
      mlir::scf::YieldOp::create(rewriter, op.getLoc(), nonnull);
    }
    {
      rewriter.setInsertionPointToStart(ifOp.elseBlock());
      if (!atomic) {
        auto decremented = mlir::arith::SubIOp::create(
            rewriter, op.getLoc(), prevRcCount,
            mlir::arith::ConstantIndexOp::create(rewriter, op.getLoc(), 1));
        ReussirRcSetOp::create(rewriter, op.getLoc(), op.getRcPtr(),
                               decremented.getResult());
      }
      if (op.isDestructuring()) {
        // The shared path keeps the box alive, so the consumer's bound
        // members need their own references — the retains the fusion
        // erased.
        op.rematerializeBoundRetains(rewriter);
      }
      auto null = ReussirNullableCreateOp::create(
          rewriter, op.getLoc(), op.getNullableToken().getType(), nullptr);
      mlir::scf::YieldOp::create(rewriter, op.getLoc(), null->getResults());
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
    auto module = llvm::cast<mlir::ModuleOp>(getOperation());
    if (mlir::failed(resolveDropOrder(module, dropOrder)))
      return signalPassFailure();
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());
    populateRcDecrementExpansionConversionPatterns(patterns, deferNormal);
    if (failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateRcDecrementExpansionConversionPatterns(
    mlir::RewritePatternSet &patterns, bool deferNormal) {
  patterns.add<RcDecrementExpansionPattern>(patterns.getContext(), deferNormal);
}

} // namespace reussir
