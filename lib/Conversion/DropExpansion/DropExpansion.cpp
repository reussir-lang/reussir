//===-- DropExpansion.cpp - Reussir drop expansion impl ---------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <algorithm>
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
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/Conversion/DropExpansion.h"
#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRDROPEXPANSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

class DropExpansionPattern : public mlir::OpRewritePattern<ReussirRefDropOp> {
private:
  mlir::LogicalResult rewriteDropRc(RcType rcType, ReussirRefDropOp op,
                                    mlir::PatternRewriter &rewriter) const {
    // Replace drop of ref rc with load then dec
    NullableType nullableType = nullptr;
    if (rcType.getCapability() != Capability::rigid) {
      auto layout = mlir::DataLayout::closest(op.getOperation());
      RcBoxType rcBoxType = rcType.getInnerBoxType();
      size_t size = layout.getTypeSize(rcBoxType).getFixedValue();
      size_t align = layout.getTypeABIAlignment(rcBoxType);
      TokenType tokenType = TokenType::get(op.getContext(), align, size);
      nullableType = NullableType::get(op.getContext(), tokenType);
    }
    mlir::Value loaded =
        rewriter.create<ReussirRefLoadOp>(op.getLoc(), rcType, op.getRef());
    rewriter.create<ReussirRcDecOp>(op.getLoc(), nullableType, loaded);
    rewriter.eraseOp(op);
    return mlir::success();
  }

  mlir::LogicalResult
  rewriteDropCompound(RecordType recordType, Capability refCap,
                      ReussirRefDropOp op,
                      mlir::PatternRewriter &rewriter) const {
    assert(recordType.isCompound());
    for (auto [idx, memberTy, memberCap] : llvm::enumerate(
             recordType.getMembers(), recordType.getMemberCapabilities())) {
      if (memberCap == Capability::field)
        continue;
      auto projectedTy = getProjectedType(memberTy, memberCap, refCap);
      if (isTriviallyCopyable(projectedTy))
        continue;
      RefType projectedRefTy =
          RefType::get(op.getContext(), projectedTy, refCap);
      mlir::IntegerAttr index = rewriter.getIndexAttr(idx);
      mlir::Value projectedVal = rewriter.create<ReussirRefProjectOp>(
          op.getLoc(), projectedRefTy, op.getRef(), index);
      rewriter.create<ReussirRefDropOp>(op.getLoc(), projectedVal);
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }

  mlir::LogicalResult
  rewriteDropVariant(RecordType recordType, Capability refCap,
                     ReussirRefDropOp op,
                     mlir::PatternRewriter &rewriter) const {
    assert(recordType.isVariant());
    llvm::SmallVector<mlir::Attribute> tagSets;
    for (auto idx : llvm::seq<int64_t>(0, recordType.getMembers().size()))
      tagSets.push_back(rewriter.getDenseI64ArrayAttr({idx}));
    auto tagSetsAttr = rewriter.getArrayAttr(tagSets);
    auto dispatcher = rewriter.create<ReussirRecordDispatchOp>(
        op.getLoc(), mlir::Type{}, op.getRef(), tagSetsAttr, tagSets.size());
    for (auto [idx, memberTy, memberCap] : llvm::enumerate(
             recordType.getMembers(), recordType.getMemberCapabilities())) {
      auto projectedTy = getProjectedType(memberTy, memberCap, refCap);
      RefType projectedRefTy =
          RefType::get(op.getContext(), projectedTy, refCap);
      mlir::Block *block = rewriter.createBlock(
          &dispatcher.getRegions()[idx], dispatcher.getRegions()[idx].begin(),
          {projectedRefTy}, {op.getLoc()});
      rewriter.setInsertionPointToStart(block);
      if (memberCap != Capability::field && !isTriviallyCopyable(projectedTy))
        rewriter.create<ReussirRefDropOp>(op.getLoc(), block->getArgument(0),
                                          true, nullptr);

      rewriter.create<ReussirScfYieldOp>(op.getLoc(), nullptr);
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }

  mlir::LogicalResult
  rewriteDropVariant(RecordType recordType, size_t tag, Capability refCap,
                     ReussirRefDropOp op,
                     mlir::PatternRewriter &rewriter) const {
    assert(recordType.isVariant());
    auto targetType = recordType.getMembers()[tag];
    auto targetRefType = rewriter.getType<RefType>(targetType, refCap);
    auto targetRef = rewriter.create<ReussirRecordCoerceOp>(
        op.getLoc(), targetRefType, rewriter.getIndexAttr(tag), op.getRef());
    rewriter.create<ReussirRefDropOp>(op.getLoc(), targetRef);
    rewriter.eraseOp(op);
    return mlir::success();
  }

  mlir::LogicalResult
  rewriteDropNullable(NullableType nullableType, ReussirRefDropOp op,
                      mlir::PatternRewriter &rewriter) const {
    if (auto rcType = llvm::dyn_cast<RcType>(nullableType.getPtrTy())) {
      mlir::Value loaded = rewriter.create<ReussirRefLoadOp>(
          op.getLoc(), nullableType, op.getRef());
      auto dispatcher = rewriter.create<ReussirNullableDispatchOp>(
          op.getLoc(), mlir::Type{}, loaded);
      // do nothing if null
      mlir::Block *nullBlock = rewriter.createBlock(
          &dispatcher.getNullRegion(), dispatcher.getNullRegion().begin());
      rewriter.setInsertionPointToStart(nullBlock);
      rewriter.create<ReussirScfYieldOp>(op.getLoc(), nullptr);

      // drop inner if not null
      mlir::Block *nonNullBlock = rewriter.createBlock(
          &dispatcher.getNonNullRegion(), dispatcher.getNonNullRegion().begin(),
          {nullableType.getPtrTy()}, {op.getLoc()});
      rewriter.setInsertionPointToStart(nonNullBlock);
      NullableType retNullableTy = nullptr;
      if (rcType.getCapability() != Capability::rigid) {
        auto layout = mlir::DataLayout::closest(op.getOperation());
        RcBoxType rcBoxType = rcType.getInnerBoxType();
        size_t size = layout.getTypeSize(rcBoxType).getFixedValue();
        size_t align = layout.getTypeABIAlignment(rcBoxType);
        TokenType tokenType = TokenType::get(op.getContext(), align, size);
        retNullableTy = NullableType::get(op.getContext(), tokenType);
      }
      rewriter.create<ReussirRcDecOp>(op.getLoc(), retNullableTy,
                                      nonNullBlock->getArgument(0));
      rewriter.create<ReussirScfYieldOp>(op.getLoc(), nullptr);
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }

  bool outlineRecord;

  bool shouldOutline(ReussirRefDropOp op, RecordType type) const {
    if (!outlineRecord)
      return false;

    if (isTriviallyCopyable(type))
      return false;

    if (op.getInlined())
      return false;

    return type.getName() != nullptr;
  }

public:
  DropExpansionPattern(mlir::MLIRContext *context, bool outlineRecord)
      : mlir::OpRewritePattern<ReussirRefDropOp>(context),
        outlineRecord(outlineRecord) {}

  mlir::LogicalResult
  matchAndRewrite(ReussirRefDropOp op,
                  mlir::PatternRewriter &rewriter) const override {
    RefType refType = op.getRef().getType();
    Capability refCap = refType.getCapability();
    if (isTriviallyCopyable(refType.getElementType())) {
      rewriter.eraseOp(op);
      return mlir::success();
    }

    mlir::Type elementType = refType.getElementType();

    return llvm::TypeSwitch<mlir::Type, llvm::LogicalResult>(elementType)
        .Case<RcType>(
            [&](RcType rcType) { return rewriteDropRc(rcType, op, rewriter); })
        .Case<RecordType>([&](RecordType recordType) {
          if (shouldOutline(op, recordType)) {
            mlir::ModuleOp moduleOp = op->getParentOfType<mlir::ModuleOp>();
            mlir::func::FuncOp dtor =
                createDtorIfNotExists(moduleOp, recordType, rewriter);
            rewriter.create<mlir::func::CallOp>(op.getLoc(), dtor, op.getRef());
            rewriter.eraseOp(op);
            return llvm::success();
          }
          if (recordType.isCompound())
            return rewriteDropCompound(recordType, refCap, op, rewriter);
          if (op.getVariant())
            return rewriteDropVariant(recordType,
                                      op.getVariant()->getZExtValue(), refCap,
                                      op, rewriter);
          return rewriteDropVariant(recordType, refCap, op, rewriter);
        })
        .Case<NullableType>([&](NullableType nullableType) {
          return rewriteDropNullable(nullableType, op, rewriter);
        })
        .Default([&](mlir::Type) { return mlir::failure(); });
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// Exposed utility
//===----------------------------------------------------------------------===//

mlir::func::FuncOp createDtorIfNotExists(mlir::ModuleOp moduleOp,
                                         RecordType type,
                                         mlir::OpBuilder &builder) {
  mlir::SymbolTable symTable(moduleOp);
  auto dtorName = type.getDtorName();
  if (!dtorName)
    llvm::report_fatal_error("only named record types have destructors");
  if (auto funcOp = symTable.lookup<mlir::func::FuncOp>(dtorName.getValue()))
    return funcOp;
  mlir::OpBuilder::InsertionGuard guard(builder);
  builder.setInsertionPointToStart(moduleOp.getBody());
  RefType refType = builder.getType<RefType>(type);
  auto dtor = builder.create<mlir::func::FuncOp>(
      builder.getUnknownLoc(), dtorName.getValue(),
      builder.getFunctionType({refType}, {}));
  dtor.setPrivate();
  dtor->setAttr("llvm.linkage", builder.getAttr<mlir::LLVM::LinkageAttr>(
                                    mlir::LLVM::linkage::Linkage::LinkonceODR));
  mlir::Block *entryBlock = dtor.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);
  auto ref = entryBlock->getArgument(0);
  builder.create<ReussirRefDropOp>(builder.getUnknownLoc(), ref, true, nullptr);
  builder.create<mlir::func::ReturnOp>(builder.getUnknownLoc());
  return dtor;
}

//===----------------------------------------------------------------------===//
// DropExpansionPass
//===----------------------------------------------------------------------===//

namespace {
struct DropExpansionPass
    : public impl::ReussirDropExpansionPassBase<DropExpansionPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());

    populateDropExpansionConversionPatterns(patterns, outlineRecord);
    if (expandDecrement)
      populateRcDecrementExpansionConversionPatterns(patterns);
    if (failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateDropExpansionConversionPatterns(mlir::RewritePatternSet &patterns,
                                             bool outlineRecord) {
  patterns.add<DropExpansionPattern>(patterns.getContext(), outlineRecord);
}

} // namespace reussir
