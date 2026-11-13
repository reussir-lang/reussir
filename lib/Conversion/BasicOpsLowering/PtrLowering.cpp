// Part of the Reussir Project, dual licensed under the Apache License v2.0
// or the MIT License. See LICENSE for details.
// SPDX-License-Identifier: Apache-2.0 OR MIT

#include "Reussir/Conversion/BasicOpsLowering.h"
#include <mlir/Conversion/LLVMCommon/Pattern.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/Ptr/IR/PtrOps.h>

namespace reussir {
namespace {
using namespace mlir;

LLVM::AtomicOrdering convertOrdering(ptr::AtomicOrdering ordering) {
  switch (ordering) {
  case ptr::AtomicOrdering::not_atomic:
    return LLVM::AtomicOrdering::not_atomic;
  case ptr::AtomicOrdering::unordered:
    return LLVM::AtomicOrdering::unordered;
  case ptr::AtomicOrdering::monotonic:
    return LLVM::AtomicOrdering::monotonic;
  case ptr::AtomicOrdering::acquire:
    return LLVM::AtomicOrdering::acquire;
  case ptr::AtomicOrdering::release:
    return LLVM::AtomicOrdering::release;
  case ptr::AtomicOrdering::acq_rel:
    return LLVM::AtomicOrdering::acq_rel;
  case ptr::AtomicOrdering::seq_cst:
    return LLVM::AtomicOrdering::seq_cst;
  }
  llvm_unreachable("unknown atomic ordering");
}

/// The integer type as wide as an address of the converted pointer.
IntegerType addressType(const LLVMTypeConverter &converter, Value pointer) {
  auto type = llvm::cast<LLVM::LLVMPointerType>(pointer.getType());
  return IntegerType::get(pointer.getContext(),
                          converter.getPointerBitwidth(type.getAddressSpace()));
}

/// Extend or truncate `value` to `type`.
Value resize(OpBuilder &builder, Location loc, Value value, Type type,
             bool isSigned) {
  unsigned from = value.getType().getIntOrFloatBitWidth();
  unsigned to = type.getIntOrFloatBitWidth();
  if (from == to)
    return value;
  if (from > to)
    return LLVM::TruncOp::create(builder, loc, type, value);
  if (isSigned)
    return LLVM::SExtOp::create(builder, loc, type, value);
  return LLVM::ZExtOp::create(builder, loc, type, value);
}

// The upstream PtrToLLVM conversion covers pointer arithmetic and casts but
// not memory accesses or pointer differences.
struct PtrLoadConversion : ConvertOpToLLVMPattern<ptr::LoadOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ptr::LoadOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type type = getTypeConverter()->convertType(op.getValue().getType());
    if (!type)
      return rewriter.notifyMatchFailure(op, "cannot convert the loaded type");
    rewriter.replaceOpWithNewOp<LLVM::LoadOp>(
        op, type, adaptor.getPtr(), op.getAlignment().value_or(0),
        op.getVolatile_(), op.getNontemporal(), op.getInvariant(),
        op.getInvariantGroup(), convertOrdering(op.getOrdering()),
        op.getSyncscope().value_or(StringRef()));
    return success();
  }
};

struct PtrStoreConversion : ConvertOpToLLVMPattern<ptr::StoreOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ptr::StoreOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<LLVM::StoreOp>(
        op, adaptor.getValue(), adaptor.getPtr(), op.getAlignment().value_or(0),
        op.getVolatile_(), op.getNontemporal(), op.getInvariantGroup(),
        convertOrdering(op.getOrdering()),
        op.getSyncscope().value_or(StringRef()));
    return success();
  }
};

struct PtrDiffConversion : ConvertOpToLLVMPattern<ptr::PtrDiffOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ptr::PtrDiffOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    if (!llvm::isa<ptr::PtrType>(op.getLhs().getType()))
      return rewriter.notifyMatchFailure(op, "expected scalar pointers");
    Type type = getTypeConverter()->convertType(op.getResult().getType());
    if (!type)
      return rewriter.notifyMatchFailure(op, "cannot convert the result type");
    Location loc = op.getLoc();
    IntegerType address = addressType(*getTypeConverter(), adaptor.getLhs());
    Value lhs =
        LLVM::PtrToAddrOp::create(rewriter, loc, address, adaptor.getLhs());
    Value rhs =
        LLVM::PtrToAddrOp::create(rewriter, loc, address, adaptor.getRhs());
    auto flags = LLVM::IntegerOverflowFlags::none;
    if (bitEnumContainsAll(op.getFlags(), ptr::PtrDiffFlags::nuw))
      flags = flags | LLVM::IntegerOverflowFlags::nuw;
    if (bitEnumContainsAll(op.getFlags(), ptr::PtrDiffFlags::nsw))
      flags = flags | LLVM::IntegerOverflowFlags::nsw;
    Value diff = LLVM::SubOp::create(rewriter, loc, lhs, rhs, flags);
    rewriter.replaceOp(op, resize(rewriter, loc, diff, type, true));
    return success();
  }
};

} // namespace

void populatePtrLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                 mlir::RewritePatternSet &patterns) {
  patterns.add<PtrLoadConversion, PtrStoreConversion, PtrDiffConversion>(
      converter);
}
} // namespace reussir
