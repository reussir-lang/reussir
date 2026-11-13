// Part of the Reussir Project, dual licensed under the Apache License v2.0
// or the MIT License. See LICENSE for details.
// SPDX-License-Identifier: Apache-2.0 OR MIT

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/IR/ReussirOps.h"
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

/// The integer type as wide as an address of the converted pointer, or null
/// if `pointer` did not convert to an LLVM pointer.
IntegerType addressType(const LLVMTypeConverter &converter, Value pointer) {
  auto type = llvm::dyn_cast<LLVM::LLVMPointerType>(pointer.getType());
  if (!type)
    return {};
  return IntegerType::get(
      pointer.getContext(),
      converter.getDataLayout().getIndexSizeInBits(type.getAddressSpace()));
}

/// The alignment of a memory access as the LLVM dialect takes it, where zero
/// is the default. Empty if the alignment is too large for the LLVM dialect.
std::optional<unsigned> convertAlignment(std::optional<int64_t> alignment) {
  if (!alignment)
    return 0;
  if (!llvm::isUInt<32>(*alignment))
    return std::nullopt;
  return static_cast<unsigned>(*alignment);
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
    if (!llvm::isa<LLVM::LLVMPointerType>(adaptor.getPtr().getType()))
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    std::optional<unsigned> alignment = convertAlignment(op.getAlignment());
    if (!alignment)
      return rewriter.notifyMatchFailure(op, "alignment is too large");
    rewriter.replaceOpWithNewOp<LLVM::LoadOp>(
        op, type, adaptor.getPtr(), *alignment, op.getVolatile_(),
        op.getNontemporal(), op.getInvariant(), op.getInvariantGroup(),
        convertOrdering(op.getOrdering()),
        op.getSyncscope().value_or(StringRef()));
    return success();
  }
};

struct PtrStoreConversion : ConvertOpToLLVMPattern<ptr::StoreOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ptr::StoreOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    if (!llvm::isa<LLVM::LLVMPointerType>(adaptor.getPtr().getType()))
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    std::optional<unsigned> alignment = convertAlignment(op.getAlignment());
    if (!alignment)
      return rewriter.notifyMatchFailure(op, "alignment is too large");
    rewriter.replaceOpWithNewOp<LLVM::StoreOp>(
        op, adaptor.getValue(), adaptor.getPtr(), *alignment, op.getVolatile_(),
        op.getNontemporal(), op.getInvariantGroup(),
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
    IntegerType address = addressType(*getTypeConverter(), adaptor.getLhs());
    if (!address)
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    Location loc = op.getLoc();
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

struct PtrAddrConversion : ConvertOpToLLVMPattern<ReussirPtrAddrOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ReussirPtrAddrOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type type = getTypeConverter()->convertType(op.getAddr().getType());
    if (!type)
      return rewriter.notifyMatchFailure(op, "cannot convert the result type");
    IntegerType address = addressType(*getTypeConverter(), adaptor.getPtr());
    if (!address)
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    Value addr = LLVM::PtrToAddrOp::create(rewriter, op.getLoc(), address,
                                           adaptor.getPtr());
    rewriter.replaceOp(op, resize(rewriter, op.getLoc(), addr, type, false));
    return success();
  }
};

struct PtrExposeAddrConversion
    : ConvertOpToLLVMPattern<ReussirPtrExposeAddrOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ReussirPtrExposeAddrOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type type = getTypeConverter()->convertType(op.getAddr().getType());
    if (!type)
      return rewriter.notifyMatchFailure(op, "cannot convert the result type");
    if (!llvm::isa<LLVM::LLVMPointerType>(adaptor.getPtr().getType()))
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    // `ptrtoint` extends or truncates the address itself.
    rewriter.replaceOpWithNewOp<LLVM::PtrToIntOp>(op, type, adaptor.getPtr());
    return success();
  }
};

/// Convert an operation from a pointer to a pointer of the same type to the
/// LLVM intrinsic `Intrinsic` of that shape.
template <typename Op, typename Intrinsic>
struct PtrIntrinsicConversion : ConvertOpToLLVMPattern<Op> {
  using ConvertOpToLLVMPattern<Op>::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(Op op, typename Op::Adaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Value pointer = adaptor.getPtr();
    if (!llvm::isa<LLVM::LLVMPointerType>(pointer.getType()))
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    rewriter.replaceOpWithNewOp<Intrinsic>(op, pointer.getType(), pointer);
    return success();
  }
};

struct PtrFromAddrConversion : ConvertOpToLLVMPattern<ReussirPtrFromAddrOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ReussirPtrFromAddrOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type type = getTypeConverter()->convertType(op.getPtr().getType());
    if (!type)
      return rewriter.notifyMatchFailure(op, "cannot convert the pointer type");
    // `inttoptr` extends or truncates the address itself.
    rewriter.replaceOpWithNewOp<LLVM::IntToPtrOp>(op, type, adaptor.getAddr());
    return success();
  }
};

struct PtrMaskConversion : ConvertOpToLLVMPattern<ReussirPtrMaskOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  LogicalResult
  matchAndRewrite(ReussirPtrMaskOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    IntegerType address = addressType(*getTypeConverter(), adaptor.getPtr());
    if (!address)
      return rewriter.notifyMatchFailure(op, "incompatible pointer type");
    Value mask =
        resize(rewriter, op.getLoc(), adaptor.getMask(), address, false);
    rewriter.replaceOpWithNewOp<LLVM::PtrMaskOp>(op, adaptor.getPtr().getType(),
                                                 adaptor.getPtr(), mask);
    return success();
  }
};
} // namespace

void populatePtrLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                 mlir::RewritePatternSet &patterns) {
  patterns.add<PtrLoadConversion, PtrStoreConversion, PtrDiffConversion,
               PtrAddrConversion, PtrExposeAddrConversion,
               PtrFromAddrConversion, PtrMaskConversion,
               PtrIntrinsicConversion<ReussirPtrLaunderInvariantGroupOp,
                                      LLVM::LaunderInvariantGroupOp>,
               PtrIntrinsicConversion<ReussirPtrStripInvariantGroupOp,
                                      LLVM::StripInvariantGroupOp>>(converter);
}
} // namespace reussir
