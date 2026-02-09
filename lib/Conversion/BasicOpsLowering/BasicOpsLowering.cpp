//===-- BasicOpsLowering.cpp - Reussir basic ops lowering impl --*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/DebugInfo/DWARF/DWARFAttribute.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/LogicalResult.h>
#include <llvm/Support/xxhash.h>
#include <mlir/Conversion/ArithToLLVM/ArithToLLVM.h>
#include <mlir/Conversion/ControlFlowToLLVM/ControlFlowToLLVM.h>
#include <mlir/Conversion/FuncToLLVM/ConvertFuncToLLVM.h>
#include <mlir/Conversion/LLVMCommon/TypeConverter.h>
#include <mlir/Conversion/MathToLLVM/MathToLLVM.h>
#include <mlir/Conversion/MathToLibm/MathToLibm.h>
#include <mlir/Conversion/MemRefToLLVM/MemRefToLLVM.h>
#include <mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h>
#include <mlir/Conversion/UBToLLVM/UBToLLVM.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/LLVMIR/LLVMTypes.h>
#include <mlir/Dialect/Math/IR/Math.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/IR/Value.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/Conversion/TypeConverter.h"
#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "mlir/IR/Location.h"
#include "mlir/Support/LLVM.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TypeSize.h"

namespace reussir {
#define GEN_PASS_DEF_REUSSIRBASICOPSLOWERINGPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

template <typename Op>
void addLifetimeOrInvariantOp(mlir::OpBuilder &rewriter, mlir::Location loc,
                              mlir::Type type, mlir::Value value,
                              const LLVMTypeConverter &converter) {
#if LLVM_VERSION_MAJOR >= 22
  // no size argument
  rewriter.create<Op>(loc, value);
#else
  // size argument
  size_t size = converter.getDataLayout().getTypeSize(type);
  rewriter.create<Op>(loc, size, value);
#endif
}

struct ReussirPanicConversionPattern
    : public mlir::OpConversionPattern<ReussirPanicOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirPanicOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = converter->getIndexType();

    // Get the panic message
    llvm::StringRef message = op.getMessage();

    // Create a unique symbol name for the global string using xxh128
    llvm::ArrayRef<uint8_t> messageBytes(
        reinterpret_cast<const uint8_t *>(message.data()), message.size());
    llvm::XXH128_hash_t hash = llvm::xxh3_128bits(messageBytes);
    std::string globalName = "__panic_message_" + llvm::utohexstr(hash.high64) +
                             "_" + llvm::utohexstr(hash.low64);

    // Get or create the global string constant
    auto moduleOp = op->getParentOfType<mlir::ModuleOp>();
    auto existingGlobal =
        moduleOp.lookupSymbol<mlir::LLVM::GlobalOp>(globalName);

    if (!existingGlobal) {
      // Create an LLVM array type for the string (no null terminator needed)
      auto i8Type = mlir::IntegerType::get(rewriter.getContext(), 8);
      auto arrayType = mlir::LLVM::LLVMArrayType::get(i8Type, message.size());

      // Create the string attribute
      auto stringAttr = rewriter.getStringAttr(message);

      // Insert global at module level
      mlir::OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());
      rewriter.create<mlir::LLVM::GlobalOp>(loc, arrayType, /*isConstant=*/true,
                                            mlir::LLVM::Linkage::LinkonceODR,
                                            globalName, stringAttr);
    }

    // Get address of the global string
    auto strPtr =
        rewriter.create<mlir::LLVM::AddressOfOp>(loc, llvmPtrType, globalName);

    // Create the length constant
    auto lenVal = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, message.size()));

    // Call __reussir_panic(ptr, len) - this function does not return
    rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
        op, "__reussir_panic", mlir::TypeRange{},
        mlir::ValueRange{strPtr, lenVal});

    return mlir::success();
  }
};

struct ReussirTokenAllocConversionPattern
    : public mlir::OpConversionPattern<ReussirTokenAllocOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirTokenAllocOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    mlir::MLIRContext *ctx = rewriter.getContext();

    // Get the token type and extract alignment and size
    TokenType tokenType = op.getToken().getType();
    uint64_t alignment = tokenType.getAlign();
    uint64_t size = tokenType.getSize();

    // Create constants for alignment and size
    auto alignConst = rewriter.create<mlir::arith::ConstantOp>(
        loc, rewriter.getIndexAttr(alignment));
    auto sizeConst = rewriter.create<mlir::arith::ConstantOp>(
        loc, rewriter.getIndexAttr(size));

    // Get the LLVM pointer type for the result
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(ctx);

    // Create the runtime function call
    auto funcOp = rewriter.create<mlir::func::CallOp>(
        loc, "__reussir_allocate", mlir::TypeRange{llvmPtrType},
        mlir::ValueRange{alignConst, sizeConst});

    // Replace the original operation with the function call result
    rewriter.replaceOp(op, funcOp.getResult(0));

    return mlir::success();
  }
};

struct ReussirTokenFreeConversionPattern
    : public mlir::OpConversionPattern<ReussirTokenFreeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirTokenFreeOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();

    // Get the token operand (already converted to LLVM pointer)
    mlir::Value tokenPtr = adaptor.getToken();

    // Get the token type and extract alignment and size
    TokenType tokenType = llvm::dyn_cast<TokenType>(op.getToken().getType());
    if (!tokenType) {
      auto nullableType = llvm::dyn_cast<NullableType>(op.getToken().getType());
      if (nullableType)
        tokenType = llvm::dyn_cast<TokenType>(nullableType.getPtrTy());
    }
    if (!tokenType)
      return op.emitOpError(
          "token operand must be of TokenType or NullableTokenType");

    uint64_t alignment = tokenType.getAlign();
    uint64_t size = tokenType.getSize();

    // Create constants for alignment and size
    auto alignConst = rewriter.create<mlir::arith::ConstantOp>(
        loc, rewriter.getIndexAttr(alignment));
    auto sizeConst = rewriter.create<mlir::arith::ConstantOp>(
        loc, rewriter.getIndexAttr(size));

    // Replace the original operation with the runtime function call
    rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
        op, "__reussir_deallocate", mlir::TypeRange{}, // No return type
        mlir::ValueRange{tokenPtr, alignConst, sizeConst});

    return mlir::success();
  }
};

struct ReussirTokenReinterpretConversionPattern
    : public mlir::OpConversionPattern<ReussirTokenReinterpretOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirTokenReinterpretOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // For reinterpret, we just use the input token directly since it's
    // already converted to an LLVM pointer by the type converter
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }
};

struct ReussirRcReinterpretConversionPattern
    : public mlir::OpConversionPattern<ReussirRcReinterpretOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcReinterpretOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // For reinterpret, we just use the input token directly since it's
    // already converted to an LLVM pointer by the type converter
    rewriter.replaceOp(op, adaptor.getRcPtr());
    return mlir::success();
  }
};

struct ReussirRcFetchDectConversionPattern
    : public mlir::OpConversionPattern<ReussirRcFetchDecOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcFetchDecOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto indexTy = static_cast<const LLVMTypeConverter *>(getTypeConverter())
                       ->getIndexType();
    mlir::Value loaded = rewriter.create<mlir::LLVM::LoadOp>(
        op.getLoc(), indexTy, adaptor.getRcPtr());
    mlir::Value one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), rewriter.getIntegerAttr(indexTy, 1));
    mlir::Value updated =
        rewriter.create<mlir::arith::SubIOp>(op.getLoc(), loaded, one);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), updated,
                                         adaptor.getRcPtr());
    rewriter.replaceOp(op, loaded);
    return mlir::success();
  }
};

struct ReussirTokenReallocConversionPattern
    : public mlir::OpConversionPattern<ReussirTokenReallocOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirTokenReallocOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    TokenType inputTokenType =
        llvm::TypeSwitch<mlir::Type, TokenType>(op.getToken().getType())
            .Case<TokenType>([](TokenType type) { return type; })
            .Case<NullableType>([](NullableType type) {
              return llvm::cast<TokenType>(type.getPtrTy());
            })
            .Default([](mlir::Type type) -> TokenType {
              llvm::report_fatal_error("Unexpected token type");
            });
    TokenType outputTokenType = op.getRealloced().getType();
    size_t oldAlign = inputTokenType.getAlign();
    size_t oldSize = inputTokenType.getSize();
    size_t newAlign = outputTokenType.getAlign();
    size_t newSize = outputTokenType.getSize();
    mlir::Value oldAlignVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), rewriter.getIndexAttr(oldAlign));
    mlir::Value oldSizeVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), rewriter.getIndexAttr(oldSize));
    mlir::Value newAlignVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), rewriter.getIndexAttr(newAlign));
    mlir::Value newSizeVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), rewriter.getIndexAttr(newSize));
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
        op, "__reussir_reallocate", mlir::TypeRange{llvmPtrType},
        mlir::ValueRange{adaptor.getToken(), oldAlignVal, oldSizeVal,
                         newAlignVal, newSizeVal});
    return mlir::success();
  }
};

struct ReussirRefStoreConversionPattern
    : public mlir::OpConversionPattern<ReussirRefStoreOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefStoreOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Get the converted operands (reference pointer and value)
    mlir::Value refPtr = adaptor.getRef();
    mlir::Value value = adaptor.getValue();

    // Create LLVM store operation
    rewriter.replaceOpWithNewOp<mlir::LLVM::StoreOp>(op, value, refPtr);

    return mlir::success();
  }
};

struct ReussirRefSpilledConversionPattern
    : public mlir::OpConversionPattern<ReussirRefSpilledOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefSpilledOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();

    // Get the value to spill (already converted by the type converter)
    mlir::Value value = adaptor.getValue();

    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    auto valueType = converter->convertType(op.getValue().getType());
    auto llvmPtrType = converter->convertType(op.getSpilled().getType());
    auto alignment =
        converter->getDataLayout().getTypePreferredAlignment(valueType);

    // Allocate stack space using llvm.alloca
    auto convertedIndexType = converter->getIndexType();
    auto constantArraySize = rewriter.create<mlir::arith::ConstantOp>(
        loc, rewriter.getIntegerAttr(convertedIndexType, 1));
    auto allocaOp = rewriter.create<mlir::LLVM::AllocaOp>(
        loc, llvmPtrType, valueType, constantArraySize, alignment);

    // Store the value to the allocated space
    rewriter.create<mlir::LLVM::StoreOp>(loc, value, allocaOp);
    rewriter.create<mlir::LLVM::InvariantStartOp>(
        loc, converter->getDataLayout().getTypeABIAlignment(valueType),
        allocaOp);
    rewriter.replaceOp(op, allocaOp);

    return mlir::success();
  }
};

struct ReussirRecordCompoundConversionPattern
    : public mlir::OpConversionPattern<ReussirRecordCompoundOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRecordCompoundOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the record type and convert it to LLVM struct type
    RecordType recordType = op.getCompound().getType();
    mlir::Type llvmStructType = converter->convertType(recordType);

    if (!llvmStructType)
      return op.emitOpError("failed to convert record type to LLVM type");

    // Create an undef value of the struct type
    auto undefOp = rewriter.create<mlir::LLVM::UndefOp>(loc, llvmStructType);

    // Get the field values (already converted by the type converter)
    auto fieldValues = adaptor.getFields();

    // Insert each field using insertvalue
    mlir::Value result = undefOp;
    for (size_t i = 0; i < fieldValues.size(); ++i)
      result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result,
                                                          fieldValues[i], i);

    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct ReussirRecordExtractConversionPattern
    : public mlir::OpConversionPattern<ReussirRecordExtractOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRecordExtractOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // adaptor.getRecord() is the struct value
    // op.getIndex() is the index attribute
    rewriter.replaceOpWithNewOp<mlir::LLVM::ExtractValueOp>(
        op, adaptor.getRecord(),
        llvm::ArrayRef<int64_t>{
            static_cast<int64_t>(op.getIndex().getZExtValue())});
    return mlir::success();
  }
};

struct ReussirRecordVariantConversionPattern
    : public mlir::OpConversionPattern<ReussirRecordVariantOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRecordVariantOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the record type and convert it to LLVM struct type
    RecordType recordType = op.getVariant().getType();
    auto llvmStructType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(
        converter->convertType(recordType));

    if (!llvmStructType)
      return op.emitOpError("failed to convert record type to LLVM type");
    auto indexType = converter->getIndexType();
    // Get the tag and value (already converted by the type converter)
    mlir::Value tag = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, op.getTag().getZExtValue()));
    mlir::Value value = adaptor.getValue();

    // Get the preferred alignment for the struct type
    auto alignment =
        converter->getDataLayout().getTypePreferredAlignment(llvmStructType);
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, 1));
    // Allocate stack space for the struct
    auto allocaOp = rewriter.create<mlir::LLVM::AllocaOp>(
        loc, ptrType, llvmStructType, one, alignment);
    addLifetimeOrInvariantOp<mlir::LLVM::LifetimeStartOp>(
        rewriter, loc, llvmStructType, allocaOp, *converter);
    // Get a pointer to the tag field (index 0) and store the tag
    auto tagPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, ptrType, llvmStructType, allocaOp,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    rewriter.create<mlir::LLVM::StoreOp>(loc, tag, tagPtr);

    // Get a pointer to the value field (index 1) and store the value
    if (llvmStructType.getSubelementIndexMap()->size() > 1) {
      auto valuePtr = rewriter.create<mlir::LLVM::GEPOp>(
          loc, ptrType, llvmStructType, allocaOp,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1});
      rewriter.create<mlir::LLVM::StoreOp>(loc, value, valuePtr);
    }
    // Load the complete struct from the allocated space
    auto result =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmStructType, allocaOp);
    addLifetimeOrInvariantOp<mlir::LLVM::LifetimeEndOp>(
        rewriter, loc, llvmStructType, allocaOp, *converter);
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct ReussirRefLoadConversionPattern
    : public mlir::OpConversionPattern<ReussirRefLoadOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefLoadOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Type pointeeTy = op.getResult().getType();
    mlir::Type llvmPointeeTy = getTypeConverter()->convertType(pointeeTy);
    rewriter.replaceOpWithNewOp<mlir::LLVM::LoadOp>(op, llvmPointeeTy,
                                                    adaptor.getRef());
    return mlir::success();
  }
};

struct ReussirReferenceProjectConversionPattern
    : public mlir::OpConversionPattern<ReussirRefProjectOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefProjectOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the reference pointer (already converted by the type converter)
    mlir::Value refPtr = adaptor.getRef();
    // Get the result type (should be a pointer type after conversion)
    mlir::Type resultType = converter->convertType(op.getProjected().getType());
    auto llvmPtrType = llvm::dyn_cast<mlir::LLVM::LLVMPointerType>(resultType);
    if (!llvmPtrType)
      return op.emitOpError("projected result must be an LLVM pointer type");

    // Get the element type that the reference points to
    RefType refType = op.getRef().getType();
    mlir::Type elementType = converter->convertType(refType.getElementType());

    // Create GEP operation to get the field pointer
    llvm::SmallVector<mlir::LLVM::GEPArg> gepArgs;
    auto gepOp = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, elementType, refPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{
            0, static_cast<int>(op.getIndex().getZExtValue())});

    rewriter.replaceOp(op, gepOp);
    return mlir::success();
  }
};

struct ReussirRecordTagConversionPattern
    : public mlir::OpConversionPattern<ReussirRecordTagOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRecordTagOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the reference pointer (already converted by the type converter)
    mlir::Value refPtr = adaptor.getVariant();

    // Get the element type that the reference points to
    RefType refType = op.getVariant().getType();
    RecordType recordType =
        llvm::dyn_cast<RecordType>(refType.getElementType());
    if (!recordType)
      return op.emitOpError("expected a record type");
    mlir::Type elementType = converter->convertType(recordType);

    // Get the index type for the result
    auto indexType = converter->getIndexType();

    // Create GEP operation to get the tag field pointer (index 0, 0)
    // For variant records, the tag is always at the first field
    auto tagPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto tagPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, tagPtrType, elementType, refPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});

    // Load the tag value
    auto tagValue =
        rewriter.replaceOpWithNewOp<mlir::LLVM::LoadOp>(op, indexType, tagPtr);

    // Assume that the tag is always in bounds
    auto numberMembers = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, recordType.getMembers().size()));
    auto tagInRange = rewriter.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::ult, tagValue, numberMembers);
    rewriter.create<mlir::LLVM::AssumeOp>(loc, tagInRange);
    return mlir::success();
  }
};

struct ReussirNullableCheckConversionPattern
    : public mlir::OpConversionPattern<ReussirNullableCheckOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirNullableCheckOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Type llvmPtrType =
        mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    mlir::Value nullable = adaptor.getNullable();
    mlir::Value nullConstant =
        rewriter.create<mlir::LLVM::ZeroOp>(op.getLoc(), llvmPtrType);
    rewriter.replaceOpWithNewOp<mlir::LLVM::ICmpOp>(
        op, mlir::LLVM::ICmpPredicate::ne, nullable, nullConstant);
    return mlir::success();
  }
};

struct ReussirNullableCreateConversionPattern
    : public mlir::OpConversionPattern<ReussirNullableCreateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirNullableCreateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Check if the operation has input. If so, replace it directly with
    // adaptor value. Otherwise, create a new null value.
    if (op.getPtr())
      rewriter.replaceOp(op, adaptor.getPtr());
    else
      rewriter.replaceOpWithNewOp<mlir::LLVM::ZeroOp>(
          op, mlir::LLVM::LLVMPointerType::get(rewriter.getContext()));
    return mlir::success();
  }
};

struct ReussirRcIncConversionPattern
    : public mlir::OpConversionPattern<ReussirRcIncOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcIncOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    RcType rcPtrTy = op.getRcPtr().getType();
    if (rcPtrTy.getCapability() == Capability::value ||
        rcPtrTy.getCapability() == Capability::flex ||
        rcPtrTy.getCapability() == Capability::field)
      return op.emitOpError("unsupported capability");

    // If it is rigid, we directly emit __reussir_acquire_rigid_object
    if (rcPtrTy.getCapability() == Capability::rigid) {
      rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
          op, "__reussir_acquire_rigid_object", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getRcPtr()});
      return mlir::success();
    }
    mlir::Value refcntPtr;
    // If inner element type is a FFI object, we do not reuse gep to expose
    // the struct.
    if (auto eleTy = mlir::dyn_cast<FFIObjectType>(rcPtrTy.getElementType())) {
      refcntPtr = adaptor.getRcPtr();
    } else {
      RcBoxType rcBoxType = rcPtrTy.getInnerBoxType();
      // GEP [0].1
      auto convertedBoxType = getTypeConverter()->convertType(rcBoxType);
      auto llvmPtrType =
          mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
      refcntPtr = rewriter.create<mlir::LLVM::GEPOp>(
          op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getRcPtr(),
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    }
    auto indexType = static_cast<const LLVMTypeConverter *>(getTypeConverter())
                         ->getIndexType();
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), mlir::IntegerAttr::get(indexType, 1));
    mlir::Value oldRefCnt;
    if (rcPtrTy.getAtomicKind() == AtomicKind::normal) {
      oldRefCnt = rewriter.create<mlir::LLVM::LoadOp>(op.getLoc(), indexType,
                                                      refcntPtr);
      auto newRefCnt = rewriter.create<mlir::arith::AddIOp>(
          op.getLoc(), indexType, oldRefCnt, one);
      rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), newRefCnt, refcntPtr);
    } else {
      oldRefCnt = rewriter.create<mlir::LLVM::AtomicRMWOp>(
          op.getLoc(), mlir::LLVM::AtomicBinOp::add, refcntPtr, one,
          mlir::LLVM::AtomicOrdering::monotonic);
    }
    auto geOne = rewriter.create<mlir::LLVM::ICmpOp>(
        op.getLoc(), mlir::LLVM::ICmpPredicate::uge, oldRefCnt, one);
    rewriter.create<mlir::LLVM::AssumeOp>(op.getLoc(), geOne);

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct ReussirRcCreateOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRcCreateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewriteNormalRc(ReussirRcCreateOp op, OpAdaptor adaptor,
                          RcType rcPtrTy, RcBoxType rcBoxType,
                          mlir::ConversionPatternRewriter &rewriter) const {
    // Implement the lowering for normal reference counted objects
    if (rcPtrTy.getAtomicKind() == AtomicKind::atomic)
      return op->emitError("TODO: atomic rc create");

    auto convertedBoxType = getTypeConverter()->convertType(rcBoxType);
    auto indexType = static_cast<const LLVMTypeConverter *>(getTypeConverter())
                         ->getIndexType();
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), mlir::IntegerAttr::get(indexType, 1));
    auto refcntPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    auto elementPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1});
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), one, refcntPtr);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), adaptor.getValue(),
                                         elementPtr);
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }

  mlir::LogicalResult
  matchAndRewriteRegionalRc(ReussirRcCreateOp op, OpAdaptor adaptor,
                            RcType rcPtrTy, RcBoxType rcBoxType,
                            mlir::ConversionPatternRewriter &rewriter) const {
    // Implement the lowering for normal reference counted objects
    if (rcPtrTy.getAtomicKind() == AtomicKind::atomic)
      return op->emitError("TODO: atomic rc create");
    if (!op.getToken())
      return op->emitError("token is required but not provided");
    auto convertedBoxType = getTypeConverter()->convertType(rcBoxType);
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto regionPtr = adaptor.getRegion();
    auto tailPtr = rewriter.create<mlir::LLVM::LoadOp>(op.getLoc(), llvmPtrType,
                                                       regionPtr);
    auto null = rewriter.create<mlir::LLVM::ZeroOp>(op.getLoc(), llvmPtrType);

    mlir::Value vtable;
    if (op.needsVTable()) {
      if (!op.getVtable())
        return op->emitError("vtable is required but not provided");
      vtable = rewriter.create<mlir::LLVM::AddressOfOp>(
          op.getLoc(), llvmPtrType, *op.getVtable());
    } else
      vtable = null.getRes();

    auto statePtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    auto nextPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1});
    auto vtablePtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 2});
    auto elementPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getToken(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 3});
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), null, statePtr);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), tailPtr, nextPtr);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), vtable, vtablePtr);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), adaptor.getValue(),
                                         elementPtr);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), adaptor.getToken(),
                                         regionPtr);
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }

  mlir::LogicalResult
  matchAndRewrite(ReussirRcCreateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    RcType rcPtrTy = op.getRcPtr().getType();
    RcBoxType rcBoxType = rcPtrTy.getInnerBoxType();
    if (rcBoxType.isRegional())
      return matchAndRewriteRegionalRc(op, adaptor, rcPtrTy, rcBoxType,
                                       rewriter);

    return matchAndRewriteNormalRc(op, adaptor, rcPtrTy, rcBoxType, rewriter);
  }
};

struct ReussirRcBorrowOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRcBorrowOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcBorrowOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    RcType rcPtrTy = op.getRcPtr().getType();
    RcBoxType rcBoxType = rcPtrTy.getInnerBoxType();
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    rewriter.replaceOpWithNewOp<mlir::LLVM::GEPOp>(
        op, llvmPtrType, getTypeConverter()->convertType(rcBoxType),
        adaptor.getRcPtr(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{
            0, static_cast<int>(rcBoxType.getElementIndex())});
    return mlir::success();
  }
};

struct ReussirNullableCoerceConversionPattern
    : public mlir::OpConversionPattern<ReussirNullableCoerceOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirNullableCoerceOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // For nullable coerce, we just use the input nullable directly since both
    // nullable and non-nullable pointers are LLVM pointers after conversion
    rewriter.replaceOp(op, adaptor.getNullable());
    return mlir::success();
  }
};

struct ReussirRecordCoerceConversionPattern
    : public mlir::OpConversionPattern<ReussirRecordCoerceOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRecordCoerceOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto dataLayout = mlir::DataLayout::closest(op.getOperation());

    // Get the variant reference pointer (already converted by the type
    // converter)
    mlir::Value variantPtr = adaptor.getVariant();

    // Get the element type that the reference points to (the variant record)
    RefType refType = op.getVariant().getType();
    auto elementType = llvm::cast<mlir::LLVM::LLVMStructType>(
        converter->convertType(refType.getElementType()));
    bool variantHasNonZeroChild =
        elementType.getSubelementIndexMap()->size() > 1;

    // Get the result type (should be a pointer type after conversion)
    mlir::Type resultType = converter->convertType(op.getCoerced().getType());

    // Create GEP operation to get the value field pointer (index 0, 1)
    // For variant records, the value is at the second field (index 1)
    if (variantHasNonZeroChild)
      rewriter.replaceOpWithNewOp<mlir::LLVM::GEPOp>(
          op, resultType, elementType, variantPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1});
    else
      rewriter.replaceOpWithNewOp<mlir::ub::PoisonOp>(op, resultType);
    return mlir::success();
  }
};

/*
Create a global constant
pub struct VTable {
    pub drop: Option<unsafe extern "C" fn(*mut u8)>,
    pub scan_instrs: *const PackedInstr,
    pub size: usize,
    pub alignment: usize,
}
*/
struct ReussirRegionVTableOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRegionVTableOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRegionVTableOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::LLVM::LLVMPointerType llvmPtrType =
        mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    mlir::Type indexType = converter->getIndexType();
    mlir::LLVM::LLVMStructType vtableType =
        mlir::LLVM::LLVMStructType::getLiteral(
            rewriter.getContext(),
            {llvmPtrType, llvmPtrType, indexType, indexType});
    std::string arrayName =
        (llvm::Twine("_RNv") + op.getSymName().ltrim("_R") + "11scan_instrs")
            .str();
    reussir::RecordType recordType =
        llvm::dyn_cast<reussir::RecordType>(op.getType());

    mlir::LLVM::GlobalOp scannerOp;
    if (recordType) {
      llvm::SmallVector<int32_t> buffer;
      recordType.emitScannerInstructions(buffer, converter->getDataLayout(),
                                         {});
      mlir::LLVM::LLVMArrayType arrayType = mlir::LLVM::LLVMArrayType::get(
          mlir::IntegerType::get(rewriter.getContext(), 32), buffer.size());
      auto dataAttr = rewriter.getI32TensorAttr(buffer);
      scannerOp = rewriter.create<mlir::LLVM::GlobalOp>(
          op.getLoc(), arrayType, /*isConstant=*/true,
          mlir::LLVM::Linkage::LinkonceODR, arrayName, dataAttr);
    }
    mlir::LLVM::GlobalOp vtableOp = rewriter.create<mlir::LLVM::GlobalOp>(
        op.getLoc(), vtableType, /*isConstant=*/true,
        mlir::LLVM::Linkage::LinkonceODR, op.getSymName(), nullptr);
    mlir::Block *initBlock =
        rewriter.createBlock(&vtableOp.getInitializerRegion());
    rewriter.setInsertionPointToEnd(initBlock);
    mlir::Value scannerPtr, dropPtr;
    if (scannerOp) {
      scannerPtr = rewriter.create<mlir::LLVM::AddressOfOp>(
          op.getLoc(), llvmPtrType, arrayName);
    } else {
      scannerPtr =
          rewriter.create<mlir::LLVM::ZeroOp>(op.getLoc(), llvmPtrType);
    }
    if (op.getDrop()) {
      dropPtr = rewriter.create<mlir::LLVM::AddressOfOp>(
          op.getLoc(), llvmPtrType, *op.getDrop());
    } else {
      dropPtr = rewriter.create<mlir::LLVM::ZeroOp>(op.getLoc(), llvmPtrType);
    }
    auto sizeVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(),
        mlir::IntegerAttr::get(
            indexType, converter->getDataLayout().getTypeSize(op.getType())));
    auto alignVal = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(),
        mlir::IntegerAttr::get(
            indexType,
            converter->getDataLayout().getTypeABIAlignment(op.getType())));
    auto undef = rewriter.create<mlir::LLVM::UndefOp>(op.getLoc(), vtableType);
    auto withDrop = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), undef, dropPtr, 0);
    auto withScanner = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), withDrop, scannerPtr, 1);
    auto withSize = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), withScanner, sizeVal, 2);
    auto withAlign = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), withSize, alignVal, 3);
    rewriter.create<mlir::LLVM::ReturnOp>(op.getLoc(), withAlign);
    rewriter.replaceOp(op, vtableOp);
    return mlir::success();
  }
};

struct ReussirClosureVtableOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureVtableOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureVtableOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::LLVM::LLVMPointerType llvmPtrType =
        mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    // Create LLVM struct type for vtable: { void*, void*, void* }
    // representing { drop, clone, evaluate } function pointers
    mlir::LLVM::LLVMStructType vtableType =
        mlir::LLVM::LLVMStructType::getLiteral(
            rewriter.getContext(), {llvmPtrType, llvmPtrType, llvmPtrType});

    // Create the global vtable
    mlir::LLVM::GlobalOp vtableOp = rewriter.create<mlir::LLVM::GlobalOp>(
        op.getLoc(), vtableType, /*isConstant=*/true,
        mlir::LLVM::Linkage::Internal, op.getSymName(), nullptr);

    // Create initializer block
    mlir::Block *initBlock =
        rewriter.createBlock(&vtableOp.getInitializerRegion());
    rewriter.setInsertionPointToEnd(initBlock);

    // Get addresses of the three functions
    auto dropPtr = rewriter.create<mlir::LLVM::AddressOfOp>(
        op.getLoc(), llvmPtrType, op.getDrop());
    auto clonePtr = rewriter.create<mlir::LLVM::AddressOfOp>(
        op.getLoc(), llvmPtrType, op.getClone());
    auto funcPtr = rewriter.create<mlir::LLVM::AddressOfOp>(
        op.getLoc(), llvmPtrType, op.getFunc());

    // Build the struct { drop, clone, evaluate }
    auto undef = rewriter.create<mlir::LLVM::UndefOp>(op.getLoc(), vtableType);
    auto withDrop = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), undef, dropPtr, ClosureType::VTABLE_DROP_INDEX);
    auto withClone = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), withDrop, clonePtr, ClosureType::VTABLE_CLONE_INDEX);
    auto withFunc = rewriter.create<mlir::LLVM::InsertValueOp>(
        op.getLoc(), withClone, funcPtr, ClosureType::VTABLE_EVALUATE_INDEX);

    rewriter.create<mlir::LLVM::ReturnOp>(op.getLoc(), withFunc);
    rewriter.replaceOp(op, vtableOp);
    return mlir::success();
  }
};

struct ReussirClosureCreateOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureCreateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureCreateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Only handle outlined closures (vtable must be present, body must be
    // empty)
    if (!op.isOutlined())
      return op.emitOpError("closure create must be outlined before lowering");

    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = converter->getIndexType();

    // Get the RcBox<ClosureBox> type
    RcBoxType rcBoxType = op.getRcClosureBoxType();
    auto convertedRcBoxType = converter->convertType(rcBoxType);

    // Token is the pointer to RcBox<ClosureBox>
    mlir::Value tokenPtr = adaptor.getToken();

    // 1. Assign refcnt (GEP[0, 0]) to 1
    auto refcntPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, convertedRcBoxType, tokenPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, 1));
    rewriter.create<mlir::LLVM::StoreOp>(loc, one, refcntPtr);

    // 2. Assign vtable (GEP[0, 1, 0]) to address of the vtable
    auto vtablePtrSlot = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, convertedRcBoxType, tokenPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1, ClosureBoxType::VTABLE_INDEX});
    auto vtableAddr = rewriter.create<mlir::LLVM::AddressOfOp>(loc, llvmPtrType,
                                                               *op.getVtable());
    rewriter.create<mlir::LLVM::StoreOp>(loc, vtableAddr, vtablePtrSlot);

    // 3. Assign cursor (GEP[0, 1, 1]) to the value of GEP[0, 1, 2]
    //    (cursor points to the start of payload area)
    auto cursorSlot = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, convertedRcBoxType, tokenPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1,
                                           ClosureBoxType::ARG_CURSOR_INDEX});
    auto payloadStart = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, convertedRcBoxType, tokenPtr,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1, 2});
    rewriter.create<mlir::LLVM::StoreOp>(loc, payloadStart, cursorSlot);

    // Replace with the token pointer (which is now a valid RcPtr<Closure>)
    rewriter.replaceOp(op, tokenPtr);
    return mlir::success();
  }
};

struct ReussirRcDecOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRcDecOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcDecOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // simply check if this is decreasing a rigid object. If so, convert it to
    // function call. otherwise return failure.
    RcType rcPtrTy = op.getRcPtr().getType();
    if (rcPtrTy.getCapability() == Capability::rigid) {
      rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
          op, "__reussir_release_rigid_object", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getRcPtr()});
      return mlir::success();
    }
    if (auto eleTy = mlir::dyn_cast<FFIObjectType>(rcPtrTy.getElementType())) {
      mlir::FlatSymbolRefAttr cleanupHook = eleTy.getCleanupHook();
      rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
          op, cleanupHook, mlir::TypeRange{},
          mlir::ValueRange{adaptor.getRcPtr()});
      return mlir::success();
    }
    if (auto closureTy =
            mlir::dyn_cast<ClosureType>(rcPtrTy.getElementType())) {
      mlir::Location loc = op.getLoc();
      auto llvmPtrType =
          mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

      // Get the RcBox<ClosureBox> type
      RcBoxType rcBoxType = rcPtrTy.getInnerBoxType();
      auto convertedBoxType = getTypeConverter()->convertType(rcBoxType);

      // GEP [0, 1, VTABLE_INDEX] to get the vtable pointer slot
      auto vtablePtr = rewriter.create<mlir::LLVM::GEPOp>(
          loc, llvmPtrType, convertedBoxType, adaptor.getRcPtr(),
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1,
                                             ClosureBoxType::VTABLE_INDEX});

      // Load the vtable pointer
      auto vtable =
          rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, vtablePtr);

      // Create LLVM struct type for vtable: { void*, void*, void* }
      // representing { drop, clone, evaluate } function pointers
      mlir::LLVM::LLVMStructType vtableType =
          mlir::LLVM::LLVMStructType::getLiteral(
              rewriter.getContext(), {llvmPtrType, llvmPtrType, llvmPtrType});

      // GEP [0, VTABLE_DROP_INDEX] to get the drop function pointer
      auto dropPtr = rewriter.create<mlir::LLVM::GEPOp>(
          loc, llvmPtrType, vtableType, vtable,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0,
                                             ClosureType::VTABLE_DROP_INDEX});

      // Load the drop function pointer
      auto dropFunc =
          rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, dropPtr);

      // Create function type for drop: void (*)(void*)
      auto voidType = mlir::LLVM::LLVMVoidType::get(rewriter.getContext());
      auto funcType =
          mlir::LLVM::LLVMFunctionType::get(voidType, {llvmPtrType});

      // Call the drop function with the RC pointer as argument
      rewriter.replaceOpWithNewOp<mlir::LLVM::CallOp>(
          op, funcType, mlir::ValueRange{dropFunc, adaptor.getRcPtr()});
      return mlir::success();
    }
    return mlir::failure();
  }
};

struct ReussirRcFreezeOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRcFreezeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcFreezeOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Call the runtime function __reussir_freeze_flex_object
    mlir::Type llvmPtrType =
        mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
        op, "__reussir_freeze_flex_object", mlir::TypeRange{llvmPtrType},
        mlir::ValueRange{adaptor.getRcPtr()});
    return mlir::success();
  }
};

struct ReussirRcIsUniqueOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRcIsUniqueOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcIsUniqueOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    RcType rcPtrTy = op.getRcPtr().getType();

    RcBoxType rcBoxType = rcPtrTy.getInnerBoxType();
    auto convertedBoxType = getTypeConverter()->convertType(rcBoxType);
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = static_cast<const LLVMTypeConverter *>(getTypeConverter())
                         ->getIndexType();

    // GEP [0, 0] to locate refcnt
    auto refcntPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, convertedBoxType, adaptor.getRcPtr(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});

    // Load refcnt
    auto refcnt =
        rewriter.create<mlir::LLVM::LoadOp>(op.getLoc(), indexType, refcntPtr);

    // Compare with 1 (unique means refcnt == 1)
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), mlir::IntegerAttr::get(indexType, 1));
    auto isUnique = rewriter.create<mlir::arith::CmpIOp>(
        op.getLoc(), mlir::arith::CmpIPredicate::eq, refcnt, one);

    rewriter.replaceOp(op, isUnique);
    return mlir::success();
  }
};

struct ReussirRegionCleanupOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRegionCleanupOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRegionCleanupOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Call the runtime function __reussir_freeze_flex_object
    rewriter.replaceOpWithNewOp<mlir::func::CallOp>(
        op, "__reussir_cleanup_region", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getRegion()});
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    addLifetimeOrInvariantOp<mlir::LLVM::LifetimeEndOp>(
        rewriter, op.getLoc(), ptrType, adaptor.getRegion(), *converter);
    return mlir::success();
  }
};

struct ReussirRegionCreateOpConversionPattern
    : public mlir::OpConversionPattern<ReussirRegionCreateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRegionCreateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Call the runtime function __reussir_create_region
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), mlir::IntegerAttr::get(converter->getIndexType(), 1));
    auto alloca = rewriter.replaceOpWithNewOp<mlir::LLVM::AllocaOp>(
        op, ptrType, ptrType, one);
    addLifetimeOrInvariantOp<mlir::LLVM::LifetimeStartOp>(
        rewriter, op.getLoc(), ptrType, alloca, *converter);
    auto nullValue = rewriter.create<mlir::LLVM::ZeroOp>(op.getLoc(), ptrType);
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), nullValue, alloca);
    return mlir::success();
  }
};

struct ReussirClosureApplyOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureApplyOp> {
  using OpConversionPattern::OpConversionPattern;

private:
  mlir::Value emitPointerAlign(mlir::Value ptr, mlir::Type type,
                               mlir::OpBuilder &builder) const {
    const LLVMTypeConverter *converter =
        static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto alignment = converter->getDataLayout().getTypeABIAlignment(type);
    if (alignment <= 1)
      return ptr;
    auto addr = builder.create<mlir::LLVM::PtrToIntOp>(
        ptr.getLoc(), converter->getIndexType(), ptr);
    auto mask = builder.create<mlir::arith::ConstantOp>(
        ptr.getLoc(),
        mlir::IntegerAttr::get(converter->getIndexType(), alignment - 1));
    auto zero = builder.create<mlir::arith::ConstantOp>(
        ptr.getLoc(), mlir::IntegerAttr::get(converter->getIndexType(), 0));
    auto negAddr = builder.create<mlir::arith::SubIOp>(
        ptr.getLoc(), converter->getIndexType(), zero, addr);
    auto offset = builder.create<mlir::arith::AndIOp>(
        ptr.getLoc(), converter->getIndexType(), negAddr, mask);
    auto alignedAddr = builder.create<mlir::arith::AddIOp>(
        ptr.getLoc(), converter->getIndexType(), addr, offset,
        mlir::arith::IntegerOverflowFlags::nuw);
    return builder.create<mlir::LLVM::IntToPtrOp>(ptr.getLoc(), ptr.getType(),
                                                  alignedAddr.getResult());
  }

  mlir::Value emitPointerBump(mlir::Value ptr, mlir::Type type,
                              mlir::OpBuilder &builder) const {
    const LLVMTypeConverter *converter =
        static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto size = converter->getDataLayout().getTypeSize(type);
    if (size == 0)
      return ptr;
    auto addr = builder.create<mlir::LLVM::PtrToIntOp>(
        ptr.getLoc(), converter->getIndexType(), ptr);
    auto sizeVal = builder.create<mlir::arith::ConstantOp>(
        ptr.getLoc(), mlir::IntegerAttr::get(converter->getIndexType(), size));
    auto newAddr = builder.create<mlir::arith::AddIOp>(
        ptr.getLoc(), converter->getIndexType(), addr, sizeVal,
        mlir::arith::IntegerOverflowFlags::nuw);
    return builder.create<mlir::LLVM::IntToPtrOp>(ptr.getLoc(), ptr.getType(),
                                                  newAddr.getResult());
  }

public:
  mlir::LogicalResult
  matchAndRewrite(ReussirClosureApplyOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto rcClosureBox = op.getType().getInnerBoxType();
    auto structType = typeConverter->convertType(rcClosureBox);
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    // This operation assumes that the closure is already uniquely owned.
    // First, use GEP[0, 1, ARG_CURSOR_INDEX] and load op to read the cursor
    // RcBox {
    //    size_t refcnt;
    //    {
    //        void* vtable;
    //        void* cursor;
    //        // trailing payload
    //    };
    // }
    auto cursorPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType, structType, adaptor.getClosure(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1,
                                           ClosureBoxType::ARG_CURSOR_INDEX});
    auto cursor = rewriter.create<mlir::LLVM::LoadOp>(op.getLoc(), llvmPtrType,
                                                      cursorPtr);

    // Second, align the cursor to current input type's TypeABIAlignment.
    auto inputType = typeConverter->convertType(op.getArg().getType());
    auto alignedCursor = emitPointerAlign(cursor, inputType, rewriter);

    // Third, copy the input value to the aligned cursor.
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), adaptor.getArg(),
                                         alignedCursor);

    // Fourth, bump the cursor by the input type's size.
    auto newCursor = emitPointerBump(alignedCursor, inputType, rewriter);

    // Fifth, store the new cursor back to the RcBox.
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), newCursor, cursorPtr);

    // Return the same closure pointer (in-place modification)
    rewriter.replaceOp(op, adaptor.getClosure());
    return mlir::success();
  }
};

struct ReussirClosureCloneOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureCloneOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureCloneOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the RC closure box type and convert it to LLVM struct type
    auto rcClosureBox = op.getType().getInnerBoxType();
    auto structType = converter->convertType(rcClosureBox);

    // First, GEP [0, 1, VTABLE_INDEX] to get the vtable pointer
    auto vtablePtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, structType, adaptor.getClosure(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1, ClosureBoxType::VTABLE_INDEX});

    // Load the vtable
    auto vtable =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, vtablePtr);

    // Create LLVM struct type for vtable: { void*, void*, void* }
    // representing { drop, clone, evaluate } function pointers
    mlir::LLVM::LLVMStructType vtableType =
        mlir::LLVM::LLVMStructType::getLiteral(
            rewriter.getContext(), {llvmPtrType, llvmPtrType, llvmPtrType});

    // Second, GEP [0, VTABLE_CLONE_INDEX] to get the clone function pointer
    auto clonePtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, vtableType, vtable,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, ClosureType::VTABLE_CLONE_INDEX});

    // Load the clone function pointer
    auto cloneFunc =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, clonePtr);

    // Create function type for clone: void* (*)(void*)
    auto funcType =
        mlir::LLVM::LLVMFunctionType::get(llvmPtrType, {llvmPtrType});

    // Call the clone function with the RC pointer as argument
    rewriter.replaceOpWithNewOp<mlir::LLVM::CallOp>(
        op, funcType,
        mlir::ValueRange{cloneFunc.getResult(), adaptor.getClosure()});
    return mlir::success();
  }
};

struct ReussirClosureEvalOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureEvalOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureEvalOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the RC closure box type and convert it to LLVM struct type
    RcType rcClosureType = op.getClosure().getType();
    auto rcClosureBox = rcClosureType.getInnerBoxType();
    auto structType = converter->convertType(rcClosureBox);

    // GEP [0, 1, VTABLE_INDEX] to get the vtable pointer slot
    auto vtablePtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, structType, adaptor.getClosure(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1, ClosureBoxType::VTABLE_INDEX});

    // Load the vtable pointer
    auto vtable =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, vtablePtr);

    // Create LLVM struct type for vtable: { void*, void*, void* }
    // representing { drop, clone, evaluate } function pointers
    mlir::LLVM::LLVMStructType vtableType =
        mlir::LLVM::LLVMStructType::getLiteral(
            rewriter.getContext(), {llvmPtrType, llvmPtrType, llvmPtrType});

    // GEP [0, VTABLE_EVALUATE_INDEX] to get the evaluate function pointer
    auto evalPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, vtableType, vtable,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0,
                                           ClosureType::VTABLE_EVALUATE_INDEX});

    // Load the evaluate function pointer
    auto evalFunc =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, evalPtr);

    // Determine if the closure has a return value
    mlir::Type resultType =
        op.getResult() ? op.getResult().getType() : mlir::Type{};
    if (resultType) {
      // Closure has a return value: T (*)(void*)
      mlir::Type llvmResultType = converter->convertType(resultType);
      auto funcType =
          mlir::LLVM::LLVMFunctionType::get(llvmResultType, {llvmPtrType});
      rewriter.replaceOpWithNewOp<mlir::LLVM::CallOp>(
          op, funcType, mlir::ValueRange{evalFunc, adaptor.getClosure()});
    } else {
      // Closure has no return value: void (*)(void*)
      auto voidType = mlir::LLVM::LLVMVoidType::get(rewriter.getContext());
      auto funcType =
          mlir::LLVM::LLVMFunctionType::get(voidType, {llvmPtrType});
      rewriter.replaceOpWithNewOp<mlir::LLVM::CallOp>(
          op, funcType, mlir::ValueRange{evalFunc, adaptor.getClosure()});
    }
    return mlir::success();
  }
};

struct ReussirClosureInspectPayloadOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureInspectPayloadOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureInspectPayloadOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Get the ClosureBox type from the reference
    RefType refType = op.getClosureBoxRef().getType();
    ClosureBoxType closureBoxType =
        llvm::cast<ClosureBoxType>(refType.getElementType());

    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto structType = getTypeConverter()->convertType(closureBoxType);

    // GEP[0, N+2] where N is the payload index
    // ClosureBox layout: { vtable (0), cursor (1), payload... (2+) }
    int64_t gepIndex =
        static_cast<int64_t>(op.getPayloadIndex().getZExtValue()) + 2;
    rewriter.replaceOpWithNewOp<mlir::LLVM::GEPOp>(
        op, llvmPtrType, structType, adaptor.getClosureBoxRef(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, gepIndex});
    return mlir::success();
  }
};

struct ReussirClosureCursorOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureCursorOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureCursorOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Get the ClosureBox type from the reference
    RefType refType = op.getClosureBoxRef().getType();
    ClosureBoxType closureBoxType =
        llvm::cast<ClosureBoxType>(refType.getElementType());

    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto structType = getTypeConverter()->convertType(closureBoxType);

    // GEP[0, 1] to access the cursor field
    // ClosureBox layout: { vtable (0), cursor (1), payload... (2+) }
    auto cursor = rewriter.create<mlir::LLVM::GEPOp>(
        op->getLoc(), llvmPtrType, structType, adaptor.getClosureBoxRef(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0,
                                           ClosureBoxType::ARG_CURSOR_INDEX});
    rewriter.replaceOpWithNewOp<mlir::LLVM::LoadOp>(op, llvmPtrType, cursor);
    return mlir::success();
  }
};

struct ReussirClosureTransferOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureTransferOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureTransferOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = converter->getIndexType();

    // Get the ClosureBox type from the source reference
    RefType srcRefType = op.getSrc().getType();
    ClosureBoxType closureBoxType =
        llvm::cast<ClosureBoxType>(srcRefType.getElementType());
    auto structType = converter->convertType(closureBoxType);

    // 1. Extract the cursor pointer from src
    // GEP[0, 1] to access the cursor field
    auto cursorPtrSlot = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, structType, adaptor.getSrc(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0,
                                           ClosureBoxType::ARG_CURSOR_INDEX});
    auto cursor =
        rewriter.create<mlir::LLVM::LoadOp>(loc, llvmPtrType, cursorPtrSlot);

    // 2. Compute the offset from src to cursor
    // Convert pointers to integers
    mlir::Value srcInt = rewriter.create<mlir::LLVM::PtrToIntOp>(
        loc, indexType, adaptor.getSrc());
    mlir::Value cursorInt =
        rewriter.create<mlir::LLVM::PtrToIntOp>(loc, indexType, cursor);

    // Compute offset: cursor - src
    mlir::Value offset = rewriter.create<mlir::arith::SubIOp>(
        loc, cursorInt, srcInt,
        mlir::arith::IntegerOverflowFlags::nsw |
            mlir::arith::IntegerOverflowFlags::nuw);
    // Call llvm.memcpy intrinsic
    auto size =
        converter->getDataLayout().getTypeSize(closureBoxType).getFixedValue();
    if (size > 32)
      rewriter.create<mlir::LLVM::MemcpyOp>(loc, adaptor.getDst(),
                                            adaptor.getSrc(), offset, false);
    else {
      // directly copy the whole closure box
      mlir::IntegerAttr sizeAttr = mlir::IntegerAttr::get(indexType, size);
      rewriter.create<mlir::LLVM::MemcpyInlineOp>(
          loc, adaptor.getDst(), adaptor.getSrc(), sizeAttr, false);
    }

    // Update cursor to (dst + offset)
    auto newCursor = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, rewriter.getI8Type(), adaptor.getDst(), offset,
        mlir::LLVM::GEPNoWrapFlags::inbounds);

    // Get the cursor slot in the destination closure box
    auto dstCursorSlot = rewriter.create<mlir::LLVM::GEPOp>(
        loc, llvmPtrType, structType, adaptor.getDst(),
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0,
                                           ClosureBoxType::ARG_CURSOR_INDEX});

    // Store the new cursor to the destination's cursor slot
    rewriter.create<mlir::LLVM::StoreOp>(loc, newCursor, dstCursorSlot);

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct ReussirClosureInstantiateOpConversionPattern
    : public mlir::OpConversionPattern<ReussirClosureInstantiateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureInstantiateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    // Set RC value to 1 and return the original pointer
    auto one = rewriter.create<mlir::arith::ConstantOp>(
        op.getLoc(), mlir::IntegerAttr::get(converter->getIndexType(), 1));
    auto refcntPtr = rewriter.create<mlir::LLVM::GEPOp>(
        op.getLoc(), llvmPtrType,
        converter->convertType(
            op.getClosureBoxRc().getType().getInnerBoxType()),
        adaptor.getToken(), llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    rewriter.create<mlir::LLVM::StoreOp>(op.getLoc(), one, refcntPtr);
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }
};

struct ReussirStrGlobalOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrGlobalOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrGlobalOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    llvm::StringRef payload = op.getPayload();

    // Create an LLVM array type for the string (including null terminator)
    auto i8Type = mlir::IntegerType::get(rewriter.getContext(), 8);
    auto arrayType = mlir::LLVM::LLVMArrayType::get(i8Type, payload.size() + 1);

    // Create the string attribute with null terminator
    std::string payloadWithNull = payload.str();
    payloadWithNull.push_back('\0');
    auto stringAttr = rewriter.getStringAttr(payloadWithNull);

    // Create LLVM global op
    rewriter.replaceOpWithNewOp<mlir::LLVM::GlobalOp>(
        // TODO: add target info in module attributes and use OnceDOR to
        // allow linker to merge globals with the same name
        op, arrayType, /*isConstant=*/true, mlir::LLVM::Linkage::Internal,
        op.getSymName(), stringAttr);

    return mlir::success();
  }
};

struct ReussirStrLiteralOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrLiteralOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrLiteralOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = converter->getIndexType();

    // The result type is a struct { ptr, index } representing { data, len }
    auto structType = mlir::LLVM::LLVMStructType::getLiteral(
        rewriter.getContext(), {llvmPtrType, indexType});

    // Look up the global string to get its length
    auto moduleOp = op->getParentOfType<mlir::ModuleOp>();
    auto globalOp =
        moduleOp.lookupSymbol<mlir::LLVM::GlobalOp>(op.getSymName());
    if (!globalOp)
      return op.emitOpError("referenced global string not found");

    // Get the string length from the array type (minus null terminator)
    auto arrayType =
        llvm::dyn_cast<mlir::LLVM::LLVMArrayType>(globalOp.getType());
    if (!arrayType)
      return op.emitOpError("global is not an array type");
    size_t strLen = arrayType.getNumElements() - 1; // exclude null terminator

    // Get address of the global string
    auto strPtr = rewriter.create<mlir::LLVM::AddressOfOp>(loc, llvmPtrType,
                                                           op.getSymName());

    // Create the length constant
    auto lenVal = rewriter.create<mlir::arith::ConstantOp>(
        loc, mlir::IntegerAttr::get(indexType, strLen));

    // Build the struct { ptr, len }
    auto undef = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);
    auto withPtr =
        rewriter.create<mlir::LLVM::InsertValueOp>(loc, undef, strPtr, 0);
    auto withLen =
        rewriter.create<mlir::LLVM::InsertValueOp>(loc, withPtr, lenVal, 1);

    rewriter.replaceOp(op, withLen);
    return mlir::success();
  }
};

struct ReussirStrLenOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrLenOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrLenOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    // Extract the length field (index 1) from the string struct
    rewriter.replaceOpWithNewOp<mlir::LLVM::ExtractValueOp>(
        op, adaptor.getStr(), llvm::ArrayRef<int64_t>{1});
    return mlir::success();
  }
};

struct ReussirStrUnsafeByteAtOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrUnsafeByteAtOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrUnsafeByteAtOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto i8Type = rewriter.getI8Type();

    // Extract the pointer field (index 0) from the string struct
    auto ptr = rewriter.create<mlir::LLVM::ExtractValueOp>(
        loc, adaptor.getStr(), llvm::ArrayRef<int64_t>{0});

    // Calculate the address of the character
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto charPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, ptrType, i8Type, ptr, mlir::ValueRange{adaptor.getIndex()});

    // Load the character
    rewriter.replaceOpWithNewOp<mlir::LLVM::LoadOp>(op, i8Type, charPtr);

    return mlir::success();
  }
};

struct ReussirStrUnsafeStartWithOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrUnsafeStartWithOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrUnsafeStartWithOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    mlir::ModuleOp module = op->getParentOfType<mlir::ModuleOp>();
    llvm::StringRef prefix = op.getPrefix();
    size_t len = prefix.size();

    // Extract the pointer field (index 0) from the string struct
    auto ptr = rewriter.create<mlir::LLVM::ExtractValueOp>(
        loc, adaptor.getStr(), llvm::ArrayRef<int64_t>{0});

    // Use memcmp for all prefix lengths as LLVM optimizes it well.
    // 1. Create global string for prefix
    // Use hash to avoid duplicate strings (similar to PanicOp)
    llvm::ArrayRef<uint8_t> messageBytes(
        reinterpret_cast<const uint8_t *>(prefix.data()), prefix.size());
    llvm::XXH128_hash_t hash = llvm::xxh3_128bits(messageBytes);
    std::string globalName = "__str_prefix_" + llvm::utohexstr(hash.high64) +
                             "_" + llvm::utohexstr(hash.low64);

    auto existingGlobal = module.lookupSymbol<mlir::LLVM::GlobalOp>(globalName);
    if (!existingGlobal) {
      auto i8Type = rewriter.getI8Type();
      auto arrayType = mlir::LLVM::LLVMArrayType::get(i8Type, prefix.size());
      auto stringAttr = rewriter.getStringAttr(prefix);
      mlir::OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(module.getBody());
      rewriter.create<mlir::LLVM::GlobalOp>(loc, arrayType, true,
                                            mlir::LLVM::Linkage::Internal,
                                            globalName, stringAttr);
    }

    // 2. Get address of global string
    auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto prefixPtr =
        rewriter.create<mlir::LLVM::AddressOfOp>(loc, llvmPtrType, globalName);

    // 3. Declare memcmp if needed
    // declare i32 @memcmp(ptr, ptr, i64)
    if (!module.lookupSymbol<mlir::LLVM::LLVMFuncOp>("memcmp")) {
      mlir::OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(module.getBody());
      auto i32Type = rewriter.getI32Type();
      auto i64Type = rewriter.getI64Type();
      auto fnType = mlir::LLVM::LLVMFunctionType::get(
          i32Type, {llvmPtrType, llvmPtrType, i64Type});
      rewriter.create<mlir::LLVM::LLVMFuncOp>(loc, "memcmp", fnType);
    }

    // 4. Call memcmp
    auto lenVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, len, 64);
    auto call = rewriter.create<mlir::LLVM::CallOp>(
        loc, rewriter.getI32Type(),
        mlir::SymbolRefAttr::get(rewriter.getContext(), "memcmp"),
        mlir::ValueRange{ptr, prefixPtr, lenVal});

    // 5. Compare result == 0
    auto zero = rewriter.create<mlir::arith::ConstantIntOp>(loc, 0, 32);
    auto res = rewriter.create<mlir::LLVM::ICmpOp>(
        loc, mlir::LLVM::ICmpPredicate::eq, call.getResult(), zero);
    rewriter.replaceOp(op, res);

    return mlir::success();
  }
};

struct ReussirStrSliceOpConversionPattern
    : public mlir::OpConversionPattern<ReussirStrSliceOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirStrSliceOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {

    mlir::Location loc = op.getLoc();
    auto startOffset = adaptor.getOffset();

    // 1. Get length and pointer
    auto ptr = rewriter.create<mlir::LLVM::ExtractValueOp>(
        loc, adaptor.getStr(), llvm::ArrayRef<int64_t>{0});
    auto len = rewriter.create<mlir::LLVM::ExtractValueOp>(
        loc, adaptor.getStr(), llvm::ArrayRef<int64_t>{1});

    // 2. Check bounds
    // offset > len ?
    auto outOfBounds = rewriter.create<mlir::LLVM::ICmpOp>(
        loc, mlir::LLVM::ICmpPredicate::ugt, startOffset, len);

    // 3. Select new length and pointer adjustment
    // If outOfBounds, newLen = 0, newPtr = ptr (or undefined)
    // Actually, safest is:
    // adjustment = min(offset, len)
    // newLen = len - adjustment
    // newPtr = ptr + adjustment

    // adjustment = min(offset, len)
    // umin is not directly available in LLVM dialect as a single op usually?
    // Use select.
    auto adjustment = rewriter.create<mlir::LLVM::SelectOp>(loc, outOfBounds,
                                                            len, startOffset);
    auto newLen = rewriter.create<mlir::LLVM::SubOp>(loc, len, adjustment);

    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto newPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, ptrType, rewriter.getI8Type(), ptr, mlir::ValueRange{adjustment});

    // 4. Create new struct
    auto structType = getTypeConverter()->convertType(op.getType());
    auto newStr = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);
    auto s1 = rewriter.create<mlir::LLVM::InsertValueOp>(
        loc, newStr, newPtr, llvm::ArrayRef<int64_t>{0});
    auto s2 = rewriter.create<mlir::LLVM::InsertValueOp>(
        loc, s1, newLen, llvm::ArrayRef<int64_t>{1});

    rewriter.replaceOp(op, s2.getResult());
    return mlir::success();
  }
};

struct ReussirRefDiffConversionPattern
    : public mlir::OpConversionPattern<ReussirRefDiffOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefDiffOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto indexType = converter->getIndexType();

    // Convert base and target pointers to integers
    mlir::Value baseInt = rewriter.create<mlir::LLVM::PtrToIntOp>(
        op.getLoc(), indexType, adaptor.getBase());
    mlir::Value targetInt = rewriter.create<mlir::LLVM::PtrToIntOp>(
        op.getLoc(), indexType, adaptor.getTarget());

    // Compute difference: target - base
    rewriter.replaceOpWithNewOp<mlir::arith::SubIOp>(
        op, targetInt, baseInt, mlir::arith::IntegerOverflowFlags::nsw);
    return mlir::success();
  }
};

struct ReussirRefCmpConversionPattern
    : public mlir::OpConversionPattern<ReussirRefCmpOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefCmpOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());
    auto indexType = converter->getIndexType();

    // Convert lhs and rhs pointers to integers
    mlir::Value lhsInt = rewriter.create<mlir::LLVM::PtrToIntOp>(
        op.getLoc(), indexType, adaptor.getLhs());
    mlir::Value rhsInt = rewriter.create<mlir::LLVM::PtrToIntOp>(
        op.getLoc(), indexType, adaptor.getRhs());

    // Perform the comparison
    rewriter.replaceOpWithNewOp<mlir::arith::CmpIOp>(op, op.getPredicate(),
                                                     lhsInt, rhsInt);
    return mlir::success();
  }
};

struct ReussirRefMemcpyConversionPattern
    : public mlir::OpConversionPattern<ReussirRefMemcpyOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRefMemcpyOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto converter = static_cast<const LLVMTypeConverter *>(getTypeConverter());

    // Get the element type and its size
    RefType srcType = op.getSrc().getType();
    mlir::Type elementType = converter->convertType(srcType.getElementType());
    size_t size = converter->getDataLayout().getTypeSize(elementType);

    // Create LLVM memcpy intrinsic (non-overlapping, so isVolatile = false)
    rewriter.replaceOpWithNewOp<mlir::LLVM::MemcpyInlineOp>(
        op, adaptor.getDst(), adaptor.getSrc(),
        rewriter.getIntegerAttr(converter->getIndexType(), size),
        /*isVolatile=*/false);
    return mlir::success();
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// Runtime Functions
//===----------------------------------------------------------------------===//

namespace {
void addRuntimeFunction(mlir::Block *body, llvm::StringRef name,
                        llvm::ArrayRef<mlir::Type> inputs,
                        llvm::ArrayRef<mlir::Type> outputs) {
  mlir::MLIRContext *ctx = body->getParentOp()->getContext();
  mlir::FunctionType type = mlir::FunctionType::get(ctx, inputs, outputs);
  mlir::func::FuncOp func =
      mlir::func::FuncOp::create(mlir::UnknownLoc::get(ctx), name, type);
  func.setPrivate();
  body->push_front(func);
}

void addRuntimeFunctions(mlir::ModuleOp module,
                         const LLVMTypeConverter &converter) {
  mlir::MLIRContext *ctx = module.getContext();
  mlir::Block *body = module.getBody();
  auto llvmPtrType = mlir::LLVM::LLVMPointerType::get(ctx);
  auto indexType = mlir::IndexType::get(ctx);
  addRuntimeFunction(body, "__reussir_freeze_flex_object", {llvmPtrType},
                     {llvmPtrType});
  addRuntimeFunction(body, "__reussir_cleanup_region", {llvmPtrType}, {});
  addRuntimeFunction(body, "__reussir_acquire_rigid_object", {llvmPtrType}, {});
  addRuntimeFunction(body, "__reussir_release_rigid_object", {llvmPtrType}, {});
  addRuntimeFunction(body, "__reussir_allocate", {indexType, indexType},
                     {llvmPtrType});
  addRuntimeFunction(body, "__reussir_deallocate",
                     {llvmPtrType, indexType, indexType}, {});
  addRuntimeFunction(body, "__reussir_reallocate",
                     {llvmPtrType, indexType, indexType, indexType, indexType},
                     {llvmPtrType});
  // currently this will abort execution after printing the message and
  // stacktrace. No unwinding is attempted yet.
  addRuntimeFunction(body, "__reussir_panic", {llvmPtrType, indexType}, {});
}

// Helper function to extract line and column from MLIR locations.
// Walks through FusedLoc to find the first FileLineColLoc with valid line info.
std::pair<unsigned, unsigned> extractLineCol(mlir::Location loc) {
  if (auto fileLoc = llvm::dyn_cast<mlir::FileLineColLoc>(loc))
    return {fileLoc.getLine(), fileLoc.getColumn()};
  if (auto fusedLoc = llvm::dyn_cast<mlir::FusedLoc>(loc)) {
    for (auto inner : fusedLoc.getLocations()) {
      auto [line, col] = extractLineCol(inner);
      if (line != 0)
        return {line, col};
    }
  }
  if (auto nameLoc = llvm::dyn_cast<mlir::NameLoc>(loc))
    return extractLineCol(nameLoc.getChildLoc());
  if (auto callSiteLoc = llvm::dyn_cast<mlir::CallSiteLoc>(loc))
    return extractLineCol(callSiteLoc.getCallee());
  return {0, 0};
}

mlir::Type getUnderlyingTypeFromDbgAttr(mlir::Attribute dbgAttr) {
  return llvm::TypeSwitch<mlir::Attribute, mlir::Type>(dbgAttr)
      .template Case<DBGFPTypeAttr>(
          [](DBGFPTypeAttr fptAttr) { return fptAttr.getInnerType(); })
      .template Case<DBGIntTypeAttr>(
          [](DBGIntTypeAttr intAttr) { return intAttr.getInnerType(); })
      .template Case<DBGRecordTypeAttr>(
          [](DBGRecordTypeAttr recAttr) { return recAttr.getUnderlyingType(); })
      .Default([](auto attr) { return mlir::Type{}; });
}
template <typename RetType = mlir::Attribute>
RetType translateDBGAttrToLLVM(mlir::ModuleOp moduleOp, mlir::Attribute dbgAttr,
                               mlir::LLVM::DIFileAttr diFile,
                               mlir::LLVM::DICompileUnitAttr diCU,
                               mlir::LLVM::LLVMFuncOp funcOp,
                               mlir::LLVM::DIScopeAttr funcScope,
                               mlir::Location loc) {
  mlir::DataLayout dataLayout{moduleOp};
  return llvm::dyn_cast_if_present<RetType>(
      llvm::TypeSwitch<mlir::Attribute, mlir::Attribute>(dbgAttr)
          .template Case<DBGFPTypeAttr>([&](DBGFPTypeAttr fptAttr) {
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(fptAttr.getInnerType());
            return mlir::LLVM::DIBasicTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_base_type,
                fptAttr.getDbgName(), sizeInBits, llvm::dwarf::DW_ATE_float);
          })
          .template Case<DBGIntTypeAttr>([&](DBGIntTypeAttr intAttr) {
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(intAttr.getInnerType());
            return mlir::LLVM::DIBasicTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_base_type,
                intAttr.getDbgName(), sizeInBits,
                intAttr.getIsSigned() ? llvm::dwarf::DW_ATE_signed
                                      : llvm::dwarf::DW_ATE_unsigned);
          })
          // TODO: we ignore boxed and variant for now
          .template Case<DBGRecordTypeAttr>([&](DBGRecordTypeAttr recAttr)
                                                -> mlir::Attribute {
            if (recAttr.getIsVariant())
              return nullptr;
            llvm::SmallVector<mlir::LLVM::DINodeAttr> members;
            size_t currentOffset = 0;
            for (auto element : recAttr.getMembers()) {
              auto memberAttr = llvm::dyn_cast<DBGRecordMemberAttr>(element);
              if (!memberAttr)
                return nullptr;
              auto memberTy = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                  moduleOp, memberAttr.getTypeAttr(), diFile, diCU, funcOp,
                  funcScope, loc);
              if (!memberTy)
                return nullptr;
              auto memberUnderlyingTy =
                  getUnderlyingTypeFromDbgAttr(memberAttr.getTypeAttr());
              if (!memberUnderlyingTy)
                return nullptr;
              auto sizeInBits =
                  dataLayout.getTypeSizeInBits(memberUnderlyingTy);
              auto alignInBytes =
                  dataLayout.getTypeABIAlignment(memberUnderlyingTy);
              currentOffset = llvm::alignTo(currentOffset, alignInBytes);
              // align current offset
              members.push_back(mlir::LLVM::DIDerivedTypeAttr::get(
                  moduleOp->getContext(), llvm::dwarf::DW_TAG_member,
                  memberAttr.getName(), memberTy, sizeInBits, alignInBytes * 8,
                  currentOffset * 8,
                  /*address space=*/std::nullopt, /*extraData=*/nullptr));
              currentOffset += sizeInBits / 8;
            }
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(recAttr.getUnderlyingType());
            auto alignInBits =
                dataLayout.getTypeABIAlignment(recAttr.getUnderlyingType()) * 8;
#if LLVM_VERSION_MAJOR >= 22
            return mlir::LLVM::DICompositeTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_class_type,
                recAttr.getDbgName(), /*file=*/diFile, /*line=*/0, diCU,
                /*baseType=*/nullptr, /*flags=*/mlir::LLVM::DIFlags::Zero,
                sizeInBits, alignInBits,
                /*dataLocation=*/nullptr, /*rank=*/nullptr,
                /*allocated=*/nullptr, /*associated=*/nullptr, members);
#else
            return mlir::LLVM::DICompositeTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_class_type,
                recAttr.getDbgName(), /*file=*/diFile, /*line=*/0, diCU,
                /*baseType=*/nullptr, /*flags=*/mlir::LLVM::DIFlags::Zero,
                sizeInBits, alignInBits, members, nullptr, nullptr, nullptr,
                nullptr);
#endif
          })
          .template Case<DBGSubprogramAttr>(
              [&](DBGSubprogramAttr spAttr) -> mlir::Attribute {
                auto linkageName = funcOp.getSymNameAttr();
                llvm::SmallVector<mlir::LLVM::DINodeAttr> argTypes;
                for (auto paramAttr : spAttr.getTypeParams()) {
                  auto paramTy = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                      moduleOp, paramAttr, diFile, diCU, funcOp, funcScope,
                      loc);
                  if (!paramTy)
                    return nullptr;
                  argTypes.push_back(paramTy);
                }
                // TODO: function type is not emitted now
                auto emptyRoutine = mlir::LLVM::DISubroutineTypeAttr::get(
                    moduleOp.getContext(), {});
                // Extract line/column from the function's location
                auto [line, col] = extractLineCol(loc);
                auto res = mlir::LLVM::DISubprogramAttr::get(
                    moduleOp.getContext(),
                    mlir::DistinctAttr::create(
                        mlir::UnitAttr::get(moduleOp.getContext())),
                    false,
                    mlir::DistinctAttr::create(
                        mlir::UnitAttr::get(moduleOp.getContext())),
                    diCU, diFile, spAttr.getRawName(), linkageName, diFile,
                    line, col, mlir::LLVM::DISubprogramFlags::Definition,
                    emptyRoutine, {}, {});

                return res;
              })
          .template Case<DBGLocalVarAttr>(
              [&](DBGLocalVarAttr localVarAttr) -> mlir::Attribute {
                auto underlyingTy =
                    getUnderlyingTypeFromDbgAttr(localVarAttr.getDbgType());
                auto varType = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                    moduleOp, localVarAttr.getDbgType(), diFile, diCU, funcOp,
                    funcScope, loc);
                if (!varType)
                  return nullptr;
                auto scope = funcScope ? funcScope : diFile;
                auto alignInBits =
                    dataLayout.getTypeABIAlignment(underlyingTy) * 8;
                // Extract line/column from the operation's location
                // Note: 5th parameter is 'arg' (argument number), not column.
                // For local variables (not function parameters), arg should be
                // 0.
                auto [line, col] = extractLineCol(loc);
                (void)col; // Column is not used in DILocalVariable
                return mlir::LLVM::DILocalVariableAttr::get(
                    moduleOp.getContext(), scope, localVarAttr.getVarName(),
                    diFile, line, /*arg=*/0, alignInBits, varType,
                    mlir::LLVM::DIFlags::Zero);
              })
          .template Case<DBGFuncArgAttr>(
              [&](DBGFuncArgAttr funcArgAttr) -> mlir::Attribute {
                auto underlyingTy =
                    getUnderlyingTypeFromDbgAttr(funcArgAttr.getDbgType());
                auto varType = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                    moduleOp, funcArgAttr.getDbgType(), diFile, diCU, funcOp,
                    funcScope, loc);
                if (!varType)
                  return nullptr;
                auto scope = funcScope ? funcScope : diFile;
                auto alignInBits =
                    dataLayout.getTypeABIAlignment(underlyingTy) * 8;
                // For function arguments, use the 1-based arg index
                auto [line, col] = extractLineCol(loc);
                (void)col;
                return mlir::LLVM::DILocalVariableAttr::get(
                    moduleOp.getContext(), scope, funcArgAttr.getArgName(),
                    diFile, line, funcArgAttr.getArgIndex(), alignInBits,
                    varType, mlir::LLVM::DIFlags::Zero);
              })
          .Default([](auto attr) { return attr; }));
}
void lowerFusedDBGAttributeInLocations(mlir::ModuleOp moduleOp) {
  auto context = moduleOp.getContext();
  auto fileBasename = mlir::dyn_cast_if_present<mlir::StringAttr>(
      moduleOp->getAttr("reussir.dbg.file_basename"));
  auto fileDirectory = mlir::dyn_cast_if_present<mlir::StringAttr>(
      moduleOp->getAttr("reussir.dbg.file_directory"));
  if (!fileBasename || !fileDirectory)
    return;
  auto llvmDIFileAttr =
      mlir::LLVM::DIFileAttr::get(context, fileBasename, fileDirectory);
  auto dbgCompileUnitAttr = mlir::LLVM::DICompileUnitAttr::get(
      mlir::DistinctAttr::create(mlir::UnitAttr::get(context)),
      llvm::dwarf::DW_LANG_C_plus_plus_20, llvmDIFileAttr,
      mlir::StringAttr::get(context, "reussir"), true,
      mlir::LLVM::DIEmissionKind::Full);
  mlir::OpBuilder builder(moduleOp);
  moduleOp->walk([&](mlir::LLVM::LLVMFuncOp funcOp) {
    mlir::LocationAttr funcLoc = funcOp->getLoc();
    if (auto fused =
            llvm::dyn_cast_if_present<mlir::FusedLocWith<DBGSubprogramAttr>>(
                funcLoc)) {

      auto subprogram = translateDBGAttrToLLVM<mlir::LLVM::DISubprogramAttr>(
          moduleOp, fused.getMetadata(), llvmDIFileAttr, dbgCompileUnitAttr,
          funcOp, nullptr, funcLoc);
      if (subprogram) {
        auto updated =
            mlir::FusedLoc::get(context, fused.getLocations(), subprogram);
        funcOp->setLoc(updated);
      }

      // Process function argument debug info attribute
      // Note: Use funcOp->getLoc() which has the updated subprogram
      auto updatedFuncLoc = funcOp->getLoc();
      if (auto dbgFuncArgsAttr =
              funcOp->getAttrOfType<mlir::ArrayAttr>("reussir.dbg_func_args")) {
        for (auto [idx, argAttr] :
             llvm::enumerate(dbgFuncArgsAttr.getValue())) {
          if (auto funcArgAttr = mlir::dyn_cast<DBGFuncArgAttr>(argAttr)) {
            auto translated =
                translateDBGAttrToLLVM<mlir::LLVM::DILocalVariableAttr>(
                    moduleOp, funcArgAttr, llvmDIFileAttr, dbgCompileUnitAttr,
                    funcOp, subprogram, updatedFuncLoc);
            if (translated && idx < funcOp.getNumArguments()) {
              auto argValue = funcOp.getArgument(idx);
              auto &entryBlock = funcOp.getBody().front();
              builder.setInsertionPointToStart(&entryBlock);
              builder.create<mlir::LLVM::DbgValueOp>(updatedFuncLoc, argValue,
                                                     translated);
            }
          }
        }
        // Remove the attribute after processing
        funcOp->removeAttr("reussir.dbg_func_args");
      }
      funcOp->walk([&](mlir::Operation *op) {
        mlir::LocationAttr opLoc = op->getLoc();
        if (auto innerFused =
                llvm::dyn_cast_if_present<mlir::FusedLoc>(opLoc)) {
          auto translated = translateDBGAttrToLLVM(
              moduleOp, innerFused.getMetadata(), llvmDIFileAttr,
              dbgCompileUnitAttr, funcOp, subprogram, opLoc);
          if (translated) {
            if (auto localVar = mlir::dyn_cast<mlir::LLVM::DILocalVariableAttr>(
                    translated)) {
              auto value = op->getResult(0);
              builder.setInsertionPointAfterValue(value);
              builder.create<mlir::LLVM::DbgValueOp>(op->getLoc(), value,
                                                     localVar);
            }
            auto updatedInnerLoc =
                mlir::FusedLoc::get(context, fused.getLocations(), translated);
            op->setLoc(updatedInnerLoc);
          }
        }
      });
    }
  });
}
} // namespace

//===----------------------------------------------------------------------===//
// BasicOpsLoweringPass
//===----------------------------------------------------------------------===//

namespace {
struct BasicOpsLoweringPass
    : public impl::ReussirBasicOpsLoweringPassBase<BasicOpsLoweringPass> {
  using Base::Base;
  void runOnOperation() override {
    {
      mlir::LLVMConversionTarget target(getContext());
      mlir::RewritePatternSet patterns(&getContext());
      LLVMTypeConverter converter(getOperation());
      populateBasicOpsLoweringToLLVMConversionPatterns(converter, patterns);
      mlir::populateFuncToLLVMFuncOpConversionPattern(converter, patterns);
      mlir::populateFuncToLLVMConversionPatterns(converter, patterns);
      mlir::arith::populateArithToLLVMConversionPatterns(converter, patterns);
      mlir::cf::populateControlFlowToLLVMConversionPatterns(converter,
                                                            patterns);
      mlir::populateMathToLLVMConversionPatterns(converter, patterns);
      mlir::populateMathToLibmConversionPatterns(patterns);
      mlir::ub::populateUBToLLVMConversionPatterns(converter, patterns);
      addRuntimeFunctions(getOperation(), converter);
      target.addIllegalDialect<mlir::func::FuncDialect,
                               mlir::arith::ArithDialect>();
      target.addIllegalOp<
          ReussirTokenAllocOp, ReussirTokenFreeOp, ReussirTokenReinterpretOp,
          ReussirTokenReallocOp, ReussirRefLoadOp, ReussirRefStoreOp,
          ReussirRefSpilledOp, ReussirRefDiffOp, ReussirRefCmpOp,
          ReussirRefMemcpyOp, ReussirNullableCheckOp, ReussirNullableCreateOp,
          ReussirNullableCoerceOp, ReussirRcIncOp, ReussirRcCreateOp,
          ReussirRcDecOp, ReussirRcBorrowOp, ReussirRcIsUniqueOp,
          ReussirRecordCompoundOp, ReussirRecordVariantOp, ReussirRefProjectOp,
          ReussirRecordTagOp, ReussirRecordExtractOp, ReussirRecordCoerceOp,
          ReussirRegionVTableOp, ReussirRcFreezeOp, ReussirRegionCleanupOp,
          ReussirRegionCreateOp, ReussirRcReinterpretOp, ReussirClosureApplyOp,
          ReussirClosureCloneOp, ReussirClosureEvalOp,
          ReussirClosureInspectPayloadOp, ReussirClosureCursorOp,
          ReussirClosureInstantiateOp, ReussirClosureVtableOp,
          ReussirClosureCreateOp, ReussirRcFetchDecOp, ReussirStrGlobalOp,
          ReussirStrLiteralOp, ReussirPanicOp, ReussirStrLenOp,
          ReussirStrUnsafeByteAtOp, ReussirStrUnsafeStartWithOp,
          ReussirStrSliceOp>();
      target.addLegalDialect<mlir::LLVM::LLVMDialect>();
      if (failed(applyPartialConversion(getOperation(), target,
                                        std::move(patterns))))
        signalPassFailure();
    }
    lowerFusedDBGAttributeInLocations(getOperation());
  }
};
} // namespace

void populateBasicOpsLoweringToLLVMConversionPatterns(
    LLVMTypeConverter &converter, mlir::RewritePatternSet &patterns) {
  patterns.add<
      ReussirTokenAllocConversionPattern, ReussirTokenFreeConversionPattern,
      ReussirTokenReinterpretConversionPattern,
      ReussirTokenReallocConversionPattern, ReussirRefLoadConversionPattern,
      ReussirRefStoreConversionPattern, ReussirRefSpilledConversionPattern,
      ReussirRefDiffConversionPattern, ReussirRefCmpConversionPattern,
      ReussirRefMemcpyConversionPattern, ReussirNullableCheckConversionPattern,
      ReussirNullableCreateConversionPattern,
      ReussirNullableCoerceConversionPattern, ReussirRcIncConversionPattern,
      ReussirRcDecOpConversionPattern, ReussirRcCreateOpConversionPattern,
      ReussirRcBorrowOpConversionPattern, ReussirRcIsUniqueOpConversionPattern,
      ReussirRecordCompoundConversionPattern,
      ReussirRecordExtractConversionPattern,
      ReussirRecordVariantConversionPattern,
      ReussirReferenceProjectConversionPattern,
      ReussirRecordTagConversionPattern, ReussirRecordCoerceConversionPattern,
      ReussirRegionVTableOpConversionPattern,
      ReussirRcFreezeOpConversionPattern,
      ReussirRegionCleanupOpConversionPattern,
      ReussirRegionCreateOpConversionPattern,
      ReussirClosureApplyOpConversionPattern,
      ReussirClosureCloneOpConversionPattern,
      ReussirClosureEvalOpConversionPattern,
      ReussirClosureInspectPayloadOpConversionPattern,
      ReussirClosureCursorOpConversionPattern,
      ReussirClosureTransferOpConversionPattern,
      ReussirClosureInstantiateOpConversionPattern,
      ReussirClosureVtableOpConversionPattern,
      ReussirClosureCreateOpConversionPattern,
      ReussirRcReinterpretConversionPattern,
      ReussirRcFetchDectConversionPattern, ReussirStrGlobalOpConversionPattern,
      ReussirStrLiteralOpConversionPattern, ReussirPanicConversionPattern,
      ReussirStrLenOpConversionPattern,
      ReussirStrUnsafeByteAtOpConversionPattern,
      ReussirStrUnsafeStartWithOpConversionPattern,
      ReussirStrSliceOpConversionPattern>(converter, patterns.getContext());
}
} // namespace reussir
