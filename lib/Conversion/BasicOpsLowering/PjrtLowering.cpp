// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/LLVMIR/FunctionCallUtils.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Interfaces/FunctionInterfaces.h>
#include <numeric>

namespace reussir {
namespace {
#ifdef REUSSIR_ENABLE_OPENXLA
mlir::Value integer(mlir::OpBuilder &builder, mlir::Location loc,
                    mlir::Type type, int64_t value) {
  return mlir::LLVM::ConstantOp::create(builder, loc, type,
                                        mlir::IntegerAttr::get(type, value));
}

mlir::Value bytesAddress(mlir::Operation *op, llvm::StringRef bytes,
                         mlir::ConversionPatternRewriter &rewriter) {
  auto type =
      mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), bytes.size());
  auto name = mangledBlake3Symbol("REUSSIR_PJRT_DATA", bytes);
  auto module = op->getParentOfType<mlir::ModuleOp>();
  auto global = module.lookupSymbol<mlir::LLVM::GlobalOp>(name);
  if (!global) {
    mlir::OpBuilder::InsertionGuard guard(rewriter);
    rewriter.setInsertionPointToStart(module.getBody());
    global = mlir::LLVM::GlobalOp::create(rewriter, op->getLoc(), type, true,
                                          mlir::LLVM::Linkage::LinkonceODR,
                                          name, rewriter.getStringAttr(bytes));
  }
  return mlir::LLVM::AddressOfOp::create(rewriter, op->getLoc(), global);
}

mlir::Value slots(mlir::Operation *op, mlir::Type element, mlir::Type index,
                  size_t count, mlir::ConversionPatternRewriter &rewriter) {
  auto ptr = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
  if (count == 0)
    return mlir::LLVM::ZeroOp::create(rewriter, op->getLoc(), ptr);
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  auto function = op->getParentOfType<mlir::FunctionOpInterface>();
  rewriter.setInsertionPointToStart(&function.getFunctionBody().front());
  auto size = integer(rewriter, op->getLoc(), index, count);
  return mlir::LLVM::AllocaOp::create(rewriter, op->getLoc(), ptr, element,
                                      size);
}

void storeSlot(mlir::Operation *op, mlir::Value storage, unsigned index,
               mlir::Value value, mlir::ConversionPatternRewriter &rewriter) {
  auto address = mlir::LLVM::GEPOp::create(
      rewriter, op->getLoc(), storage.getType(), value.getType(), storage,
      llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(index)});
  mlir::LLVM::StoreOp::create(rewriter, op->getLoc(), value, address);
}

#endif

struct Bytecode : mlir::ConvertOpToLLVMPattern<ReussirIFRTBytecodeOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirIFRTBytecodeOp op, OpAdaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto bytesType = mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(),
                                                    op.getBytecode().size());
    auto checksumType =
        mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), 64);
    auto type = mlir::LLVM::LLVMStructType::getLiteral(
        getContext(), {checksumType, bytesType});
    auto global = mlir::LLVM::GlobalOp::create(
        rewriter, op.getLoc(), type, true, mlir::LLVM::Linkage::Internal,
        op.getSymName(), mlir::Attribute{});
    {
      mlir::OpBuilder::InsertionGuard guard(rewriter);
      rewriter.createBlock(&global.getInitializerRegion());
      auto constantBytes = [&](llvm::StringRef bytes, mlir::Type type) {
        llvm::SmallVector<llvm::APInt> data;
        for (unsigned char byte : bytes)
          data.emplace_back(8, byte);
        auto value = mlir::DenseIntElementsAttr::get(
            mlir::RankedTensorType::get({static_cast<int64_t>(bytes.size())},
                                        rewriter.getI8Type()),
            data);
        return mlir::LLVM::ConstantOp::create(rewriter, op.getLoc(), type,
                                              value);
      };
      auto bytes = constantBytes(op.getBytecode(), bytesType);
      auto checksum = constantBytes(op.getChecksum(), checksumType);
      auto empty = mlir::LLVM::PoisonOp::create(rewriter, op.getLoc(), type);
      auto withChecksum = mlir::LLVM::InsertValueOp::create(
          rewriter, op.getLoc(), empty, checksum, llvm::ArrayRef<int64_t>{0});
      auto complete =
          mlir::LLVM::InsertValueOp::create(rewriter, op.getLoc(), withChecksum,
                                            bytes, llvm::ArrayRef<int64_t>{1});
      mlir::LLVM::ReturnOp::create(rewriter, op.getLoc(), complete);
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct JitCall : mlir::ConvertOpToLLVMPattern<ReussirPJRTJitCallOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirPJRTJitCallOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
#ifndef REUSSIR_ENABLE_OPENXLA
    return op.emitOpError("requires REUSSIR_ENABLE_OPENXLA");
#else
    if (!op->getParentOfType<mlir::FunctionOpInterface>())
      return op.emitOpError("requires an enclosing function");
    if (!op.getIoAliases().empty() || !op.getDonatedInputIndices().empty())
      return op.emitOpError(
          "runtime execution does not support donation or aliases");
    for (auto type : llvm::concat<mlir::Type>(op.getInputs().getTypes(),
                                              op.getOutputs().getTypes()))
      if (getIfrtArrayDevices(type) != op.getDevices())
        return op.emitOpError(
            "runtime arrays must use all call devices in call order");
    for (auto output : op.getOutputs())
      if (!output.hasOneUse() ||
          !llvm::isa<ReussirArrayFromIfrtOp>(*output.getUsers().begin()))
        return op.emitOpError(
            "runtime results must be adopted once with array.from_ifrt");

    if (!op.getCalleeAttr().getNestedReferences().empty())
      return op.emitOpError("runtime callee must be in the same symbol table");
    auto *code =
        mlir::SymbolTable::lookupNearestSymbolFrom(op, op.getCalleeAttr());
    mlir::LLVM::LLVMStructType codeType;
    if (auto original = llvm::dyn_cast_or_null<ReussirIFRTBytecodeOp>(code))
      codeType = mlir::LLVM::LLVMStructType::getLiteral(
          getContext(),
          {mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), 64),
           mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(),
                                          original.getBytecode().size())});
    else if (auto global = llvm::dyn_cast_or_null<mlir::LLVM::GlobalOp>(code))
      codeType =
          llvm::dyn_cast<mlir::LLVM::LLVMStructType>(global.getGlobalType());
    if (!codeType)
      return op.emitOpError("cannot resolve kernel bytecode");
    auto byteCount =
        llvm::cast<mlir::LLVM::LLVMArrayType>(codeType.getBody()[1])
            .getNumElements();
    auto options = op.getCompileOptions().value_or(llvm::StringRef{});
    auto loc = op.getLoc();
    auto ptr = mlir::LLVM::LLVMPointerType::get(getContext());
    auto index = getTypeConverter()->getIndexType();
    auto voidTy = mlir::LLVM::LLVMVoidType::get(getContext());
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto compile =
        mlir::LLVM::lookupOrCreateFn(rewriter, module, "__reussir_pjrt_compile",
                                     {ptr, index, ptr, ptr, index}, ptr);
    auto validate = mlir::LLVM::lookupOrCreateFn(
        rewriter, module, "__reussir_pjrt_executable_validate_devices",
        {ptr, ptr, index}, voidTy);
    auto execute = mlir::LLVM::lookupOrCreateFn(
        rewriter, module, "__reussir_pjrt_executable_execute",
        {ptr, ptr, index, ptr, index, ptr, index}, voidTy);
    if (mlir::failed(compile) || mlir::failed(validate) ||
        mlir::failed(execute))
      return mlir::failure();
    auto length = [&](size_t n) { return integer(rewriter, loc, index, n); };
    auto nonDonatable = [&](llvm::ArrayRef<int32_t> arguments,
                            llvm::ArrayRef<int32_t> donors) {
      llvm::SmallVector<int64_t> indices;
      for (auto [i, original] : llvm::enumerate(arguments))
        if (!llvm::is_contained(donors, original))
          indices.push_back(i);
      auto size = length(indices.size());
      if (indices.empty())
        return std::make_pair(
            mlir::Value(mlir::LLVM::ZeroOp::create(rewriter, loc, ptr)), size);
      std::string key;
      for (int64_t i : indices)
        key += std::to_string(i) + ",";
      auto name = mangledBlake3Symbol("REUSSIR_PJRT_NON_DONATABLE", key);
      auto global = module.lookupSymbol<mlir::LLVM::GlobalOp>(name);
      if (!global) {
        mlir::OpBuilder::InsertionGuard guard(rewriter);
        rewriter.setInsertionPointToStart(module.getBody());
        auto type = mlir::LLVM::LLVMArrayType::get(rewriter.getI64Type(),
                                                   indices.size());
        global = mlir::LLVM::GlobalOp::create(rewriter, loc, type, true,
                                              mlir::LLVM::Linkage::LinkonceODR,
                                              name, mlir::Attribute{}, 8);
        rewriter.createBlock(&global.getInitializerRegion());
        auto value = mlir::LLVM::ConstantOp::create(
            rewriter, loc, type,
            mlir::DenseIntElementsAttr::get(
                mlir::RankedTensorType::get({int64_t(indices.size())},
                                            rewriter.getI64Type()),
                llvm::ArrayRef<int64_t>(indices)));
        mlir::LLVM::ReturnOp::create(rewriter, loc, value.getResult());
      }
      return std::make_pair(
          mlir::Value(mlir::LLVM::AddressOfOp::create(rewriter, loc, global)),
          size);
    };
    auto object = mlir::LLVM::AddressOfOp::create(
        rewriter, loc, ptr, op.getCalleeAttr().getRootReference().getValue());
    auto field = [&](int32_t i) {
      return mlir::LLVM::GEPOp::create(
          rewriter, loc, ptr, codeType, object,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, i, 0});
    };
    auto executable =
        mlir::LLVM::CallOp::create(
            rewriter, loc, *compile,
            mlir::ValueRange{field(1), length(byteCount), field(0),
                             bytesAddress(op, options, rewriter),
                             length(options.size())})
            .getResult();
    auto devices = slots(op, rewriter.getI32Type(), index,
                         op.getDevices().size(), rewriter);
    for (auto [i, device] : llvm::enumerate(op.getDevices()))
      storeSlot(op, devices, i,
                integer(rewriter, loc, rewriter.getI32Type(), device),
                rewriter);
    mlir::LLVM::CallOp::create(
        rewriter, loc, *validate,
        mlir::ValueRange{executable, devices, length(op.getDevices().size())});
    auto inputs = slots(op, ptr, index, adaptor.getInputs().size(), rewriter);
    for (auto [i, input] : llvm::enumerate(adaptor.getInputs()))
      storeSlot(op, inputs, i, input, rewriter);
    auto outputs = slots(op, ptr, index, op.getOutputs().size(), rewriter);
    llvm::SmallVector<mlir::Value> results;
    for (size_t i = 0; i < op.getOutputs().size(); ++i) {
      auto descriptor = slots(op, ptr, index, op.getDevices().size(), rewriter);
      storeSlot(op, outputs, i, descriptor, rewriter);
      results.push_back(descriptor);
    }
    llvm::SmallVector<int32_t> identity(op.getInputs().size());
    std::iota(identity.begin(), identity.end(), 0);
    auto [denied, deniedCount] = nonDonatable(identity, {});
    mlir::LLVM::CallOp::create(
        rewriter, loc, *execute,
        mlir::ValueRange{executable, inputs, length(op.getInputs().size()),
                         outputs, length(op.getOutputs().size()),
                         denied, deniedCount});
    // Execution returns only after every device completes. The resulting
    // control handle is therefore already ready; the side-effecting runtime
    // calls preserve ordering even though no token reaches the runtime ABI.
    results.push_back(integer(rewriter, loc, rewriter.getI1Type(), 1));
    rewriter.replaceOp(op, results);
    return mlir::success();
#endif
  }
};

#ifdef REUSSIR_ENABLE_OPENXLA
struct After : mlir::ConvertToLLVMPattern {
  After(llvm::StringRef name, mlir::LLVMTypeConverter &converter,
        mlir::MLIRContext *context)
      : ConvertToLLVMPattern(name, context, converter) {}
  mlir::LogicalResult
  matchAndRewrite(mlir::Operation *op, llvm::ArrayRef<mlir::Value> operands,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto ptr = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto wait = mlir::LLVM::lookupOrCreateFn(
        rewriter, op->getParentOfType<mlir::ModuleOp>(),
        "__reussir_pjrt_array_wait_ready", {ptr},
        mlir::LLVM::LLVMVoidType::get(rewriter.getContext()));
    if (mlir::failed(wait))
      return mlir::failure();
    for (auto [original, descriptor] :
         llvm::zip_equal(op->getOperands(), operands)) {
      for (size_t i = 0; i < getIfrtArrayDevices(original.getType()).size();
           ++i) {
        auto slot = mlir::LLVM::GEPOp::create(
            rewriter, op->getLoc(), ptr, ptr, descriptor,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(i)});
        auto handle =
            mlir::LLVM::LoadOp::create(rewriter, op->getLoc(), ptr, slot);
        mlir::LLVM::CallOp::create(rewriter, op->getLoc(), *wait,
                                   mlir::ValueRange{handle});
      }
    }
    rewriter.replaceOp(
        op, integer(rewriter, op->getLoc(), rewriter.getI1Type(), 1));
    return mlir::success();
  }
};
#endif
} // namespace

void populatePjrtLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                  mlir::RewritePatternSet &patterns) {
  patterns.add<Bytecode, JitCall>(converter);
#ifdef REUSSIR_ENABLE_OPENXLA
  patterns.add<After>("ifrt.After", converter, patterns.getContext());
  patterns.add<After>("reussir.pjrt.after", converter, patterns.getContext());
#endif
}
} // namespace reussir
