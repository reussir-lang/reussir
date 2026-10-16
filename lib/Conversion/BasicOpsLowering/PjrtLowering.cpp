// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirOps.h"
#include <llvm/ADT/BitVector.h>
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
    if ((!op.getIoAliases().empty() || !op.getDonatedInputIndices().empty()) &&
        !op->hasAttr("pjrt.donation_materialized"))
      return op.emitOpError("runtime execution does not support donation or "
                            "aliases before reussir-pjrt-donation");
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

    auto loc = op.getLoc();
    auto ptr = mlir::LLVM::LLVMPointerType::get(getContext());
    auto index = getTypeConverter()->getIndexType();
    auto voidTy = mlir::LLVM::LLVMVoidType::get(getContext());
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto compile =
        mlir::LLVM::lookupOrCreateFn(rewriter, module, "__reussir_pjrt_compile_specialized",
                                     {ptr, index, ptr, ptr, index, ptr, index}, ptr);
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
    auto codeFields = [&](mlir::SymbolRefAttr symbol)
        -> mlir::FailureOr<llvm::SmallVector<mlir::Value>> {
      if (!symbol.getNestedReferences().empty())
        return mlir::failure();
      auto *code = mlir::SymbolTable::lookupNearestSymbolFrom(op, symbol);
      mlir::LLVM::LLVMStructType type;
      if (auto original = llvm::dyn_cast_or_null<ReussirIFRTBytecodeOp>(code))
        type = mlir::LLVM::LLVMStructType::getLiteral(
            getContext(),
            {mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), 64),
             mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(),
                                            original.getBytecode().size())});
      else if (auto global = llvm::dyn_cast_or_null<mlir::LLVM::GlobalOp>(code))
        type =
            llvm::dyn_cast<mlir::LLVM::LLVMStructType>(global.getGlobalType());
      if (!type)
        return mlir::failure();
      auto object = mlir::LLVM::AddressOfOp::create(
          rewriter, loc, ptr, symbol.getRootReference().getValue());
      auto field = [&](int32_t i) {
        return mlir::LLVM::GEPOp::create(
                   rewriter, loc, ptr, type, object,
                   llvm::ArrayRef<mlir::LLVM::GEPArg>{0, i, 0})
            .getResult();
      };
      return llvm::SmallVector<mlir::Value>{
          field(1),
          length(llvm::cast<mlir::LLVM::LLVMArrayType>(type.getBody()[1])
                     .getNumElements()),
          field(0)};
    };
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
    auto fields = codeFields(op.getCalleeAttr());
    if (mlir::failed(fields))
      return op.emitOpError("cannot resolve kernel bytecode");
    llvm::SmallVector<int32_t> identity(op.getInputs().size());
    std::iota(identity.begin(), identity.end(), 0);
    auto inputs = slots(op, ptr, index, adaptor.getInputs().size(), rewriter);
    for (auto [i, input] : llvm::enumerate(adaptor.getInputs()))
      storeSlot(op, inputs, i, input, rewriter);
    // Matches pjrt::ffi::SpecializationArgument. Input descriptors borrowed
    // from array.to_ifrt retain the RC payload's global dynamic extents after
    // the handles and offset. Never reconstruct these from physical shards.
    auto argumentType = mlir::LLVM::LLVMStructType::getLiteral(
        getContext(), {ptr, index, ptr, index, rewriter.getI8Type()});
    auto shapeArguments = slots(op, argumentType, index, identity.size(), rewriter);
    llvm::SmallVector<mlir::Value> argumentValues;
    for (auto [i, original] : llvm::enumerate(op.getInputs())) {
      auto shape = getIfrtArrayShape(original.getType());
      auto dims = slots(op, rewriter.getI64Type(), index, shape.getRank(), rewriter);
      llvm::SmallVector<mlir::Type> payload{
          mlir::LLVM::LLVMArrayType::get(ptr, op.getDevices().size()), index};
      payload.append(shape.getNumDynamicDims(), index);
      bool shapeValueStorage =
          shape.getRank() == 0 && shape.getElementType().isInteger(64);
      if (shapeValueStorage) {
        payload.push_back(rewriter.getI8Type());
        payload.push_back(rewriter.getI64Type());
      }
      auto payloadType =
          mlir::LLVM::LLVMStructType::getLiteral(getContext(), payload);
      unsigned dynamicField = 2;
      for (auto [dim, size] : llvm::enumerate(shape.getShape())) {
        mlir::Value extent;
        if (mlir::ShapedType::isDynamic(size)) {
          auto address = mlir::LLVM::GEPOp::create(
              rewriter, loc, ptr, payloadType, adaptor.getInputs()[i],
              llvm::ArrayRef<mlir::LLVM::GEPArg>{0, int32_t(dynamicField++)});
          extent = mlir::LLVM::LoadOp::create(rewriter, loc, index, address);
          if (index.getIntOrFloatBitWidth() < 64)
            extent = mlir::LLVM::SExtOp::create(rewriter, loc, rewriter.getI64Type(), extent);
        } else {
          extent = integer(rewriter, loc, rewriter.getI64Type(), size);
        }
        storeSlot(op, dims, dim, extent, rewriter);
      }
      mlir::Value value = mlir::LLVM::PoisonOp::create(rewriter, loc, argumentType);
      llvm::SmallVector<mlir::Value> metadata{
          dims, length(shape.getRank()),
          mlir::LLVM::ZeroOp::create(rewriter, loc, ptr), length(0),
          integer(rewriter, loc, rewriter.getI8Type(), 0)};
      if (shapeValueStorage) {
        auto field = [&](int32_t index) {
          return mlir::LLVM::GEPOp::create(
              rewriter, loc, ptr, payloadType, adaptor.getInputs()[i],
              llvm::ArrayRef<mlir::LLVM::GEPArg>{0, index});
        };
        metadata[2] = field(ArrayType::SHAPE_VALUE_INDEX);
        metadata[3] = length(1);
        metadata[4] = mlir::LLVM::LoadOp::create(
            rewriter, loc, rewriter.getI8Type(),
            field(ArrayType::SHAPE_VALUE_PRESENT_INDEX));
      }
      for (auto [field, item] : llvm::enumerate(metadata))
        value = mlir::LLVM::InsertValueOp::create(
            rewriter, loc, value, item, llvm::ArrayRef<int64_t>{int64_t(field)});
      argumentValues.push_back(value);
      storeSlot(op, shapeArguments, i, value, rewriter);
    }
    mlir::Value selectedShapes = shapeArguments;
    mlir::Value inputCount = length(identity.size());
    llvm::SmallVector<int32_t> explicitDonors(op.getDonatedInputIndices());
    for (auto alias : op.getIoAliases())
      explicitDonors.push_back(llvm::cast<mlir::DenseI32ArrayAttr>(alias)[0]);
    auto [denied, deniedCount] = nonDonatable(identity, explicitDonors);
    llvm::SmallVector<llvm::SmallVector<mlir::Value>> handles;
    if (!op.getDonationVariants().empty()) {
      for (auto input : adaptor.getInputs()) {
        auto &row = handles.emplace_back();
        for (unsigned device = 0; device < op.getDevices().size(); ++device) {
          auto address = mlir::LLVM::GEPOp::create(
              rewriter, loc, ptr, ptr, input,
              llvm::ArrayRef<mlir::LLVM::GEPArg>{int32_t(device)});
          row.push_back(
              mlir::LLVM::LoadOp::create(rewriter, loc, ptr, address));
        }
      }
    }
    auto baselineInputs = inputs;
    for (auto attribute : llvm::reverse(op.getDonationVariants())) {
      auto variant = llvm::cast<mlir::DictionaryAttr>(attribute);
      auto arguments =
          llvm::cast<mlir::DenseI32ArrayAttr>(variant.get("arguments"))
              .asArrayRef();
      auto donors = llvm::cast<mlir::DenseI32ArrayAttr>(variant.get("donors"))
                        .asArrayRef();
      mlir::Value eligible = integer(rewriter, loc, rewriter.getI1Type(), 1);
      for (int32_t donor : donors) {
        eligible = mlir::LLVM::AndOp::create(
            rewriter, loc, eligible, adaptor.getDonationGuards()[donor]);
        for (int32_t other : arguments) {
          if (other == donor)
            continue;
          for (unsigned device = 0; device < op.getDevices().size(); ++device) {
            auto distinct = mlir::LLVM::ICmpOp::create(
                rewriter, loc, mlir::LLVM::ICmpPredicate::ne,
                handles[donor][device], handles[other][device]);
            eligible =
                mlir::LLVM::AndOp::create(rewriter, loc, eligible, distinct);
          }
        }
      }
      auto candidate =
          codeFields(llvm::cast<mlir::SymbolRefAttr>(variant.get("callee")));
      if (mlir::failed(candidate))
        return op.emitOpError("cannot resolve donation variant");
      auto select = [&](mlir::Value yes, mlir::Value no) {
        return mlir::LLVM::SelectOp::create(rewriter, loc, eligible, yes, no)
            .getResult();
      };
      for (unsigned i = 0; i < fields->size(); ++i)
        (*fields)[i] = select((*candidate)[i], (*fields)[i]);
      auto [candidateDenied, candidateCount] = nonDonatable(arguments, donors);
      denied = select(candidateDenied, denied);
      deniedCount = select(candidateCount, deniedCount);
      mlir::Value variantInputs = baselineInputs;
      mlir::Value variantShapes = shapeArguments;
      if (arguments != llvm::ArrayRef<int32_t>(identity)) {
        variantShapes = slots(op, argumentType, index, arguments.size(), rewriter);
        for (auto [i, original] : llvm::enumerate(arguments))
          storeSlot(op, variantShapes, i, argumentValues[original], rewriter);
        auto storage = slots(op, ptr, index, arguments.size(), rewriter);
        for (auto [i, original] : llvm::enumerate(arguments))
          storeSlot(op, storage, i, adaptor.getInputs()[original], rewriter);
        variantInputs = storage;
      }
      selectedShapes = select(variantShapes, selectedShapes);
      inputs = select(variantInputs, inputs);
      inputCount = select(length(arguments.size()), inputCount);
    }
    auto options = op.getCompileOptions().value_or(llvm::StringRef{});
    auto executable =
        mlir::LLVM::CallOp::create(
            rewriter, loc, *compile,
            mlir::ValueRange{(*fields)[0], (*fields)[1], (*fields)[2],
                             bytesAddress(op, options, rewriter),
                             length(options.size()), selectedShapes, inputCount})
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
    auto outputs = slots(op, ptr, index, op.getOutputs().size(), rewriter);
    llvm::SmallVector<mlir::Value> results;
    for (size_t i = 0; i < op.getOutputs().size(); ++i) {
      auto descriptor = slots(op, ptr, index, op.getDevices().size(), rewriter);
      storeSlot(op, outputs, i, descriptor, rewriter);
      results.push_back(descriptor);
    }
    mlir::LLVM::CallOp::create(
        rewriter, loc, *execute,
        mlir::ValueRange{executable, inputs, inputCount, outputs,
                         length(op.getOutputs().size()), denied, deniedCount});
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
