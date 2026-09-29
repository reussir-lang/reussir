// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <mlir/Conversion/LLVMCommon/MemRefBuilder.h>
#include <mlir/Dialect/ControlFlow/IR/ControlFlowOps.h>
#include <mlir/Dialect/LLVMIR/FunctionCallUtils.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Interfaces/FunctionInterfaces.h>

namespace reussir {
namespace {
// Stable PJRT_Buffer_Type C ABI values, independent of the optional OpenXLA
// helpers for resolving layout and sharding metadata.
std::optional<uint32_t> pjrtElementType(mlir::Type type) {
  if (auto integer = llvm::dyn_cast<mlir::IntegerType>(type)) {
    if (integer.getWidth() == 1)
      return 1;                                   // PRED
    unsigned base = integer.isUnsigned() ? 6 : 2; // U8 / S8
    switch (integer.getWidth()) {
    case 8:
      return base;
    case 16:
      return base + 1;
    case 32:
      return base + 2;
    case 64:
      return base + 3;
    default:
      return std::nullopt;
    }
  }
  if (type.isF16())
    return 10;
  if (type.isF32())
    return 11;
  if (type.isF64())
    return 12;
  if (type.isBF16())
    return 13;
  if (auto complex = llvm::dyn_cast<mlir::ComplexType>(type)) {
    if (complex.getElementType().isF32())
      return 14;
    if (complex.getElementType().isF64())
      return 15;
  }
  return std::nullopt;
}

mlir::Value constant(mlir::OpBuilder &builder, mlir::Location loc,
                     mlir::Type type, int64_t value) {
  return mlir::LLVM::ConstantOp::create(builder, loc, type,
                                        mlir::IntegerAttr::get(type, value));
}

mlir::Value entryAlloca(mlir::Operation *op, mlir::Type elementType,
                        mlir::Type indexType, int64_t count,
                        mlir::ConversionPatternRewriter &rewriter) {
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  auto function = op->getParentOfType<mlir::FunctionOpInterface>();
  rewriter.setInsertionPointToStart(&function.getFunctionBody().front());
  auto size = constant(rewriter, op->getLoc(), indexType, count);
  return mlir::LLVM::AllocaOp::create(
      rewriter, op->getLoc(),
      mlir::LLVM::LLVMPointerType::get(op->getContext()), elementType, size);
}

mlir::Value constantAddress(mlir::Operation *op, mlir::Type type,
                            mlir::Attribute value, mlir::OpBuilder &builder) {
  std::string key;
  llvm::raw_string_ostream stream(key);
  stream << type << value;
  auto name = mangledBlake3Symbol("REUSSIR_PJRT_ALLOCATION", key);
  auto module = op->getParentOfType<mlir::ModuleOp>();
  auto global = module.lookupSymbol<mlir::LLVM::GlobalOp>(name);
  if (!global) {
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(module.getBody());
    global = mlir::LLVM::GlobalOp::create(builder, op->getLoc(), type, true,
                                          mlir::LLVM::Linkage::LinkonceODR,
                                          name, value);
  }
  return mlir::LLVM::AddressOfOp::create(builder, op->getLoc(), global);
}

mlir::Value allocationOptions(mlir::Operation *op, TargetAttr target,
                              const std::optional<PjrtLayout> &layout,
                              mlir::Type indexType,
                              mlir::ConversionPatternRewriter &rewriter) {
  auto loc = op->getLoc();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
  mlir::Value null = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
  if (!target.getMemoryKind() && !layout)
    return null;
  auto arrayAddress = [&](llvm::ArrayRef<int64_t> values,
                          mlir::Type type) -> mlir::Value {
    if (values.empty())
      return null;
    llvm::SmallVector<llvm::APInt> integers;
    for (int64_t value : values)
      integers.emplace_back(type.getIntOrFloatBitWidth(), value);
    auto data = mlir::DenseIntElementsAttr::get(
        mlir::RankedTensorType::get({static_cast<int64_t>(values.size())},
                                    type),
        integers);
    return constantAddress(op,
                           mlir::LLVM::LLVMArrayType::get(type, values.size()),
                           data, rewriter);
  };
  mlir::Value memory = null;
  int64_t memorySize = 0;
  if (auto kind = target.getMemoryKind()) {
    memorySize = kind.getValue().size();
    memory = constantAddress(
        op, mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), memorySize),
        kind, rewriter);
  }
  // Must match pjrt::ffi::AllocationOptions. Only the call borrows this slot;
  // the array descriptor never retains a pointer to allocation options.
  auto type = mlir::LLVM::LLVMStructType::getLiteral(
      rewriter.getContext(),
      {ptrType, indexType, ptrType, ptrType, ptrType, indexType});
  llvm::SmallVector<mlir::Value> fields{
      memory,
      constant(rewriter, loc, indexType, memorySize),
      layout ? arrayAddress(layout->minorToMajor, rewriter.getI64Type()) : null,
      layout ? arrayAddress(layout->tileDims, rewriter.getI64Type()) : null,
      layout ? arrayAddress(layout->tileSizes, indexType) : null,
      constant(rewriter, loc, indexType,
               layout ? layout->tileSizes.size() : 0)};
  mlir::Value value = mlir::LLVM::PoisonOp::create(rewriter, loc, type);
  for (auto [i, field] : llvm::enumerate(fields))
    value = mlir::LLVM::InsertValueOp::create(
        rewriter, loc, value, field,
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
  auto address = entryAlloca(op, type, indexType, 1, rewriter);
  mlir::LLVM::StoreOp::create(rewriter, loc, value, address);
  return address;
}

mlir::FailureOr<mlir::LLVM::LLVMFuncOp>
runtimeFunction(mlir::Operation *op, mlir::OpBuilder &builder,
                llvm::StringRef name, llvm::ArrayRef<mlir::Type> inputs,
                mlir::Type output = {}) {
  if (!output)
    output = mlir::LLVM::LLVMVoidType::get(builder.getContext());
  return mlir::LLVM::lookupOrCreateFn(
      builder, op->getParentOfType<mlir::ModuleOp>(), name, inputs, output);
}

// Allocation handles occupy the first field of the descriptor. The logical
// offset and extents do not change which allocation must be released.
mlir::LogicalResult dropAllocation(mlir::Operation *op, mlir::Value descriptor,
                                   ArrayType arrayType,
                                   mlir::ConversionPatternRewriter &rewriter) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
  auto release = runtimeFunction(op, rewriter,
                                 "__reussir_pjrt_array_deallocate", {ptrType});
  if (mlir::failed(release))
    return mlir::failure();
  for (int32_t i = 0; i < arrayType.getTarget().getDevices().size(); ++i) {
    auto slot = mlir::LLVM::GEPOp::create(
        rewriter, op->getLoc(), ptrType, ptrType, descriptor,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{i});
    auto handle =
        mlir::LLVM::LoadOp::create(rewriter, op->getLoc(), ptrType, slot);
    mlir::LLVM::CallOp::create(rewriter, op->getLoc(), *release,
                               mlir::ValueRange{handle});
  }
  return mlir::success();
}

void initializeTargetBox(mlir::Operation *op, RcType rcType,
                         llvm::ArrayRef<mlir::Value> handles,
                         mlir::ValueRange extents,
                         const mlir::LLVMTypeConverter &converter,
                         mlir::ConversionPatternRewriter &rewriter,
                         mlir::Value box) {
  auto loc = op->getLoc();
  auto indexType = converter.getIndexType();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(op->getContext());
  auto boxType = rcType.getInnerBoxType();
  auto one = constant(rewriter, loc, rewriter.getI32Type(), 1);
  mlir::LLVM::StoreOp::create(rewriter, loc, one, box);
  auto loweredBox = converter.convertType(boxType);
  auto storeField = [&](unsigned field, mlir::Value value) {
    auto address = mlir::LLVM::GEPOp::create(
        rewriter, loc, ptrType, loweredBox, box,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1, static_cast<int32_t>(field)});
    mlir::LLVM::StoreOp::create(rewriter, loc, value, address);
  };
  mlir::Value allocations = handles.front();
  if (handles.size() > 1) {
    allocations = mlir::LLVM::PoisonOp::create(
        rewriter, loc, mlir::LLVM::LLVMArrayType::get(ptrType, handles.size()));
    for (auto [i, handle] : llvm::enumerate(handles))
      allocations = mlir::LLVM::InsertValueOp::create(
          rewriter, loc, allocations, handle,
          llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
  }
  storeField(ArrayType::ALLOCATION_INDEX, allocations);
  storeField(ArrayType::OFFSET_INDEX, constant(rewriter, loc, indexType, 0));
  for (auto [i, extent] : llvm::enumerate(extents))
    storeField(ArrayType::DYNAMIC_EXTENTS_INDEX + i, extent);
}

mlir::FailureOr<std::optional<PjrtLayout>> resolveLayout(mlir::Operation *op,
                                                         ArrayType arrayType) {
  auto target = arrayType.getTarget();
  std::optional<PjrtLayout> pjrtLayout;
  if (target.getLayout() && target.getLayout().getValue() == "auto") {
    op->emitOpError("auto layout must be resolved before allocation");
    return mlir::failure();
  }
  if (target.getMemoryKind() && target.getMemoryKind().getValue().empty()) {
    op->emitOpError("memory kind must not be empty");
    return mlir::failure();
  }
  if (target.getLayout() && target.getLayout().getValue() != "default") {
#ifdef REUSSIR_ENABLE_OPENXLA
    auto parsed =
        parsePjrtLayout(target.getLayout(), arrayType.getRank(), op->getLoc());
    if (mlir::failed(parsed))
      return mlir::failure();
    pjrtLayout = std::move(*parsed);
#else
    op->emitOpError("explicit XLA layouts require REUSSIR_ENABLE_OPENXLA");
    return mlir::failure();
#endif
  }
  return pjrtLayout;
}

struct CreateTargetArray : mlir::ConvertOpToLLVMPattern<ReussirArrayCreateOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirArrayCreateOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto rcType = op.getRcPtr().getType();
    auto arrayType = llvm::cast<ArrayType>(rcType.getElementType());
    auto target = arrayType.getTarget();
    if (!target)
      return mlir::failure();
    if (!op.getToken())
      return op.emitOpError("token is required but not provided");
    auto function = op->getParentOfType<mlir::FunctionOpInterface>();
    if (!function)
      return rewriter.notifyMatchFailure(op, "expected an enclosing function");
    auto parsedLayout = resolveLayout(op, arrayType);
    if (mlir::failed(parsedLayout))
      return mlir::failure();
    auto &pjrtLayout = *parsedLayout;
    llvm::SmallVector<int64_t> dimShards(arrayType.getRank(), 1);
    if (target.getSharding() || target.getDevices().size() != 1) {
#ifdef REUSSIR_ENABLE_OPENXLA
      auto shards = getPjrtDimShards(
          target.getSharding(),
          mlir::RankedTensorType::get(arrayType.getShape(),
                                      arrayType.getElementType()),
          target.getDevices().asArrayRef(), op.getLoc());
      if (mlir::failed(shards))
        return mlir::failure();
      dimShards = std::move(*shards);
#else
      return op.emitOpError(
          "sharded allocation requires REUSSIR_ENABLE_OPENXLA");
#endif
    }
    auto elementType = pjrtElementType(arrayType.getElementType());
    if (!elementType)
      return op.emitOpError("unsupported PjRt element type: ")
             << arrayType.getElementType();

    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto indexType = getTypeConverter()->getIndexType();
    auto i64Type = rewriter.getI64Type();
    auto allocateDevice = runtimeFunction(
        op, rewriter, "__reussir_pjrt_array_allocate",
        {indexType, rewriter.getI32Type(), ptrType, indexType, ptrType},
        ptrType);
    if (mlir::failed(allocateDevice))
      return mlir::failure();

    // PjRt shape dimensions are i64 on every host, while usize arguments and
    // descriptor offsets/extents follow the host index width.
    auto dims =
        entryAlloca(op, i64Type, indexType, arrayType.getRank(), rewriter);
    auto rank = constant(rewriter, loc, indexType, arrayType.getRank());
    size_t nextExtent = 0;
    for (auto [i, extent] : llvm::enumerate(arrayType.getShape())) {
      mlir::Value size;
      if (mlir::ShapedType::isDynamic(extent)) {
        size = adaptor.getExtents()[nextExtent++];
        if (indexType != i64Type)
          size = mlir::LLVM::SExtOp::create(rewriter, loc, i64Type, size);
      } else {
        size = constant(rewriter, loc, i64Type, extent);
      }
      if (dimShards[i] != 1) {
        auto divisor = constant(rewriter, loc, i64Type, dimShards[i]);
        if (mlir::ShapedType::isDynamic(extent)) {
          auto zero = constant(rewriter, loc, i64Type, 0);
          auto remainder =
              mlir::LLVM::SRemOp::create(rewriter, loc, size, divisor);
          auto divisible = mlir::LLVM::ICmpOp::create(
              rewriter, loc, mlir::LLVM::ICmpPredicate::eq, remainder, zero);
          auto nonnegative = mlir::LLVM::ICmpOp::create(
              rewriter, loc, mlir::LLVM::ICmpPredicate::sge, size, zero);
          auto valid =
              mlir::LLVM::AndOp::create(rewriter, loc, divisible, nonnegative);
          mlir::cf::AssertOp::create(rewriter, loc, valid,
                                     "target array extent must be nonnegative "
                                     "and divisible by its shard count");
        }
        size = mlir::LLVM::SDivOp::create(rewriter, loc, size, divisor);
      }
      auto address = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, i64Type, dims,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(i)});
      mlir::LLVM::StoreOp::create(rewriter, loc, size, address);
    }
    auto options =
        allocationOptions(op, target, pjrtLayout, indexType, rewriter);
    auto dtype = constant(rewriter, loc, rewriter.getI32Type(), *elementType);
    llvm::SmallVector<mlir::Value> handles;
    for (int32_t id : target.getDevices().asArrayRef()) {
      auto device = constant(rewriter, loc, indexType, id);
      handles.push_back(
          mlir::LLVM::CallOp::create(
              rewriter, loc, *allocateDevice,
              mlir::ValueRange{device, dtype, dims, rank, options})
              .getResult());
    }

    initializeTargetBox(op, rcType, handles, adaptor.getExtents(),
                        *getTypeConverter(), rewriter, adaptor.getToken());
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }
};

// Transfers currently cover one complete, addressable device buffer.
mlir::LogicalResult verifyTransferSharding(mlir::Operation *op,
                                           ArrayType array) {
  auto target = array.getTarget();
  if (!target.getSharding())
    return mlir::success();
#ifdef REUSSIR_ENABLE_OPENXLA
  auto shards = getPjrtDimShards(
      target.getSharding(),
      mlir::RankedTensorType::get(array.getShape(), array.getElementType()),
      target.getDevices().asArrayRef(), op->getLoc());
  if (mlir::failed(shards))
    return mlir::failure();
  if (llvm::any_of(*shards, [](int64_t n) { return n != 1; }))
    return op->emitOpError("transfer requires an unsharded device buffer");
  return mlir::success();
#else
  return op->emitOpError("sharding metadata requires REUSSIR_ENABLE_OPENXLA");
#endif
}

void assertEqual(mlir::OpBuilder &builder, mlir::Location loc, mlir::Value lhs,
                 mlir::Value rhs, llvm::StringRef message) {
  auto equal = mlir::LLVM::ICmpOp::create(
      builder, loc, mlir::LLVM::ICmpPredicate::eq, lhs, rhs);
  mlir::cf::AssertOp::create(builder, loc, equal, message);
}

// Use LLVM's checked arithmetic for byte counts and signed byte strides.
template <typename MulOp>
mlir::Value checkedMultiply(mlir::OpBuilder &builder, mlir::Location loc,
                            mlir::Value lhs, mlir::Value rhs) {
  auto resultType = mlir::LLVM::LLVMStructType::getLiteral(
      builder.getContext(), {lhs.getType(), builder.getI1Type()});
  auto product = MulOp::create(builder, loc, resultType, lhs, rhs);
  auto overflow = mlir::LLVM::ExtractValueOp::create(
      builder, loc, product, llvm::ArrayRef<int64_t>{1});
  assertEqual(builder, loc, overflow,
              constant(builder, loc, builder.getI1Type(), 0),
              "array transfer size or stride overflow");
  return mlir::LLVM::ExtractValueOp::create(builder, loc, product,
                                            llvm::ArrayRef<int64_t>{0});
}

mlir::Value hostDataPointer(mlir::OpBuilder &builder, mlir::Location loc,
                            mlir::MemRefDescriptor descriptor,
                            mlir::Type elementType) {
  auto base = descriptor.alignedPtr(builder, loc);
  return mlir::LLVM::GEPOp::create(
      builder, loc, base.getType(), elementType, base,
      mlir::ValueRange{descriptor.offset(builder, loc)});
}

struct ToDevice : mlir::ConvertOpToLLVMPattern<ReussirArrayToDeviceOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirArrayToDeviceOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    if (!op.getToken())
      return op.emitOpError("token is required but not provided");
    if (!op->getParentOfType<mlir::FunctionOpInterface>())
      return op.emitOpError("requires an enclosing function");
    auto rcType = op.getArray().getType();
    auto array = llvm::cast<ArrayType>(rcType.getElementType());
    if (mlir::failed(verifyTransferSharding(op, array)))
      return mlir::failure();
    auto dtype = pjrtElementType(array.getElementType());
    if (!dtype)
      return op.emitOpError("unsupported PjRt element type: ")
             << array.getElementType();
    auto layout = resolveLayout(op, array);
    if (mlir::failed(layout))
      return mlir::failure();
    auto loc = op.getLoc();
    auto indexType = getTypeConverter()->getIndexType();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(getContext());
    auto i64 = rewriter.getI64Type();
    auto upload =
        runtimeFunction(op, rewriter, "__reussir_pjrt_array_from_host",
                        {indexType, rewriter.getI32Type(), ptrType, indexType,
                         ptrType, ptrType, ptrType},
                        ptrType);
    if (mlir::failed(upload))
      return mlir::failure();
    auto dims = entryAlloca(op, i64, indexType, array.getRank(), rewriter);
    auto strides = entryAlloca(op, i64, indexType, array.getRank(), rewriter);
    auto elementBytes = mlir::DataLayout::closest(op)
                            .getTypeSize(array.getElementType())
                            .getFixedValue();
    mlir::MemRefDescriptor source(adaptor.getSource());
    llvm::SmallVector<mlir::Value> extents;
    for (unsigned i = 0; i < array.getRank(); ++i) {
      auto size = source.size(rewriter, loc, i);
      auto valid = mlir::LLVM::ICmpOp::create(
          rewriter, loc, mlir::LLVM::ICmpPredicate::sge, size,
          constant(rewriter, loc, indexType, 0));
      mlir::cf::AssertOp::create(rewriter, loc, valid,
                                 "array transfer extent must be nonnegative");
      if (array.isDynamicDim(i))
        extents.push_back(size);
      else
        assertEqual(rewriter, loc, size,
                    constant(rewriter, loc, indexType, array.getDimSize(i)),
                    "array transfer shape mismatch");
      mlir::Value stride = source.stride(rewriter, loc, i);
      if (indexType != i64) {
        size = mlir::LLVM::SExtOp::create(rewriter, loc, i64, size);
        stride = mlir::LLVM::SExtOp::create(rewriter, loc, i64, stride);
      }
      auto byteStride = checkedMultiply<mlir::LLVM::SMulWithOverflowOp>(
          rewriter, loc, stride, constant(rewriter, loc, i64, elementBytes));
      auto slot = [&](mlir::Value base) {
        return mlir::LLVM::GEPOp::create(
            rewriter, loc, ptrType, i64, base,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(i)});
      };
      mlir::LLVM::StoreOp::create(rewriter, loc, size, slot(dims));
      mlir::LLVM::StoreOp::create(rewriter, loc, byteStride, slot(strides));
    }
    auto target = array.getTarget();
    auto handle =
        mlir::LLVM::CallOp::create(
            rewriter, loc, *upload,
            mlir::ValueRange{
                constant(rewriter, loc, indexType,
                         target.getDevices().asArrayRef().front()),
                constant(rewriter, loc, rewriter.getI32Type(), *dtype), dims,
                constant(rewriter, loc, indexType, array.getRank()),
                hostDataPointer(
                    rewriter, loc, source,
                    getTypeConverter()->convertType(array.getElementType())),
                strides,
                allocationOptions(op, target, *layout, indexType, rewriter)})
            .getResult();
    initializeTargetBox(op, rcType, {handle}, extents, *getTypeConverter(),
                        rewriter, adaptor.getToken());
    rewriter.replaceOp(op, adaptor.getToken());
    return mlir::success();
  }
};

struct ToHost : mlir::ConvertOpToLLVMPattern<ReussirArrayToHostOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirArrayToHostOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    if (!op->getParentOfType<mlir::FunctionOpInterface>())
      return op.emitOpError("requires an enclosing function");
    auto rcType = op.getArray().getType();
    auto array = llvm::cast<ArrayType>(rcType.getElementType());
    if (mlir::failed(verifyTransferSharding(op, array)))
      return mlir::failure();
    if (!pjrtElementType(array.getElementType()))
      return op.emitOpError("unsupported PjRt element type: ")
             << array.getElementType();
    auto loc = op.getLoc();
    auto indexType = getTypeConverter()->getIndexType();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(getContext());
    auto hostSize =
        runtimeFunction(op, rewriter, "__reussir_pjrt_array_host_size",
                        {ptrType, ptrType}, indexType);
    auto download =
        runtimeFunction(op, rewriter, "__reussir_pjrt_array_to_host",
                        {ptrType, ptrType, indexType, ptrType});
    if (mlir::failed(hostSize) || mlir::failed(download))
      return mlir::failure();
    auto loadField = [&](unsigned field, mlir::Type type) -> mlir::Value {
      auto address = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType,
          getTypeConverter()->convertType(rcType.getInnerBoxType()),
          adaptor.getArray(),
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1,
                                             static_cast<int32_t>(field)});
      return mlir::LLVM::LoadOp::create(rewriter, loc, type, address);
    };
    auto zero = constant(rewriter, loc, indexType, 0);
    auto one = constant(rewriter, loc, indexType, 1);
    assertEqual(rewriter, loc, loadField(ArrayType::OFFSET_INDEX, indexType),
                zero, "array transfer requires a complete device buffer");
    mlir::MemRefDescriptor destination(adaptor.getDestination());
    llvm::SmallVector<mlir::Value> sizes;
    mlir::Value empty = constant(rewriter, loc, rewriter.getI1Type(), 0);
    unsigned dynamicDim = 0;
    for (unsigned i = 0; i < array.getRank(); ++i) {
      auto size = destination.size(rewriter, loc, i);
      auto expected =
          array.isDynamicDim(i)
              ? loadField(ArrayType::DYNAMIC_EXTENTS_INDEX + dynamicDim++,
                          indexType)
              : constant(rewriter, loc, indexType, array.getDimSize(i));
      assertEqual(rewriter, loc, size, expected,
                  "array transfer shape mismatch");
      auto valid = mlir::LLVM::ICmpOp::create(
          rewriter, loc, mlir::LLVM::ICmpPredicate::sge, size, zero);
      mlir::cf::AssertOp::create(rewriter, loc, valid,
                                 "array transfer extent must be nonnegative");
      auto isZero = mlir::LLVM::ICmpOp::create(
          rewriter, loc, mlir::LLVM::ICmpPredicate::eq, size, zero);
      empty = mlir::LLVM::OrOp::create(rewriter, loc, empty, isZero);
      sizes.push_back(size);
    }
    // Empty arrays touch no storage; singleton dimensions have no stride
    // constraint.
    mlir::Value count =
        mlir::LLVM::SelectOp::create(rewriter, loc, empty, zero, one);
    for (unsigned i = array.getRank(); i-- > 0;) {
      auto matches = mlir::LLVM::ICmpOp::create(
          rewriter, loc, mlir::LLVM::ICmpPredicate::eq,
          destination.stride(rewriter, loc, i), count);
      auto singleton = mlir::LLVM::ICmpOp::create(
          rewriter, loc, mlir::LLVM::ICmpPredicate::eq, sizes[i], one);
      auto valid = mlir::LLVM::OrOp::create(rewriter, loc, matches, singleton);
      valid = mlir::LLVM::OrOp::create(rewriter, loc, valid, empty);
      mlir::cf::AssertOp::create(
          rewriter, loc, valid,
          "array.to_host requires dense row-major storage");
      count = checkedMultiply<mlir::LLVM::UMulWithOverflowOp>(rewriter, loc,
                                                              count, sizes[i]);
    }
    auto elementBytes = mlir::DataLayout::closest(op)
                            .getTypeSize(array.getElementType())
                            .getFixedValue();
    auto bytes = checkedMultiply<mlir::LLVM::UMulWithOverflowOp>(
        rewriter, loc, count, constant(rewriter, loc, indexType, elementBytes));
    llvm::SmallVector<int64_t> order;
    for (int64_t i = array.getRank(); i-- > 0;)
      order.push_back(i);
    auto orderData = mlir::DenseIntElementsAttr::get(
        mlir::RankedTensorType::get({static_cast<int64_t>(order.size())},
                                    rewriter.getI64Type()),
        order);
    auto orderPtr = constantAddress(
        op, mlir::LLVM::LLVMArrayType::get(rewriter.getI64Type(), order.size()),
        orderData, rewriter);
    // Matches pjrt::ffi::HostLayout; the synchronous calls borrow this slot.
    auto layoutType = mlir::LLVM::LLVMStructType::getLiteral(
        getContext(),
        {indexType, ptrType, ptrType, ptrType, ptrType, indexType});
    auto null = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
    llvm::SmallVector<mlir::Value> fields{
        constant(rewriter, loc, indexType, array.getRank()),
        null,
        orderPtr,
        null,
        null,
        zero};
    mlir::Value layout =
        mlir::LLVM::PoisonOp::create(rewriter, loc, layoutType);
    for (auto [i, field] : llvm::enumerate(fields))
      layout = mlir::LLVM::InsertValueOp::create(
          rewriter, loc, layout, field,
          llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
    auto layoutPtr = entryAlloca(op, layoutType, indexType, 1, rewriter);
    mlir::LLVM::StoreOp::create(rewriter, loc, layout, layoutPtr);
    auto handle = loadField(ArrayType::ALLOCATION_INDEX, ptrType);
    auto required =
        mlir::LLVM::CallOp::create(rewriter, loc, *hostSize,
                                   mlir::ValueRange{handle, layoutPtr})
            .getResult();
    assertEqual(rewriter, loc, required, bytes,
                "array transfer host byte size mismatch");
    auto data = hostDataPointer(
        rewriter, loc, destination,
        getTypeConverter()->convertType(array.getElementType()));
    mlir::LLVM::CallOp::create(
        rewriter, loc, *download,
        mlir::ValueRange{handle, data, bytes, layoutPtr});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct DropTargetArray : mlir::ConvertOpToLLVMPattern<ReussirRefDropOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirRefDropOp op, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto arrayType =
        llvm::dyn_cast<ArrayType>(op.getRef().getType().getElementType());
    if (!arrayType || !arrayType.hasTargetAttr())
      return mlir::failure();
    if (mlir::failed(dropAllocation(op, adaptor.getRef(), arrayType, rewriter)))
      return mlir::failure();
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

} // namespace

void populateTargetArrayLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                         mlir::RewritePatternSet &patterns) {
  patterns.add<CreateTargetArray, ToDevice, ToHost, DropTargetArray>(converter);
}
} // namespace reussir
