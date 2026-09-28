// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
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

mlir::Value allocationOptions(ReussirArrayCreateOp op, TargetAttr target,
                              const std::optional<PjrtLayout> &layout,
                              mlir::Type indexType,
                              mlir::ConversionPatternRewriter &rewriter) {
  auto loc = op.getLoc();
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
    std::optional<PjrtLayout> pjrtLayout;
    llvm::SmallVector<int64_t> dimShards(arrayType.getRank(), 1);
    if (target.getLayout() && target.getLayout().getValue() == "auto")
      return op.emitOpError("auto layout must be resolved before allocation");
    if (target.getMemoryKind() && target.getMemoryKind().getValue().empty())
      return op.emitOpError("memory kind must not be empty");
    if (target.getLayout() && target.getLayout().getValue() != "default") {
#ifdef REUSSIR_ENABLE_OPENXLA
      auto parsed =
          parsePjrtLayout(target.getLayout(), arrayType.getRank(), op.getLoc());
      if (mlir::failed(parsed))
        return mlir::failure();
      pjrtLayout = std::move(*parsed);
#else
      return op.emitOpError(
          "explicit XLA layouts require REUSSIR_ENABLE_OPENXLA");
#endif
    }
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

    auto boxType = rcType.getInnerBoxType();
    auto box = adaptor.getToken();
    auto one = constant(rewriter, loc, rewriter.getI32Type(), 1);
    mlir::LLVM::StoreOp::create(rewriter, loc, one, box);
    auto loweredBox = getTypeConverter()->convertType(boxType);
    auto storeField = [&](unsigned field, mlir::Value value) {
      auto address =
          mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, loweredBox, box,
                                    llvm::ArrayRef<mlir::LLVM::GEPArg>{
                                        0, 1, static_cast<int32_t>(field)});
      mlir::LLVM::StoreOp::create(rewriter, loc, value, address);
    };
    mlir::Value allocations = handles.front();
    if (handles.size() > 1) {
      allocations = mlir::LLVM::PoisonOp::create(
          rewriter, loc,
          mlir::LLVM::LLVMArrayType::get(ptrType, handles.size()));
      for (auto [i, handle] : llvm::enumerate(handles))
        allocations = mlir::LLVM::InsertValueOp::create(
            rewriter, loc, allocations, handle,
            llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
    }
    storeField(ArrayType::ALLOCATION_INDEX, allocations);
    storeField(ArrayType::OFFSET_INDEX, constant(rewriter, loc, indexType, 0));
    for (auto [i, extent] : llvm::enumerate(adaptor.getExtents()))
      storeField(ArrayType::DYNAMIC_EXTENTS_INDEX + i, extent);
    rewriter.replaceOp(op, box);
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
  patterns.add<CreateTargetArray, DropTargetArray>(converter);
}
} // namespace reussir
