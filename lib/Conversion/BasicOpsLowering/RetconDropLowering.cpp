//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {
namespace {
using namespace mlir;
using namespace mlir::LLVM;

struct ConvertSuspend : ConvertOpToLLVMPattern<ReussirDtorSuspendOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;
  LogicalResult
  matchAndRewrite(ReussirDtorSuspendOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<ReussirDtorSuspendOp>(op, op.getCalleeAttr(),
                                                      adaptor.getObject());
    return success();
  }
};

class RetconDropLowering {
  ModuleOp module;
  OpBuilder builder;
  Location loc;
  LLVMPointerType ptr;
  IntegerType sizeType;
  LLVMStructType stepType;
  LLVMStructType routineType;
  uint64_t bufferSize;
  uint64_t bufferAlign;

  Value constant(IntegerType type, uint64_t value) {
    return ConstantOp::create(builder, loc, type,
                              builder.getIntegerAttr(type, value));
  }
  Value address(StringRef name) {
    return AddressOfOp::create(builder, loc, ptr, name);
  }
  LLVMFuncOp function(StringRef name, Type result, ArrayRef<Type> args,
                      Linkage linkage = Linkage::Private,
                      bool variadic = false) {
    OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToEnd(module.getBody());
    return LLVMFuncOp::create(builder, loc, name,
                              LLVMFunctionType::get(result, args, variadic),
                              linkage);
  }
  LLVMFuncOp runtime(StringRef name, Type result, ArrayRef<Type> args,
                     bool variadic = false) {
    if (auto existing = module.lookupSymbol<LLVMFuncOp>(name))
      return existing;
    return function(name, result, args, Linkage::External, variadic);
  }
  void coroutineAttributes(LLVMFuncOp target, LLVMFuncOp source,
                           bool presplit) {
    SmallVector<Attribute> attrs{builder.getStringAttr("nounwind"),
                                 builder.getStringAttr("reussir.retcon_abi")};
    if (presplit)
      attrs.push_back(builder.getStringAttr("presplitcoroutine"));
    if (auto passthrough = source.getPassthrough())
      for (auto attr : *passthrough)
        if (auto name = dyn_cast<StringAttr>(attr))
          if (name.getValue().starts_with("sanitize_"))
            attrs.push_back(attr);
    target.setPassthroughAttr(builder.getArrayAttr(attrs));
  }
  GlobalOp descriptor(LLVMFuncOp entry, StringRef name) {
    OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToEnd(module.getBody());
    auto global = GlobalOp::create(builder, loc, routineType, true,
                                   Linkage::Private, name, Attribute{});
    builder.createBlock(&global.getInitializerRegion());
    Value value = UndefOp::create(builder, loc, routineType);
    value = InsertValueOp::create(
        builder, loc, value, address(entry.getSymName()), ArrayRef<int64_t>{0});
    value = InsertValueOp::create(builder, loc, value,
                                  constant(sizeType, bufferSize),
                                  ArrayRef<int64_t>{1});
    value = InsertValueOp::create(builder, loc, value,
                                  constant(sizeType, bufferAlign),
                                  ArrayRef<int64_t>{2});
    ReturnOp::create(builder, loc, value);
    return global;
  }
  LLVMFuncOp unavailableAllocator(StringRef name, Type result,
                                  ArrayRef<Type> args) {
    auto target = function(name, result, args);
    builder.setInsertionPointToStart(target.addEntryBlock(builder));
    UnreachableOp::create(builder, loc);
    return target;
  }
  void executorWrapper(LLVMFuncOp wrapper, GlobalOp routine) {
    // CoroSplit determines the complete frame layout. Leave an arena-backed
    // fallback here; the LLVM ABI pass replaces small roots with lazy entry.
    SmallVector<Attribute> attrs;
    if (auto passthrough = wrapper.getPassthroughAttr())
      llvm::append_range(attrs, passthrough);
    attrs.push_back(builder.getArrayAttr(
        {builder.getStringAttr("reussir.retcon_wrapper"),
         builder.getStringAttr(routine.getSymName())}));
    wrapper.setPassthroughAttr(builder.getArrayAttr(attrs));
    builder.setInsertionPointToStart(wrapper.addEntryBlock(builder));
    auto size = CallOp::create(builder, loc, TypeRange{sizeType},
                               "__reussir_dtor_executor_size", ValueRange{})
                    .getResult();
    auto align = CallOp::create(builder, loc, TypeRange{sizeType},
                                "__reussir_dtor_executor_align", ValueRange{})
                     .getResult();
    Value padding = SubOp::create(builder, loc, align, constant(sizeType, 1));
    Value bytes = AddOp::create(builder, loc, size, padding);
    Value storage =
        AllocaOp::create(builder, loc, ptr, builder.getI8Type(), bytes);
    Value integer = PtrToIntOp::create(builder, loc, sizeType, storage);
    integer = AddOp::create(builder, loc, integer, padding);
    Value mask = SubOp::create(builder, loc, constant(sizeType, 0), align);
    integer = AndOp::create(builder, loc, integer, mask);
    Value aligned = IntToPtrOp::create(builder, loc, ptr, integer);
    Value executor =
        CallOp::create(builder, loc, TypeRange{ptr},
                       "__reussir_create_dtor_executor", ValueRange{aligned})
            .getResult();
    CallOp::create(builder, loc, TypeRange{}, "__reussir_run_dtor",
                   ValueRange{executor, wrapper.getArgument(0),
                              address(routine.getSymName())});
    CallOp::create(builder, loc, TypeRange{}, "__reussir_destroy_dtor_executor",
                   ValueRange{executor});
    ReturnOp::create(builder, loc, ValueRange{});
  }

public:
  RetconDropLowering(ModuleOp module, const LLVMTypeConverter &converter)
      : module(module), builder(module.getContext()), loc(module.getLoc()),
        ptr(LLVMPointerType::get(module.getContext())),
        sizeType(cast<IntegerType>(converter.getIndexType())),
        stepType(LLVMStructType::getLiteral(module.getContext(), {ptr, ptr})),
        routineType(LLVMStructType::getLiteral(module.getContext(),
                                               {ptr, sizeType, sizeType})),
        bufferSize(48),
        bufferAlign(
            converter.getDataLayout().getPointerABIAlignment(0).value()) {}

  LogicalResult run(ArrayRef<LLVMFuncOp> functions) {
    auto voidType = LLVMVoidType::get(module.getContext());
    runtime("__reussir_dtor_executor_size", sizeType, {});
    runtime("__reussir_dtor_executor_align", sizeType, {});
    runtime("__reussir_create_dtor_executor", ptr, {ptr});
    runtime("__reussir_destroy_dtor_executor", voidType, {ptr});
    runtime("__reussir_run_dtor", voidType, {ptr, ptr, ptr});
    auto suspendFn =
        runtime("llvm.coro.suspend.retcon.i1", builder.getI1Type(), {}, true);
    for (auto wrapper : functions) {
      if (wrapper.isExternal() || wrapper.getNumArguments() != 1 ||
          wrapper.getArgument(0).getType() != ptr ||
          wrapper.getFunctionType().getReturnType() != voidType)
        return wrapper.emitError("invalid retcon destructor signature");
      loc = wrapper.getLoc();
      std::string name = wrapper.getSymName().str();
      auto entry = function(name + ".start", stepType, {ptr, ptr});
      coroutineAttributes(entry, wrapper, true);
      entry.getBody().takeBody(wrapper.getBody());
      entry.getBody().front().insertArgument(0u, ptr, loc);
      entry.setArgAttrs(1, wrapper.getArgAttrDict(0));
      wrapper->removeAttr("reussir.retcon_drop");

      auto prototype =
          function(name + ".prototype", stepType, {ptr, builder.getI1Type()});
      prototype.setArgAttr(1, "llvm.zeroext", builder.getUnitAttr());
      coroutineAttributes(prototype, wrapper, false);
      builder.setInsertionPointToStart(prototype.addEntryBlock(builder));
      UnreachableOp::create(builder, loc);
      auto routine = descriptor(entry, name + ".routine");

      auto attrs = llvm::to_vector(entry.getPassthroughAttr());
      attrs.push_back(
          builder.getArrayAttr({builder.getStringAttr("reussir.retcon_routine"),
                                builder.getStringAttr(routine.getSymName())}));
      entry.setPassthroughAttr(builder.getArrayAttr(attrs));
      auto alloc = unavailableAllocator(name + ".allocate", ptr, {sizeType});
      auto dealloc =
          unavailableAllocator(name + ".deallocate", voidType, {ptr});
      builder.setInsertionPointToStart(&entry.getBody().front());
      Value task =
          AllocaOp::create(builder, loc, ptr, stepType, constant(sizeType, 1));
      Value handle = ReussirDtorBeginOp::create(
          builder, loc, ptr, builder.getI32IntegerAttr(bufferSize),
          builder.getI32IntegerAttr(bufferAlign), entry.getArgument(0),
          address(prototype.getSymName()), address(alloc.getSymName()),
          address(dealloc.getSymName()));
      entry.walk([&](ReussirDtorSuspendOp suspend) {
        builder.setInsertionPoint(suspend);
        Value child = UndefOp::create(builder, loc, stepType);
        child = InsertValueOp::create(
            builder, loc, child,
            address((suspend.getCallee() + ".routine").str()),
            ArrayRef<int64_t>{0});
        child = InsertValueOp::create(builder, loc, child, suspend.getObject(),
                                      ArrayRef<int64_t>{1});
        StoreOp::create(builder, loc, child, task);
        CallOp::create(builder, loc, suspendFn, ValueRange{task});
        suspend.erase();
      });
      entry.walk([&](ReturnOp ret) {
        builder.setInsertionPoint(ret);
        ReussirDtorEndOp::create(builder, loc, handle);
        ret.erase();
      });
      executorWrapper(wrapper, routine);
    }
    return success();
  }
};
} // namespace

void populateRetconDropLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                        mlir::RewritePatternSet &patterns) {
  patterns.add<ConvertSuspend>(converter);
}

mlir::LogicalResult
lowerRetconDropFunctions(mlir::ModuleOp module,
                         const mlir::LLVMTypeConverter &converter) {
  llvm::SmallVector<mlir::LLVM::LLVMFuncOp> functions;
  module.walk([&](mlir::LLVM::LLVMFuncOp function) {
    if (function->hasAttr("reussir.retcon_drop"))
      functions.push_back(function);
  });
  if (functions.empty())
    return mlir::success();
  return RetconDropLowering(module, converter).run(functions);
}
} // namespace reussir
