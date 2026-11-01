// Part of the Reussir Project, dual licensed under the Apache License v2.0
// or the MIT License. See LICENSE for details.
// SPDX-License-Identifier: Apache-2.0 OR MIT

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {
namespace {
using namespace mlir;

LLVM::LLVMFuncOp runtimeFunction(Operation *op, StringRef name,
                                 llvm::ArrayRef<Type> arguments) {
  auto module = op->getParentOfType<ModuleOp>();
  if (auto existing = module.lookupSymbol<LLVM::LLVMFuncOp>(name))
    return existing;
  OpBuilder builder(module.getBodyRegion());
  auto type = LLVM::LLVMFunctionType::get(
      LLVM::LLVMVoidType::get(op->getContext()), arguments);
  auto function = LLVM::LLVMFuncOp::create(builder, op->getLoc(), name, type);
  function->setAttr("passthrough", builder.getStrArrayAttr({"nounwind"}));
  return function;
}

bool hasWideHeader(RcType type, const LLVMTypeConverter &converter,
                   Operation *op) {
  auto record = llvm::dyn_cast<RecordType>(type.getElementType());
  if (!record || type.getCapability() != Capability::shared)
    return false;
  RcBoxType box = type.getInnerBoxType();
  if (box.isHeaderFused())
    return record.getMembers().size() <= (1u << 16);
  if (box.getHeaderTypes().size() != 1)
    return false;
  // The box is `{i32, payload}`, not packed: the payload starts at its
  // alignment (a packed record's is 1).
  Type payload = converter.convertType(record);
  return payload && DataLayout::closest(op).getTypeABIAlignment(payload) >= 8;
}

template <typename Op> struct DropStackCall : ConvertOpToLLVMPattern<Op> {
  StringRef runtime;
  DropStackCall(LLVMTypeConverter &converter, StringRef runtime)
      : ConvertOpToLLVMPattern<Op>(converter), runtime(runtime) {}

  LogicalResult
  matchAndRewrite(Op op, typename Op::Adaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    auto ptr = LLVM::LLVMPointerType::get(op.getContext());
    auto [symbol, object] = [&] {
      if constexpr (std::is_same_v<Op, ReussirRcDeferOp>)
        return std::pair{op.getReleaseAttr(), adaptor.getBox()};
      else
        return std::pair{op.getCalleeAttr(), adaptor.getObject()};
    }();
    Value address =
        LLVM::AddressOfOp::create(rewriter, op.getLoc(), ptr, symbol);
    SmallVector<Type> inputs{ptr};
    SmallVector<Value> arguments{object};
    if constexpr (std::is_same_v<Op, ReussirDropDeferIndexedOp>) {
      inputs.push_back(this->getTypeConverter()->getIndexType());
      arguments.push_back(adaptor.getIndex());
    }
    inputs.push_back(ptr);
    arguments.push_back(address);
    StringRef name = runtime;
    if constexpr (std::is_same_v<Op, ReussirRcDeferOp>)
      if (hasWideHeader(op.getBox().getType(), *this->getTypeConverter(), op))
        name = "__reussir_drop_defer_wide";
    auto callee = runtimeFunction(op, name, inputs);
    rewriter.replaceOpWithNewOp<LLVM::CallOp>(op, callee, arguments);
    return success();
  }
};
} // namespace

void populateDropStackLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                       mlir::RewritePatternSet &patterns) {
  patterns
      .add<DropStackCall<ReussirRcDeferOp>, DropStackCall<ReussirDropDeferOp>>(
          converter, "__reussir_drop_defer");
  patterns.add<DropStackCall<ReussirDropJoinOp>>(converter,
                                                 "__reussir_drop_join_or_run");
  patterns.add<DropStackCall<ReussirDropRunOp>>(converter,
                                                "__reussir_drop_run");
  patterns.add<DropStackCall<ReussirDropDeferIndexedOp>>(
      converter, "__reussir_drop_defer_indexed");
}
} // namespace reussir
