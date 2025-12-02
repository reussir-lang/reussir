//===-- ClosureOutlining.cpp - Reussir closure outlining impl ----*- C++
//-*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <mlir/Pass/Pass.h>

#include "Reussir/Conversion/ClosureOutlining.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "mlir/IR/BuiltinOps.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/xxhash.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/PatternMatch.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRCLOSUREOUTLININGPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// ClosureOutliningPass
//===----------------------------------------------------------------------===//

namespace {
class ClosureNameUniquifier {
  llvm::StringMap<size_t> nameOccurences;

public:
  ClosureNameUniquifier() {}
  std::string uniquify(ReussirClosureCreateOp op) {
    llvm::SmallString<128> buffer;
    auto function = op->getParentOfType<mlir::func::FuncOp>();
    auto moduleOp = function->getParentOfType<mlir::ModuleOp>();
    buffer.append(moduleOp.getName() ? *moduleOp.getName() : "<unamed>");
    buffer.append(function.getName());
    std::stringstream ss;
    auto [high, lower] = llvm::xxh3_128bits(llvm::ArrayRef(
        reinterpret_cast<const uint8_t *>(buffer.data()), buffer.size()));
    ss << "reussir_closure_" << std::hex << high << '_' << std::hex << lower;
    std::string name = ss.str();
    if (nameOccurences[name]++) {
      name.append("_");
      name.append(std::to_string(nameOccurences[name]));
    }
    return name;
  }
};
struct ClosureOutliningPass
    : public impl::ReussirClosureOutliningPassBase<ClosureOutliningPass> {
  using Base::Base;

  /// Creates a function from the closure's inlined region and returns it.
  /// The function will have:
  /// - Arguments matching the closure's input types
  /// - Return type matching the closure's output type (if any)
  /// - Body cloned from the closure's region with yield replaced by return
  mlir::func::FuncOp createFunctionAndInlineRegion(ReussirClosureCreateOp op,
                                                   llvm::StringRef name,
                                                   mlir::IRRewriter &rewriter) {
    mlir::ModuleOp moduleOp = getOperation();
    mlir::OpBuilder builder(moduleOp.getBodyRegion());
    builder.setInsertionPointToEnd(moduleOp.getBody());

    // Get the closure type to determine function signature
    ClosureType closureType =
        llvm::cast<ClosureType>(op.getClosure().getType().getElementType());
    auto inputTypes = closureType.getInputTypes();
    mlir::Type outputType = closureType.getOutputType();

    // Build function type
    llvm::SmallVector<mlir::Type> resultTypes;
    if (outputType)
      resultTypes.push_back(outputType);
    mlir::FunctionType funcType =
        builder.getFunctionType(inputTypes, resultTypes);

    // Create the function
    auto funcOp =
        builder.create<mlir::func::FuncOp>(op.getLoc(), name, funcType);
    funcOp.setPrivate();
    funcOp->setAttr("llvm.linkage",
                    mlir::LLVM::LinkageAttr::get(
                        builder.getContext(), mlir::LLVM::Linkage::Internal));

    // Create entry block for the function and inline the closure's single block
    mlir::Block *entryBlock = funcOp.addEntryBlock();
    mlir::Block &closureBlock = op.getBody().front();

    // Inline the closure block into the function entry block
    rewriter.inlineBlockBefore(&closureBlock, entryBlock, entryBlock->end(),
                               entryBlock->getArguments());

    // Replace all ReussirClosureYieldOp with func.return
    funcOp.walk([&](ReussirClosureYieldOp yieldOp) {
      mlir::OpBuilder returnBuilder(yieldOp);
      if (yieldOp.getValue())
        returnBuilder.create<mlir::func::ReturnOp>(yieldOp.getLoc(),
                                                   yieldOp.getValue());
      else
        returnBuilder.create<mlir::func::ReturnOp>(yieldOp.getLoc());

      yieldOp.erase();
    });

    return funcOp;
  }

  void runOnOperation() override {
    mlir::ModuleOp moduleOp = getOperation();
    llvm::SmallVector<ReussirClosureCreateOp> closureCreateOps;
    moduleOp.walk([&](ReussirClosureCreateOp op) {
      if (op.isInlined())
        closureCreateOps.push_back(op);
    });
    ClosureNameUniquifier nameUniquifier;
    mlir::IRRewriter rewriter(moduleOp.getContext());
    for (auto op : closureCreateOps) {
      auto name = nameUniquifier.uniquify(op);
      // First, create a function, inline the region into the function and
      // change the yield op to return op during the translation.
      mlir::func::FuncOp function =
          createFunctionAndInlineRegion(op, name, rewriter);
      (void)function; // TODO: Use function to update the closure op

      // Second, create closure's clone and drop functions
      // Third, create vtable operation
      // Fourth, update the closure to use outlined version (e.g. remove the
      // region and set vtable attribute)
    }
  }
};
} // namespace

} // namespace reussir
