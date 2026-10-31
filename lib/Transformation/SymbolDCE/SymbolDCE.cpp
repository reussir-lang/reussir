//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/Transformation/Passes.h"

#include <llvm/ADT/SetVector.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/AttrTypeSubElements.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRSYMBOLDCEPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

constexpr llvm::StringLiteral kTypeSymbolsAttr = "reussir.ssa_type_symbols";

struct ReussirSymbolDCEPass
    : public impl::ReussirSymbolDCEPassBase<ReussirSymbolDCEPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    module.walk([](mlir::func::FuncOp function) {
      llvm::SmallSetVector<mlir::Attribute, 4> symbols;
      llvm::SmallVector<mlir::Type> recordMembers;
      mlir::AttrTypeWalker walker;
      walker.addWalk(
          [&](mlir::SymbolRefAttr symbol) { symbols.insert(symbol); });
      // Mutable record storage does not expose its members as sub-elements.
      walker.addWalk([&](RecordType record) {
        llvm::append_range(recordMembers, record.getMembers());
      });
      function.walk([&](mlir::Operation *op) {
        for (mlir::Type type : op->getResultTypes())
          walker.walk(type);
        for (mlir::Region &region : op->getRegions())
          for (mlir::Block &block : region)
            for (mlir::Type type : block.getArgumentTypes())
              walker.walk(type);
      });
      while (!recordMembers.empty())
        walker.walk(recordMembers.pop_back_val());
      // Symbol DCE sees attributes, but not types carried only by SSA values.
      if (!symbols.empty())
        function->setAttr(
            kTypeSymbolsAttr,
            mlir::ArrayAttr::get(function.getContext(), symbols.getArrayRef()));
    });

    mlir::OpPassManager pipeline(mlir::ModuleOp::getOperationName());
    pipeline.addPass(mlir::createSymbolDCEPass());
    mlir::LogicalResult result = runPipeline(pipeline, module);
    module.walk([](mlir::func::FuncOp function) {
      function->removeAttr(kTypeSymbolsAttr);
    });
    if (mlir::failed(result))
      signalPassFailure();
  }
};

} // namespace
} // namespace reussir
