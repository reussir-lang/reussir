//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements the pass that gives scalable vectors a scale of one.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/FixVectorScale.h"

#include <llvm/ADT/STLExtras.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Vector/IR/VectorOps.h>
#include <mlir/IR/AttrTypeSubElements.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/Pass/Pass.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRFIXVECTORSCALEPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {
using namespace mlir;

struct ReussirFixVectorScalePass
    : public impl::ReussirFixVectorScalePassBase<ReussirFixVectorScalePass> {
  void runOnOperation() override {
    ModuleOp module = getOperation();
    IRRewriter rewriter(&getContext());

    // The operations that exist only for scalable vectors get their fixed
    // counterparts. Their operand types are replaced below.
    llvm::SmallVector<Operation *> scaled;
    module->walk([&](Operation *op) {
      if (llvm::isa<vector::VectorScaleOp, vector::ScalableExtractOp,
                    vector::ScalableInsertOp>(op))
        scaled.push_back(op);
    });
    for (Operation *op : scaled) {
      rewriter.setInsertionPoint(op);
      if (auto scale = llvm::dyn_cast<vector::VectorScaleOp>(op)) {
        rewriter.replaceOpWithNewOp<arith::ConstantIndexOp>(scale, 1);
      } else if (auto extract = llvm::dyn_cast<vector::ScalableExtractOp>(op)) {
        int64_t size = extract.getResultVectorType().getDimSize(0);
        rewriter.replaceOpWithNewOp<vector::ExtractStridedSliceOp>(
            extract, extract.getSource(),
            llvm::ArrayRef<int64_t>{static_cast<int64_t>(extract.getPos())},
            llvm::ArrayRef<int64_t>{size}, llvm::ArrayRef<int64_t>{1});
      } else if (auto insert = llvm::dyn_cast<vector::ScalableInsertOp>(op)) {
        rewriter.replaceOpWithNewOp<vector::InsertStridedSliceOp>(
            insert, insert.getValueToStore(), insert.getDest(),
            llvm::ArrayRef<int64_t>{static_cast<int64_t>(insert.getPos())},
            llvm::ArrayRef<int64_t>{1});
      }
    }

    auto fixed = [](VectorType type) {
      return VectorType::get(type.getShape(), type.getElementType());
    };
    AttrTypeReplacer replacer;
    replacer.addReplacement([&](VectorType type) -> std::optional<Type> {
      if (!type.isScalable())
        return std::nullopt;
      return fixed(type);
    });
    // The type of a constant's elements is not one of its sub-elements.
    replacer.addReplacement(
        [&](DenseElementsAttr attr) -> std::optional<Attribute> {
          auto type = llvm::dyn_cast<VectorType>(attr.getType());
          if (!type || !type.isScalable())
            return std::nullopt;
          return attr.reshape(fixed(type));
        });
    replacer.recursivelyReplaceElementsIn(module, /*replaceAttrs=*/true,
                                          /*replaceLocs=*/false,
                                          /*replaceTypes=*/true);
  }
};
} // namespace
} // namespace reussir
