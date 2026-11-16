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
/// This file implements the pass that gives scalable vectors a fixed scale.
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
  using impl::ReussirFixVectorScalePassBase<
      ReussirFixVectorScalePass>::ReussirFixVectorScalePassBase;

  void runOnOperation() override {
    ModuleOp module = getOperation();
    if (scale == 0) {
      module->emitError("the scale of a scalable vector is positive");
      return signalPassFailure();
    }
    const int64_t factor = scale;
    IRRewriter rewriter(&getContext());

    // The operations that exist only for scalable vectors, with the position
    // of a sub-vector in lanes: it counts multiples of the scale when the
    // sub-vector is scalable.
    llvm::SmallVector<vector::VectorScaleOp> scales;
    llvm::SmallVector<std::pair<vector::ScalableExtractOp, int64_t>> extracts;
    llvm::SmallVector<std::pair<vector::ScalableInsertOp, int64_t>> inserts;
    auto lanes = [&](VectorType part, uint64_t pos) {
      return static_cast<int64_t>(pos) * (part.isScalable() ? factor : 1);
    };
    module->walk([&](Operation *op) {
      if (auto vscale = llvm::dyn_cast<vector::VectorScaleOp>(op))
        scales.push_back(vscale);
      else if (auto extract = llvm::dyn_cast<vector::ScalableExtractOp>(op))
        extracts.emplace_back(
            extract, lanes(extract.getResultVectorType(), extract.getPos()));
      else if (auto insert = llvm::dyn_cast<vector::ScalableInsertOp>(op))
        inserts.emplace_back(
            insert, lanes(insert.getSourceVectorType(), insert.getPos()));
    });

    auto fixed = [&](VectorType type) {
      llvm::SmallVector<int64_t> shape(type.getShape());
      for (auto [extent, scalable] :
           llvm::zip_equal(shape, type.getScalableDims()))
        if (scalable)
          extent *= factor;
      return VectorType::get(shape, type.getElementType());
    };
    AttrTypeReplacer replacer;
    replacer.addReplacement([&](VectorType type) -> std::optional<Type> {
      if (!type.isScalable())
        return std::nullopt;
      return fixed(type);
    });
    // The type of a constant's elements is not one of its sub-elements. A
    // constant of a scalable vector type is a splat.
    replacer.addReplacement(
        [&](DenseElementsAttr attr) -> std::optional<Attribute> {
          auto type = llvm::dyn_cast<VectorType>(attr.getType());
          if (!type || !type.isScalable() || !attr.isSplat())
            return std::nullopt;
          return attr.resizeSplat(fixed(type));
        });
    replacer.recursivelyReplaceElementsIn(module, /*replaceAttrs=*/true,
                                          /*replaceLocs=*/false,
                                          /*replaceTypes=*/true);

    // With every type fixed, they get their fixed counterparts.
    for (vector::VectorScaleOp vscale : scales) {
      rewriter.setInsertionPoint(vscale);
      rewriter.replaceOpWithNewOp<arith::ConstantIndexOp>(vscale, factor);
    }
    for (auto [extract, pos] : extracts) {
      rewriter.setInsertionPoint(extract);
      int64_t size = extract.getResultVectorType().getDimSize(0);
      rewriter.replaceOpWithNewOp<vector::ExtractStridedSliceOp>(
          extract, extract.getSource(), llvm::ArrayRef<int64_t>{pos},
          llvm::ArrayRef<int64_t>{size}, llvm::ArrayRef<int64_t>{1});
    }
    for (auto [insert, pos] : inserts) {
      rewriter.setInsertionPoint(insert);
      rewriter.replaceOpWithNewOp<vector::InsertStridedSliceOp>(
          insert, insert.getValueToStore(), insert.getDest(),
          llvm::ArrayRef<int64_t>{pos}, llvm::ArrayRef<int64_t>{1});
    }
  }
};
} // namespace
} // namespace reussir
