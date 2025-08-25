#include <algorithm>
#include <cassert>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Twine.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/LogicalResult.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRRCDECREMENTEXPANSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

class RcDecrementExpansionPattern
    : public mlir::OpRewritePattern<ReussirRcDecOp> {

  bool inlineAll;

  bool shouldInline(RcType type) const;

public:
  RcDecrementExpansionPattern(mlir::MLIRContext *context, bool inlineAll)
      : mlir::OpRewritePattern<ReussirRcDecOp>(context), inlineAll(inlineAll) {}

  mlir::LogicalResult
  matchAndRewrite(ReussirRcDecOp op,
                  mlir::PatternRewriter &rewriter) const override {
    RcType type = op.getRcPtr().getType();
    // No need to proceed if dec operation is applied to a rigid type.
    if (type.getCapability() == Capability::rigid)
      return mlir::success();
    mlir::SymbolTable symTable(op->getParentOfType<mlir::ModuleOp>());
    return mlir::failure();
  }
};

bool RcDecrementExpansionPattern::shouldInline(RcType type) const {
  if (inlineAll)
    return true;

  if (isTriviallyCopyable(type))
    return true;

  if (RecordType recType = llvm::dyn_cast<RecordType>(type.getElementType()))
    return !recType.getName();

  return false;
}
} // namespace

//===----------------------------------------------------------------------===//
// RcDecrementExpansionPass
//===----------------------------------------------------------------------===//

namespace {
struct RcDecrementExpansionPass
    : public impl::ReussirRcDecrementExpansionPassBase<
          RcDecrementExpansionPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());

    populateRcDecrementExpansionConversionPatterns(patterns, inlineAll);

    if (failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateRcDecrementExpansionConversionPatterns(
    mlir::RewritePatternSet &patterns, bool inlineAll) {
  patterns.add<RcDecrementExpansionPattern>(patterns.getContext(), inlineAll);
}

mlir::FlatSymbolRefAttr getNameForRcDtor(RecordType type, bool isRigid) {
  assert(type.getName() && "only named record types have destructors");
}

} // namespace reussir
