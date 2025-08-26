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
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/Conversion/DropExpansion.h"
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

  bool shouldInline(RecordType type) const;

  mlir::func::FuncOp createDtorIfNotExists(mlir::ModuleOp moduleOp,
                                           RecordType type,
                                           mlir::OpBuilder &builder) const;

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

    auto prevRcCount =
        rewriter.create<ReussirRcFetchDecOp>(op.getLoc(), op.getRcPtr());
    auto isOne = rewriter.create<mlir::arith::CmpIOp>(
        op.getLoc(), mlir::arith::CmpIPredicate::eq, prevRcCount.getRefCount(),
        rewriter.create<mlir::arith::ConstantIndexOp>(op.getLoc(), 1));
    auto ifOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), isOne, true, true);
    RecordType recType = llvm::dyn_cast<RecordType>(type.getElementType());
    RefType borrowedRefType = rewriter.getType<RefType>(
        recType, Capability::unspecified, type.getAtomicKind());
    TokenType tokenType = llvm::cast<TokenType>(
        llvm::cast<NullableType>(op.getNullableToken().getType()).getPtrTy());
    {
      rewriter.setInsertionPointToStart(ifOp.thenBlock());
      mlir::Value ref = rewriter.create<ReussirRcBorrowOp>(
          op.getLoc(), borrowedRefType, op.getRcPtr());
      if (recType && !shouldInline(recType)) {
        auto moduleOp = op->getParentOfType<mlir::ModuleOp>();
        auto dtor = createDtorIfNotExists(moduleOp, recType, rewriter);
        rewriter.create<mlir::func::CallOp>(op.getLoc(), dtor, ref);
      } else {
        // insert drop inline
        rewriter.create<ReussirRefDropOp>(op.getLoc(), ref);
      }
      mlir::Value token = rewriter.create<ReussirRcReinterpretOp>(
          op.getLoc(), tokenType, op.getRcPtr());
      mlir::Value nonnull = rewriter.create<ReussirNullableCreateOp>(
          op.getLoc(), op.getNullableToken().getType(), token);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), nonnull);
    }
    {
      rewriter.setInsertionPointToStart(ifOp.elseBlock());
      auto null = rewriter.create<ReussirNullableCreateOp>(
          op.getLoc(), op.getNullableToken().getType(), nullptr);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), null->getResults());
    }
    rewriter.replaceOp(op, ifOp);
    return mlir::success();
  }
};

bool RcDecrementExpansionPattern::shouldInline(RecordType type) const {
  if (inlineAll)
    return true;

  if (isTriviallyCopyable(type))
    return true;

  return !type.getName();
}

mlir::func::FuncOp RcDecrementExpansionPattern::createDtorIfNotExists(
    mlir::ModuleOp moduleOp, RecordType type, mlir::OpBuilder &builder) const {
  mlir::SymbolTable symTable(moduleOp);
  auto dtorName = type.getDtorName();
  if (!dtorName)
    llvm::report_fatal_error("only named record types have destructors");
  if (auto funcOp = symTable.lookup<mlir::func::FuncOp>(dtorName.getValue()))
    return funcOp;
  mlir::OpBuilder::InsertionGuard guard(builder);
  builder.setInsertionPointToStart(moduleOp.getBody());
  RefType refType = builder.getType<RefType>(type);
  auto dtor = builder.create<mlir::func::FuncOp>(
      builder.getUnknownLoc(), dtorName.getValue(),
      builder.getFunctionType({refType}, {}));
  dtor.setPrivate();
  dtor->setAttr("llvm.linkage", builder.getAttr<mlir::LLVM::LinkageAttr>(
                                    mlir::LLVM::linkage::Linkage::LinkonceODR));
  mlir::Block *entryBlock = dtor.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);
  auto ref = entryBlock->getArgument(0);
  builder.create<ReussirRefDropOp>(builder.getUnknownLoc(), ref);
  builder.create<mlir::func::ReturnOp>(builder.getUnknownLoc());
  return dtor;
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
    populateDropExpansionConversionPatterns(patterns);

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

} // namespace reussir
