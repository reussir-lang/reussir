// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>

namespace reussir {
namespace {
struct Bytecode : mlir::ConvertOpToLLVMPattern<ReussirIFRTBytecodeOp> {
  using ConvertOpToLLVMPattern::ConvertOpToLLVMPattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirIFRTBytecodeOp op, OpAdaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    auto bytesType = mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(),
                                                    op.getBytecode().size());
    auto checksumType =
        mlir::LLVM::LLVMArrayType::get(rewriter.getI8Type(), 64);
    auto type = mlir::LLVM::LLVMStructType::getLiteral(
        getContext(), {checksumType, bytesType});
    auto global = mlir::LLVM::GlobalOp::create(
        rewriter, op.getLoc(), type, true, mlir::LLVM::Linkage::Internal,
        op.getSymName(), mlir::Attribute{});
    {
      mlir::OpBuilder::InsertionGuard guard(rewriter);
      rewriter.createBlock(&global.getInitializerRegion());
      auto constantBytes = [&](llvm::StringRef bytes, mlir::Type type) {
        llvm::SmallVector<llvm::APInt> data;
        for (unsigned char byte : bytes)
          data.emplace_back(8, byte);
        auto value = mlir::DenseIntElementsAttr::get(
            mlir::RankedTensorType::get({static_cast<int64_t>(bytes.size())},
                                        rewriter.getI8Type()),
            data);
        return mlir::LLVM::ConstantOp::create(rewriter, op.getLoc(), type,
                                              value);
      };
      auto bytes = constantBytes(op.getBytecode(), bytesType);
      auto checksum = constantBytes(op.getChecksum(), checksumType);
      auto empty = mlir::LLVM::PoisonOp::create(rewriter, op.getLoc(), type);
      auto withChecksum = mlir::LLVM::InsertValueOp::create(
          rewriter, op.getLoc(), empty, checksum, llvm::ArrayRef<int64_t>{0});
      auto complete =
          mlir::LLVM::InsertValueOp::create(rewriter, op.getLoc(), withChecksum,
                                            bytes, llvm::ArrayRef<int64_t>{1});
      mlir::LLVM::ReturnOp::create(rewriter, op.getLoc(), complete);
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

} // namespace

void populatePjrtLoweringPatterns(mlir::LLVMTypeConverter &converter,
                                  mlir::RewritePatternSet &patterns) {
  patterns.add<Bytecode>(converter);
}
} // namespace reussir
