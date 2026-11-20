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
/// This file implements the pass that lowers the vector dialect to the LLVM
/// dialect.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/Passes.h"

#include <mlir/Conversion/LLVMCommon/ConversionTarget.h>
#include <mlir/Conversion/LLVMCommon/TypeConverter.h>
#include <mlir/Conversion/VectorToLLVM/ConvertVectorToLLVM.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/ArmSVE/IR/ArmSVEDialect.h>
#include <mlir/Dialect/ArmSVE/Transforms/Transforms.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/Tensor/IR/Tensor.h>
#include <mlir/Dialect/Vector/Transforms/LoweringPatterns.h>
#include <mlir/Dialect/Vector/Transforms/VectorRewritePatterns.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRCONVERTVECTORTOLLVMPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {
using namespace mlir;

// The pattern sets, their order and their options are those of MLIR's
// `convert-vector-to-llvm` with its default options.
struct ReussirConvertVectorToLLVMPass
    : public impl::ReussirConvertVectorToLLVMPassBase<
          ReussirConvertVectorToLLVMPass> {
  using impl::ReussirConvertVectorToLLVMPassBase<
      ReussirConvertVectorToLLVMPass>::ReussirConvertVectorToLLVMPassBase;

  void getDependentDialects(DialectRegistry &registry) const override {
    registry.insert<LLVM::LLVMDialect, arith::ArithDialect,
                    memref::MemRefDialect, tensor::TensorDialect>();
    if (armSVE)
      registry.insert<arm_sve::ArmSVEDialect>();
  }

  void runOnOperation() override {
    const bool force32BitVectorIndices = true;
    {
      RewritePatternSet patterns(&getContext());
      vector::populateVectorToVectorCanonicalizationPatterns(patterns);
      vector::populateVectorBitCastLoweringPatterns(patterns);
      vector::populateVectorBroadcastLoweringPatterns(patterns);
      vector::populateVectorContractLoweringPatterns(
          patterns, vector::VectorContractLowering::Dot);
      vector::populateVectorMaskOpLoweringPatterns(patterns);
      vector::populateVectorShapeCastLoweringPatterns(patterns);
      vector::populateVectorInterleaveLoweringPatterns(patterns);
      vector::populateVectorTransposeLoweringPatterns(
          patterns, vector::VectorTransposeLowering::EltWise);
      vector::populateVectorTransferLoweringPatterns(patterns,
                                                     /*maxTransferRank=*/1);
      vector::populateVectorMaskMaterializationPatterns(
          patterns, force32BitVectorIndices);
      vector::populateVectorInsertExtractStridedSliceTransforms(patterns);
      vector::populateVectorRankReducingFMAPattern(patterns);
      vector::populateVectorGatherLoweringPatterns(patterns);
      vector::populateVectorFromElementsUnrollPatterns(patterns);
      vector::populateVectorToElementsUnrollPatterns(patterns);
      // MLIR's pass uses the driver's default configuration. All of it stays
      // but the block merging of the `Aggressive` region simplification,
      // which rewrites the blocks of code without any vector at a cost that
      // has no bound. `Normal`, the level of MLIR's canonicalizer, still
      // removes unreachable blocks and dead operations and arguments.
      GreedyRewriteConfig config;
      config.setRegionSimplificationLevel(GreedySimplifyRegionLevel::Normal);
      (void)applyPatternsGreedily(getOperation(), std::move(patterns), config);
    }

    LowerToLLVMOptions options(&getContext());
    LLVMTypeConverter converter(&getContext(), options);
    RewritePatternSet patterns(&getContext());
    vector::populateVectorTransferLoweringPatterns(patterns);
    populateVectorToLLVMConversionPatterns(converter, patterns,
                                           /*reassociateFPReductions=*/false,
                                           force32BitVectorIndices);
    LLVMConversionTarget target(getContext());
    target.addLegalDialect<arith::ArithDialect>();
    target.addLegalDialect<memref::MemRefDialect>();
    target.addLegalOp<UnrealizedConversionCastOp>();
    if (armSVE) {
      configureArmSVELegalizeForExportTarget(target);
      populateArmSVELegalizeForLLVMExportPatterns(converter, patterns);
    }
    if (failed(applyPartialConversion(getOperation(), target,
                                      std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace
} // namespace reussir
