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
/// This file implements shape refinement of dynamically shaped programs.
///
//===----------------------------------------------------------------------===//

#include "Program.h"

#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/MathExtras.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>
#include <stablehlo/dialect/StablehloOps.h>
#include <stablehlo/transforms/Passes.h>

using namespace reussir::stablehlo;

namespace {
// Replace the uses of a rank-zero integer argument with `value`.
ReussirStablehloStatus
substituteConstant(Program &program, mlir::BlockArgument arg, int64_t value) {
  auto type = llvm::dyn_cast<mlir::RankedTensorType>(arg.getType());
  auto integer =
      type ? llvm::dyn_cast<mlir::IntegerType>(type.getElementType()) : nullptr;
  if (!type || type.getRank() != 0 || !integer) {
    program.report("a constant argument must be a rank-zero integer");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  unsigned width = integer.getWidth();
  bool fits = width >= 64 ||
              (integer.isUnsigned() ? llvm::isUIntN(width, uint64_t(value))
                                    : llvm::isIntN(width, value));
  if (!fits) {
    program.report("constant argument value does not fit its integer type");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  mlir::OpBuilder builder(&program.getContext());
  builder.setInsertionPointToStart(arg.getOwner());
  llvm::APInt bits(width, uint64_t(value), !integer.isUnsigned(),
                   /*implicitTrunc=*/true);
  auto constant = mlir::stablehlo::ConstantOp::create(
      builder, arg.getLoc(), mlir::DenseIntElementsAttr::get(type, bits));
  arg.replaceAllUsesWith(constant);
  return REUSSIR_STABLEHLO_OK;
}

// Check a runtime argument's shape against its declared type and record the
// type refinement should give it.
ReussirStablehloStatus
addArgumentType(Program &program, mlir::BlockArgument arg,
                const ReussirStablehloArgument &shape,
                llvm::SmallVectorImpl<mlir::Type> &types) {
  auto type = llvm::dyn_cast<mlir::RankedTensorType>(arg.getType());
  if (!type || size_t(type.getRank()) != shape.rank ||
      (shape.rank && !shape.dimensions)) {
    program.report("argument shape does not match a ranked tensor argument");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  llvm::ArrayRef<int64_t> dims(shape.dimensions, shape.rank);
  for (auto [declared, actual] : llvm::zip(type.getShape(), dims)) {
    if (actual < 0 ||
        (!mlir::ShapedType::isDynamic(declared) && declared != actual)) {
      program.report(
          "logical dimensions must be nonnegative and match static dimensions");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
  }
  // Check bounds here: refinement drops unused arguments before it would see
  // them.
  if (auto encoding = type.getEncoding()) {
    auto bounds = llvm::dyn_cast<mlir::stablehlo::TypeExtensionsAttr>(encoding);
    if (!bounds) {
      program.report("unsupported tensor encoding on an argument");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    for (auto [actual, bound] : llvm::zip(dims, bounds.getBounds())) {
      if (!mlir::ShapedType::isDynamic(bound) && actual > bound) {
        program.report("logical dimension exceeds its declared bound");
        return REUSSIR_STABLEHLO_INVALID_INPUT;
      }
    }
  }
  types.push_back(mlir::RankedTensorType::get(dims, type.getElementType()));
  return REUSSIR_STABLEHLO_OK;
}

bool hasResidualDynamism(mlir::ModuleOp module) {
  bool dynamic = false;
  auto check = [&](mlir::Type type) {
    if (auto shaped = llvm::dyn_cast<mlir::ShapedType>(type))
      dynamic |= !shaped.hasStaticShape();
  };
  module->walk([&](mlir::Operation *op) {
    for (auto type : op->getResultTypes())
      check(type);
    for (auto &region : op->getRegions())
      for (auto &block : region)
        for (auto arg : block.getArguments())
          check(arg.getType());
    if (auto function = llvm::dyn_cast<mlir::func::FuncOp>(op))
      for (auto type : function.getResultTypes())
        check(type);
    // Shape assertions that refinement could not discharge.
    if (auto call = llvm::dyn_cast<mlir::stablehlo::CustomCallOp>(op))
      dynamic |= call.getCallTargetName() == "shape_assertion" ||
                 call.getCallTargetName() ==
                     "stablehlo.shape_refinement_operand_wrapper";
  });
  return dynamic;
}

uint32_t elementBits(mlir::Type element) {
  if (element.isIntOrFloat())
    return element.getIntOrFloatBitWidth();
  if (auto complex = llvm::dyn_cast<mlir::ComplexType>(element))
    return 2 * complex.getElementType().getIntOrFloatBitWidth();
  return 0;
}
} // namespace

extern "C" uint32_t reussir_stablehlo_refine_shapes(
    const uint8_t *programData, size_t programSize,
    const ReussirStablehloArgument *arguments, size_t argumentCount,
    ReussirStablehloBytes refined, ReussirStablehloResultType resultType,
    ReussirStablehloBytes diagnostic, void *userData) {
  Program program(diagnostic, userData);
  if (!arguments && argumentCount) {
    program.report("null arguments with nonzero count");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  if (auto status = program.parse(programData, programSize, argumentCount))
    return status;
  auto entry = program.getEntry();
  llvm::SmallVector<mlir::Type> types;
  llvm::BitVector constants(argumentCount);
  for (size_t i = 0; i < argumentCount; ++i) {
    auto arg = entry.getArgument(i);
    const auto &argument = arguments[i];
    constants[i] = argument.is_constant;
    if (auto status = argument.is_constant
                          ? substituteConstant(program, arg, argument.constant)
                          : addArgumentType(program, arg, argument, types))
      return status;
  }
  if (mlir::failed(entry.eraseArguments(constants)))
    return REUSSIR_STABLEHLO_INVALID_INPUT;

  // The pinned upstream pipeline, as used by XLA's polymorphic shape
  // refinement (xla/python/refine_polymorphic_shapes.cc).
  mlir::PassManager pm(&program.getContext());
  pm.addPass(mlir::createInlinerPass());
  pm.addPass(mlir::createCSEPass());
  mlir::stablehlo::createStablehloRemoveDynamismPipeline(pm, types);
  pm.addPass(mlir::createCanonicalizerPass());
  pm.addPass(mlir::createSymbolDCEPass());
  if (mlir::failed(pm.run(program.getModule())))
    return REUSSIR_STABLEHLO_REFINEMENT_FAILED;
  if (hasResidualDynamism(program.getModule())) {
    program.report("dynamism or shape assertions remain after refinement");
    return REUSSIR_STABLEHLO_RESIDUAL_DYNAMISM;
  }
  if (mlir::failed(mlir::verify(program.getModule())))
    return REUSSIR_STABLEHLO_REFINEMENT_FAILED;
  if (!llvm::all_of(entry.getResultTypes(),
                    llvm::IsaPred<mlir::RankedTensorType>)) {
    program.report("@main must return ranked tensors");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  if (auto status = program.write(refined))
    return status;
  for (auto type : entry.getResultTypes()) {
    auto tensor = llvm::cast<mlir::RankedTensorType>(type);
    resultType(tensor.getShape().data(), tensor.getRank(),
               elementBits(tensor.getElementType()), userData);
  }
  return REUSSIR_STABLEHLO_OK;
}
