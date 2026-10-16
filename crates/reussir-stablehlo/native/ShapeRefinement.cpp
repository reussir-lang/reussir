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

#include <llvm/ADT/SmallVector.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>
#include <stablehlo/dialect/StablehloOps.h>
#include <stablehlo/transforms/Passes.h>

using namespace reussir::stablehlo;

namespace {
// Check one descriptor against the declared argument type and, for a shape
// value, replace the argument's uses with its constant contents.
ReussirStablehloStatus applyArgument(Program &program, mlir::BlockArgument arg,
                                     const ReussirStablehloArgument &desc,
                                     llvm::SmallVectorImpl<mlir::Type> &types) {
  auto type = llvm::dyn_cast<mlir::RankedTensorType>(arg.getType());
  if (!type || size_t(type.getRank()) != desc.rank ||
      (desc.rank && !desc.dimensions) || (desc.value_count && !desc.values)) {
    program.report("invalid ranked tensor argument descriptor");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  llvm::ArrayRef<int64_t> dims(desc.dimensions, desc.rank);
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
  if (!desc.has_values)
    return REUSSIR_STABLEHLO_OK;

  auto integer = llvm::dyn_cast<mlir::IntegerType>(type.getElementType());
  if (desc.rank > 1 || !type.hasStaticShape() || !integer ||
      !integer.isSignless() ||
      (integer.getWidth() != 32 && integer.getWidth() != 64) ||
      uint64_t(type.getNumElements()) != desc.value_count) {
    program.report("shape values require a static i32 or i64 scalar or vector "
                   "of matching length");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  llvm::SmallVector<llvm::APInt> values;
  for (int64_t value : llvm::ArrayRef(desc.values, desc.value_count)) {
    if (integer.getWidth() == 32 && (value < INT32_MIN || value > INT32_MAX)) {
      program.report("shape value does not fit i32");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    values.emplace_back(integer.getWidth(), value, /*isSigned=*/true);
  }
  auto entry = llvm::cast<mlir::func::FuncOp>(arg.getOwner()->getParentOp());
  mlir::OpBuilder builder(&program.getContext());
  builder.setInsertionPointToStart(arg.getOwner());
  auto constant = mlir::stablehlo::ConstantOp::create(
      builder, entry.getLoc(), mlir::DenseIntElementsAttr::get(type, values));
  arg.replaceAllUsesWith(constant);
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
    program.report("null argument descriptors with nonzero count");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  if (auto status = program.parse(programData, programSize, argumentCount))
    return status;
  auto entry = program.getEntry();
  llvm::SmallVector<mlir::Type> types;
  for (size_t i = 0; i < argumentCount; ++i)
    if (auto status =
            applyArgument(program, entry.getArgument(i), arguments[i], types))
      return status;

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
