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
/// This file implements the rewrite of a program for one donation choice.
///
//===----------------------------------------------------------------------===//

#include "program.h"

#include <llvm/ADT/BitVector.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/Verifier.h>

using namespace reussir::stablehlo;

extern "C" uint32_t reussir_stablehlo_apply_donation(
    const uint8_t *programData, size_t programSize, const uint32_t *mapping,
    size_t argumentCount, const uint32_t *donors, size_t donorCount,
    const uint32_t *aliases, size_t aliasCount, ReussirStablehloBytes rewritten,
    ReussirStablehloBytes diagnostic, void *userData) {
  Program program(diagnostic, userData);
  if ((!mapping && argumentCount) || (!donors && donorCount) ||
      (!aliases && aliasCount)) {
    program.report("null donation array with nonzero count");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  if (auto status = program.parse(programData, programSize, argumentCount))
    return status;
  auto entry = program.getEntry();

  llvm::BitVector erased(argumentCount);
  for (size_t i = 0; i < argumentCount; ++i) {
    uint32_t rep = mapping[i];
    if (rep > i || mapping[rep] != rep ||
        entry.getArgument(i).getType() != entry.getArgument(rep).getType() ||
        entry.getArgAttrDict(i) != entry.getArgAttrDict(rep)) {
      program.report("each argument must map to itself or to an earlier "
                     "equivalent argument that maps to itself");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    erased[i] = rep != i;
  }
  // Only this choice decides donation; drop any earlier markers.
  for (size_t i = 0; i < argumentCount; ++i) {
    entry.removeArgAttr(i, "jax.buffer_donor");
    entry.removeArgAttr(i, "tf.aliasing_output");
  }

  mlir::Builder builder(&program.getContext());
  llvm::BitVector donated(argumentCount), aliased(entry.getNumResults());
  for (uint32_t donor : llvm::ArrayRef(donors, donorCount)) {
    if (donor >= argumentCount || erased[donor] || donated[donor]) {
      program.report("donors must be distinct retained arguments");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    donated.set(donor);
    entry.setArgAttr(donor, "jax.buffer_donor", builder.getBoolAttr(true));
  }
  for (size_t i = 0; i < aliasCount; ++i) {
    uint32_t input = aliases[2 * i], result = aliases[2 * i + 1];
    if (input >= argumentCount || result >= entry.getNumResults() ||
        !donated[input] || aliased[result] ||
        entry.getArgAttr(input, "tf.aliasing_output") ||
        entry.getArgument(input).getType() != entry.getResultTypes()[result]) {
      program.report("each alias must pair a donor with a distinct result of "
                     "the same type");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    aliased.set(result);
    entry.setArgAttr(input, "tf.aliasing_output",
                     builder.getI32IntegerAttr(result));
  }

  for (size_t i = 0; i < argumentCount; ++i)
    if (erased[i])
      entry.getArgument(i).replaceAllUsesWith(entry.getArgument(mapping[i]));
  if (mlir::failed(entry.eraseArguments(erased)) ||
      mlir::failed(mlir::verify(program.getModule())))
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  return program.write(rewritten);
}
