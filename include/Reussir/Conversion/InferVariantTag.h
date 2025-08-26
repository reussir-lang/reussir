//===-- InferVariantTag.h - Reussir variant tag inference -*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides patterns for inferring variant tags in Reussir
// drop operations.
//
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_CONVERSION_INFERVARIANTTAG_H
#define REUSSIR_CONVERSION_INFERVARIANTTAG_H

#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DECL_REUSSIRINFERVARIANTTAGPASS
#include "Reussir/Conversion/Passes.h.inc"

void populateInferVariantTagConversionPatterns(
    mlir::RewritePatternSet &patterns);

} // namespace reussir

#endif // REUSSIR_CONVERSION_INFERVARIANTTAG_H
