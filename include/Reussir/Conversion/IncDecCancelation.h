//===-- IncDecCancelation.h - Reussir inc/dec cancelation -*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides patterns for canceling adjacent increment and
// decrement operations in Reussir.
//
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_CONVERSION_INCDECCANCELATION_H
#define REUSSIR_CONVERSION_INCDECCANCELATION_H

#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DECL_REUSSIRINCDECCANCELATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

void runIncDecCancelation(mlir::ModuleOp module);

} // namespace reussir

#endif // REUSSIR_CONVERSION_INCDECCANCELATION_H
