//===-- IncDecCancellation.h - Reussir inc/dec cancellation -*- c++ -*-===//
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

#pragma once

#ifndef REUSSIR_CONVERSION_INCDECCANCELLATION_H
#define REUSSIR_CONVERSION_INCDECCANCELLATION_H

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DECL_REUSSIRINCDECCANCELLATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// IncDecCancellationPass
//===----------------------------------------------------------------------===//
//
// This pass only cancels out subfield increment and decrement operations
// locally.
//
//===----------------------------------------------------------------------===//
llvm::LogicalResult runIncDecCancellation(mlir::func::FuncOp func);

} // namespace reussir

#endif // REUSSIR_CONVERSION_INCDECCANCELLATION_H
