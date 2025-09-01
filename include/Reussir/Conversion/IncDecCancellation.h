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
// We want to cancel out adjacent increment and decrement operations. This
// reduces overhead due to RC management operations. This is conducted with
// dataflow analysis.
//
// To its simplest form, a decrement operation can be cancelled out by an
// dominating increment operation. However, to conduct the cancellation, the
// increment operation may need to be moved or even duplicated along the control
// flow.
//
// - If the increment operation is post-dominated by the decrement operation,
// then everything is simple.
// - Otherwise, such increment operations need to be duplicated at some
// splitting point.
//
// We can perform the analysis and transformation in the following way:
// - We maintain an orderred set of all increment operations lived at each
// program point.
// - Inside each block, we examine if there is a suitable decrement operation
//   that can be paired with an increment operation (via AliasAnalysis). If so,
//   we remove it from the output set and record the selection.
// - At dataflow join points, we intersect the sets. This means, if a increment
//   is ever cancelled inside a SCF region, it is no longer live outside the
//   region. Hence, we also know the exact postpone target for the increment
//   operation. That is the last block where it is live across while it still
//   gets removed in the joint lattice.
llvm::LogicalResult runIncDecCancellation(mlir::func::FuncOp func);

} // namespace reussir

#endif // REUSSIR_CONVERSION_INCDECCANCELLATION_H
