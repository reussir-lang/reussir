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
/// Whether a value may still be used after an operation, for a chosen set of
/// values in one function.
///
/// This is classical backward liveness, solved by MLIR's dense dataflow
/// framework (`DenseBackwardDataFlowAnalysis`): the framework follows
/// branches, structured regions and loops through their control-flow
/// interfaces, so the analysis only gives the transfer functions. A use
/// generates a value and its definition kills it, so a value defined inside
/// a loop does not stay live around the back edge. Calls are opaque: their
/// operands are uses. Blocks the framework proves unreachable keep nothing
/// live. (`mlir::Liveness` stops at region boundaries: a use after the op
/// that holds the region is not live inside it.)
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_ANALYSIS_VALUELIVENESS_H
#define REUSSIR_ANALYSIS_VALUELIVENESS_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/DenseSet.h>
#include <mlir/Analysis/DataFlowFramework.h>
#include <mlir/IR/Operation.h>
#include <mlir/IR/SymbolTable.h>

#include <memory>

namespace reussir {

class ValueLiveness {
public:
  /// Solves liveness of `values` inside `function`, which must hold them.
  ValueLiveness(mlir::Operation *function, llvm::ArrayRef<mlir::Value> values);
  ValueLiveness(const ValueLiveness &) = delete;
  ValueLiveness &operator=(const ValueLiveness &) = delete;

  /// Whether `value`, one of the chosen values, may be used again after `op`
  /// runs. False when the solver failed.
  bool isLiveAfter(mlir::Value value, mlir::Operation *op) const;

private:
  llvm::DenseSet<mlir::Value> values;
  mlir::SymbolTableCollection symbols;
  std::unique_ptr<mlir::DataFlowSolver> solver;
  bool solved = false;
};

} // namespace reussir

#endif // REUSSIR_ANALYSIS_VALUELIVENESS_H
