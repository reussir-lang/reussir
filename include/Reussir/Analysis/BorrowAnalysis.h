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
/// Which counted parameters could be borrowed, and at which call sites.
///
/// A counted (shared `!reussir.rc`) parameter is *borrowable* when the function
/// never consumes the parameter itself. Its only uses are `rc.borrow`,
/// `rc.inc`, non-destructuring `rc.dec`, and arguments at borrowable
/// positions of direct calls. The last condition is a fixed point that
/// starts optimistic, so recursion through the parameter is allowed.
/// Returning, storing, capturing, uniqueness tests and owned call positions
/// all consume it. Values loaded through the parameter (loans) may be
/// consumed: a borrowed clone keeps the owned body's retain for them. This
/// follows Lean's borrow inference (`Lean.IR.Borrow`), restricted to
/// parameters that are never consumed.
///
/// A direct call that passes a value at a borrowable position is
/// *caller-live* when the caller uses the value after the call on the same
/// path. The callee's release of it is then never the last one: it cannot
/// free the object or donate its cell for reuse, and the object outlives
/// the call either way. Switching only those calls to a borrowed callee
/// therefore changes no reuse decision and extends no lifetime. Every other
/// call is *last-use* and stays owned.
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_ANALYSIS_BORROWANALYSIS_H
#define REUSSIR_ANALYSIS_BORROWANALYSIS_H

#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>

namespace reussir {

class BorrowAnalysis {
public:
  /// A direct call with at least one argument at a borrowable position.
  struct CallSite {
    mlir::func::CallOp call;
    mlir::func::FuncOp callee;
    /// Borrowable positions whose argument is caller-live.
    llvm::BitVector callerLive;
    /// Borrowable positions whose argument is passed at its last use.
    llvm::BitVector lastUse;
  };

  explicit BorrowAnalysis(mlir::ModuleOp module);

  /// The borrowable positions of `function` (empty if it has none).
  llvm::BitVector getBorrowable(mlir::func::FuncOp function) const;
  bool isBorrowable(mlir::func::FuncOp function, unsigned index) const;

  /// Every call with a borrowable position, in module order.
  llvm::ArrayRef<CallSite> getCallSites() const { return callSites; }

private:
  llvm::DenseMap<mlir::func::FuncOp, llvm::BitVector> borrowable;
  llvm::SmallVector<CallSite> callSites;
};

/// Whether `value` is used by an operation that may run after `op`, within
/// the function containing both.
bool isUsedAfter(mlir::Value value, mlir::Operation *op);

} // namespace reussir

#endif // REUSSIR_ANALYSIS_BORROWANALYSIS_H
