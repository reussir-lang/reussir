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
/// The analysis reads frontend IR, where it runs: every counted value is
/// owned or a loan, and retains and releases are the plain `rc.inc` and
/// `rc.dec` the frontend inserts (no tokens, no destructuring releases).
///
/// A counted (shared `!reussir.rc`) parameter is *borrowable* when the function
/// never consumes the parameter itself. It may be retained, released, and
/// passed at borrowable positions of direct calls; the last is a fixed point
/// that starts optimistic, so recursion through the parameter is allowed. It
/// may be borrowed to read only (`isReadOnlyBorrow`): a store, acquire or
/// drop through the reference would change a field's ownership. Returning,
/// storing, capturing, uniqueness tests and owned call positions consume
/// it. Values loaded through the parameter (loans) may be consumed: a
/// borrowed clone keeps the owned body's retain for them. This follows
/// Lean's borrow inference (`Lean.IR.Borrow`), restricted to parameters that
/// are never consumed.
///
/// A direct call that passes a value once, at a borrowable position, is
/// *live-through* when the caller keeps its own reference to the value across
/// the call:
///   - a retain right before the call gives the call its reference
///     (`findFeedingRetain`), so a borrowed callee can do without it; and
///   - the value is live after the call (`mlir::Liveness`, asked at the call
///     and at each enclosing op; a loop that repeats the call uses it again);
///     in frontend IR the caller then holds it across the call, itself or
///     through a box it keeps. A retain alone does not show that: for
///     `f(y.head, y)` the frontend retains the loaded head only to own it,
///     and `f` may free `y`, and the head with it, while it reads the head.
/// The object's count then stays at least two during the call, so the
/// callee's release of it is never the last one: it cannot free the object
/// or donate its cell for reuse. Switching only those calls to a borrowed
/// callee therefore changes no reuse decision and extends no lifetime.
/// Every other call is *last-use* and stays owned.
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

#include "Reussir/IR/ReussirOps.h"

namespace reussir {

class BorrowAnalysis {
public:
  /// A direct call with at least one argument at a borrowable position.
  struct CallSite {
    mlir::func::CallOp call;
    mlir::func::FuncOp callee;
    /// Borrowable positions whose argument is live-through.
    llvm::BitVector liveThrough;
    /// Borrowable positions whose argument is passed at its last use.
    llvm::BitVector lastUse;
  };

  explicit BorrowAnalysis(mlir::ModuleOp module);

  /// The borrowable positions of `function` (empty if it has none).
  llvm::BitVector getBorrowable(mlir::func::FuncOp function) const;
  bool isBorrowable(mlir::func::FuncOp function, unsigned index) const;

  /// Every call with a borrowable position in the module's own functions
  /// (nested modules are separate symbol tables and are left alone).
  llvm::ArrayRef<CallSite> getCallSites() const { return callSites; }

private:
  llvm::DenseMap<mlir::func::FuncOp, llvm::BitVector> borrowable;
  llvm::SmallVector<CallSite> callSites;
};

/// Whether the reference a borrow yields is only read: every user at most
/// reads memory, and so does every user of a reference or view derived from
/// it, including the payload references a dispatch binds.
bool isReadOnlyBorrow(ReussirRcBorrowOp borrow);

/// Whether `use` leaves a counted value unconsumed, so a borrowed clone can
/// do without a reference to it: a retain, a plain release, a read-only
/// borrow, or an argument at a position `isBorrowablePosition` accepts.
bool isNonConsumingUse(mlir::OpOperand &use,
                       llvm::function_ref<bool(mlir::func::CallOp, unsigned)>
                           isBorrowablePosition);

/// The single retain right before `call` that gives it its reference to
/// `value`, if nothing in between can release anything: only ops that leave
/// counts alone (`leavesCountsAlone`) may lie between.
ReussirRcIncOp findFeedingRetain(mlir::Value value, mlir::func::CallOp call);

} // namespace reussir

#endif // REUSSIR_ANALYSIS_BORROWANALYSIS_H
