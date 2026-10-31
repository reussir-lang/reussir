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
/// Which counted parameters could be borrowed.
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
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_ANALYSIS_BORROWANALYSIS_H
#define REUSSIR_ANALYSIS_BORROWANALYSIS_H

#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/DenseMap.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>

#include "Reussir/IR/ReussirOps.h"

namespace reussir {

class BorrowAnalysis {
public:
  explicit BorrowAnalysis(mlir::ModuleOp module);

  /// The borrowable positions of `function` (empty if it has none).
  llvm::BitVector getBorrowable(mlir::func::FuncOp function) const;
  bool isBorrowable(mlir::func::FuncOp function, unsigned index) const;

private:
  llvm::DenseMap<mlir::func::FuncOp, llvm::BitVector> borrowable;
};

/// Whether the reference a borrow yields is only read: every user at most
/// reads memory, and so does every user of a reference or view derived from
/// it, including the payload references a dispatch binds.
bool isReadOnlyBorrow(ReussirRcBorrowOp borrow);

} // namespace reussir

#endif // REUSSIR_ANALYSIS_BORROWANALYSIS_H
