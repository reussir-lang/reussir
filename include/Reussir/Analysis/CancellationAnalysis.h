#ifndef REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H
#define REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H

#include "Reussir/IR/ReussirOps.h"
#include <immer/flex_vector.hpp>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallVector.h>
#include <mlir/IR/Operation.h>
#include <mlir/Support/LLVM.h>

namespace mlir {
class Value;
class Operation;
class Block;
class Region;
} // namespace mlir

namespace reussir {

/// Analysis that tracks increment and decrement operations for cancellation
/// optimization.
class CancellationAnalysis {
public:
  CancellationAnalysis() = default;

  /// Lattice element representing increment operations at a program point
  class IncrementLattice {
  public:
    IncrementLattice() = default;
    IncrementLattice(const IncrementLattice &) = default;
    IncrementLattice &operator=(const IncrementLattice &) = default;

  private:
    immer::flex_vector<ReussirRcIncOp> increments;
  };

  /// Set the entry state for a block or region
  void setToEntryState(IncrementLattice *lattice);

  /// Visit an operation to update the lattice
  void visitOperation(mlir::Operation *op, const IncrementLattice *input,
                      IncrementLattice *output);

  /// Visit a block to update the lattice
  void visitBlock(mlir::Block *block, const IncrementLattice *input,
                  IncrementLattice *output);

  /// Visit a region to update the lattice
  void visitRegion(mlir::Region *region, const IncrementLattice *input,
                   IncrementLattice *output);

  /// Get the lattice element for a value
  IncrementLattice getLatticeElement(mlir::Value value);

  /// Check if an increment operation can be cancelled with a decrement
  /// operation
  bool canCancel(mlir::Operation *increment, mlir::Operation *decrement);

  /// Get the set of increment operations that can be cancelled at a program
  /// point
  llvm::DenseSet<mlir::Operation *>
  getCancellableIncrements(mlir::Operation *op);

private:
  /// Check if an operation is an increment operation
  bool isIncrementOp(mlir::Operation *op);

  /// Check if an operation is a decrement operation
  bool isDecrementOp(mlir::Operation *op);

  /// Find decrement operations that can cancel with a given increment
  llvm::SmallVector<mlir::Operation *>
  findCancellingDecrements(mlir::Operation *increment, mlir::Block *block);
};

} // namespace reussir

#endif // REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H
