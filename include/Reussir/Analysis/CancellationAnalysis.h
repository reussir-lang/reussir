#ifndef REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H
#define REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H

#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/Support/Immer.h"

#include <algorithm>
#include <cstddef>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>
#include <mlir/Analysis/AliasAnalysis.h>
#include <mlir/Analysis/DataFlow/DenseAnalysis.h>
#include <mlir/Analysis/DataFlowFramework.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Operation.h>
#include <mlir/Interfaces/CallInterfaces.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Support/LLVM.h>

namespace mlir {
class Value;
class Operation;
class Block;
class Region;
} // namespace mlir

namespace reussir {

class RcAliasCluster {
  mlir::AliasAnalysis &aliasAnalysis;
  llvm::SmallVector<mlir::TypedValue<RcType>> rcPtrs;

  void populate(mlir::TypedValue<RcType> rcPtr);

public:
  RcAliasCluster(mlir::AliasAnalysis &aliasAnalysis, mlir::Operation *op);

  size_t getCluster(mlir::TypedValue<RcType> rcPtr) const;

  size_t size() const { return rcPtrs.size(); }
  mlir::TypedValue<RcType> representative(size_t index) const {
    return rcPtrs[index];
  }
};
/// Analysis that tracks increment and decrement operations for cancellation
/// optimization.
class RcIncLattice final : public mlir::dataflow::AbstractDenseLattice {
  using Op = ReussirRcIncOp;
  using RcValue = mlir::TypedValue<RcType>;
  using RcIncMap = UnsyncMap<size_t, size_t>;
  // Map from cluster index to the number of increments to the cluster.
  RcIncMap pendingIncrement;

private:
  mlir::ChangeResult unify(RcIncMap rhs);
  static RcIncMap join(RcIncMap lhs, RcIncMap rhs);
  static size_t getIncCount(const RcIncMap &map, size_t clusterIndex);

public:
  using AbstractDenseLattice::AbstractDenseLattice;
  bool isEmpty() const { return pendingIncrement.empty(); }

  mlir::ChangeResult join(const AbstractDenseLattice &rhs) override;
  void print(llvm::raw_ostream &os) const override;
  mlir::ChangeResult setToEmpty();
  mlir::ChangeResult setAsAppend(const AbstractDenseLattice &prefix,
                                 size_t clusterIndex);
  mlir::ChangeResult setAsRemove(const AbstractDenseLattice &prefix,
                                 size_t clusterIndex);
  mlir::ChangeResult setAsCopy(const AbstractDenseLattice &rhs);
};

class CancellationAnalysis final
    : public mlir::dataflow::DenseForwardDataFlowAnalysis<RcIncLattice> {
public:
  CancellationAnalysis(mlir::DataFlowSolver &solver,
                       mlir::AliasAnalysis &aliasAnalysis,
                       mlir::Operation *entryOp)
      : DenseForwardDataFlowAnalysis(solver),
        aliasCluster(aliasAnalysis, entryOp) {}

  /// Visit an operation with the dense lattice before its execution.
  /// This function is expected to set the dense lattice after its execution
  /// and trigger change propagation in case of change.
  mlir::LogicalResult visitOperation(mlir::Operation *op,
                                     const RcIncLattice &before,
                                     RcIncLattice *after) override;

  /// Hook for customizing the behavior of lattice propagation along the call
  /// control flow edges.
  void visitCallControlFlowTransfer(
      mlir::CallOpInterface call, mlir::dataflow::CallControlFlowAction action,
      const RcIncLattice &before, RcIncLattice *after) override;

  /// Visit a branch of a conditional operation.
  void visitRegionBranchControlFlowTransfer(
      mlir::RegionBranchOpInterface branch, std::optional<unsigned> regionFrom,
      std::optional<unsigned> regionTo, const RcIncLattice &before,
      RcIncLattice *after) override;

  /// Set the dense lattice at control flow entry point and propagate an update
  /// if it changed.
  void setToEntryState(RcIncLattice *lattice) override;

  RcAliasCluster aliasCluster;
};

} // namespace reussir

#endif // REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H
