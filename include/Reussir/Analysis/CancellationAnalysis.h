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

  void populate(mlir::TypedValue<RcType> rcPtr) {
    auto iter = std::find_if(rcPtrs.begin(), rcPtrs.end(),
                             [&](mlir::TypedValue<RcType> ptr) {
                               return aliasAnalysis.alias(rcPtr, ptr) ==
                                      mlir::AliasResult::MustAlias;
                             });
    if (iter != rcPtrs.end())
      return;
    rcPtrs.push_back(rcPtr);
  }

public:
  RcAliasCluster(mlir::AliasAnalysis &aliasAnalysis, mlir::Operation *op)
      : aliasAnalysis(aliasAnalysis) {
    op->walk([&](mlir::Operation *op) {
      for (mlir::Value result : op->getResults())
        if (auto rcPtr = result.getType().dyn_cast<RcType>())
          populate(result.cast<mlir::TypedValue<RcType>>());

      for (mlir::Value operand : op->getOperands())
        if (auto rcPtr = operand.getType().dyn_cast<RcType>())
          populate(operand.cast<mlir::TypedValue<RcType>>());
    });
    op->walk([&](mlir::Block *blk) {
      for (mlir::Value arg : blk->getArguments())
        if (auto rcPtr = arg.getType().dyn_cast<RcType>())
          populate(arg.cast<mlir::TypedValue<RcType>>());
    });
  }

  size_t getCluster(mlir::TypedValue<RcType> rcPtr) const {
    auto iter = std::find_if(rcPtrs.begin(), rcPtrs.end(),
                             [&](mlir::TypedValue<RcType> ptr) {
                               return aliasAnalysis.alias(rcPtr, ptr) ==
                                      mlir::AliasResult::MustAlias;
                             });
    return iter - rcPtrs.begin();
  }

  size_t size() const { return rcPtrs.size(); }
};
/// Analysis that tracks increment and decrement operations for cancellation
/// optimization.

class IncOpSequenceLattice : public mlir::dataflow::AbstractDenseLattice {
  using Op = ReussirRcIncOp;
  std::optional<UnsyncFlexVector<Op>> sequence = std::nullopt;
  static UnsyncFlexVector<Op> EMPTY_SEQUENCE;
  mlir::ChangeResult unify(std::optional<UnsyncFlexVector<Op>> rhs);

public:
  using AbstractDenseLattice::AbstractDenseLattice;
  size_t size() const { return sequence->size(); }
  bool isUninitialized() const { return sequence == std::nullopt; }
  bool isEmpty() const { return sequence->empty(); }
  static std::optional<UnsyncFlexVector<Op>>
  join(std::optional<UnsyncFlexVector<Op>> lhs,
       std::optional<UnsyncFlexVector<Op>> rhs);
  mlir::ChangeResult join(const AbstractDenseLattice &rhs) override;
  void print(llvm::raw_ostream &os) const override;
  mlir::ChangeResult setToEmpty();
  mlir::ChangeResult setAsAppend(const IncOpSequenceLattice &prefix, Op op);
  mlir::ChangeResult setAsRemove(const IncOpSequenceLattice &prefix,
                                 mlir::TypedValue<RcType> rcPtr,
                                 mlir::AliasAnalysis &aliasAnalysis);
  mlir::ChangeResult unify(const AbstractDenseLattice &rhs);
  mlir::ChangeResult
  setAsRemoveAll(const IncOpSequenceLattice &prefix,
                 llvm::ArrayRef<mlir::TypedValue<RcType>> rcPtrs,
                 mlir::AliasAnalysis &aliasAnalysis);
};

class CancellationAnalysis
    : public mlir::dataflow::DenseForwardDataFlowAnalysis<
          IncOpSequenceLattice> {
public:
  CancellationAnalysis(mlir::DataFlowSolver &solver,
                       mlir::AliasAnalysis &aliasAnalysis)
      : DenseForwardDataFlowAnalysis(solver), aliasAnalysis(aliasAnalysis) {}

  /// Visit an operation with the dense lattice before its execution.
  /// This function is expected to set the dense lattice after its execution
  /// and trigger change propagation in case of change.
  mlir::LogicalResult visitOperation(mlir::Operation *op,
                                     const IncOpSequenceLattice &before,
                                     IncOpSequenceLattice *after) override;

  /// Hook for customizing the behavior of lattice propagation along the call
  /// control flow edges.
  void visitCallControlFlowTransfer(
      mlir::CallOpInterface call, mlir::dataflow::CallControlFlowAction action,
      const IncOpSequenceLattice &before, IncOpSequenceLattice *after) override;

protected:
  /// Set the dense lattice at control flow entry point and propagate an update
  /// if it changed.
  void setToEntryState(IncOpSequenceLattice *lattice) override;

  mlir::AliasAnalysis &aliasAnalysis;
};

} // namespace reussir

#endif // REUSSIR_ANALYSIS_CANCELLATIONANALYSIS_H
