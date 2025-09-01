#include <algorithm>
#include <cstddef>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>
#include <mlir/Analysis/AliasAnalysis.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Operation.h>
#include <mlir/IR/Region.h>
#include <mlir/IR/Value.h>
#include <mlir/Support/LLVM.h>

#include "Reussir/Analysis/CancellationAnalysis.h"
#include "Reussir/IR/ReussirOps.h"

namespace reussir {

using namespace mlir;

//===----------------------------------------------------------------------===//
// RcAliasCluster Implementation
//===----------------------------------------------------------------------===//

void RcAliasCluster::populate(TypedValue<RcType> rcPtr) {
  auto iter =
      std::find_if(rcPtrs.begin(), rcPtrs.end(), [&](TypedValue<RcType> ptr) {
        return aliasAnalysis.alias(rcPtr, ptr) == AliasResult::MustAlias;
      });
  if (iter != rcPtrs.end())
    return;
  rcPtrs.push_back(rcPtr);
}

RcAliasCluster::RcAliasCluster(AliasAnalysis &aliasAnalysis, Operation *op)
    : aliasAnalysis(aliasAnalysis) {
  // Initialize by iterating all RC pointers involved in operations
  op->walk([&](Operation *op) {
    for (Value result : op->getResults())
      if (auto rcPtr = result.dyn_cast<TypedValue<RcType>>())
        populate(rcPtr);

    for (Value operand : op->getOperands())
      if (auto rcPtr = operand.dyn_cast<TypedValue<RcType>>())
        populate(rcPtr);
  });
}

size_t RcAliasCluster::getCluster(TypedValue<RcType> rcPtr) const {
  auto iter =
      std::find_if(rcPtrs.begin(), rcPtrs.end(), [&](TypedValue<RcType> ptr) {
        return aliasAnalysis.alias(rcPtr, ptr) == AliasResult::MustAlias;
      });
  return iter - rcPtrs.begin();
}

//===----------------------------------------------------------------------===//
// RcIncLattice Implementation
//===----------------------------------------------------------------------===//
ChangeResult RcIncLattice::unify(RcIncMap rhs) {
  if (pendingIncrement == rhs)
    return ChangeResult::NoChange;
  pendingIncrement = rhs;
  return ChangeResult::Change;
}

size_t RcIncLattice::getIncCount(const RcIncMap &map, size_t clusterIndex) {
  if (auto iter = map.find(clusterIndex))
    return *iter;
  return 0;
}

RcIncLattice::RcIncMap RcIncLattice::join(RcIncMap lhs, RcIncMap rhs) {
  if (lhs == rhs)
    return lhs;
  RcIncMap result{};
  for (auto [clusterIndex, count] : lhs) {
    auto rhsCount = getIncCount(rhs, clusterIndex);
    size_t minCount = std::min(count, rhsCount);
    if (minCount > 0)
      result = result.insert({clusterIndex, minCount});
  }
  return result;
}

ChangeResult RcIncLattice::join(const AbstractDenseLattice &rhs) {
  auto rhsLattice = static_cast<const RcIncLattice &>(rhs);
  return unify(join(pendingIncrement, rhsLattice.pendingIncrement));
}

void RcIncLattice::print(raw_ostream &os) const {
  os << "{";
  llvm::interleaveComma(pendingIncrement, os, [&](auto pair) {
    os << std::get<0>(pair) << ": " << std::get<1>(pair);
  });
  os << "}";
}

ChangeResult RcIncLattice::setToEmpty() { return unify(RcIncMap()); }

ChangeResult RcIncLattice::setAsAppend(const AbstractDenseLattice &prefix,
                                       size_t clusterIndex) {
  auto prefixLattice = static_cast<const RcIncLattice &>(prefix);
  RcIncMap result = prefixLattice.pendingIncrement.update(
      clusterIndex, [](size_t count) { return count + 1; });
  return unify(result);
}

ChangeResult RcIncLattice::setAsRemove(const AbstractDenseLattice &prefix,
                                       size_t clusterIndex) {
  auto prefixLattice = static_cast<const RcIncLattice &>(prefix);
  auto count = getIncCount(prefixLattice.pendingIncrement, clusterIndex);
  if (count == 0)
    return ChangeResult::NoChange;
  if (count == 1)
    return unify(pendingIncrement.erase(clusterIndex));
  return unify(prefixLattice.pendingIncrement.update(
      clusterIndex, [](size_t count) { return count - 1; }));
}

ChangeResult RcIncLattice::setAsCopy(const AbstractDenseLattice &rhs) {
  auto rhsLattice = static_cast<const RcIncLattice &>(rhs);
  return unify(rhsLattice.pendingIncrement);
}

//===----------------------------------------------------------------------===//
// CancellationAnalysis Implementation
//===----------------------------------------------------------------------===//

LogicalResult CancellationAnalysis::visitOperation(Operation *op,
                                                   const RcIncLattice &before,
                                                   RcIncLattice *after) {
  return llvm::TypeSwitch<Operation *, LogicalResult>(op)
      .Case<ReussirRcIncOp>([&](ReussirRcIncOp op) {
        auto clusterIndex = aliasCluster.getCluster(op.getRcPtr());
        propagateIfChanged(after, after->setAsAppend(before, clusterIndex));
        return success();
      })
      .Case<ReussirRcDecOp>([&](ReussirRcDecOp op) {
        auto clusterIndex = aliasCluster.getCluster(op.getRcPtr());
        propagateIfChanged(after, after->setAsRemove(before, clusterIndex));
        return success();
      })
      .Default([&](Operation *) {
        propagateIfChanged(after, after->setAsCopy(before));
        return success();
      });
}

void CancellationAnalysis::visitCallControlFlowTransfer(
    CallOpInterface call, dataflow::CallControlFlowAction action,
    const RcIncLattice &before, RcIncLattice *after) {
  // Brute forcelly forbid all cancellable consideration if there is a function
  // call.
  if (action == dataflow::CallControlFlowAction::ExitCallee)
    propagateIfChanged(after, after->setToEmpty());
}

void CancellationAnalysis::visitRegionBranchControlFlowTransfer(
    RegionBranchOpInterface branch, std::optional<unsigned> regionFrom,
    std::optional<unsigned> regionTo, const RcIncLattice &before,
    RcIncLattice *after) {
  if (regionFrom)
    join(after, before);
  else
    propagateIfChanged(after, after->setAsCopy(before));
}

void CancellationAnalysis::setToEntryState(RcIncLattice *lattice) {
  propagateIfChanged(lattice, lattice->setToEmpty());
}

} // namespace reussir
