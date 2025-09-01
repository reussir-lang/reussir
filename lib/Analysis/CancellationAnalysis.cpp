#include <algorithm>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/ScopeExit.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
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
// IncOpSequenceLattice Implementation
//===----------------------------------------------------------------------===//

UnsyncFlexVector<ReussirRcIncOp>
    reussir::IncOpSequenceLattice::EMPTY_SEQUENCE{};

std::optional<reussir::UnsyncFlexVector<reussir::ReussirRcIncOp>>
reussir::IncOpSequenceLattice::join(
    std::optional<UnsyncFlexVector<ReussirRcIncOp>> lhs,
    std::optional<UnsyncFlexVector<ReussirRcIncOp>> rhs) {
  if (lhs == std::nullopt)
    return rhs;
  if (rhs == std::nullopt)
    return lhs;
  if (lhs->empty() || rhs->empty())
    return EMPTY_SEQUENCE;
  if (lhs->identity() == rhs->identity())
    return lhs;
  llvm::SmallSet<ReussirRcIncOp, 8> lhsSet;
  UnsyncFlexVector<ReussirRcIncOp> orderedIntersection;
  auto lhsIter = lhs->begin();
  bool exactMatch = lhs->size() == rhs->size();
  for (auto &op : *rhs) {
    // We process the intersection lazily.
    // If we find that lhs and rhs exactly matches, we don't create a new
    // flex vector.
    if (exactMatch) {
      // Failed to find an exact match: initialize the process for computing
      // a new flex vector of the intersection.
      if (*lhsIter != op) {
        exactMatch = false;
        for (auto iter = lhsIter; iter != lhs->end(); ++iter)
          lhsSet.insert(*iter);
        orderedIntersection = lhs->take(lhsIter - lhs->begin());
      }
      ++lhsIter;
      continue;
    }
    if (lhsSet.contains(op))
      orderedIntersection = orderedIntersection.push_back(op);
  }
  return exactMatch ? lhs : orderedIntersection;
}

mlir::ChangeResult reussir::IncOpSequenceLattice::join(
    const mlir::dataflow::AbstractDenseLattice &rhs) {
  auto updatedSequence =
      join(sequence, static_cast<const IncOpSequenceLattice &>(rhs).sequence);
  return unify(updatedSequence);
}

void reussir::IncOpSequenceLattice::print(llvm::raw_ostream &os) const {
  if (isUninitialized())
    os << "uninitialized";
  else {
    os << '[';
    llvm::interleaveComma(*sequence, os);
    os << ']';
  }
}

mlir::ChangeResult reussir::IncOpSequenceLattice::setToEmpty() {
  auto guard = llvm::make_scope_exit([&] { sequence = EMPTY_SEQUENCE; });
  if (sequence && sequence->empty())
    return mlir::ChangeResult::NoChange;
  return mlir::ChangeResult::Change;
}

mlir::ChangeResult
reussir::IncOpSequenceLattice::unify(std::optional<UnsyncFlexVector<Op>> rhs) {
  mlir::ChangeResult result = mlir::ChangeResult::Change;
  if (sequence == std::nullopt && rhs == std::nullopt)
    result = mlir::ChangeResult::NoChange;
  else if (sequence != std::nullopt && rhs != std::nullopt)
    result = (sequence->identity() == rhs->identity() || *sequence == *rhs)
                 ? mlir::ChangeResult::NoChange
                 : mlir::ChangeResult::Change;
  if (result == mlir::ChangeResult::Change)
    sequence = rhs;
  return result;
};

mlir::ChangeResult
reussir::IncOpSequenceLattice::unify(const AbstractDenseLattice &rhs) {
  return unify(static_cast<const IncOpSequenceLattice &>(rhs).sequence);
}

mlir::ChangeResult
reussir::IncOpSequenceLattice::setAsAppend(const IncOpSequenceLattice &prefix,
                                           Op op) {
  if (prefix.isUninitialized())
    return unify(std::nullopt);
  if (prefix.isEmpty())
    return unify(UnsyncFlexVector<Op>{op});
  if (std::find(prefix.sequence->begin(), prefix.sequence->end(), op) !=
      prefix.sequence->end())
    return unify(prefix.sequence);
  return unify(prefix.sequence->push_back(op));
}

mlir::ChangeResult
reussir::IncOpSequenceLattice::setAsRemove(const IncOpSequenceLattice &prefix,
                                           mlir::TypedValue<RcType> rcPtr,
                                           mlir::AliasAnalysis &aliasAnalysis) {
  if (prefix.isUninitialized())
    return unify(std::nullopt);
  if (prefix.isEmpty())
    return setToEmpty();
  auto removeCandidate =
      std::find_if(prefix.sequence->begin(), prefix.sequence->end(),
                   [&aliasAnalysis, rcPtr](ReussirRcIncOp op) {
                     return aliasAnalysis.alias(op.getRcPtr(), rcPtr) ==
                            AliasResult::MustAlias;
                   });
  if (removeCandidate == prefix.sequence->end())
    return unify(prefix.sequence);
  return unify(
      prefix.sequence->erase(removeCandidate - prefix.sequence->begin()));
}

mlir::ChangeResult reussir::IncOpSequenceLattice::setAsRemoveAll(
    const IncOpSequenceLattice &prefix,
    llvm::ArrayRef<mlir::TypedValue<RcType>> rcPtrs,
    mlir::AliasAnalysis &aliasAnalysis) {
  if (prefix.isUninitialized())
    return unify(std::nullopt);
  if (prefix.isEmpty())
    return unify(std::nullopt);
  UnsyncFlexVector<ReussirRcIncOp> updatedSequence = *prefix.sequence;
  for (auto rcPtr : rcPtrs) {
    auto removeCandidate =
        std::find_if(updatedSequence.begin(), updatedSequence.end(),
                     [&aliasAnalysis, rcPtr](ReussirRcIncOp op) {
                       return aliasAnalysis.alias(op.getRcPtr(), rcPtr) ==
                              AliasResult::MustAlias;
                     });
    if (removeCandidate != updatedSequence.end())
      updatedSequence =
          updatedSequence.erase(removeCandidate - updatedSequence.begin());
  }
  return unify(updatedSequence);
}

//===----------------------------------------------------------------------===//
// CancellationAnalysis Implementation
//===----------------------------------------------------------------------===//
mlir::LogicalResult reussir::CancellationAnalysis::visitOperation(
    mlir::Operation *op, const IncOpSequenceLattice &before,
    IncOpSequenceLattice *after) {
  // TODO: Implement operation transfer function
  if (auto incOp = dyn_cast<ReussirRcIncOp>(op))
    propagateIfChanged(after, after->setAsAppend(before, incOp));
  else if (auto decOp = dyn_cast<ReussirRcDecOp>(op))
    propagateIfChanged(
        after, after->setAsRemove(before, decOp.getRcPtr(), aliasAnalysis));
  else
    propagateIfChanged(after, after->unify(before));
  return mlir::success();
}

void reussir::CancellationAnalysis::visitCallControlFlowTransfer(
    mlir::CallOpInterface call, mlir::dataflow::CallControlFlowAction action,
    const IncOpSequenceLattice &before, IncOpSequenceLattice *after) {
  DenseForwardDataFlowAnalysis::visitCallControlFlowTransfer(call, action,
                                                             before, after);
  // Remove all alisaed inc RC operations from lattice
  if (action == mlir::dataflow::CallControlFlowAction::ExitCallee) {
    llvm::SmallVector<mlir::TypedValue<RcType>> rcPtrs;
    for (mlir::Value operand : call->getOperands())
      if (auto rcValue = llvm::dyn_cast<mlir::TypedValue<RcType>>(operand))
        rcPtrs.push_back(rcValue);
    propagateIfChanged(after,
                       after->setAsRemoveAll(before, rcPtrs, aliasAnalysis));
  }
}

void reussir::CancellationAnalysis::setToEntryState(
    IncOpSequenceLattice *lattice) {
  propagateIfChanged(lattice, lattice->setToEmpty());
}

} // namespace reussir
