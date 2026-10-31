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
/// Liveness of chosen values on the dense dataflow framework; see
/// ValueLiveness.h.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/ValueLiveness.h"

#include <mlir/Analysis/DataFlow/DenseAnalysis.h>
#include <mlir/Analysis/DataFlow/Utils.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>

namespace reussir {
namespace {

using namespace mlir;
using namespace mlir::dataflow;

// The chosen values that may be used from a program point on.
class LiveValues : public AbstractDenseLattice {
public:
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(LiveValues)
  using AbstractDenseLattice::AbstractDenseLattice;

  // Control-flow paths merge by union.
  ChangeResult meet(const AbstractDenseLattice &rhs) override {
    return addAllExcept(static_cast<const LiveValues &>(rhs), {});
  }

  // Adds what is live in `other`, except the values `defined` here.
  ChangeResult addAllExcept(const LiveValues &other, ValueRange defined) {
    ChangeResult changed = ChangeResult::NoChange;
    for (Value value : other.live)
      if (!llvm::is_contained(defined, value) && live.insert(value).second)
        changed = ChangeResult::Change;
    return changed;
  }

  ChangeResult add(Value value) {
    return live.insert(value).second ? ChangeResult::Change
                                     : ChangeResult::NoChange;
  }

  bool contains(Value value) const { return live.contains(value); }

  void print(raw_ostream &os) const override {
    os << live.size() << " live values";
  }

private:
  llvm::SmallDenseSet<Value, 4> live;
};

class LivenessAnalysis : public DenseBackwardDataFlowAnalysis<LiveValues> {
public:
  LivenessAnalysis(DataFlowSolver &solver, SymbolTableCollection &symbols,
                   const llvm::DenseSet<Value> &values)
      : DenseBackwardDataFlowAnalysis(solver, symbols), values(values) {}

  LogicalResult visitOperation(Operation *op, const LiveValues &after,
                               LiveValues *before) override {
    ChangeResult changed = before->addAllExcept(after, op->getResults());
    changed |= use(op, before);
    // The framework does not enter regions without a control-flow
    // interface, so what is used inside them is used by the op.
    for (Region &region : op->getRegions())
      region.walk([&](Operation *nested) { changed |= use(nested, before); });
    propagateIfChanged(before, changed);
    return success();
  }

  // Calls are opaque (the solver is not interprocedural).
  void visitCallControlFlowTransfer(CallOpInterface call, CallControlFlowAction,
                                    const LiveValues &after,
                                    LiveValues *before) override {
    (void)visitOperation(call, after, before);
  }

  // Entering the op uses its operands; the target's inputs (entry block
  // arguments, or the op's results) are defined on the edge.
  void visitRegionBranchControlFlowTransfer(RegionBranchOpInterface branch,
                                            RegionBranchPoint from,
                                            RegionSuccessor to,
                                            const LiveValues &after,
                                            LiveValues *before) override {
    ChangeResult changed =
        before->addAllExcept(after, branch.getSuccessorInputs(to));
    if (from.isParent())
      changed |= use(branch, before);
    propagateIfChanged(before, changed);
  }

  void visitBlockTransfer(Block *, ProgramPoint *, Block *successor,
                          const LiveValues &after,
                          LiveValues *before) override {
    propagateIfChanged(before,
                       before->addAllExcept(after, successor->getArguments()));
  }

  // Nothing is used after the function returns.
  void setToExitState(LiveValues *) override {}

private:
  ChangeResult use(Operation *op, LiveValues *live) {
    ChangeResult changed = ChangeResult::NoChange;
    for (Value operand : op->getOperands())
      if (values.contains(operand))
        changed |= live->add(operand);
    return changed;
  }

  const llvm::DenseSet<Value> &values;
};

} // namespace

ValueLiveness::ValueLiveness(Operation *function, ArrayRef<Value> chosen)
    : values(chosen.begin(), chosen.end()),
      solver(std::make_unique<DataFlowSolver>(
          DataFlowConfig().setInterprocedural(false))) {
  // Dense analyses only visit blocks that dead-code analysis, fed by
  // constant propagation, finds executable.
  loadBaselineAnalyses(*solver);
  solver->load<LivenessAnalysis>(symbols, values);
  solved = succeeded(solver->initializeAndRun(function));
}

bool ValueLiveness::isLiveAfter(Value value, Operation *op) const {
  if (!solved)
    return false;
  const auto *live =
      solver->lookupState<LiveValues>(solver->getProgramPointAfter(op));
  return live && live->contains(value);
}

} // namespace reussir
