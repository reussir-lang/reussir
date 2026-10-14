// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include "Reussir/Analysis/PjrtDonationAnalysis.h"
#include "Reussir/IR/ReussirOps.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <mlir/Analysis/DataFlow/ConstantPropagationAnalysis.h>
#include <mlir/Analysis/DataFlow/DeadCodeAnalysis.h>
#include <mlir/Analysis/DataFlow/SparseAnalysis.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Dominance.h>
#include <mlir/IR/Matchers.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Interfaces/LoopLikeInterface.h>

using namespace mlir;
namespace reussir {
namespace {
bool targetArray(Type type) {
  auto rc = dyn_cast<RcType>(type);
  auto array = rc ? dyn_cast<ArrayType>(rc.getElementType()) : ArrayType{};
  return array && array.hasTargetAttr() &&
         rc.getCapability() == Capability::shared;
}
bool freshProducer(Operation *op) {
  return isa_and_nonnull<ReussirArrayToDeviceOp, ReussirArrayFromIfrtOp>(op);
}
bool bookkeeping(Operation *op) {
  return isa<ReussirRcDecOp, ReussirRcIsUniqueOp, ReussirRcAssumeUniqueOp>(op);
}
bool transparentBorrow(Operation *op) {
  return isa<ReussirRcBorrowOp, ReussirArrayToIfrtOp, ReussirRefProjectOp>(op);
}
struct WorkBudget {
  uint64_t work = 0, limit;
  bool exhausted = false;

  bool tick(uint64_t n = 1) {
    if (exhausted || n > limit - std::min(limit, work)) {
      exhausted = true;
      return false;
    }
    work += n;
    return true;
  }
};

struct Ownership {
  enum Kind { Uninitialized, Unique, Owned, Unknown } kind = Uninitialized;
  bool operator==(const Ownership &) const = default;
  static Ownership join(Ownership lhs, Ownership rhs) {
    return {std::max(lhs.kind, rhs.kind)};
  }
  void print(llvm::raw_ostream &os) const { os << kind; }
  bool isOwned() const { return kind == Unique || kind == Owned; }
};

class OwnershipLattice : public dataflow::Lattice<Ownership> {
public:
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(OwnershipLattice)
  explicit OwnershipLattice(Value value)
      : Lattice(value),
        shared(targetArray(value.getType()) &&
               llvm::any_of(value.getUsers(), [](Operation *op) {
                 return isa<ReussirRcIncOp, CallOpInterface>(op) ||
                        (!isa<BranchOpInterface, RegionBranchOpInterface,
                              RegionBranchTerminatorOpInterface, func::ReturnOp,
                              ReussirArrayToIfrtOp, ReussirArrayToHostOp,
                              ReussirRcBorrowOp>(op) &&
                         !bookkeeping(op));
               })) {}

  using Lattice::join;
  ChangeResult join(const AbstractSparseLattice &rhs) override {
    const auto &source = static_cast<const OwnershipLattice &>(rhs);
    auto facts = source.getValue();
    // A forwarded reference may have aliases left behind. Local balanced
    // retains are handled separately when querying the original SSA value.
    if (facts.kind == Ownership::Unique && source.shared)
      facts.kind = Ownership::Owned;
    return join(facts);
  }

private:
  const bool shared;
};

class OwnershipAnalysis
    : public dataflow::SparseForwardDataFlowAnalysis<OwnershipLattice> {
public:
  OwnershipAnalysis(DataFlowSolver &solver, WorkBudget &budget)
      : SparseForwardDataFlowAnalysis(solver), budget(budget) {}

  LogicalResult visit(ProgramPoint *point) override {
    if (!budget.tick())
      return failure();
    return SparseForwardDataFlowAnalysis::visit(point);
  }

  LogicalResult visitOperation(Operation *op,
                               ArrayRef<const OwnershipLattice *>,
                               ArrayRef<OwnershipLattice *> results) override {
    if (!budget.tick())
      return failure();
    for (auto *result : results) {
      Ownership facts{targetArray(result->getAnchor().getType()) &&
                              freshProducer(op)
                          ? Ownership::Unique
                          : Ownership::Unknown};
      propagateIfChanged(result, result->join(facts));
    }
    return success();
  }

private:
  void setToEntryState(OwnershipLattice *lattice) override {
    Value value = lattice->getAnchor();
    auto arg = dyn_cast<BlockArgument>(value);
    bool owned = targetArray(value.getType()) &&
                 ((arg && isa<func::FuncOp>(arg.getOwner()->getParentOp())) ||
                  isa_and_nonnull<CallOpInterface>(value.getDefiningOp()));
    propagateIfChanged(lattice,
                       lattice->join(Ownership{owned ? Ownership::Owned
                                                     : Ownership::Unknown}));
  }

  void visitCallableOperation(
      CallableOpInterface,
      ArrayRef<dataflow::AbstractSparseLattice *> arguments) override {
    // A callee cannot assume that its caller has transferred a unique
    // reference. Return values still use the driver's interprocedural join.
    for (auto *argument : arguments)
      setToEntryState(static_cast<OwnershipLattice *>(argument));
  }

  WorkBudget &budget;
};
} // namespace

struct PjrtDonationAnalysis::Impl {
  struct Uses {
    SmallVector<Operation *, 4> contents;
    SmallVector<ReussirRcDecOp, 2> releases;
    SmallVector<ReussirRcIncOp, 2> retains;
    bool escapedBorrow = false;
  };
  ModuleOp module;
  DominanceInfo dominance;
  PostDominanceInfo postDominance;
  DataFlowSolver localSolver;
  std::unique_ptr<DataFlowSolver> moduleSolver;
  DenseSet<Operation *> solvedFunctions;
  WorkBudget budget;
  DenseMap<Value, Uses> uses;
  DenseMap<Block *, bool> cyclicBlocks;

  bool tick(uint64_t n = 1) { return budget.tick(n); }

  explicit Impl(ModuleOp module, uint64_t limit)
      : module(module), dominance(module), postDominance(module),
        budget{0, limit} {
    module.walk([&](Operation *) {
      return tick() ? WalkResult::advance() : WalkResult::interrupt();
    });
  }

  DataFlowSolver *getSolver(Value value) {
    if (moduleSolver)
      return moduleSolver.get();
    auto function = value.getParentRegion()->getParentOfType<func::FuncOp>();
    if (function && solvedFunctions.contains(function))
      return &localSolver;
    // Function scopes have disjoint SSA values and no array-return summaries.
    // Reuse the driver and its completed states across these scopes.
    bool interprocedural =
        !function ||
        function
            .walk([&](CallOpInterface call) {
              return llvm::any_of(call->getResultTypes(), targetArray)
                         ? WalkResult::interrupt()
                         : WalkResult::advance();
            })
            .wasInterrupted();
    if (interprocedural)
      moduleSolver = std::make_unique<DataFlowSolver>();
    auto *solver = interprocedural ? moduleSolver.get() : &localSolver;
    if (interprocedural || solvedFunctions.empty()) {
      solver->load<dataflow::DeadCodeAnalysis>();
      solver->load<OwnershipAnalysis>(budget);
    }
    Operation *scope =
        interprocedural ? module.getOperation() : function.getOperation();
    // Donation needs conservative reachability, not another SCCP run. Seed
    // control-flow inputs with literal constants or overdefined values.
    auto initialized = scope->walk([&](Operation *op) {
      if (!tick())
        return WalkResult::interrupt();
      if (isa<BranchOpInterface, RegionBranchOpInterface,
              RegionBranchTerminatorOpInterface>(op)) {
        for (Value operand : op->getOperands()) {
          Attribute constant;
          matchPattern(operand, m_Constant(&constant));
          solver
              ->getOrCreateState<dataflow::Lattice<dataflow::ConstantValue>>(
                  operand)
              ->join(dataflow::ConstantValue(constant, nullptr));
        }
      }
      return WalkResult::advance();
    });
    if (initialized.wasInterrupted())
      return nullptr;
    if (failed(solver->initializeAndRun(scope))) {
      budget.exhausted = true;
      return nullptr;
    }
    if (!interprocedural)
      solvedFunctions.insert(function);
    return solver;
  }

  Ownership getOwnership(Value value) {
    if (freshProducer(value.getDefiningOp()))
      return {Ownership::Unique};
    if (auto arg = dyn_cast<BlockArgument>(value);
        arg && arg.getOwner()->isEntryBlock() &&
        isa<func::FuncOp>(arg.getOwner()->getParentOp()))
      return {Ownership::Owned};
    auto *solver = getSolver(value);
    const auto *lattice =
        solver ? solver->lookupState<OwnershipLattice>(value) : nullptr;
    return lattice ? lattice->getValue() : Ownership{Ownership::Unknown};
  }

  void appendSources(Value value, SmallVectorImpl<Value> &sources) {
    auto argument = dyn_cast<BlockArgument>(value);
    Block *block = argument ? argument.getOwner() : nullptr;
    if (block && !block->isEntryBlock()) {
      for (auto it = block->pred_begin(); it != block->pred_end(); ++it) {
        if (!tick())
          return;
        auto branch = dyn_cast<BranchOpInterface>((*it)->getTerminator());
        if (!branch)
          continue;
        auto operands = branch.getSuccessorOperands(it.getSuccessorIndex());
        if (Value source = operands[argument.getArgNumber()])
          sources.push_back(source);
      }
      return;
    }
    Operation *op = block ? block->getParentOp() : value.getDefiningOp();
    auto branch = dyn_cast_or_null<RegionBranchOpInterface>(op);
    if (!branch)
      return;
    auto *solver = getSolver(value);
    if (!solver)
      return;
    auto *point = block ? solver->getProgramPointBefore(block)
                        : solver->getProgramPointAfter(op);
    const auto *predecessors =
        solver->lookupState<dataflow::PredecessorState>(point);
    if (!predecessors)
      return;
    RegionSuccessor successor =
        block ? RegionSuccessor(block->getParent()) : RegionSuccessor(op);
    for (Operation *predecessor : predecessors->getKnownPredecessors()) {
      if (!tick())
        return;
      ValueRange inputs = predecessors->getSuccessorInputs(predecessor);
      auto found = llvm::find(inputs, value);
      if (found == inputs.end())
        continue;
      auto operands = predecessor == op
                          ? branch.getEntrySuccessorOperands(successor)
                          : cast<RegionBranchTerminatorOpInterface>(predecessor)
                                .getSuccessorOperands(successor);
      sources.push_back(operands[found - inputs.begin()]);
    }
  }

  Uses &getUses(Value value) {
    auto [it, inserted] = uses.try_emplace(value);
    if (!inserted)
      return it->second;
    Uses &result = it->second;
    SmallVector<Value> pending{value};
    llvm::SmallPtrSet<Value, 16> seen;
    while (!pending.empty() && tick()) {
      Value current = pending.pop_back_val();
      if (!seen.insert(current).second)
        continue;
      bool borrowed = current != value;
      for (Operation *user : current.getUsers()) {
        if (!tick())
          break;
        if (!borrowed) {
          if (auto dec = dyn_cast<ReussirRcDecOp>(user)) {
            result.releases.push_back(dec);
            continue;
          }
          if (auto inc = dyn_cast<ReussirRcIncOp>(user)) {
            result.retains.push_back(inc);
            continue;
          }
          if (bookkeeping(user))
            continue;
        }
        if (transparentBorrow(user)) {
          llvm::append_range(pending, user->getResults());
          continue;
        }
        result.contents.push_back(user);
        if (borrowed && !isa<ReussirPJRTJitCallOp, ReussirPJRTAfterOp>(user) &&
            !isa<ReussirRefLoadOp>(user))
          result.escapedBorrow = true;
      }
    }
    return result;
  }

  bool canFollow(Operation *use, Operation *call, Value value) {
    if (use == call || insideMutuallyExclusiveRegions(use, call))
      return false;
    // Captured storage survives a loop iteration. A loop-carried SSA value is
    // checked separately: forwarding it on the backedge is itself a use.
    for (Operation *parent = call->getParentOp(); parent && parent != module;
         parent = parent->getParentOp()) {
      if (isa<LoopLikeOpInterface>(parent) && parent->isAncestor(use) &&
          !parent->isAncestor(value.getParentBlock()->getParentOp()))
        return true;
    }
    if (use->getBlock() == call->getBlock())
      return !use->isBeforeInBlock(call);
    if (cyclic(call->getBlock()) &&
        use->getParentRegion() == call->getParentRegion())
      return true;
    if (dominance.properlyDominates(use, call))
      return false;
    // A use inside a completed structured region precedes the call even when
    // the nested operation itself does not dominate the continuation block.
    Operation *enclosing = use;
    while (enclosing && enclosing->getParentRegion() != call->getParentRegion())
      enclosing = enclosing->getParentOp();
    if (enclosing && enclosing != call && !enclosing->isAncestor(call) &&
        dominance.properlyDominates(enclosing, call))
      return false;
    // Dominance alone cannot exclude a CFG backedge. Non-structured control
    // flow therefore needs the post-use liveness proof below to be decisive.
    return true;
  }

  bool cyclic(Block *block) {
    auto [it, inserted] = cyclicBlocks.try_emplace(block, false);
    if (!inserted)
      return it->second;
    SmallVector<Block *> pending(block->getSuccessors());
    llvm::SmallPtrSet<Block *, 16> seen;
    while (!pending.empty() && tick()) {
      Block *next = pending.pop_back_val();
      if (next == block)
        return it->second = true;
      if (seen.insert(next).second)
        llvm::append_range(pending, next->getSuccessors());
    }
    return it->second = budget.exhausted;
  }

  PjrtDonationAnalysis::Decision analyze(Value value,
                                         ReussirPJRTJitCallOp call) {
    using Decision = PjrtDonationAnalysis::Decision;
    if (!targetArray(value.getType()) || !tick())
      return Decision::Forbidden;
    Ownership facts = getOwnership(value);
    if (budget.exhausted)
      return Decision::Forbidden;
    for (Region *region = getEnclosingRepetitiveRegion(call); region;
         region = getEnclosingRepetitiveRegion(region->getParentOp()))
      if (!region->isAncestor(value.getParentRegion()))
        return Decision::Forbidden;
    if (cyclic(call->getBlock()) && value.getParentBlock() != call->getBlock())
      return Decision::Forbidden;
    SmallVector<Value> ancestors;
    appendSources(value, ancestors);
    llvm::SmallPtrSet<Value, 16> seen;
    while (!ancestors.empty() && tick()) {
      Value source = ancestors.pop_back_val();
      if (!seen.insert(source).second || source == value)
        continue;
      if (source.getParentBlock()
                  ->getParentOp()
                  ->getParentOfType<func::FuncOp>() !=
              call->getParentOfType<func::FuncOp>() &&
          source.getParentBlock()->getParentOp() !=
              call->getParentOfType<func::FuncOp>())
        continue;
      if (auto *definition = source.getDefiningOp();
          definition && getEnclosingRepetitiveRegion(definition) &&
          getEnclosingRepetitiveRegion(definition) ==
              getEnclosingRepetitiveRegion(call) &&
          dominance.properlyDominates(call, definition))
        continue;
      auto &sourceUses = getUses(source);
      if (sourceUses.escapedBorrow)
        return Decision::Forbidden;
      for (Operation *use : sourceUses.contents) {
        if (!tick())
          return Decision::Forbidden;
        if (isa<RegionBranchOpInterface>(use) && use->isAncestor(call))
          continue;
        if (isa<RegionBranchTerminatorOpInterface>(use) &&
            isa<LoopLikeOpInterface>(use->getParentOp()) &&
            use->getParentOp()->isAncestor(call))
          continue;
        if (canFollow(use, call, source))
          return Decision::Forbidden;
      }
      appendSources(source, ancestors);
    }
    Uses &u = getUses(value);
    if (u.escapedBorrow || budget.exhausted)
      return Decision::Forbidden;
    for (Operation *use : u.contents)
      if (!tick() || canFollow(use, call, value))
        return Decision::Forbidden;
    // Retaining the invalidated contents after execution is also a use.
    for (auto retain : u.retains)
      if (!tick() || canFollow(retain, call, value))
        return Decision::Forbidden;
    bool released = llvm::any_of(u.releases, [&](auto dec) {
      return postDominance.postDominates(dec, call);
    });
    // An unused borrow with count one is not an owned reference.
    if (!released)
      return Decision::Forbidden;
    bool owned = facts.isOwned();
    for (auto retain : u.retains)
      owned |= dominance.properlyDominates(retain, call);
    if (!owned)
      return Decision::Forbidden;
    if (facts.kind != Ownership::Unique)
      return Decision::Guarded;
    for (Operation *use : u.contents)
      if (use != call &&
          !isa<ReussirArrayToHostOp, ReussirPJRTJitCallOp, ReussirPJRTAfterOp>(
              use))
        return Decision::Guarded;
    int64_t extra = 0;
    for (auto retain : u.retains) {
      if (retain->getBlock() != call->getBlock())
        return Decision::Guarded;
      if (retain->isBeforeInBlock(call)) {
        llvm::APInt count(64, 1);
        if (retain.getDelta() &&
            !matchPattern(retain.getDelta(), m_ConstantInt(&count)))
          return Decision::Guarded;
        if (count.isNegative() || count.getActiveBits() > 63 ||
            count.getSExtValue() > INT64_MAX - extra)
          return Decision::Guarded;
        extra += count.getSExtValue();
      }
    }
    for (auto dec : u.releases) {
      if (dec->getBlock() == call->getBlock() && dec->isBeforeInBlock(call))
        extra -= 1;
      else if (!postDominance.postDominates(dec, call) &&
               !insideMutuallyExclusiveRegions(dec, call))
        return Decision::Guarded;
    }
    return extra == 0 ? Decision::Unconditional : Decision::Guarded;
  }
};

PjrtDonationAnalysis::PjrtDonationAnalysis(ModuleOp module, uint64_t maxWork)
    : impl(std::make_unique<Impl>(module, maxWork)) {}
PjrtDonationAnalysis::~PjrtDonationAnalysis() = default;
PjrtDonationAnalysis::Decision
PjrtDonationAnalysis::analyze(Value array, ReussirPJRTJitCallOp call) {
  return impl->analyze(array, call);
}
uint64_t PjrtDonationAnalysis::getWork() const { return impl->budget.work; }
bool PjrtDonationAnalysis::exhausted() const { return impl->budget.exhausted; }
} // namespace reussir
