// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include "Reussir/Analysis/PjrtDonationAnalysis.h"
#include "Reussir/IR/ReussirOps.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Dominance.h>
#include <mlir/IR/Matchers.h>
#include <mlir/IR/SymbolTable.h>
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
} // namespace

struct PjrtDonationAnalysis::Impl {
  struct Node {
    Value value;
    SmallVector<unsigned, 2> sources, users;
    bool initialized = false;
    bool unique = false;
    bool owned = false;
    bool shared = false;
  };
  struct Uses {
    SmallVector<Operation *, 4> contents, borrowedContents;
    SmallVector<ReussirRcDecOp, 2> releases;
    SmallVector<ReussirRcIncOp, 2> retains;
    bool escapedBorrow = false;
  };
  ModuleOp module;
  DominanceInfo dominance;
  PostDominanceInfo postDominance;
  SymbolTableCollection symbols;
  DenseMap<Value, unsigned> indices;
  SmallVector<Node> nodes;
  DenseMap<Value, Uses> uses;
  DenseMap<Block *, bool> cyclicBlocks;
  uint64_t work = 0, limit;
  bool overBudget = false;

  bool tick(uint64_t n = 1) {
    if (overBudget || n > limit - std::min(limit, work)) {
      overBudget = true;
      return false;
    }
    work += n;
    return true;
  }
  unsigned index(Value value) {
    auto [it, inserted] = indices.try_emplace(value, nodes.size());
    if (inserted) {
      nodes.emplace_back();
      nodes.back().value = value;
    }
    return it->second;
  }
  void edge(Value source, Value destination) {
    if (!targetArray(source.getType()) || !targetArray(destination.getType()))
      return;
    unsigned src = index(source), dst = index(destination);
    nodes[src].users.push_back(dst);
    nodes[dst].sources.push_back(src);
  }
  void edges(ValueRange sources, ValueRange destinations) {
    if (sources.size() != destinations.size())
      return;
    for (auto [source, destination] : llvm::zip_equal(sources, destinations))
      edge(source, destination);
  }
  explicit Impl(ModuleOp module, uint64_t limit)
      : module(module), dominance(module), postDominance(module), limit(limit) {
    module.walk([&](Operation *op) {
      if (!tick())
        return;
      for (Value result : op->getResults())
        if (targetArray(result.getType()))
          index(result);
      for (Region &region : op->getRegions())
        for (Block &block : region)
          for (Value arg : block.getArguments())
            if (targetArray(arg.getType()))
              index(arg);
      if (auto branch = dyn_cast<BranchOpInterface>(op)) {
        for (unsigned i = 0; i < op->getNumSuccessors(); ++i) {
          auto operands = branch.getSuccessorOperands(i);
          edges(operands.getForwardedOperands(),
                op->getSuccessor(i)->getArguments().drop_front(
                    operands.getProducedOperandCount()));
        }
      }
      if (auto branch = dyn_cast<RegionBranchOpInterface>(op)) {
        SmallVector<RegionSuccessor> successors;
        branch.getSuccessorRegions(RegionBranchPoint::parent(), successors);
        for (auto successor : successors)
          edges(branch.getEntrySuccessorOperands(successor),
                branch.getSuccessorInputs(successor));
        for (Region &region : op->getRegions())
          for (Block &block : region)
            if (auto term = dyn_cast<RegionBranchTerminatorOpInterface>(
                    block.getTerminator())) {
              successors.clear();
              branch.getSuccessorRegions(region, successors);
              for (auto successor : successors)
                edges(term.getSuccessorOperands(successor),
                      branch.getSuccessorInputs(successor));
            }
      }
    });
    // Callee results are shared summary nodes. Entry arguments remain unknown;
    // only results independent of those arguments can become statically unique.
    DenseMap<Operation *, SmallVector<func::ReturnOp>> returns;
    module.walk([&](func::CallOp call) {
      if (!tick())
        return;
      auto callee = symbols.lookupNearestSymbolFrom<func::FuncOp>(
          call, call.getCalleeAttr());
      if (!callee || callee.isDeclaration())
        return;
      auto [it, inserted] = returns.try_emplace(callee);
      if (inserted)
        callee.walk([&](func::ReturnOp ret) {
          if (tick() && ret->getParentOfType<func::FuncOp>() == callee)
            it->second.push_back(ret);
        });
      for (auto ret : it->second)
        if (tick())
          edges(ret.getOperands(), call.getResults());
    });
    llvm::SetVector<unsigned> pending;
    for (unsigned i = 0; i < nodes.size(); ++i) {
      Node &node = nodes[i];
      node.shared = llvm::any_of(node.value.getUsers(), [](Operation *op) {
        return isa<ReussirRcIncOp, func::CallOp>(op) ||
               (!isa<BranchOpInterface, RegionBranchOpInterface,
                     RegionBranchTerminatorOpInterface, func::ReturnOp,
                     ReussirArrayToIfrtOp, ReussirArrayToHostOp,
                     ReussirRcBorrowOp>(op) &&
                !bookkeeping(op));
      });
      if (freshProducer(node.value.getDefiningOp())) {
        node.initialized = node.owned = node.unique = true;
      } else if (node.sources.empty()) {
        node.initialized = true;
        auto arg = dyn_cast<BlockArgument>(node.value);
        node.owned =
            (arg && isa<func::FuncOp>(arg.getOwner()->getParentOp())) ||
            isa_and_nonnull<func::CallOp>(node.value.getDefiningOp());
      }
      pending.insert(i);
    }
    while (!pending.empty() && tick()) {
      unsigned i = pending.pop_back_val();
      Node &node = nodes[i];
      if (node.sources.empty() || freshProducer(node.value.getDefiningOp()))
        continue;
      bool initialized = false, unique = true, owned = true;
      for (unsigned source : node.sources) {
        if (!tick())
          break;
        Node &from = nodes[source];
        if (!from.initialized)
          continue;
        initialized = true;
        owned &= from.owned;
        unique &= from.unique && !from.shared;
      }
      if (!initialized)
        continue;
      unique &= owned;
      if (node.initialized && node.unique == unique && node.owned == owned)
        continue;
      node.initialized = true;
      node.unique = unique;
      node.owned = owned;
      for (unsigned user : node.users)
        pending.insert(user);
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
        if (borrowed)
          result.borrowedContents.push_back(user);
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
    return it->second = overBudget;
  }

  PjrtDonationAnalysis::Decision analyze(Value value,
                                         ReussirPJRTJitCallOp call) {
    using Decision = PjrtDonationAnalysis::Decision;
    if (!targetArray(value.getType()) || !tick())
      return Decision::Forbidden;
    auto found = indices.find(value);
    if (found == indices.end())
      return Decision::Forbidden;
    for (Region *region = getEnclosingRepetitiveRegion(call); region;
         region = getEnclosingRepetitiveRegion(region->getParentOp()))
      if (!region->isAncestor(value.getParentRegion()))
        return Decision::Forbidden;
    if (cyclic(call->getBlock()) && value.getParentBlock() != call->getBlock())
      return Decision::Forbidden;
    SmallVector<unsigned> ancestors(
        llvm::ArrayRef<unsigned>(nodes[found->second].sources));
    llvm::SmallDenseSet<unsigned, 16> seen;
    while (!ancestors.empty() && tick()) {
      unsigned i = ancestors.pop_back_val();
      if (!seen.insert(i).second || nodes[i].value == value)
        continue;
      auto source = nodes[i].value;
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
      llvm::append_range(ancestors, nodes[i].sources);
    }
    Uses &u = getUses(value);
    if (u.escapedBorrow || overBudget)
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
    Node &node = nodes[found->second];
    bool owned = node.initialized && node.owned;
    for (auto retain : u.retains)
      owned |= dominance.properlyDominates(retain, call);
    if (!owned)
      return Decision::Forbidden;
    if (!node.unique)
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
uint64_t PjrtDonationAnalysis::getWork() const { return impl->work; }
bool PjrtDonationAnalysis::exhausted() const { return impl->overBudget; }
} // namespace reussir
