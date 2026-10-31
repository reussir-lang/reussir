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
/// Borrowed-parameter passes. `reussir-borrow-analysis` reports what
/// `BorrowAnalysis` finds without changing the module.
/// `reussir-borrow-specialization` acts on it.
///
/// A call whose argument is caller-live at a borrowable position is switched
/// to a private clone of the callee that takes the argument borrowed. The
/// owned original stays for every other call. The caller's retain feeding the
/// call goes away; the clone neither retains nor releases the parameter.
///
/// In a clone, the borrowed parameters are pinned: their caller keeps them
/// alive for the whole call. So is every counted value loaded through them,
/// because fields of a shared box never change. A pinned value that the body
/// only releases or passes at borrowable positions needs no reference: its
/// retains and releases are erased, and those calls are switched too (a
/// pinned argument always outlives the call). A loan that is stored, returned
/// or passed owned keeps the owned body's retain and its calls stay owned.
/// The parameters themselves never escape (`BorrowAnalysis`). No release is
/// ever added after a call, so tail calls stay tail calls.
///
/// Clones are made on demand, one per callee and set of borrowed positions,
/// and their bodies get the same treatment; this covers recursion, mutual
/// recursion and chains of borrowed calls.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/Transformation/Passes.h"
#include "Reussir/Transformation/Specialization.h"

#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/StringMap.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRBORROWANALYSISPASS
#define GEN_PASS_DEF_REUSSIRBORROWSPECIALIZATIONPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {
struct BorrowAnalysisPass
    : public impl::ReussirBorrowAnalysisPassBase<BorrowAnalysisPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    BorrowAnalysis analysis(module);
    for (auto function : module.getOps<mlir::func::FuncOp>())
      for (unsigned index : analysis.getBorrowable(function).set_bits())
        function.emitRemark() << "parameter #" << index << " is borrowable";
    for (const BorrowAnalysis::CallSite &site : analysis.getCallSites()) {
      mlir::Operation *call = site.call;
      for (unsigned index : site.callerLive.set_bits())
        call->emitRemark() << "argument #" << index
                           << " is caller-live: may be borrowed";
      for (unsigned index : site.lastUse.set_bits())
        call->emitRemark() << "argument #" << index
                           << " is at its last use: stays owned";
    }
  }
};

// The retain right before `call` that gives it its reference to `value`. In
// between there may only be ops that cannot release anything: retains of
// other values, or ops that take no counted value and have no regions.
ReussirRcIncOp findFeedingRetain(mlir::Value value, mlir::func::CallOp call) {
  for (mlir::Operation *op = call->getPrevNode(); op; op = op->getPrevNode()) {
    if (auto retain = llvm::dyn_cast<ReussirRcIncOp>(op)) {
      if (retain.getRcPtr() == value)
        return retain.getDelta() ? ReussirRcIncOp() : retain;
      continue;
    }
    if (op->getNumRegions() != 0 ||
        llvm::any_of(op->getOperandTypes(),
                     [](mlir::Type type) { return llvm::isa<RcType>(type); }))
      return {};
  }
  return {};
}

// Counted values loaded through `root`: an `rc.borrow`, then projections and
// dispatch arms, then a load.
void collectLoans(mlir::Value root, llvm::SmallVectorImpl<mlir::Value> &out) {
  llvm::SmallVector<mlir::Value> refs;
  for (mlir::Operation *user : root.getUsers())
    if (auto borrow = llvm::dyn_cast<ReussirRcBorrowOp>(user))
      refs.push_back(borrow.getResult());
  for (size_t i = 0; i < refs.size(); ++i)
    for (mlir::Operation *user : refs[i].getUsers()) {
      if (auto project = llvm::dyn_cast<ReussirRefProjectOp>(user))
        refs.push_back(project.getResult());
      else if (llvm::isa<ReussirRecordDispatchOp>(user))
        for (mlir::Region &arm : user->getRegions())
          llvm::append_range(refs, arm.getArguments());
      else if (auto load = llvm::dyn_cast<ReussirRefLoadOp>(user);
               load && llvm::isa<RcType>(load.getType()))
        out.push_back(load.getResult());
    }
}

class BorrowSpecializer {
public:
  explicit BorrowSpecializer(mlir::ModuleOp module)
      : module(module), symbols(module), analysis(module) {}

  mlir::LogicalResult run() {
    llvm::SmallVector<mlir::func::FuncOp> functions(
        module.getOps<mlir::func::FuncOp>());
    for (auto function : functions)
      if (!function.isExternal())
        rewriteCalls(function, {});
    while (!pending.empty()) {
      auto [clone, positions] = pending.pop_back_val();
      llvm::DenseSet<mlir::Value> pinned;
      if (mlir::failed(unpin(clone, positions, pinned)))
        return mlir::failure();
      rewriteCalls(clone, pinned);
    }
    return mlir::success();
  }

private:
  // A callee as the function it copies and the positions it borrows.
  struct Origin {
    mlir::func::FuncOp function;
    llvm::BitVector positions;
  };

  Origin resolve(mlir::func::CallOp call) {
    auto callee = symbols.lookup<mlir::func::FuncOp>(call.getCallee());
    if (auto it = origins.find(callee); it != origins.end())
      return it->second;
    return {callee, llvm::BitVector(callee ? callee.getNumArguments() : 0u)};
  }

  mlir::func::FuncOp getClone(mlir::func::FuncOp function,
                              const llvm::BitVector &positions) {
    std::string name = (function.getName() + ".borrowed").str();
    for (unsigned index : positions.set_bits())
      name += "_" + std::to_string(index);
    if (auto it = clones.find(name); it != clones.end())
      return it->second;
    auto clone = clonePrivateSpecialization(function, name, symbols);
    for (unsigned index : positions.set_bits())
      clone.setArgAttr(index, BorrowedAttr::name,
                       BorrowedAttr::get(module.getContext()));
    clones[name] = clone;
    origins[clone] = {function, positions};
    pending.push_back({clone, positions});
    return clone;
  }

  // A use that leaves a pinned value unconsumed once the calls are switched.
  bool isPinnedUse(mlir::OpOperand &use) {
    mlir::Operation *user = use.getOwner();
    if (llvm::isa<ReussirRcBorrowOp, ReussirRcIncOp>(user))
      return true;
    if (auto dec = llvm::dyn_cast<ReussirRcDecOp>(user))
      return !dec.isDestructuring() && dec->use_empty();
    if (auto call = llvm::dyn_cast<mlir::func::CallOp>(user)) {
      Origin origin = resolve(call);
      return origin.function &&
             analysis.isBorrowable(origin.function, use.getOperandNumber());
    }
    return false;
  }

  // Erases the counts of the clone's pinned values that need none and
  // collects those values: the borrowed parameters, and the loans through
  // them that never escape.
  mlir::LogicalResult unpin(mlir::func::FuncOp clone,
                            const llvm::BitVector &positions,
                            llvm::DenseSet<mlir::Value> &pinned) {
    llvm::SmallVector<mlir::Value> candidates;
    for (unsigned index : positions.set_bits())
      candidates.push_back(clone.getArgument(index));
    for (size_t i = 0; i < candidates.size(); ++i)
      collectLoans(candidates[i], candidates);
    for (mlir::Value value : candidates)
      if (llvm::all_of(value.getUses(),
                       [&](mlir::OpOperand &use) { return isPinnedUse(use); }))
        pinned.insert(value);
    for (unsigned index : positions.set_bits())
      if (!pinned.contains(clone.getArgument(index)))
        return clone.emitError()
               << "borrowed parameter #" << index << " escapes its clone";
    for (mlir::Value value : pinned)
      for (mlir::Operation *user : llvm::make_early_inc_range(value.getUsers()))
        if (llvm::isa<ReussirRcIncOp, ReussirRcDecOp>(user))
          user->erase();
    return mlir::success();
  }

  // Switches the calls of `function` that can take arguments borrowed:
  // pinned ones, and caller-live ones whose feeding retain is removed.
  void rewriteCalls(mlir::func::FuncOp function,
                    const llvm::DenseSet<mlir::Value> &pinned) {
    llvm::SmallVector<mlir::func::CallOp> calls;
    function.walk([&](mlir::func::CallOp call) { calls.push_back(call); });
    for (auto call : calls) {
      Origin origin = resolve(call);
      if (!origin.function)
        continue;
      llvm::BitVector positions = origin.positions;
      llvm::SmallVector<ReussirRcIncOp> retains;
      for (unsigned index :
           analysis.getBorrowable(origin.function).set_bits()) {
        mlir::Value arg = call.getOperand(index);
        if (positions.test(index))
          continue;
        if (pinned.contains(arg)) {
          positions.set(index);
          continue;
        }
        if (llvm::count(call.getOperands(), arg) != 1 ||
            !isUsedAfter(arg, call))
          continue;
        if (auto retain = findFeedingRetain(arg, call)) {
          positions.set(index);
          retains.push_back(retain);
        }
      }
      if (positions == origin.positions)
        continue;
      // Clone before dropping the retains: the callee may be this very
      // function, and its clone must copy a body in which every call and
      // its retains still agree.
      mlir::func::FuncOp clone = getClone(origin.function, positions);
      for (ReussirRcIncOp retain : retains)
        retain.erase();
      call.setCallee(clone.getName());
    }
  }

  mlir::ModuleOp module;
  mlir::SymbolTable symbols;
  BorrowAnalysis analysis;
  llvm::StringMap<mlir::func::FuncOp> clones;
  llvm::DenseMap<mlir::func::FuncOp, Origin> origins;
  llvm::SmallVector<std::pair<mlir::func::FuncOp, llvm::BitVector>> pending;
};

struct BorrowSpecializationPass
    : public impl::ReussirBorrowSpecializationPassBase<
          BorrowSpecializationPass> {
  using Base::Base;
  void runOnOperation() override {
    if (mlir::failed(BorrowSpecializer(getOperation()).run()))
      signalPassFailure();
  }
};
} // namespace

} // namespace reussir
