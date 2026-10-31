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
/// `reussir-borrow-specialization`, which acts on `BorrowAnalysis`.
///
/// A call whose argument is live-through at a borrowable position is switched
/// to a private clone of the callee that takes the argument borrowed. The
/// owned original stays for every other call. The caller's retain feeding the
/// call goes away; the clone neither retains nor releases the parameter. The
/// pass follows the analysis's call sites; a call in a clone is classified as
/// the call it copies.
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

#define GEN_PASS_DEF_REUSSIRBORROWSPECIALIZATIONPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {
// Counted values loaded through `root`: a read-only `rc.borrow`, then
// projections and dispatch arms, then a load.
void collectLoans(mlir::Value root, llvm::SmallVectorImpl<mlir::Value> &out) {
  llvm::SmallVector<mlir::Value> refs;
  for (mlir::Operation *user : root.getUsers())
    if (auto borrow = llvm::dyn_cast<ReussirRcBorrowOp>(user);
        borrow && isReadOnlyBorrow(borrow))
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
      : module(module), symbols(module), analysis(module) {
    for (const BorrowAnalysis::CallSite &site : analysis.getCallSites())
      sites[site.call] = &site;
  }

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
    mlir::IRMapping mapping;
    auto clone = clonePrivateSpecialization(function, name, symbols, &mapping);
    function.walk([&](mlir::func::CallOp call) {
      copies[mapping.lookup(call.getOperation())] = call;
    });
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
    return isNonConsumingUse(use, [&](mlir::func::CallOp call, unsigned index) {
      Origin origin = resolve(call);
      return origin.function && analysis.isBorrowable(origin.function, index);
    });
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
  // pinned ones, and live-through ones whose feeding retain is removed.
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
      // A call in a clone is classified as the call it copies.
      mlir::func::CallOp original = copies.lookup(call);
      const BorrowAnalysis::CallSite *site =
          sites.lookup(original ? original : call);
      for (unsigned index :
           analysis.getBorrowable(origin.function).set_bits()) {
        mlir::Value arg = call.getOperand(index);
        if (positions.test(index))
          continue;
        if (pinned.contains(arg)) {
          positions.set(index);
          continue;
        }
        if (!site || !site->liveThrough.test(index))
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
  llvm::DenseMap<mlir::Operation *, const BorrowAnalysis::CallSite *> sites;
  // Calls in clones, mapped to the calls they copy.
  llvm::DenseMap<mlir::Operation *, mlir::func::CallOp> copies;
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
