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
/// Borrowable parameters and caller-live call sites; see BorrowAnalysis.h.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/FunctionInterfaces.h>
#include <mlir/Interfaces/LoopLikeInterface.h>

namespace reussir {
namespace {

using BorrowableMap = llvm::DenseMap<mlir::func::FuncOp, llvm::BitVector>;

// A function whose parameters may be borrowed: it has a body, and it is
// neither a clone that asserts its parameters unique (UCRA's `.unique`) nor
// one that already borrows.
bool isCandidate(mlir::func::FuncOp function) {
  if (function.isExternal() || function->hasAttr("reussir.unique_clone"))
    return false;
  return llvm::none_of(function.getArguments(), [](mlir::BlockArgument arg) {
    return isBorrowedArgument(arg);
  });
}

// Whether `use` leaves the parameter unconsumed, given the positions
// currently believed borrowable.
bool isNonConsuming(mlir::OpOperand &use, mlir::SymbolTable &symbols,
                    const BorrowableMap &borrowable) {
  mlir::Operation *user = use.getOwner();
  if (llvm::isa<ReussirRcBorrowOp, ReussirRcIncOp>(user))
    return true;
  // A release that only drops the reference keeps the parameter borrowable,
  // because a borrowed clone simply erases it:
  //   reussir.rc.dec(%p : !reussir.rc<!pair>)
  // Two releases do more than that, and consume the parameter:
  //   %t = reussir.rc.dec(%p : !reussir.rc<!pair>) : !reussir.nullable<...>
  // yields a reuse token that a later allocation takes over, and
  //   reussir.rc.dec(%p : !reussir.rc<!pair>) {boundMembers = array<i64: 1>}
  // destructures: it also hands a retain to member 1, which
  // `reussir-rc-dispatch-fusion` folded into it.
  if (auto dec = llvm::dyn_cast<ReussirRcDecOp>(user))
    return !dec.isDestructuring() && dec->use_empty();
  if (auto call = llvm::dyn_cast<mlir::func::CallOp>(user)) {
    auto it =
        borrowable.find(symbols.lookup<mlir::func::FuncOp>(call.getCallee()));
    unsigned index = use.getOperandNumber();
    return it != borrowable.end() && index < it->second.size() &&
           it->second.test(index);
  }
  return false;
}

} // namespace

// Ops after `op` in its block run after it, including everything nested in
// them; then the same holds for each ancestor up to the function. Sibling
// regions of an ancestor (the other arm of an `scf.if`) do not run after
// `op`. In a loop, a value defined outside it is used again by the next
// iteration. (`mlir::Liveness::isDeadAfter` answers per block, so it misses
// uses that follow the region-holding op.)
bool isUsedAfter(mlir::Value value, mlir::Operation *op) {
  for (mlir::Operation *at = op;
       at && !llvm::isa<mlir::FunctionOpInterface>(at);
       at = at->getParentOp()) {
    if (at != op && llvm::isa<mlir::LoopLikeOpInterface>(at) &&
        !at->isAncestor(value.getParentBlock()->getParentOp()))
      return true;
    mlir::Block *block = at->getBlock();
    for (mlir::Operation *user : value.getUsers()) {
      mlir::Operation *ancestor = block->findAncestorOpInBlock(*user);
      if (ancestor && at->isBeforeInBlock(ancestor))
        return true;
    }
  }
  return false;
}

BorrowAnalysis::BorrowAnalysis(mlir::ModuleOp module) {
  mlir::SymbolTable symbols(module);
  for (auto function : module.getOps<mlir::func::FuncOp>()) {
    if (!isCandidate(function))
      continue;
    // Only immutable boxes: a flex or rigid object may be changed in place.
    llvm::BitVector counted(function.getNumArguments());
    for (auto [index, type] : llvm::enumerate(function.getArgumentTypes()))
      if (auto rc = llvm::dyn_cast<RcType>(type);
          rc && rc.getCapability() == Capability::shared)
        counted.set(index);
    if (counted.any())
      borrowable[function] = std::move(counted);
  }

  // Greatest fixed point: drop a position as soon as one use consumes it.
  // The result does not depend on the visiting order.
  for (bool changed = true; changed;) {
    changed = false;
    for (auto &[function, positions] : borrowable) {
      llvm::BitVector current = positions;
      for (unsigned index : current.set_bits()) {
        auto uses = function.getArgument(index).getUses();
        if (llvm::all_of(uses, [&](mlir::OpOperand &use) {
              return isNonConsuming(use, symbols, borrowable);
            }))
          continue;
        positions.reset(index);
        changed = true;
      }
    }
  }
  borrowable.remove_if([](auto &entry) { return entry.second.none(); });

  module.walk([&](mlir::func::CallOp call) {
    auto callee = symbols.lookup<mlir::func::FuncOp>(call.getCallee());
    auto it = borrowable.find(callee);
    if (it == borrowable.end())
      return;
    unsigned arity = callee.getNumArguments();
    CallSite site{call, callee, llvm::BitVector(arity), llvm::BitVector(arity)};
    for (unsigned index : it->second.set_bits())
      (isUsedAfter(call.getOperand(index), call) ? site.callerLive
                                                 : site.lastUse)
          .set(index);
    callSites.push_back(std::move(site));
  });
}

llvm::BitVector
BorrowAnalysis::getBorrowable(mlir::func::FuncOp function) const {
  auto it = borrowable.find(function);
  return it == borrowable.end() ? llvm::BitVector() : it->second;
}

bool BorrowAnalysis::isBorrowable(mlir::func::FuncOp function,
                                  unsigned index) const {
  auto it = borrowable.find(function);
  return it != borrowable.end() && index < it->second.size() &&
         it->second.test(index);
}

} // namespace reussir
