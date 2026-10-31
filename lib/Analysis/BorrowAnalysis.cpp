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
/// Borrowable parameters; see BorrowAnalysis.h.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/BorrowAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/FunctionInterfaces.h>

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
  if (llvm::isa<ReussirRcIncOp>(user))
    return true;
  if (auto borrow = llvm::dyn_cast<ReussirRcBorrowOp>(user))
    return isReadOnlyBorrow(borrow);
  // Releasing the parameter does not consume it: the clone only runs where
  // the caller keeps the object alive, so this release is never the last one
  // and the clone simply erases it. (The owned original, kept for last-use
  // calls, still releases, and that release may later donate its cell.)
  //
  // That holds for the plain releases the frontend inserts. Later passes
  // fold more into a release, and the specialization does not rewrite those
  // forms, so it keeps such a parameter owned (they do not occur where the
  // pipeline runs this analysis). `reussir-rc-dispatch-fusion` folds the
  // retains of members bound in a match arm into the scrutinee's release:
  //   reussir.record.dispatch(%r : ...) {
  //     [1] -> { ^bb0(%cons: !reussir.ref<!cons shared>):
  //       %f = reussir.ref.project(%cons : ...) [1] : ...
  //       %t = reussir.ref.load(%f : ...) : !reussir.rc<!list>
  //       reussir.rc.inc(%t : !reussir.rc<!list>)
  //       reussir.rc.dec(%p : !reussir.rc<!list>)
  //       ... } }
  // becomes, without the retain,
  //       reussir.rc.dec(%p : !reussir.rc<!list>)
  //           {destructureTag = 1 : i64, boundMembers = array<i64: 1>}
  // which on a unique box moves member 1 out instead of releasing it, and
  // otherwise performs the retain. `reussir-partial-move` binds the members
  // of a compound box the same way, without a tag. After token
  // instantiation every release yields a token, which a later allocation
  // reuses or `reussir.token.free` frees.
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

// Whether the reference is only read: each user leaves every count and
// stored value alone (`leavesCountsAlone`), and what it derives (a
// projection, a view) is only read too. A dispatch binds references to the
// payload in its arms; a terminator would pass the reference on.
bool isReadOnlyRef(mlir::Value ref) {
  return llvm::all_of(ref.getUsers(), [](mlir::Operation *user) {
    if (auto dispatch = llvm::dyn_cast<ReussirRecordDispatchOp>(user))
      return llvm::all_of(dispatch->getRegions(), [](mlir::Region &arm) {
        return llvm::all_of(arm.getArguments(), isReadOnlyRef);
      });
    if (user->hasTrait<mlir::OpTrait::IsTerminator>() ||
        !leavesCountsAlone(user))
      return false;
    return llvm::all_of(user->getResults(), [](mlir::Value result) {
      return !llvm::isa<RefType, mlir::MemRefType>(result.getType()) ||
             isReadOnlyRef(result);
    });
  });
}

} // namespace

bool isReadOnlyBorrow(ReussirRcBorrowOp borrow) {
  return isReadOnlyRef(borrow.getBorrowed());
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
