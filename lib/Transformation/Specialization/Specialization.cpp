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
/// Function clones that a pass specializes and calls directly.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Transformation/Specialization.h"

#include <mlir/Dialect/LLVMIR/LLVMDialect.h>

namespace reussir {
namespace {

mlir::LocationAttr stripDebugMetadata(mlir::LocationAttr loc) {
  if (!loc)
    return loc;
  auto *context = loc.getContext();

  if (auto fusedLoc = llvm::dyn_cast<mlir::FusedLoc>(loc)) {
    llvm::SmallVector<mlir::Location> strippedLocations;
    strippedLocations.reserve(fusedLoc.getLocations().size());
    for (mlir::Location innerLoc : fusedLoc.getLocations())
      strippedLocations.push_back(stripDebugMetadata(innerLoc));
    return mlir::FusedLoc::get(context, strippedLocations);
  }

  if (auto nameLoc = llvm::dyn_cast<mlir::NameLoc>(loc))
    return mlir::NameLoc::get(nameLoc.getName(),
                              stripDebugMetadata(nameLoc.getChildLoc()));

  if (auto callSiteLoc = llvm::dyn_cast<mlir::CallSiteLoc>(loc))
    return mlir::CallSiteLoc::get(stripDebugMetadata(callSiteLoc.getCallee()),
                                  stripDebugMetadata(callSiteLoc.getCaller()));

  return loc;
}

void stripCloneDebugInfo(mlir::func::FuncOp funcOp) {
  funcOp->removeAttr("reussir.dbg_func_args");
  funcOp->setLoc(stripDebugMetadata(funcOp.getLoc()));
  funcOp.walk([&](mlir::Operation *op) {
    if (op == funcOp)
      return;
    op->setLoc(stripDebugMetadata(op->getLoc()));
  });
}

} // namespace

mlir::func::FuncOp clonePrivateSpecialization(mlir::func::FuncOp function,
                                              llvm::StringRef name,
                                              mlir::SymbolTable &symbolTable) {
  auto clone = llvm::cast<mlir::func::FuncOp>(function->clone());
  clone.setName(name);
  clone.setPrivate();
  clone->removeAttr("llvm.visibility");
  clone->setAttr("llvm.linkage",
                 mlir::LLVM::LinkageAttr::get(function.getContext(),
                                              mlir::LLVM::Linkage::Internal));
  stripCloneDebugInfo(clone);
  symbolTable.insert(clone);
  return clone;
}

} // namespace reussir
