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
/// This file implements the Reussir dialect and its core functionality.
///
//===----------------------------------------------------------------------===//

// The dependent-dialect loading generated into the dialect constructor
// (`dependentDialects` in ReussirDialect.td) needs the sync dialect's
// declaration.
#include "Sync/IR/SyncDialect.h"

#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirTypes.h"

#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/FunctionInterfaces.h>

#include "Reussir/IR/ReussirDialect.cpp.inc"

namespace reussir {

void ReussirDialect::initialize() {
  // Register the builtin Attributes.
  registerAttributes();
  // Register the builtin Types.
  registerTypes();
  // Register the builtin Operations.
  registerOperations();
  // Register interfaces.
  registerInterfaces();
}

// Only where `#reussir.borrowed` may appear is checked; whether the function
// body treats the argument as borrowed is a property of the code that made
// it, not something the IR can decide.
mlir::LogicalResult ReussirDialect::verifyRegionArgAttribute(
    mlir::Operation *op, unsigned regionIndex, unsigned argIndex,
    mlir::NamedAttribute attribute) {
  if (attribute.getName() != BorrowedAttr::name)
    return mlir::success();
  auto function = llvm::dyn_cast<mlir::FunctionOpInterface>(op);
  if (!function || regionIndex != 0)
    return op->emitOpError() << "'" << BorrowedAttr::name
                             << "' is only valid on function arguments";
  if (!llvm::isa<BorrowedAttr>(attribute.getValue()))
    return op->emitOpError()
           << "'" << BorrowedAttr::name << "' must be #reussir.borrowed";
  // Only immutable boxes: a flex or rigid object may be changed in place.
  auto rc = llvm::dyn_cast<RcType>(function.getArgumentTypes()[argIndex]);
  if (!rc || rc.getCapability() != Capability::shared)
    return op->emitOpError() << "'" << BorrowedAttr::name << "' argument #"
                             << argIndex << " must be a shared !reussir.rc";
  // A declaration would be a contract on code we do not see, and a public
  // function has callers that pass the argument owned.
  if (function.isExternal())
    return op->emitOpError()
           << "'" << BorrowedAttr::name << "' requires a function body";
  if (mlir::SymbolTable::getSymbolVisibility(op) ==
      mlir::SymbolTable::Visibility::Public)
    return op->emitOpError()
           << "'" << BorrowedAttr::name << "' requires a non-public function";
  return mlir::success();
}

bool isBorrowedArgument(mlir::Value value) {
  auto argument = llvm::dyn_cast<mlir::BlockArgument>(value);
  if (!argument || !argument.getOwner()->isEntryBlock())
    return false;
  auto function = llvm::dyn_cast_if_present<mlir::FunctionOpInterface>(
      argument.getOwner()->getParentOp());
  return function && function.getArgAttrOfType<BorrowedAttr>(
                         argument.getArgNumber(), BorrowedAttr::name);
}
} // namespace reussir
