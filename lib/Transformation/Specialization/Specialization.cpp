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
mlir::func::FuncOp clonePrivateSpecialization(mlir::func::FuncOp function,
                                              llvm::StringRef name,
                                              mlir::SymbolTable &symbolTable,
                                              mlir::IRMapping *mapping) {
  mlir::IRMapping local;
  auto clone = llvm::cast<mlir::func::FuncOp>(
      function->clone(mapping ? *mapping : local));
  clone.setName(name);
  clone.setPrivate();
  clone->removeAttr("llvm.visibility");
  clone->setAttr("llvm.linkage",
                 mlir::LLVM::LinkageAttr::get(function.getContext(),
                                              mlir::LLVM::Linkage::Internal));
  symbolTable.insert(clone);
  return clone;
}

} // namespace reussir
