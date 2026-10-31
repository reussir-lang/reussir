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

#pragma once
#ifndef REUSSIR_TRANSFORMATION_SPECIALIZATION_H
#define REUSSIR_TRANSFORMATION_SPECIALIZATION_H

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/SymbolTable.h>

namespace reussir {

/// A copy of `function` named `name` for a specialization only the compiler
/// calls. It is private with internal linkage, since no external caller can
/// name it. It keeps the original's debug locations: lowering gives every
/// function its own subprogram (the source name, with the copy's symbol as
/// linkage name), so the copy is as debuggable as the original. The copy is
/// inserted into `symbolTable`, which renames it on a clash.
mlir::func::FuncOp clonePrivateSpecialization(mlir::func::FuncOp function,
                                              llvm::StringRef name,
                                              mlir::SymbolTable &symbolTable);

} // namespace reussir

#endif // REUSSIR_TRANSFORMATION_SPECIALIZATION_H
