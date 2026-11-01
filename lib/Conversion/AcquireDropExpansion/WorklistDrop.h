//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache License v2.0 or
// the MIT License. See
// https://github.com/reussir-lang/reussir/blob/main/LICENSE
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//
#pragma once

#include "Reussir/IR/ReussirOps.h"
#include <mlir/Dialect/Func/IR/FuncOps.h>

namespace reussir {
inline constexpr llvm::StringLiteral kWorklistGlueAttr =
    "reussir.worklist_drop";
inline constexpr llvm::StringLiteral kWorklistRawAttr = "reussir.worklist_raw";
bool isForwardDrop(mlir::ModuleOp module);
bool isInsideWorklistGlue(mlir::Operation *site);
void emitWorklistDrop(mlir::Value ref, mlir::Location loc,
                      mlir::PatternRewriter &rewriter,
                      mlir::SymbolTableCollection *symbols, bool run = false);
void emitWorklistRelease(mlir::Value rc, mlir::Location loc,
                         mlir::PatternRewriter &rewriter,
                         mlir::SymbolTableCollection *symbols,
                         bool run = false);
void emitWorklistArrayDrop(mlir::Value ref, mlir::Location loc,
                           mlir::PatternRewriter &rewriter,
                           mlir::SymbolTableCollection *symbols);
void emitWorklistDecrement(mlir::Value rc, mlir::Location loc,
                           mlir::PatternRewriter &rewriter,
                           mlir::SymbolTableCollection *symbols,
                           bool run = false);
} // namespace reussir
