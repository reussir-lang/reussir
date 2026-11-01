//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#pragma once

#include "Reussir/IR/ReussirOps.h"

namespace reussir {
void emitBoxFree(mlir::Value owner, mlir::Location loc, mlir::ModuleOp module,
                 mlir::OpBuilder &builder);
bool canApplyTailDrop(RcType type);
bool emitTailDropRelease(mlir::Value rc, mlir::Location loc,
                         mlir::ModuleOp module, mlir::PatternRewriter &rewriter,
                         mlir::SymbolTableCollection *symbols);
} // namespace reussir
