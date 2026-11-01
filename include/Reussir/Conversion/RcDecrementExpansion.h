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
/// This header file provides expansion patterns for Reussir rc decrement
/// operations.
///
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H
#define REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H

#include <llvm/ADT/StringRef.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirOps.h"

namespace reussir {

/// Attribute name used to mark expanded decrement operations
constexpr llvm::StringLiteral kExpandedDecrementAttr =
    "reussir.expanded_decrement";

bool canScheduleDecrement(ReussirRcDecOp op);

void populateRcDecrementExpansionConversionPatterns(
    mlir::RewritePatternSet &patterns, bool deferNormal = false);

} // namespace reussir

#endif // REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H
