//===-- RcDecrementExpansion.h - Reussir rc decrement expansion -*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides expansion patterns for Reussir rc decrement
// operations.
//
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H
#define REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H

#include <llvm/ADT/StringRef.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

/// Attribute name used to mark expanded decrement operations
constexpr llvm::StringLiteral kExpandedDecrementAttr =
    "reussir.expanded_decrement";

#define GEN_PASS_DECL_REUSSIRRCDECREMENTEXPANSIONPASS
#include "Reussir/Conversion/Passes.h.inc"

void populateRcDecrementExpansionConversionPatterns(
    mlir::RewritePatternSet &patterns);

} // namespace reussir

#endif // REUSSIR_CONVERSION_RCDECREMENTEXPANSION_H
