//===-- RegionPatterns.h - Reussir region-related patterns --*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides lowering/cleanup patterns related to region ops.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_CONVERSION_REGIONPATTERNS_H
#define REUSSIR_CONVERSION_REGIONPATTERNS_H

#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/DialectConversion.h>

namespace reussir {

#define GEN_PASS_DECL_REUSSIRREGIONPATTERNSPASS
#include "Reussir/Conversion/Passes.h.inc"

void populateRegionPatternsConversionPatterns(
    mlir::RewritePatternSet &patterns);

} // namespace reussir

#endif // REUSSIR_CONVERSION_REGIONPATTERNS_H
