//===-- InvariantGroupAnalysis.h - Invariant group analysis ------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file declares the invariant group analysis pass for Reussir
// ref.load operations.
//
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_CONVERSION_INVARIANTGROUPANALYSIS_H
#define REUSSIR_CONVERSION_INVARIANTGROUPANALYSIS_H

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DECL_REUSSIRINVARIANTGROUPANALYSISPASS
#include "Reussir/Conversion/Passes.h.inc"

} // namespace reussir

#endif // REUSSIR_CONVERSION_INVARIANTGROUPANALYSIS_H
