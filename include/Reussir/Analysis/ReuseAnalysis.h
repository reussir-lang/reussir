//===-- ReuseAnalysis.h - Reussir reuse analysis header ---------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_ANALYSIS_REUSEANALYSIS_H
#define REUSSIR_ANALYSIS_REUSEANALYSIS_H

#include "Reussir/Support/Immer.h"

#include <mlir/Analysis/DataFlow/DenseAnalysis.h>
#include <mlir/Analysis/DataFlowFramework.h>
#include <mlir/Pass/AnalysisManager.h>

namespace reussir {
class AliveTokenLattice : public mlir::dataflow::AbstractDenseLattice {};

} // namespace reussir

#endif // REUSSIR_ANALYSIS_REUSEANALYSIS_H
