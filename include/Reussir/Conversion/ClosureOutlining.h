//===-- ClosureOutlining.h - Reussir closure outlining -------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides the ClosureOutlining pass declaration.
//
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_CONVERSION_CLOSUREOUTLINING_H
#define REUSSIR_CONVERSION_CLOSUREOUTLINING_H

#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DECL_REUSSIRCLOSUREOUTLININGPASS
#include "Reussir/Conversion/Passes.h.inc"

} // namespace reussir

#endif // REUSSIR_CONVERSION_CLOSUREOUTLINING_H
