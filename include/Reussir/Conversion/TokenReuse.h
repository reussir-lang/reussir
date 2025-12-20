//===-- TokenReuse.h - Reussir token reuse pass -----------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides the definition for the token reuse pass.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_CONVERSION_TOKENREUSE_H
#define REUSSIR_CONVERSION_TOKENREUSE_H

#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DECL_REUSSIRTOKENREUSEPASS
#include "Reussir/Conversion/Passes.h.inc"

} // namespace reussir

#endif // REUSSIR_CONVERSION_TOKENREUSE_H
