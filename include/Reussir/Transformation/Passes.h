//===-- Passes.h - Reussir transformation passes --------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides the definitions for transformation passes used in
// the Reussir dialect.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_TRANSFORMATION_PASSES_H
#define REUSSIR_TRANSFORMATION_PASSES_H

#include <mlir/IR/BuiltinOps.h>
#include <mlir/Pass/Pass.h>

namespace reussir {
#define GEN_PASS_DECL
#include "Reussir/Transformation/Passes.h.inc"

} // namespace reussir

#endif // REUSSIR_TRANSFORMATION_PASSES_H
