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
/// This header file defines the Reussir dialect and its core functionality.
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_IR_REUSSIRDIALECT_H
#define REUSSIR_IR_REUSSIRDIALECT_H

#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/Math/IR/Math.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/Dialect.h>

#include "Reussir/IR/ReussirDialect.h.inc"

namespace reussir {
// Dialect translation registration is not exposed here: it would only work
// once external module linking is in place.
// void registerReussirDialectTranslation(mlir::DialectRegistry &registry);

/// Whether `value` is a parameter its function receives borrowed: an
/// entry-block argument carrying `#reussir.borrowed`. Every other counted
/// parameter is owned (drop functions aside), so a pass that infers
/// ownership or uniqueness from "this is my argument" must exclude borrowed
/// ones: a borrowed argument's count can be 1 while the caller still holds
/// the object.
bool isBorrowedArgument(mlir::Value value);
} // namespace reussir

#endif // REUSSIR_IR_REUSSIRDIALECT_H
