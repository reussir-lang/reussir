//===-- ReussirOps.h - Reussir dialect operations ---------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides the definitions for operations used in the Reussir
// dialect.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_IR_REUSSIROPS_H
#define REUSSIR_IR_REUSSIROPS_H

#include <mlir/Bytecode/BytecodeOpInterface.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Interfaces/InferTypeOpInterface.h>
#include <mlir/Interfaces/SideEffectInterfaces.h>

#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/IR/ReussirInterfaces.h"

#define GET_OP_CLASSES
#include "Reussir/IR/ReussirOps.h.inc"

namespace reussir {

//===----------------------------------------------------------------------===//
// emitOwnershipAcquisition
//===----------------------------------------------------------------------===//
//
// This function emits the ownership acquisition for the given value. The input
// value can either be a reference or a rc pointer. If other type is provided,
// the function returns failure.
// - When RC value is passed in, the function emits a RcInc operation.
// - When Reference value is passed in, the function checks the following:
//   + if the reference points to a rc pointer, then the function loads it and
//     recursively apply ownership acquisition.
//   + if the reference points to a record, then the function spills it to get a
//     reference and recursively apply ownership acquisition. For variant types,
//     the function emits a RecordDispatch operation and continue the process
//     for each variant by recursively calling emitOwnershipAcquisition in
//     corresponding regions.
//   + otherwise, the function is a no-op.
//
//===----------------------------------------------------------------------===//
mlir::LogicalResult emitOwnershipAcquisition(mlir::Value value,
                                             mlir::OpBuilder &builder,
                                             mlir::Location loc);
} // namespace reussir

#endif // REUSSIR_IR_REUSSIROPS_H
