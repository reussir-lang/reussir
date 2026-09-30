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
/// This header declares target array metadata and IFRT bridge helpers.
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_CONVERSION_OPENXLATARGET_H
#define REUSSIR_CONVERSION_OPENXLATARGET_H

#include <llvm/ADT/SmallVector.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/IR/Location.h>
#include <mlir/IR/Value.h>
#include <mlir/Support/LogicalResult.h>
#include <optional>

namespace reussir {
// These helpers are built alongside upstream IFRT, so its C++ dependencies do
// not leak into Reussir's IR or LLVM lowering sources.
struct PjrtLayout {
  llvm::SmallVector<int64_t> minorToMajor;
  llvm::SmallVector<int64_t> tileDims;
  llvm::SmallVector<int64_t> tileSizes;
};

mlir::FailureOr<PjrtLayout> parsePjrtLayout(mlir::StringAttr layout,
                                            int64_t rank, mlir::Location loc);
mlir::FailureOr<llvm::SmallVector<int64_t>>
getPjrtDimShards(mlir::Attribute sharding, mlir::RankedTensorType shape,
                 llvm::ArrayRef<int32_t> devices, mlir::Location loc);

// Exact, lossless metadata matching at the RC/IFRT boundary.
mlir::LogicalResult verifyIfrtArrayMetadata(
    mlir::Operation *op, mlir::Type type, mlir::RankedTensorType shape,
    llvm::ArrayRef<int32_t> devices, mlir::Attribute sharding,
    mlir::StringAttr memoryKind, mlir::StringAttr layout);
mlir::LogicalResult verifyIfrtArrayBorrowUse(mlir::Operation *op,
                                             mlir::OpOperand &use);
bool isIfrtCall(mlir::Operation *op);

// Verify the self-contained module stored in a bytecode object.
mlir::LogicalResult verifyIfrtKernelModule(mlir::ModuleOp module);

// Typed IFRT accessors for the Reussir pass without exposing XLA headers.
struct IfrtCallInfo {
  mlir::SymbolRefAttr callee;
  unsigned numInputs;
  unsigned numOutputs;
  mlir::DenseI32ArrayAttr devices;
  mlir::ArrayAttr ioAliases;
  mlir::DenseI32ArrayAttr donatedInputIndices;
  mlir::ArrayAttr argAttrs;
  mlir::ArrayAttr resAttrs;
};
std::optional<IfrtCallInfo> getIfrtCallInfo(mlir::Operation *op);
mlir::FailureOr<mlir::FunctionType> verifyIfrtCallSignature(
    mlir::Operation *op, mlir::TypeRange inputs, mlir::TypeRange outputs,
    mlir::TypeRange controls, mlir::Type controlOutput,
    llvm::ArrayRef<int32_t> devices, mlir::ArrayAttr aliases,
    llvm::ArrayRef<int32_t> donated);
} // namespace reussir

#endif // REUSSIR_CONVERSION_OPENXLATARGET_H
