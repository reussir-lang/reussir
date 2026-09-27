// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// SPDX-License-Identifier: Apache-2.0 OR MIT
#pragma once

#include <llvm/ADT/SmallVector.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/IR/Location.h>
#include <mlir/Support/LogicalResult.h>

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
} // namespace reussir
