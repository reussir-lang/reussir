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
/// This file implements target array metadata and IFRT bridge verification.
///
//===----------------------------------------------------------------------===//

#include <xla/layout_util.h>
#include <xla/pjrt/layout_mode.h>
#include <xla/python/ifrt/ir/ifrt_dialect.h>
#include <xla/python/ifrt/ir/ifrt_ops.h>
#include <xla/python/ifrt/ir/reussir_target.h>
#include <xla/shape_util.h>

namespace reussir {
mlir::LogicalResult verifyIfrtArrayMetadata(
    mlir::Operation *op, mlir::Type type, mlir::RankedTensorType shape,
    llvm::ArrayRef<int32_t> devices, mlir::Attribute sharding,
    mlir::StringAttr memoryKind, mlir::StringAttr layout) {
  auto array = llvm::dyn_cast<xla::ifrt::IfrtArrayType>(type);
  if (!array)
    return op->emitOpError("requires an IFRT array type");
  if (array.getShape() != shape)
    return op->emitOpError(
        "RC and IFRT shapes and element types must match exactly");
  if (array.getDevices() != devices)
    return op->emitOpError("RC and IFRT ordered devices must match");
  if (sharding ? array.getShardingAttr() != sharding
               : !llvm::isa<xla::ifrt::IfrtUnspecifiedShardingAttr>(
                     array.getShardingAttr()))
    return op->emitOpError("RC and IFRT sharding must match");
  if (array.getMemoryKindAttr() != memoryKind)
    return op->emitOpError("RC and IFRT memory kinds must match");
  if (array.getLayoutAttr() != layout)
    return op->emitOpError("RC and IFRT layouts must match");
  return mlir::success();
}

// IFRT treats both explicit aliases and donation hints as consuming inputs.
// Use upstream operation accessors, rather than matching attribute names on
// arbitrary operations. Extend the supported boundary when ownership semantics
// for other IFRT operations are implemented by the outlining/lowering passes.
template <typename CallOp>
static mlir::LogicalResult
verifyBorrowedCallInput(mlir::Operation *bridge, CallOp call, unsigned index) {
  if (llvm::is_contained(call.getDonatedInputIndices(), index))
    return bridge->emitOpError("borrowed IFRT input cannot be donated");
  for (auto attr : call.getIoAliases()) {
    auto alias = llvm::dyn_cast<mlir::DenseI32ArrayAttr>(attr);
    if (!alias || alias.size() != 2)
      return bridge->emitOpError("malformed IFRT input/output alias");
    if (alias[0] == index)
      return bridge->emitOpError(
          "borrowed IFRT input cannot alias a call result");
  }
  return mlir::success();
}

mlir::LogicalResult verifyIfrtArrayBorrow(mlir::Operation *op,
                                          mlir::Value view) {
  for (mlir::OpOperand &use : view.getUses()) {
    auto *user = use.getOwner();
    if (user->getBlock() != op->getBlock())
      return op->emitOpError(
          "borrowed IFRT array must stay in its defining block");
    if (auto call = llvm::dyn_cast<xla::ifrt::CallOp>(user)) {
      if (mlir::failed(
              verifyBorrowedCallInput(op, call, use.getOperandNumber())))
        return mlir::failure();
    } else if (auto call =
                   llvm::dyn_cast<xla::ifrt::CallLoadedExecutableOp>(user)) {
      if (mlir::failed(
              verifyBorrowedCallInput(op, call, use.getOperandNumber())))
        return mlir::failure();
    } else if (!llvm::isa<xla::ifrt::AfterOp>(user)) {
      return op->emitOpError(
          "borrowed IFRT array only supports direct IFRT call and After uses");
    }
  }
  return mlir::success();
}

mlir::LogicalResult verifyIfrtArrayAdoption(mlir::Operation *op,
                                            mlir::Value source) {
  auto *producer = source.getDefiningOp();
  if (!producer ||
      !llvm::isa<xla::ifrt::CallOp, xla::ifrt::CallLoadedExecutableOp>(
          producer))
    return op->emitOpError("requires an owned IFRT call result");
  if (producer->getBlock() != op->getBlock())
    return op->emitOpError("IFRT result must be adopted in its defining block");
  if (!source.hasOneUse())
    return op->emitOpError("adopted IFRT result must have exactly one use");
  return mlir::success();
}

mlir::FailureOr<PjrtLayout> parsePjrtLayout(mlir::StringAttr spelling,
                                            int64_t rank, mlir::Location loc) {
  auto parsed = xla::LayoutMode::FromString(spelling.getValue().str());
  if (!parsed.ok()) {
    mlir::emitError(loc) << "invalid XLA layout: " << parsed.status().message();
    return mlir::failure();
  }
  if (parsed->mode != xla::LayoutMode::Mode::kUserSpecified) {
    mlir::emitError(loc) << "allocation requires a concrete layout";
    return mlir::failure();
  }
  const auto &layout = *parsed->user_layout;
  // PjRt's allocation C ABI represents dimension order and tiles. Other XLA
  // fields must not be silently lost when converting to that representation.
  if (layout != xla::Layout(layout.minor_to_major(), layout.tiles())) {
    mlir::emitError(loc)
        << "PjRt allocation layout supports dimension order and tiles only";
    return mlir::failure();
  }
  auto status = xla::LayoutUtil::ValidateLayoutForShape(
      layout,
      xla::ShapeUtil::MakeShape(xla::F32, std::vector<int64_t>(rank, 1)));
  if (!status.ok()) {
    mlir::emitError(loc) << "invalid allocation layout: " << status.message();
    return mlir::failure();
  }
  PjrtLayout result;
  llvm::append_range(result.minorToMajor, layout.minor_to_major());
  for (const auto &tile : layout.tiles()) {
    llvm::append_range(result.tileDims, tile.dimensions());
    result.tileSizes.push_back(tile.dimensions().size());
  }
  return result;
}

mlir::FailureOr<llvm::SmallVector<int64_t>>
getPjrtDimShards(mlir::Attribute attr, mlir::RankedTensorType shape,
                 llvm::ArrayRef<int32_t> devices, mlir::Location loc) {
  if (mlir::failed(xla::ifrt::IfrtDevicesAttr::verify(
          [&] { return mlir::emitError(loc); }, devices)))
    return mlir::failure();
  if (!attr || llvm::isa<xla::ifrt::IfrtUnspecifiedShardingAttr>(attr)) {
    if (devices.size() == 1)
      return llvm::SmallVector<int64_t>(shape.getRank(), 1);
    mlir::emitError(loc)
        << "multi-device allocation requires concrete sharding";
    return mlir::failure();
  }
  auto sharding = llvm::dyn_cast<xla::ifrt::IfrtShardingParamAttr>(attr);
  if (!sharding) {
    mlir::emitError(loc) << "allocation requires an IFRT sharding_param";
    return mlir::failure();
  }
  const auto &param = sharding.getSharding();
  if (mlir::failed(param.CanApplyTo([&] { return mlir::emitError(loc); }, shape,
                                    devices)))
    return mlir::failure();
  for (auto [size, shards] :
       llvm::zip_equal(shape.getShape(), param.dim_shards())) {
    if (!mlir::ShapedType::isDynamic(size) && size % shards != 0) {
      mlir::emitError(loc) << "array extent " << size << " is not divisible by "
                           << shards << " shards";
      return mlir::failure();
    }
  }
  return llvm::to_vector(param.dim_shards());
}
} // namespace reussir
