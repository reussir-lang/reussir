// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// SPDX-License-Identifier: Apache-2.0 OR MIT

#include "xla/layout_util.h"
#include "xla/pjrt/layout_mode.h"
#include "xla/python/ifrt/ir/ifrt_dialect.h"
#include "xla/python/ifrt/ir/reussir_target.h"
#include "xla/shape_util.h"

namespace reussir {
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
