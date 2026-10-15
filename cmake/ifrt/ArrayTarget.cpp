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

#include <llvm/ADT/DenseSet.h>
#include <xla/layout_util.h>
#include <xla/pjrt/layout_mode.h>
#include <xla/pjrt/pjrt_executable.h>
#include <xla/pjrt/proto/compile_options.pb.h>
#include <xla/python/ifrt/ir/constants.h>
#include <xla/python/ifrt/ir/ifrt_dialect.h>
#include <xla/python/ifrt/ir/ifrt_ops.h>
#include <xla/python/ifrt/ir/reussir_target.h>
#include <xla/python/ifrt/ir/transforms/utils.h>
#include <xla/python/ifrt/ir/support/sharding_conversions.h>
#include <xla/shape_util.h>

namespace reussir {
mlir::Type makeIfrtArrayType(mlir::RankedTensorType shape,
                           llvm::ArrayRef<int32_t> devices,
                           mlir::Attribute sharding, mlir::StringAttr memoryKind,
                           mlir::StringAttr layout) {
  auto *ctx = shape.getContext();
  ctx->getOrLoadDialect<xla::ifrt::IfrtDialect>();
  if (!sharding)
    sharding = xla::ifrt::IfrtUnspecifiedShardingAttr::get(ctx);
  auto typed = mlir::dyn_cast<xla::ifrt::IfrtShardingAttrInterface>(sharding);
  if (!typed)
    return {};
  if (auto param = mlir::dyn_cast<xla::ifrt::IfrtShardingParamAttr>(typed);
      param && !xla::ifrt::support::ToHloSharding(param.getSharding()).ok())
    return {};
  return xla::ifrt::IfrtArrayType::get(
      ctx, shape, typed, xla::ifrt::IfrtDevicesAttr::get(ctx, devices),
      memoryKind, layout);
}
mlir::Attribute getIfrtDevicesAttr(mlir::Type type) {
  auto array = mlir::dyn_cast_or_null<xla::ifrt::IfrtArrayType>(type);
  return array ? xla::ifrt::IfrtDevicesAttr::get(type.getContext(), array.getDevices())
               : mlir::Attribute{};
}
mlir::StringAttr getIfrtHloSharding(mlir::Type type) {
  auto array = mlir::dyn_cast_or_null<xla::ifrt::IfrtArrayType>(type);
  if (!array)
    return {};
  auto sharding = mlir::dyn_cast<xla::ifrt::IfrtShardingParamAttr>(array.getShardingAttr());
  if (!sharding)
    return {};
  auto hlo = xla::ifrt::support::ToHloSharding(sharding.getSharding());
  if (!hlo.ok())
    return {};
  return mlir::StringAttr::get(type.getContext(), hlo->ToString());
}

bool ifrtArrayRequiresGlobalExtents(mlir::Type type) {
  auto array = mlir::dyn_cast_or_null<xla::ifrt::IfrtArrayType>(type);
  if (!array)
    return false;
  auto sharding = mlir::dyn_cast<xla::ifrt::IfrtShardingParamAttr>(array.getShardingAttr());
  if (!sharding)
    return false;
  auto dims = array.getShape().getShape();
  auto shards = sharding.getSharding().dim_shards();
  for (auto [dim, count] : llvm::zip(dims, shards))
    if (mlir::ShapedType::isDynamic(dim) && count != 1)
      return true;
  return false;
}

bool isIfrtArrayType(mlir::Type type) {
  return llvm::isa<xla::ifrt::IfrtArrayType>(type);
}

bool isIfrtControlType(mlir::Type type) {
  return llvm::isa<xla::ifrt::IfrtControlType>(type);
}

llvm::ArrayRef<int32_t> getIfrtArrayDevices(mlir::Type type) {
  return llvm::cast<xla::ifrt::IfrtArrayType>(type).getDevices();
}

mlir::FailureOr<mlir::StringAttr>
serializeIfrtCompileOptions(mlir::Operation *op) {
  auto call = llvm::cast<xla::ifrt::CallOp>(op);
  if (call->hasAttr(xla::ifrt::kIfrtCompileOptionsKey)) {
    call.emitOpError(
        "JIT preparation does not support external compile option overrides");
    return mlir::failure();
  }
  if (call->hasAttr("reussir.disable_donation") &&
      (!call->hasAttrOfType<mlir::UnitAttr>("reussir.disable_donation") ||
       !call.getIoAliases().empty() || !call.getDonatedInputIndices().empty())) {
    call.emitOpError("donation opt-out must be a unit attribute and cannot coexist with donation or alias contracts");
    return mlir::failure();
  }
  // Match IFRT's atom compiler: separate parameters, with sharding fixed by
  // the outlined kernel. Logical IDs are resolved when a client is available.
  auto options = xla::ifrt::GetDefaultCompileOptions(
      call, /*enable_sharding_propagation=*/false,
      /*enable_parameter_tupling=*/false);
  if (auto level = call->getAttrOfType<mlir::IntegerAttr>("reussir.backend_optimization_level")) {
    auto value = level.getInt();
    if (value < 0 || value > 3) {
      call.emitOpError("backend optimization level must be in [0, 3]");
      return mlir::failure();
    }
    options.executable_build_options.mutable_debug_options()->set_xla_backend_optimization_level(value);
  } else if (call->hasAttr("reussir.backend_optimization_level")) {
    call.emitOpError("backend optimization level must be an integer attribute");
    return mlir::failure();
  }
  auto proto = options.ToProto();
  if (!proto.ok()) {
    call.emitOpError("cannot serialize PJRT compile options: ")
        << proto.status().message();
    return mlir::failure();
  }
  return mlir::StringAttr::get(op->getContext(), proto->SerializeAsString());
}

mlir::LogicalResult verifyPjrtCompileOptions(mlir::Operation *op,
                                             llvm::StringRef bytes) {
  xla::CompileOptionsProto proto;
  if (!proto.ParseFromString(bytes.str()))
    return op->emitOpError("requires serialized XLA CompileOptionsProto bytes");
  auto options = xla::CompileOptions::FromProto(proto);
  if (!options.ok())
    return op->emitOpError("invalid PJRT compile options: ")
           << options.status().message();
  // Placement is a runtime-overridable default; the call's parameter convention
  // remains part of its ABI.
  if (proto.parameter_is_tupled_arguments() ||
      proto.compile_portable_executable())
    return op->emitOpError(
        "JIT calls require separate parameters and an assigned executable");
  return mlir::success();
}

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

mlir::LogicalResult verifyIfrtArrayBorrowUse(mlir::Operation *op,
                                             mlir::OpOperand &use) {
  auto *user = use.getOwner();
  if (auto call = llvm::dyn_cast<xla::ifrt::CallOp>(user))
    return verifyBorrowedCallInput(op, call, use.getOperandNumber());
  if (auto call = llvm::dyn_cast<xla::ifrt::CallLoadedExecutableOp>(user))
    return verifyBorrowedCallInput(op, call, use.getOperandNumber());
  if (llvm::isa<xla::ifrt::AfterOp>(user))
    return mlir::success();
  return op->emitOpError(
      "borrowed IFRT array only supports direct IFRT call and After uses");
}

bool isIfrtCall(mlir::Operation *op) {
  return op &&
         llvm::isa<xla::ifrt::CallOp, xla::ifrt::CallLoadedExecutableOp>(op);
}

bool isIfrtAfter(mlir::Operation *op) {
  return llvm::isa<xla::ifrt::AfterOp>(op);
}

std::optional<IfrtCallInfo> getIfrtCallInfo(mlir::Operation *op) {
  auto call = llvm::dyn_cast<xla::ifrt::CallOp>(op);
  if (!call)
    return std::nullopt;
  return IfrtCallInfo{call.getCalleeAttr(),
                      static_cast<unsigned>(call.getInputs().size()),
                      static_cast<unsigned>(call.getOutputs().size()),
                      mlir::DenseI32ArrayAttr::get(
                          op->getContext(), call.getDevicesAttr().getIds()),
                      call.getIoAliases(),
                      call.getDonatedInputIndicesAttr(),
                      call.getArgAttrsAttr(),
                      call.getResAttrsAttr()};
}

mlir::FailureOr<mlir::FunctionType> verifyIfrtCallSignature(
    mlir::Operation *op, mlir::TypeRange inputs, mlir::TypeRange outputs,
    mlir::TypeRange controls, mlir::Type controlOutput,
    llvm::ArrayRef<int32_t> devices, mlir::ArrayAttr aliases,
    llvm::ArrayRef<int32_t> donated) {
  using xla::ifrt::IfrtArrayType;
  if (mlir::failed(xla::ifrt::IfrtDevicesAttr::verify(
          [&] { return op->emitOpError(); }, devices)))
    return mlir::failure();
  for (auto type :
       llvm::concat<mlir::Type>(controls, mlir::TypeRange{controlOutput}))
    if (!llvm::isa<xla::ifrt::IfrtControlType>(type)) {
      op->emitOpError("requires IFRT control dependencies");
      return mlir::failure();
    }
  llvm::SmallVector<mlir::Type> inputShapes, outputShapes;
  for (auto [types, shapes] :
       {std::pair{inputs, &inputShapes}, std::pair{outputs, &outputShapes}}) {
    for (auto type : types) {
      auto array = llvm::dyn_cast<IfrtArrayType>(type);
      if (!array) {
        op->emitOpError("requires IFRT array inputs and outputs");
        return mlir::failure();
      }
      for (int device : array.getDevices())
        if (!llvm::is_contained(devices, device)) {
          op->emitOpError("array devices must be a subset of call devices");
          return mlir::failure();
        }
      shapes->push_back(array.getShape());
    }
  }
  llvm::SmallDenseSet<int32_t> consumed, aliasedOutputs;
  for (int32_t index : donated)
    if (index < 0 || index >= inputs.size() || !consumed.insert(index).second) {
      op->emitOpError("invalid or repeated donated input index");
      return mlir::failure();
    }
  for (auto attr : aliases) {
    auto alias = llvm::dyn_cast<mlir::DenseI32ArrayAttr>(attr);
    if (!alias || alias.size() != 2 || alias[0] < 0 ||
        alias[0] >= inputs.size() || alias[1] < 0 ||
        alias[1] >= outputs.size()) {
      op->emitOpError("invalid input/output alias indices");
      return mlir::failure();
    }
    if (!consumed.insert(alias[0]).second ||
        !aliasedOutputs.insert(alias[1]).second) {
      op->emitOpError("input/output alias repeats an aliased or donated index");
      return mlir::failure();
    }
    auto input = llvm::cast<IfrtArrayType>(inputs[alias[0]]);
    auto output = llvm::cast<IfrtArrayType>(outputs[alias[1]]);
    if (input == output)
      continue;
    auto inputShape = input.getShardingAttr().LocalShapeFromGlobalShape(
        input.getShape().getShape());
    auto outputShape = output.getShardingAttr().LocalShapeFromGlobalShape(
        output.getShape().getShape());
    if (input.getShape().getElementType() !=
            output.getShape().getElementType() ||
        !inputShape.ok() || !outputShape.ok() || *inputShape != *outputShape) {
      op->emitOpError(
          "aliased arrays must have equal dtypes and per-shard shapes");
      return mlir::failure();
    }
  }
  return mlir::FunctionType::get(op->getContext(), inputShapes, outputShapes);
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
        << "multi-device array placement requires concrete sharding";
    return mlir::failure();
  }
  auto sharding = llvm::dyn_cast<xla::ifrt::IfrtShardingParamAttr>(attr);
  if (!sharding) {
    mlir::emitError(loc) << "device array placement requires an IFRT sharding_param";
    return mlir::failure();
  }
  const auto &param = sharding.getSharding();
  if (mlir::failed(param.CanApplyTo([&] { return mlir::emitError(loc); }, shape,
                                    devices)))
    return mlir::failure();
  return llvm::to_vector(param.dim_shards());
}
mlir::LogicalResult verifyPjrtArrayPlacement(mlir::RankedTensorType shape,
                                           llvm::ArrayRef<int32_t> devices,
                                           mlir::Attribute sharding,
                                           mlir::StringAttr layout,
                                           mlir::Location loc) {
  if (mlir::failed(getPjrtDimShards(sharding, shape, devices, loc)))
    return mlir::failure();
  if (!layout || layout.getValue() == "default" || layout.getValue() == "auto")
    return mlir::success();
  return mlir::failure(
      mlir::failed(parsePjrtLayout(layout, shape.getRank(), loc)));
}

mlir::FailureOr<PjrtTransferSharding>
getPjrtTransferSharding(mlir::Attribute attr, mlir::RankedTensorType shape,
                        llvm::ArrayRef<int32_t> devices, mlir::Location loc) {
  auto dims = getPjrtDimShards(attr, shape, devices, loc);
  if (mlir::failed(dims))
    return mlir::failure();
  PjrtTransferSharding result{std::move(*dims), {}};
  result.coordinates.resize(devices.size());
  auto sharding =
      llvm::dyn_cast_or_null<xla::ifrt::IfrtShardingParamAttr>(attr);
  if (!sharding) {
    result.coordinates.front().resize(shape.getRank(), 0);
    return result;
  }
  const auto &param = sharding.getSharding();
  if (!param.unreduced_axes().empty()) {
    mlir::emitError(loc)
        << "host transfers require reduced values; sharding_param does not "
           "specify the reduction operator for unreduced axes";
    return mlir::failure();
  }
  // IFRT's device permutation lists tiles in row-major order, with replicas
  // in the minor positions (see ShardingParam::MinorToMajor::ToDeviceList).
  absl::InlinedVector<int, 4> deviceOrder;
  param.minor_to_major().ToDeviceList(deviceOrder);
  int64_t tiles = 1;
  for (auto count : result.dimShards)
    tiles *= count; // CanApplyTo already bounds the product by device count.
  int64_t replicas = devices.size() / tiles;
  for (auto [position, device] : llvm::enumerate(deviceOrder)) {
    auto &coordinates = result.coordinates[device];
    coordinates.resize(shape.getRank());
    int64_t tile = position / replicas;
    for (size_t dim = shape.getRank(); dim-- > 0;) {
      coordinates[dim] = tile % result.dimShards[dim];
      tile /= result.dimShards[dim];
    }
  }
  return result;
}
} // namespace reussir
