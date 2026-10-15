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
/// This file implements the Reussir type C API.
///
//===----------------------------------------------------------------------===//

#include "Reussir-c/Types.h"

// Complete mlir::DialectVersion before MLIR C API headers reach
// BytecodeWriter.h; MSVC's STL rejects destroying unique_ptr<T> when T is only
// forward declared.
#include <mlir/Bytecode/BytecodeImplementation.h>
#include <mlir/CAPI/IR.h>
#include <mlir/IR/BuiltinAttributes.h>

#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirTypes.h"
#ifdef REUSSIR_ENABLE_OPENXLA
#include "stablehlo/dialect/StablehloOps.h"
#endif
#include <llvm/ADT/DenseSet.h>

#include "llvm/ADT/SmallVector.h"

using namespace reussir;

namespace {
// Collects an MlirType array into a vector of unwrapped MLIR types.
llvm::SmallVector<mlir::Type> unwrapTypes(intptr_t n, MlirType const *types) {
  llvm::SmallVector<mlir::Type> result;
  result.reserve(n);
  for (intptr_t i = 0; i < n; ++i)
    result.push_back(unwrap(types[i]));
  return result;
}

Capability toCapability(ReussirCapability capability) {
  return static_cast<Capability>(capability);
}

AtomicKind toAtomicKind(ReussirAtomicKind atomicKind) {
  return static_cast<AtomicKind>(atomicKind);
}

CellKind toCellKind(ReussirCellKind kind) {
  return static_cast<CellKind>(kind);
}

RecordKind toRecordKind(ReussirRecordKind kind) {
  return static_cast<RecordKind>(kind);
}
} // namespace

MlirType reussirRawPtrTypeGet(MlirType elementType) {
  return wrap(RawPtrType::get(unwrap(elementType)));
}

MlirType reussirStablehloTokenTypeGet(MlirContext context) {
#ifdef REUSSIR_ENABLE_OPENXLA
  auto *ctx = unwrap(context);
  ctx->getOrLoadDialect<mlir::stablehlo::StablehloDialect>();
  return wrap(mlir::stablehlo::TokenType::get(ctx));
#else
  (void)context;
  return {nullptr};
#endif
}

MlirType reussirStablehloFutureTypeGet(MlirContext context, intptr_t n,
                                       const MlirType *payloads) {
#ifdef REUSSIR_ENABLE_OPENXLA
  auto *ctx = unwrap(context);
  // The spec requires a nonempty ordered list of ranked tensors. Validate
  // before the upstream builder, whose verifier permits an empty list.
  if (n <= 0 || !payloads)
    return {nullptr};
  for (intptr_t i = 0; i < n; ++i) {
    auto ty = unwrap(payloads[i]);
    if (!ty || ty.getContext() != ctx || !mlir::isa<mlir::RankedTensorType>(ty))
      return {nullptr};
  }
  ctx->getOrLoadDialect<mlir::stablehlo::StablehloDialect>();
  return wrap(mlir::stablehlo::FutureType::get(ctx, unwrapTypes(n, payloads)));
#else
  (void)context;
  (void)n;
  (void)payloads;
  return {nullptr};
#endif
}

MlirType reussirTokenTypeGet(MlirContext context, size_t align, size_t size) {
  return wrap(TokenType::get(unwrap(context), align, size));
}

MlirType reussirRegionTypeGet(MlirContext context) {
  return wrap(RegionType::get(unwrap(context)));
}

MlirType reussirRcTypeGet(MlirType elementType, ReussirCapability capability,
                          ReussirAtomicKind atomicKind) {
  mlir::Type ele = unwrap(elementType);
  return wrap(RcType::get(ele.getContext(), ele, toCapability(capability),
                          toAtomicKind(atomicKind)));
}

MlirType reussirNullableTypeGet(MlirType pointerType) {
  mlir::Type ptr = unwrap(pointerType);
  return wrap(NullableType::get(ptr.getContext(), ptr));
}

MlirType reussirCellTypeGet(MlirType elementType, bool exclusive) {
  mlir::Type ele = unwrap(elementType);
  return wrap(CellType::get(ele.getContext(), ele, exclusive));
}

MlirType reussirCellTypeGetWithKind(MlirType elementType,
                                    ReussirCellKind kind) {
  mlir::Type ele = unwrap(elementType);
  return wrap(CellType::get(ele.getContext(), ele, toCellKind(kind)));
}

MlirType reussirRefTypeGet(MlirType elementType, ReussirCapability capability,
                           ReussirAtomicKind atomicKind) {
  mlir::Type ele = unwrap(elementType);
  return wrap(RefType::get(ele.getContext(), ele, toCapability(capability),
                           toAtomicKind(atomicKind)));
}

MlirType reussirHoleTypeGet(MlirType elementType) {
  mlir::Type ele = unwrap(elementType);
  return wrap(HoleType::get(ele.getContext(), ele));
}

MlirType reussirRcBoxTypeGet(MlirType elementType, bool regional) {
  mlir::Type ele = unwrap(elementType);
  return wrap(RcBoxType::get(ele.getContext(), ele, regional));
}

MlirType reussirClosureTypeGet(MlirContext context, intptr_t nInputs,
                               MlirType const *inputTypes,
                               MlirType outputType) {
  llvm::SmallVector<mlir::Type> inputs = unwrapTypes(nInputs, inputTypes);
  mlir::Type output = outputType.ptr ? unwrap(outputType) : mlir::Type();
  return wrap(ClosureType::get(unwrap(context), inputs, output));
}

MlirType reussirClosureBoxTypeGet(MlirContext context, intptr_t nPayloads,
                                  MlirType const *payloadTypes) {
  llvm::SmallVector<mlir::Type> payloads = unwrapTypes(nPayloads, payloadTypes);
  return wrap(ClosureBoxType::get(unwrap(context), payloads));
}

MlirType reussirArrayTypeGetIfrtType(MlirType type) {
#ifdef REUSSIR_ENABLE_OPENXLA
  auto array = mlir::dyn_cast_or_null<ArrayType>(unwrap(type));
  if (!array || !array.getTarget())
    return {nullptr};
  auto target = array.getTarget();
  return wrap(makeIfrtArrayType(
      mlir::RankedTensorType::get(array.getShape(), array.getElementType()),
      target.getDevices().asArrayRef(), target.getSharding(),
      target.getMemoryKind(), target.getLayout()));
#else
  return {nullptr};
#endif
}
MlirAttribute reussirIfrtArrayGetDevices(MlirType type) {
#ifdef REUSSIR_ENABLE_OPENXLA
  return wrap(getIfrtDevicesAttr(unwrap(type)));
#else
  return {nullptr};
#endif
}
bool reussirIfrtArrayRequiresGlobalExtents(MlirType type) {
#ifdef REUSSIR_ENABLE_OPENXLA
  return ifrtArrayRequiresGlobalExtents(unwrap(type));
#else
  return false;
#endif
}

MlirAttribute reussirIfrtArrayGetHloSharding(MlirType type) {
#ifdef REUSSIR_ENABLE_OPENXLA
  return wrap(mlir::Attribute(getIfrtHloSharding(unwrap(type))));
#else
  return {nullptr};
#endif
}

MlirType reussirArrayTypeGetWithTarget(intptr_t nDims, int64_t const *shape,
                                     MlirType elementType,
                                     MlirAttribute target) {
  auto ele = unwrap(elementType);
  auto placement = mlir::dyn_cast_or_null<TargetAttr>(unwrap(target));
  if (!placement)
    return MlirType{nullptr};
  auto loc = mlir::UnknownLoc::get(ele.getContext());
  auto array = ArrayType::getChecked(loc, ele.getContext(),
                                    llvm::ArrayRef<int64_t>(shape, nDims), ele,
                                    placement);
  if (!array)
    return MlirType{nullptr};
  if (placement.getMemoryKind() && placement.getMemoryKind().getValue().empty()) {
    mlir::emitError(loc) << "device array memory kind must not be empty";
    return MlirType{nullptr};
  }
  llvm::SmallDenseSet<int32_t> seen;
  for (auto device : placement.getDevices().asArrayRef()) {
    if (!seen.insert(device).second) {
      mlir::emitError(loc)
          << "device array placement requires distinct logical device IDs";
      return MlirType{nullptr};
    }
  }
#ifdef REUSSIR_ENABLE_OPENXLA
  if (mlir::failed(verifyPjrtArrayPlacement(
          mlir::RankedTensorType::get(array.getShape(), ele),
          placement.getDevices().asArrayRef(), placement.getSharding(),
          placement.getLayout(), loc)))
    return MlirType{nullptr};
#else
  if (placement.getSharding() ||
      (placement.getLayout() && placement.getLayout().getValue() != "default" &&
       placement.getLayout().getValue() != "auto")) {
    mlir::emitError(loc) << "concrete device array sharding/layout requires "
                            "REUSSIR_ENABLE_OPENXLA";
    return MlirType{nullptr};
  }
  if (placement.getDevices().size() != 1) {
    mlir::emitError(loc) << "multi-device placement requires concrete sharding";
    return MlirType{nullptr};
  }
#endif
  return wrap(array);
}

MlirType reussirArrayTypeGet(intptr_t nDims, int64_t const *shape,
                             MlirType elementType) {
  mlir::Type ele = unwrap(elementType);
  return wrap(ArrayType::get(ele.getContext(),
                             llvm::ArrayRef<int64_t>(shape, nDims), ele));
}

MlirType reussirFFIObjectTypeGet(MlirContext context, MlirAttribute ffiName,
                                 MlirAttribute cleanupHook) {
  return wrap(FFIObjectType::get(
      unwrap(context), llvm::cast<mlir::StringAttr>(unwrap(ffiName)),
      llvm::cast<mlir::FlatSymbolRefAttr>(unwrap(cleanupHook))));
}

MlirType reussirStrTypeGet(MlirContext context, ReussirLifeScope lifeScope) {
  return wrap(StrType::get(unwrap(context), static_cast<LifeScope>(lifeScope)));
}

MlirType
reussirRecordTypeGetComplete(MlirContext context, intptr_t nMembers,
                             MlirType const *members, bool const *memberIsField,
                             MlirAttribute name, ReussirRecordKind kind,
                             ReussirCapability defaultCapability, bool fixed) {
  llvm::SmallVector<mlir::Type> memberTypes = unwrapTypes(nMembers, members);
  llvm::ArrayRef<bool> fields(memberIsField, nMembers);
  if (name.ptr)
    return wrap(RecordType::get(unwrap(context), memberTypes, fields,
                                llvm::cast<mlir::StringAttr>(unwrap(name)),
                                toRecordKind(kind),
                                toCapability(defaultCapability), fixed));
  return wrap(RecordType::get(unwrap(context), memberTypes, fields,
                              toRecordKind(kind),
                              toCapability(defaultCapability), fixed));
}

MlirType reussirRecordTypeGetIncomplete(MlirContext context, MlirAttribute name,
                                        ReussirRecordKind kind) {
  return wrap(RecordType::get(unwrap(context),
                              llvm::cast<mlir::StringAttr>(unwrap(name)),
                              toRecordKind(kind)));
}

void reussirRecordTypeComplete(MlirType record, intptr_t nMembers,
                               MlirType const *members,
                               bool const *memberIsField,
                               ReussirCapability defaultCapability,
                               bool fixed) {
  llvm::SmallVector<mlir::Type> memberTypes = unwrapTypes(nMembers, members);
  llvm::ArrayRef<bool> fields(memberIsField, nMembers);
  llvm::cast<RecordType>(unwrap(record))
      .complete(memberTypes, fields, toCapability(defaultCapability), fixed);
}

bool reussirRecordTypeIsComplete(MlirType record) {
  return llvm::cast<RecordType>(unwrap(record)).getComplete();
}
