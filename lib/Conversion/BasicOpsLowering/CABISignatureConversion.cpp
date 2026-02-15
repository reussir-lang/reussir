//===-- CABISignatureConversion.cpp - C ABI signature conversion -*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/CABISignatureConversion.h"
#include <llvm/Support/ErrorHandling.h>
#include <mlir/Dialect/LLVMIR/LLVMTypes.h>

namespace reussir {
namespace {

static bool isScalarIntegerLike(mlir::Type type) {
  return mlir::isa<mlir::IntegerType>(type) ||
         mlir::isa<mlir::LLVM::LLVMPointerType>(type);
}

static bool isFPScalar(mlir::Type type) { return type.isF32() || type.isF64(); }

static bool isAggregateType(mlir::Type type) {
  return mlir::isa<mlir::LLVM::LLVMStructType>(type) ||
         mlir::isa<mlir::LLVM::LLVMArrayType>(type) ||
         mlir::isa<mlir::VectorType>(type);
}

static bool isDirectWin64Aggregate(mlir::Type type, const mlir::DataLayout &dl) {
  const uint64_t size = dl.getTypeSize(type);
  return size == 1 || size == 2 || size == 4 || size == 8;
}

static bool shouldPassIndirect(mlir::Type type, const mlir::DataLayout &dl,
                               CABIKind abiKind) {
  if (isScalarIntegerLike(type) || isFPScalar(type))
    return false;

  if (!isAggregateType(type))
    return true;

  switch (abiKind) {
  case CABIKind::Win64:
    return !isDirectWin64Aggregate(type, dl);
  case CABIKind::SysVAMD64:
  case CABIKind::AArch64AAPCS:
  case CABIKind::Unknown:
    // Large aggregates are passed indirectly in these ABIs.
    return dl.getTypeSize(type) > 16;
  }

  llvm_unreachable("unhandled C ABI kind");
}

static bool shouldReturnIndirect(mlir::Type type, const mlir::DataLayout &dl,
                                 CABIKind abiKind) {
  if (mlir::isa<mlir::LLVM::LLVMVoidType>(type))
    return false;

  if (isScalarIntegerLike(type) || isFPScalar(type))
    return false;

  if (!isAggregateType(type))
    return true;

  switch (abiKind) {
  case CABIKind::Win64:
    return !isDirectWin64Aggregate(type, dl);
  case CABIKind::SysVAMD64:
  case CABIKind::AArch64AAPCS:
  case CABIKind::Unknown:
    return dl.getTypeSize(type) > 16;
  }

  llvm_unreachable("unhandled C ABI kind");
}

} // namespace

CABIKind detectCABIKind(const llvm::Triple &triple) {
  if (triple.isOSWindows() && triple.isArch64Bit())
    return CABIKind::Win64;
  if (triple.getArch() == llvm::Triple::aarch64 ||
      triple.getArch() == llvm::Triple::aarch64_be)
    return CABIKind::AArch64AAPCS;
  if (triple.getArch() == llvm::Triple::x86_64 && !triple.isOSWindows())
    return CABIKind::SysVAMD64;
  return CABIKind::Unknown;
}

CABISignature evaluateCABISignatureForC(mlir::Type returnType,
                                        llvm::ArrayRef<mlir::Type> paramTypes,
                                        const mlir::DataLayout &dl,
                                        const llvm::Triple &triple) {
  CABISignature sig;
  sig.abiReturnType = returnType;
  sig.sretType = returnType;

  const CABIKind abiKind = detectCABIKind(triple);
  auto ptrType = mlir::LLVM::LLVMPointerType::get(returnType.getContext());

  if (shouldReturnIndirect(returnType, dl, abiKind)) {
    sig.hasSRet = true;
    sig.sretIndex = 0;
    sig.abiReturnType = mlir::LLVM::LLVMVoidType::get(returnType.getContext());
    sig.abiParamTypes.push_back(ptrType);
  }

  for (mlir::Type paramType : paramTypes) {
    const bool indirect = shouldPassIndirect(paramType, dl, abiKind);
    CABIParamPassKind passKind = CABIParamPassKind::Direct;
    if (indirect) {
      if (abiKind == CABIKind::AArch64AAPCS)
        passKind = CABIParamPassKind::IndirectPointer;
      else
        passKind = CABIParamPassKind::IndirectByVal;
    }
    sig.params.push_back(CABIParamInfo{paramType, passKind});
    sig.abiParamTypes.push_back(indirect ? ptrType : paramType);
  }

  return sig;
}

} // namespace reussir
