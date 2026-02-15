//===-- CABISignatureConversion.h - C ABI signature conversion -*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header defines helpers for evaluating C ABI-facing trampoline
// signatures from internal LLVM-level function types.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {

enum class CABIKind {
  Win64,
  SysVAMD64,
  AArch64AAPCS,
  Unknown,
};

enum class CABIParamPassKind {
  Direct,
  IndirectByVal,
};

struct CABIParamInfo {
  mlir::Type originalType;
  CABIParamPassKind passKind;
};

struct CABISignature {
  mlir::Type abiReturnType;
  llvm::SmallVector<mlir::Type> abiParamTypes;
  llvm::SmallVector<CABIParamInfo> params;
  bool hasSRet = false;
  unsigned sretIndex = 0;
  mlir::Type sretType;
};

CABIKind detectCABIKind(const llvm::Triple &triple);

CABISignature evaluateCABISignatureForC(mlir::Type returnType,
                                        llvm::ArrayRef<mlir::Type> paramTypes,
                                        const mlir::DataLayout &dl,
                                        const llvm::Triple &triple);

} // namespace reussir

