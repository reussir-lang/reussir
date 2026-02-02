//===-- TypeConverter.h - Reussir type conversion utilities ---*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides type conversion utilities for the Reussir dialect.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_CONVERSION_TYPECONVERTER_H
#define REUSSIR_CONVERSION_TYPECONVERTER_H

#include <llvm/TargetParser/Triple.h>
#include <mlir/Conversion/LLVMCommon/TypeConverter.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Transforms/DialectConversion.h>

#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

class LLVMTypeConverter : public mlir::LLVMTypeConverter {
public:
  LLVMTypeConverter(mlir::ModuleOp op);

  std::optional<llvm::LogicalResult>
  convertRecordType(RecordType type,
                    llvm::SmallVectorImpl<mlir::Type> &results);

  const mlir::DataLayout &getDataLayout() const { return dataLayout; }

  /// Returns true if targeting Windows (any variant)
  bool isWindowsTarget() const { return targetTriple.isOSWindows(); }

  /// Returns true if targeting macOS/iOS/watchOS/tvOS
  bool isDarwinTarget() const { return targetTriple.isOSDarwin(); }

  /// Returns true if targeting x86_64 architecture
  bool isX86_64() const { return targetTriple.getArch() == llvm::Triple::x86_64; }

  /// Returns true if targeting AArch64/ARM64 architecture
  bool isAArch64() const {
    return targetTriple.getArch() == llvm::Triple::aarch64 ||
           targetTriple.getArch() == llvm::Triple::aarch64_be;
  }

  /// Returns true if targeting a 32-bit architecture
  bool is32Bit() const { return targetTriple.isArch32Bit(); }

  /// Returns true if a struct return value should use the sret calling
  /// convention (return via pointer argument) based on the target ABI.
  ///
  /// For Windows x64: structs of size != 1, 2, 4, 8 bytes use sret
  /// For System V AMD64: structs > 16 bytes use sret (simplified rule)
  /// For AAPCS64: structs > 16 bytes use sret (simplified rule)
  bool shouldUseSret(mlir::Type llvmType) const;

  /// Get the target triple
  const llvm::Triple &getTargetTriple() const { return targetTriple; }

private:
  mlir::DataLayout dataLayout;
  llvm::Triple targetTriple;
};
} // namespace reussir

#endif // REUSSIR_CONVERSION_TYPECONVERTER_H

