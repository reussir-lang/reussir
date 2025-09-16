//===-- ReussirTypes.h - Reussir dialect types ------------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This header file provides the definitions for types used in the Reussir
// dialect.
//
//===----------------------------------------------------------------------===//
#pragma once
#ifndef REUSSIR_IR_REUSSIRTYPES_H
#define REUSSIR_IR_REUSSIRTYPES_H

#include <mlir/IR/Attributes.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/Types.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Support/LLVM.h>
#include <variant>

#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirTypeDetails.h"

namespace reussir {
std::optional<std::tuple<llvm::TypeSize, llvm::Align, mlir::Type>>
deriveCompoundSizeAndAlignment(mlir::MLIRContext *context,
                               llvm::ArrayRef<mlir::Type> members,
                               llvm::ArrayRef<Capability> memberCapabilities,
                               const mlir::DataLayout &dataLayout);
bool isNonNullPointerType(mlir::Type type);
bool isTriviallyCopyable(mlir::Type type);
mlir::Type getProjectedType(mlir::Type type, Capability fieldCap,
                            Capability refCap);

namespace scanner {
inline int32_t end() {
  return 0; // Assuming 0 represents the 'end' instruction
}
inline int32_t variant() {
  return -1; // Assuming 1 represents the 'variant' instruction
}
inline int32_t field() { return -2; }
inline int32_t advance(uint32_t bytes) {
  assert(bytes <= INT32_MAX && "advance bytes must fit in int32");
  return static_cast<int32_t>(bytes);
}
inline int32_t skip(size_t count) {
  assert(count <= INT32_MAX - 3 && "skip count must fit in int32");
  return static_cast<int32_t>(-3 - count);
}
struct End {};
struct Variant {};
struct Field {};
struct Advance {
  uint32_t bytes;
};
struct Skip {
  size_t count;
};
using Instr = std::variant<End, Variant, Field, Advance, Skip>;
inline Instr decode(int32_t code) {
  if (code == 0)
    return End{};
  else if (code == -1)
    return Variant{};
  else if (code == -2)
    return Field{};
  else if (code <= -3)
    return Skip{static_cast<size_t>(-3 - code)};
  else
    return Advance{static_cast<uint32_t>(code)};
}
} // namespace scanner
} // namespace reussir

#define GET_TYPEDEF_CLASSES
#include "Reussir/IR/ReussirOpsTypes.h.inc"

#endif // REUSSIR_IR_REUSSIRTYPES_H
