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
/// This header file provides the definitions for types used in the Reussir
/// dialect.
///
//===----------------------------------------------------------------------===//

#pragma once
#include <mlir/IR/Builders.h>
#ifndef REUSSIR_IR_REUSSIRTYPES_H
#define REUSSIR_IR_REUSSIRTYPES_H

#include <mlir/IR/Attributes.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/BuiltinTypeInterfaces.h>
#include <mlir/IR/DialectRegistry.h>
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
                               llvm::ArrayRef<bool> memberIsField,
                               const mlir::DataLayout &dataLayout,
                               bool memBoxInternal = false);
bool isNonNullPointerType(mlir::Type type);
bool isTriviallyCopyable(mlir::Type type);
// Whether `type` is the rcgmp dialect's arbitrary-precision integer handle,
// `!rcgmp.mpz`: a one-word member that is not trivially copyable, retained and
// released through rcgmp's own operations. Always false when Reussir is built
// without rcgmp.
bool isRCGMPIntegerType(mlir::Type type);
// Registers the models that let Reussir store `!rcgmp.mpz` handles, such as
// array elements viewed as memrefs. Call wherever the rcgmp dialect is
// registered; a no-op without rcgmp.
void registerRCGMPExternalModels(mlir::DialectRegistry &registry);
// Checks that `eleTy` is eligible for atomic accesses: a byte-addressable,
// power-of-two-width signless integer or floating-point primitive. `what`
// names the checked entity in diagnostics (e.g. "atomic cell element").
mlir::LogicalResult verifyAtomicElementType(
    llvm::function_ref<mlir::InFlightDiagnostic()> emitError, mlir::Type eleTy,
    llvm::StringRef what);
// `parentAtomic` is the projecting rc/ref's atomic kind. Atomicity is a
// whole-subtree property: a member link's atomic kind is the join of what
// the member type writes and what the parent inherits down (an atomic parent
// floods atomic through derived shared links, nullable wrappers, and value
// records; an explicit atomic rc member stands on its own in normal space).
mlir::Type getProjectedType(mlir::Type type, bool fieldCap, Capability refCap,
                            AtomicKind parentAtomic = AtomicKind::normal);
mlir::Type memberStorageType(mlir::MLIRContext *context, mlir::Type rawMember,
                             bool isField, bool memBoxInternal = false);
// Prints `type` as `type.print(os)` does, except that the body of each named
// record is written only at its first occurrence: later occurrences print as
// `<kind "name">`, the form `RecordType::print` already uses for a
// self-reference. The text still determines the type (a named record's body
// is fixed by its (name, kind), and it is written once). The full form repeats a
// named record's body once per path that reaches it (exponential for a record
// nested k levels deep through two members per level); here each named
// record costs its body once. Only named records are shortened: an unnamed
// composite type (a closure type, an anonymous record) is still written at
// each occurrence, so a type that repeats such a type along many paths (for
// instance `T(i+1) = (T(i)) -> T(i)`) is still exponential, as in the full
// form. For type identities computed from text (closure WPD type ids); the
// output is not meant to be parsed back.
void printTypeWithRecordBodiesOnce(mlir::Type type, llvm::raw_ostream &os);
class CellType;
// The `sync` primitive type physically backing a lock-guarded cell:
// `!sync.mutex<T>` for a mutex cell, `!sync.combining_lock<T>` for a flatlock
// cell, `!sync.rwlock<T>` for an rwlock cell. Null for every other cell kind —
// callers use this both to compute the cell's layout and to type its storage
// views.
mlir::Type lockGuardedStorageType(CellType type);

namespace scanner {
// Encoding shared with reussir-rt's region scanner interpreter
// (crates/reussir-rt/src/region/scanner.rs): 0 = end, -1/-3/-4 = variant tag
// read of 1/2/4 bytes, -2 = field, > 0 = advance, <= -5 = jump.
inline int32_t end() {
  return 0; // Assuming 0 represents the 'end' instruction
}
inline int32_t variant(uint64_t tagByteWidth) {
  switch (tagByteWidth) {
  case 1:
    return -1;
  case 2:
    return -3;
  case 4:
    return -4;
  default:
    llvm_unreachable("unsupported variant tag width");
  }
}
inline int32_t field() { return -2; }
inline int32_t advance(uint32_t bytes) {
  assert(bytes <= INT32_MAX && "advance bytes must fit in int32");
  return static_cast<int32_t>(bytes);
}
inline int32_t skip(size_t count) {
  assert(count <= INT32_MAX - 5 && "skip count must fit in int32");
  return static_cast<int32_t>(-5 - count);
}
struct End {};
struct Variant {
  uint64_t tagByteWidth;
};
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
    return Variant{1};
  else if (code == -2)
    return Field{};
  else if (code == -3)
    return Variant{2};
  else if (code == -4)
    return Variant{4};
  else if (code <= -5)
    return Skip{static_cast<size_t>(-5 - code)};
  else
    return Advance{static_cast<uint32_t>(code)};
}
struct EmitState {
  size_t cursorPosition = 0;
  size_t scannedBytes = 0;
};
} // namespace scanner
} // namespace reussir

#define GET_TYPEDEF_CLASSES
#include "Reussir/IR/ReussirOpsTypes.h.inc"

#endif // REUSSIR_IR_REUSSIRTYPES_H
