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
/// This file declares support for encoding nullary variants as immediate
/// pointers and carrying variant discriminants in TBI pointer tags.
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_TRANSFORMATION_SPECIALPOINTERTAG_H
#define REUSSIR_TRANSFORMATION_SPECIALPOINTERTAG_H

#include "llvm/ADT/StringRef.h"

namespace mlir {
class Operation;
}

namespace reussir {
class RcType;

/// Whether this module represents the type's nullary arms as immediates.
bool usesNullaryImmediates(RcType type, mlir::Operation *scope);

/// Module encoding selection. Under TBI, shared boxed variants with at most
/// 255 constructors carry `tag + 1` in every pointer's top byte. Larger
/// types keep header tags and allocate all constructors. Nullary immediates
/// are recognized by the type's nullary tag set, not by a nonzero byte.
/// Allocator/reuse tokens are untagged; accepting constructors stamp their
/// own tag. The immortal encoding uses per-tag dummy addresses for eligible
/// nullary arms and leaves allocated pointers unchanged.
constexpr llvm::StringLiteral kSpecialPtrTagAttr = "reussir.special_ptr_tag";

/// TBI encoding (aarch64): top byte = `tag + 1`; low bits hold the allocated
/// object or nullary dummy address. Hardware ignores the byte on accesses.
constexpr llvm::StringLiteral kSpecialPtrTagTBI = "tbi";

/// Arch-independent encoding: the immediate *is* the dummy box address (no
/// pointer bit tricks). The dummy refcount is initialized to an immortal
/// value; guards test refcount magnitude instead of pointer bits.
constexpr llvm::StringLiteral kSpecialPtrTagImmortal = "immortal";

} // namespace reussir

#endif // REUSSIR_TRANSFORMATION_SPECIALPOINTERTAG_H
