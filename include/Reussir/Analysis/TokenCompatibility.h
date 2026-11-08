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
/// Whether a reuse token can serve a token acceptor, and the reach of a token
/// within the structured control flow that token reuse walks.
///
//===----------------------------------------------------------------------===//

#pragma once

#ifndef REUSSIR_ANALYSIS_TOKENCOMPATIBILITY_H
#define REUSSIR_ANALYSIS_TOKENCOMPATIBILITY_H

#include <cstdint>
#include <optional>

#include <llvm/ADT/DenseMap.h>
#include <mlir/IR/Operation.h>
#include <mlir/IR/Value.h>

#include "Reussir/Analysis/EquivalenceAnalysis.h"
#include "Reussir/IR/ReussirInterfaces.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

/// Proves equal allocation sizes of a donated token and an acceptor. Queries
/// run on unchanged IR, so constant-size proofs are cached across them.
class TokenSizeAnalysis {
public:
  bool haveEqualSizes(TokenType donorType, mlir::TypedValue<RcType> donorRc,
                      TokenAcceptor recipient);

private:
  std::optional<uint64_t> getConstantSize(TokenType type,
                                          TokenAcceptor acceptor);

  llvm::DenseMap<mlir::Value, std::optional<uint64_t>> constantSizes;
};

/// Scores of `scoreTokenForAcceptor` at or above this value reuse through
/// `token.ensure`; lower non-negative scores resize through `token.realloc`.
inline constexpr int kReallocEnsureCutoff = 2;

/// How well a token of `producedType`, donated by the box `producerRc` when
/// known, serves `consumer`'s allocation:
///   < 0 : not at all
///   == 0: realloc of a dynamic donor or recipient, resized at runtime
///   == 1: realloc of a fixed-size token in the same allocator bin
///   >= 2: ensure of an exact-size match, larger for better locality
int scoreTokenForAcceptor(TokenType producedType,
                          mlir::TypedValue<RcType> producerRc,
                          TokenAcceptor consumer,
                          EquivalenceAnalysis &equivalence,
                          TokenSizeAnalysis &sizes);

/// Whether nothing observable follows the call `op` on any path to its
/// function's return. Token reuse frees its pending tokens before such a
/// call even across calls: no acceptor can follow it.
bool isTailPositioned(mlir::Operation *op);

} // namespace reussir

#endif // REUSSIR_ANALYSIS_TOKENCOMPATIBILITY_H
