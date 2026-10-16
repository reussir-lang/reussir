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
/// This file declares the C ABI of the reussir-stablehlo crate. Each call
/// parses its program into a private MLIR context, so calls are independent
/// and may run concurrently. Arrays are borrowed for the duration of a call
/// and may be null when empty. Callbacks must not unwind; the buffers they
/// receive are only borrowed.
///
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_STABLEHLO_H
#define REUSSIR_STABLEHLO_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

// One argument of `@main`. A runtime argument has the logical (global)
// dimensions `dimensions[0..rank)`, never per-shard ones. A constant argument
// (`is_constant`) is a rank-zero integer fixed to `constant`; its uses are
// replaced with that value and it is removed from `@main`.
typedef struct {
  const int64_t *dimensions;
  size_t rank;
  int64_t constant;
  uint8_t is_constant;
} ReussirStablehloArgument;

typedef void (*ReussirStablehloBytes)(const uint8_t *data, size_t size,
                                      void *user_data);
// One result type: its dimensions and its element width in bits, or 0 if the
// element type has no fixed width.
typedef void (*ReussirStablehloResultType)(const int64_t *dimensions,
                                           size_t rank, uint32_t element_bits,
                                           void *user_data);

enum ReussirStablehloStatus {
  REUSSIR_STABLEHLO_OK = 0,
  REUSSIR_STABLEHLO_INVALID_INPUT = 1,
  REUSSIR_STABLEHLO_REFINEMENT_FAILED = 2,
  REUSSIR_STABLEHLO_RESIDUAL_DYNAMISM = 3,
  REUSSIR_STABLEHLO_SERIALIZATION_FAILED = 4
};

// Specialize `@main` in `program` (MLIR text or bytecode) on its constant
// arguments and refine its dynamic shapes from the shapes of the rest. On
// success, `refined` receives the verified bytecode, whose `@main` keeps only
// the runtime arguments, and `result_type` runs once per result, in order.
// Dynamism that neither constants nor argument shapes determine is an error.
// `diagnostic` may run on any status.
uint32_t reussir_stablehlo_refine_shapes(
    const uint8_t *program, size_t program_size,
    const ReussirStablehloArgument *arguments, size_t argument_count,
    ReussirStablehloBytes refined, ReussirStablehloResultType result_type,
    ReussirStablehloBytes diagnostic, void *user_data);

// Rewrite `@main` in `program` (MLIR text or bytecode) for one donation
// choice, given in original argument numbering. Argument `i` is replaced by
// `mapping[i]`, which must be an earlier equivalent argument or `i` itself,
// and replaced arguments are erased. `donors` are retained arguments marked
// `jax.buffer_donor`; `aliases` holds `alias_count` (input, result) pairs that
// additionally set `tf.aliasing_output`, each input being a donor. On success,
// `rewritten` receives the verified bytecode. Choosing donors is the caller's
// responsibility.
uint32_t reussir_stablehlo_apply_donation(
    const uint8_t *program, size_t program_size, const uint32_t *mapping,
    size_t argument_count, const uint32_t *donors, size_t donor_count,
    const uint32_t *aliases, size_t alias_count,
    ReussirStablehloBytes rewritten, ReussirStablehloBytes diagnostic,
    void *user_data);

#ifdef __cplusplus
}
#endif
#endif
