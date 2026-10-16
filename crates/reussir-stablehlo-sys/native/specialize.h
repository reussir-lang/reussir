// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#ifndef REUSSIR_STABLEHLO_SPECIALIZE_H
#define REUSSIR_STABLEHLO_SPECIALIZE_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
// Borrowed for the call. Nonempty arrays must be valid and aligned. Callbacks
// must not unwind. Empty arrays may be null. No context or memory is retained.
typedef struct {
  const int64_t *dimensions;
  size_t rank;
  // Explicit host-known shape values only, never implicit scalar
  // specialization. Constant arguments remain in the executable signature.
  const int64_t *values;
  size_t value_count;
  uint8_t has_values;
} ReussirStablehloArgument;
typedef void (*ReussirStablehloBytes)(const uint8_t *, size_t, void *);
typedef void (*ReussirStablehloShape)(const int64_t *, size_t, void *);
enum ReussirStablehloStatus {
  REUSSIR_STABLEHLO_OK = 0,
  REUSSIR_STABLEHLO_INVALID_INPUT = 1,
  REUSSIR_STABLEHLO_REFINEMENT_FAILED = 2,
  REUSSIR_STABLEHLO_RESIDUAL_DYNAMISM = 3,
  REUSSIR_STABLEHLO_SERIALIZATION_FAILED = 4
};
// Accepts MLIR text or bytecode, returns bytecode and logical output shapes in
// result order. Output callbacks run only on success. All callbacks are
// required.
uint32_t reussir_stablehlo_specialize(
    const uint8_t *program, size_t program_size,
    const ReussirStablehloArgument *arguments, size_t argument_count,
    ReussirStablehloBytes output, ReussirStablehloShape output_shape,
    ReussirStablehloBytes diagnostic, void *user_data);
#ifdef __cplusplus
}
#endif
#endif
