// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

// The byte-size operand and the dynamic token type come together: a
// `token<align, ?>` result needs the operand, a sized one must not have it.
func.func @dynamic_without_size() -> !reussir.token<align: 8, size: ?> {
  // expected-error @+1 {{a dynamically sized token requires a size operand}}
  %token = reussir.token.alloc : !reussir.token<align: 8, size: ?>
  return %token : !reussir.token<align: 8, size: ?>
}

// -----

func.func @static_with_size(%bytes: index) -> !reussir.token<align: 8, size: 16> {
  // expected-error @+1 {{a statically sized token takes no size operand}}
  %token = reussir.token.alloc(%bytes : index) : !reussir.token<align: 8, size: 16>
  return %token : !reussir.token<align: 8, size: 16>
}
