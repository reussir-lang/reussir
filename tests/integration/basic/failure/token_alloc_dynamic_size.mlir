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

// -----

func.func @ensure_dynamic_without_size(%old: !reussir.nullable<!reussir.token<align: 8, size: ?>>, %bytes: index)
    -> !reussir.token<align: 8, size: ?> {
  // expected-error @+1 {{a dynamically sized token requires a size operand}}
  %token = reussir.token.ensure(%old : !reussir.nullable<!reussir.token<align: 8, size: ?>>) : !reussir.token<align: 8, size: ?>
  return %token : !reussir.token<align: 8, size: ?>
}

// -----

func.func @ensure_static_with_size(%old: !reussir.nullable<!reussir.token<align: 8, size: ?>>, %bytes: index)
    -> !reussir.token<align: 8, size: 16> {
  // expected-error @+1 {{a statically sized token takes no size operand}}
  %token = reussir.token.ensure(%old : !reussir.nullable<!reussir.token<align: 8, size: ?>>) size(%bytes : index) : !reussir.token<align: 8, size: 16>
  return %token : !reussir.token<align: 8, size: 16>
}

// -----

func.func @realloc_dynamic_without_size(%old: !reussir.nullable<!reussir.token<align: 8, size: ?>>, %bytes: index)
    -> !reussir.token<align: 8, size: ?> {
  // expected-error @+1 {{a dynamically sized token requires a size operand}}
  %token = reussir.token.realloc(%old : !reussir.nullable<!reussir.token<align: 8, size: ?>>) : !reussir.token<align: 8, size: ?>
  return %token : !reussir.token<align: 8, size: ?>
}

// -----

func.func @realloc_static_with_size(%old: !reussir.nullable<!reussir.token<align: 8, size: ?>>, %bytes: index)
    -> !reussir.token<align: 8, size: 16> {
  // expected-error @+1 {{a statically sized token takes no size operand}}
  %token = reussir.token.realloc(%old : !reussir.nullable<!reussir.token<align: 8, size: ?>>) size(%bytes : index) : !reussir.token<align: 8, size: 16>
  return %token : !reussir.token<align: 8, size: 16>
}
