// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

module {
  func.func @missing_extent(%init: i32) -> !reussir.rc<!reussir.array<? x i32>> {
    // expected-error @+1 {{expects 1 extent operand(s), got 0}}
    %rc = reussir.array.create init(%init : i32) extents() : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }
}

// -----

module {
  func.func @wrong_init(%n: index, %init: i64) -> !reussir.rc<!reussir.array<? x i32>> {
    // expected-error @+1 {{initializer type must match array element type}}
    %rc = reussir.array.create init(%init : i64) extents(%n) : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }
}

// -----

module {
  func.func @wrong_token(%n: index, %init: i32, %token: !reussir.token<align: 8, size: 64>) -> !reussir.rc<!reussir.array<? x i32>> {
    // expected-error @+1 {{expected token type}}
    %rc = reussir.array.create init(%init : i32) extents(%n) token(%token : !reussir.token<align: 8, size: 64>) : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }
}

// -----

module {
  func.func @poison_payload() -> !reussir.rc<!reussir.array<? x i32>> {
    %value = ub.poison : !reussir.array<? x i32>
    // expected-error @+1 {{dynamic arrays must be created with array.create}}
    %rc = reussir.rc.create value(%value : !reussir.array<? x i32>) : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }
}

// -----

module {
  func.func @wrong_instantiate_token(%n: index, %token: !reussir.token<align: 8, size: 64>) -> !reussir.rc<!reussir.array<? x i32>> {
    // expected-error @+1 {{expected token type}}
    %rc = reussir.array.instantiate(%token : !reussir.token<align: 8, size: 64>) extents(%n) : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }
}
