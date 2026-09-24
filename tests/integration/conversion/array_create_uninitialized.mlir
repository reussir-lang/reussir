// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s --check-prefix=PARSE
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-convert-to-std="expand-arrays=false" | %FileCheck %s --check-prefix=DEFER
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --sccp --canonicalize --loop-invariant-code-motion --reussir-convert-to-std | %FileCheck %s --check-prefix=LOWER

// No initializer means no payload writes. It must still accept token
// instantiation and survive the passes that operate on initializer loops.

// PARSE-LABEL: func.func @fixed
// PARSE: reussir.array.create extents() : {{.*}}{{$}}
// DEFER-LABEL: func.func @fixed
// DEFER: %[[TOKEN:.*]] = reussir.token.alloc
// DEFER: reussir.array.create extents() token(%[[TOKEN]] : {{.*}}) : {{.*}}{{$}}
// LOWER-LABEL: func.func @fixed
// LOWER: %[[ARRAY:.*]] = reussir.array.instantiate
// LOWER-NEXT: return %[[ARRAY]]
func.func @fixed() -> !reussir.rc<!reussir.array<2 x 3 x i32>> {
  %array = reussir.array.create extents() : !reussir.rc<!reussir.array<2 x 3 x i32>>
  return %array : !reussir.rc<!reussir.array<2 x 3 x i32>>
}

// PARSE-LABEL: func.func @dynamic
// PARSE: reussir.array.create extents(%arg0) : {{.*}}{{$}}
// DEFER-LABEL: func.func @dynamic
// DEFER: reussir.token.alloc(%{{.*}} : index)
// DEFER: reussir.array.create extents(%arg0) token(
// LOWER-LABEL: func.func @dynamic
// LOWER: %[[ARRAY:.*]] = reussir.array.instantiate{{.*}}extents(%arg0)
// LOWER-NEXT: return %[[ARRAY]]
func.func @dynamic(%n: index) -> !reussir.rc<!reussir.array<? x 4 x i32>> {
  %array = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x 4 x i32>>
  return %array : !reussir.rc<!reussir.array<? x 4 x i32>>
}

// An explicitly empty region has the same meaning and prints without a body.
// PARSE-LABEL: func.func @empty_body
// PARSE: reussir.array.create extents() : {{.*}}{{$}}
// LOWER-LABEL: func.func @empty_body
// LOWER: %[[ARRAY:.*]] = reussir.array.instantiate
// LOWER-NEXT: return %[[ARRAY]]
func.func @empty_body() -> !reussir.rc<!reussir.array<0 x i32>> {
  %array = reussir.array.create extents() : !reussir.rc<!reussir.array<0 x i32>> body {}
  return %array : !reussir.rc<!reussir.array<0 x i32>>
}
