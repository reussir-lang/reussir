// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse | %FileCheck %s --check-prefix=TOKEN
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-rc-decrement-expansion | %FileCheck %s --check-prefix=DROP

!A = !reussir.rc<!reussir.array<? x 4 x f32, #reussir.target<devices = [0]>>>
!B = !reussir.rc<!reussir.array<? x 1024 x i64, #reussir.target<devices = [0]>>>
!T = !reussir.token<align: 8, size: 32>

// Different element types and dynamic payload sizes have identical metadata.
// TOKEN-LABEL: func.func @reuse
// TOKEN: %[[TOKEN:.*]] = reussir.rc.dec{{.*}} : !reussir.nullable<!reussir.token<align : 8, size : 32>>
// TOKEN: %[[ENSURED:.*]] = reussir.token.ensure(%[[TOKEN]]
// TOKEN-NOT: reussir.token.alloc
// TOKEN: reussir.array.create extents(%arg1) token(%[[ENSURED]]
// DROP-LABEL: func.func @reuse
// DROP: scf.if
// DROP: reussir.ref.drop
// DROP: reussir.rc.reinterpret
// DROP: reussir.nullable.create
// DROP: } else {
// DROP: reussir.rc.set
// DROP: reussir.nullable.create
func.func @reuse(%old: !A, %n: index) -> !B {
  reussir.rc.dec(%old : !A)
  %a = reussir.array.create extents(%n) : !B
  return %a : !B
}

// An explicit metadata token is accepted even with dynamic target extents.
// TOKEN-LABEL: func.func @explicit
// TOKEN-NOT: reussir.token.alloc
// TOKEN: reussir.array.create extents(%arg1) token(%arg0
func.func @explicit(%token: !T, %n: index) -> !A {
  %a = reussir.array.create extents(%n) token(%token : !T) : !A
  return %a : !A
}
