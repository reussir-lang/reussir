// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @token(%token: !reussir.token<align: 8, size: 4096>) {
  // expected-error @+1 {{expected token type '!reussir.token<align : 8, size : 24>'}}
  %a = reussir.array.create extents() token(%token : !reussir.token<align: 8, size: 4096>) : !reussir.rc<!array>
  return
}
// -----
!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @token_result(%a: !reussir.rc<!array>) {
  // expected-error @+1 {{token size must match managed type size}}
  %t = reussir.rc.dec(%a : !reussir.rc<!array>) : !reussir.nullable<!reussir.token<align: 8, size: 4096>>
  return
}
// -----
!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @initializer(%v: f32) {
  // expected-error @+1 {{target arrays do not support host initializer bodies}}
  %a = reussir.array.create extents() : !reussir.rc<!array> body {
  ^bb0(%i: index):
    reussir.scf.yield %v : f32
  }
  return
}
// -----
!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @host_view(%a: !reussir.ref<!array>) {
  // expected-error @+1 {{target arrays cannot expose a host memref or tensor view}}
  %view = reussir.array.view(%a : !reussir.ref<!array>) : memref<4xf32>
  return
}
// -----
!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @copy(%a: !reussir.ref<!array>, %b: !reussir.ref<!array>) {
  // expected-error @+1 {{target array descriptors cannot be copied}}
  reussir.ref.memcpy %a to %b : !reussir.ref<!array> to !reussir.ref<!array>
  return
}
// -----
!array = !reussir.array<4 x f32, #reussir.target<devices = [0]>>
func.func @load(%a: !reussir.ref<!array>) {
  // expected-error @+1 {{target arrays must remain RC wrapped}}
  %v = reussir.ref.load(%a : !reussir.ref<!array>) : !array
  return
}
