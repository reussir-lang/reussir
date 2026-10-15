// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func @host_result(%src: memref<4xf32>) {
  // expected-error @+1 {{requires a target-annotated device array}}
  %d = reussir.array.to_device %src : memref<4xf32> -> !reussir.rc<!reussir.array<4 x f32>>
  return
}
// -----
func.func @type_mismatch(%src: memref<4xi32>) {
  // expected-error @+1 {{host and device element types must match}}
  %d = reussir.array.to_device %src : memref<4xi32> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @rank_mismatch(%src: memref<2x2xf32>) {
  // expected-error @+1 {{host and device ranks must match}}
  %d = reussir.array.to_device %src : memref<2x2xf32> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return
}
// -----
!D = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
func.func @wrong_shape(%d: !D, %dst: memref<3xf32>) {
  // expected-error @+1 {{host and device shapes must match}}
  reussir.array.to_host %d into %dst : !D, memref<3xf32>
  return
}
// -----
func.func @device_memref(%src: memref<4xf32, 1>) {
  // expected-error @+1 {{requires a host memref in memory space zero}}
  %d = reussir.array.to_device %src : memref<4xf32, 1> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @non_strided(%src: memref<4xf32, affine_map<(d0) -> (d0 mod 2)>>) {
  // expected-error @+1 {{requires a strided host memref}}
  %d = reussir.array.to_device %src : memref<4xf32, affine_map<(d0) -> (d0 mod 2)>> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @payload_token(%src: memref<1024xf32>, %t: !reussir.token<align: 8, size: 4104>) {
  // expected-error @+1 {{expected descriptor token type}}
  %d = reussir.array.to_device %src token(%t : !reussir.token<align: 8, size: 4104>) : memref<1024xf32> -> !reussir.rc<!reussir.array<1024 x f32, #reussir.target<devices = [0]>>>
  return
}


// -----
func.func @extent_host(%d: !reussir.rc<!reussir.array<3 x i32>>) {
  // expected-error @+1 {{requires a target-annotated device array}}
  %n = reussir.array.device_extent %d[0] : !reussir.rc<!reussir.array<3 x i32>>
  return
}
// -----
!D = !reussir.rc<!reussir.array<? x i32, #reussir.target<devices = [0]>>>
func.func @extent_rank(%d: !D) {
  // expected-error @+1 {{dimension is outside the array rank}}
  %n = reussir.array.device_extent %d[1] : !D
  return
}
// -----
func.func @wrong_width(%src: memref<4xi32>) {
  // expected-error @+1 {{host and device element types must match}}
  %d = reussir.array.to_device %src : memref<4xi32> -> !reussir.rc<!reussir.array<4 x ui64, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @bool_not_byte(%src: memref<4xi8>) {
  // expected-error @+1 {{host and device element types must match}}
  %d = reussir.array.to_device %src : memref<4xi8> -> !reussir.rc<!reussir.array<4 x i1, #reussir.target<devices = [0]>>>
  return
}
