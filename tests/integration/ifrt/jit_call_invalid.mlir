// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_callee(%input: !I) {
  // expected-error @+1 {{callee must reference a reussir.ifrt.bytecode symbol}}
  %out, %done = reussir.pjrt.jit_call @bad_callee(%input) on devices [0] : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_control(%input: !I) {
  // expected-error @+1 {{requires IFRT control dependencies}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] : (!I) -> (!I, i32)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_device(%input: !I) {
  // expected-error @+1 {{array devices must be a subset of call devices}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [1] : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_input(%input: tensor<8xf32>) {
  // expected-error @+1 {{requires IFRT array inputs and outputs}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] : (tensor<8xf32>) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_donation(%input: !I) {
  // expected-error @+1 {{invalid or repeated donated input index}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] {donated_input_indices = array<i32: 1>} : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_alias(%input: !I) {
  // expected-error @+1 {{invalid input/output alias indices}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] {io_aliases = [array<i32: 0, -1>]} : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @repeated_alias(%input: !I) {
  // expected-error @+1 {{input/output alias repeats an aliased or donated index}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] {io_aliases = [array<i32: 0, 0>], donated_input_indices = array<i32: 0>} : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!O = !ifrt.array<tensor<8xi32>, #s, [0]>
func.func @wrong_alias_type(%input: !I) {
  // expected-error @+1 {{aliased arrays must have equal dtypes and per-shard shapes}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] {io_aliases = [array<i32: 0, 0>]} : (!I) -> (!O, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
func.func @bad_arg_attrs(%input: !I) {
  // expected-error @+1 {{arg_attrs must match the input count}}
  %out, %done = reussir.pjrt.jit_call @kernel(%input) on devices [0] {arg_attrs = []} : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
func.func @borrowed_donation(%input: !R) {
  // expected-error @+1 {{borrowed IFRT input cannot be donated}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = reussir.pjrt.jit_call @kernel(%view) on devices [0] {donated_input_indices = array<i32: 0>} : (!I) -> (!I, !ifrt.control)
  return
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
func.func @borrowed_alias(%input: !R) {
  // expected-error @+1 {{borrowed IFRT input cannot alias a call result}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = reussir.pjrt.jit_call @kernel(%view) on devices [0] {io_aliases = [array<i32: 0, 0>]} : (!I) -> (!I, !ifrt.control)
  return
}
