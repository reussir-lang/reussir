// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32>>
func.func @host_array(%input: !R) {
  // expected-error @+1 {{requires a target-annotated device array}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
func.func @wrong_type(%input: !R) {
  // expected-error @+1 {{requires an IFRT array type}}
  %view = reussir.array.to_ifrt %input : !R -> tensor<8xf32>
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
func.func @shape(%input: !R) {
  // expected-error @+1 {{shapes and element types must match exactly}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x i32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
func.func @dtype(%input: !R) {
  // expected-error @+1 {{shapes and element types must match exactly}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
func.func @dynamic_shape(%input: !R) {
  // expected-error @+1 {{shapes and element types must match exactly}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [1], sharding = #S, memory_kind = "device", layout = "{0}">>>
func.func @devices(%input: !R) {
  // expected-error @+1 {{ordered devices must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], memory_kind = "device", layout = "{0}">>>
func.func @sharding(%input: !R) {
  // expected-error @+1 {{sharding must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "pinned_host", layout = "{0}">>>
func.func @memory_kind(%input: !R) {
  // expected-error @+1 {{memory kinds must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "default">>>
func.func @layout(%input: !R) {
  // expected-error @+1 {{layouts must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device">>>
func.func @absent_layout(%input: !R) {
  // expected-error @+1 {{layouts must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @donation(%input: !R) attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT input cannot be donated}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.CallLoadedExecutable @kernel(%view) {donated_input_indices = array<i32: 0>} : (!I) -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @alias(%input: !R) attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT input cannot alias a call result}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.CallLoadedExecutable @kernel(%view) {io_aliases = [array<i32: 0, 0>]} : (!I) -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @borrow_escapes(%input: !R) -> !I attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT array only supports direct IFRT call and After uses}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return %view : !I
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @borrow_across_region(%input: !R, %cond: i1) attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT array must stay in its defining block}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  scf.if %cond {
    %out, %done = ifrt.CallLoadedExecutable @kernel(%view) : (!I) -> !I
  }
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @borrowed_argument(%input: !I) -> !R attributes {ifrt.function} {
  // expected-error @+1 {{requires an owned IFRT call result}}
  %result = reussir.array.from_ifrt %input : !I -> !R
  return %result : !R
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @adopt_twice(%input: !I) -> (!R, !R) attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel(%input) : (!I) -> !I
  // expected-error @+1 {{adopted IFRT result must have exactly one use}}
  %a = reussir.array.from_ifrt %out : !I -> !R
  %b = reussir.array.from_ifrt %out : !I -> !R
  return %a, %b : !R, !R
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @adopt_in_region(%input: !I, %cond: i1) attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel(%input) : (!I) -> !I
  scf.if %cond {
    // expected-error @+1 {{IFRT result must be adopted in its defining block}}
    %result = reussir.array.from_ifrt %out : !I -> !R
  }
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S, memory_kind = "device", layout = "{0}">>>
ifrt.LoadedExecutable @kernel on devices [0] : (!I) -> !I
func.func @wrong_token(%input: !I, %token: !reussir.token<align: 8, size: 64>) -> !R attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel(%input) : (!I) -> !I
  // expected-error @+1 {{expected descriptor token type}}
  %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align: 8, size: 64>) : !I -> !R
  return %result : !R
}
// -----
#S = #ifrt.sharding_param<2 to [0] on 2>
!I = !ifrt.array<tensor<8xf32>, #S, [1, 3]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [3, 1], sharding = #S>>>
func.func @device_order(%input: !R) {
  // expected-error @+1 {{ordered devices must match}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S>>>
func.func private @kernel(%input: tensor<8xf32>) -> tensor<8xf32> {
  return %input : tensor<8xf32>
}
func.func @call_donation(%input: !R) attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT input cannot be donated}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.Call @kernel(%view) on devices [0] {donated_input_indices = array<i32: 0>} : (!I) -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #S>>>
func.func private @kernel(%input: tensor<8xf32>) -> tensor<8xf32> {
  return %input : tensor<8xf32>
}
func.func @call_alias(%input: !R) attributes {ifrt.function} {
  // expected-error @+1 {{borrowed IFRT input cannot alias a call result}}
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.Call @kernel(%view) on devices [0] {io_aliases = [array<i32: 0, 0>]} : (!I) -> !I
  return
}
// -----
#S = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #S, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #S>>>
ifrt.LoadedExecutable @kernel on devices [0] : () -> !I
func.func @result_metadata() attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel() : () -> !I
  // expected-error @+1 {{shapes and element types must match exactly}}
  %result = reussir.array.from_ifrt %out : !I -> !R
  return
}
