// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s --check-prefix=STD
// RUN: %reussir-opt %s --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts | %FileCheck %s --check-prefix=LLVM

!token = !reussir.token<align: 8, size: ?>
!nullable = !reussir.nullable<!token>
module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // The nonnull path keeps the storage; only the null path needs the size.
  // STD-LABEL: func.func @ensure_dynamic(
  // STD: reussir.token.launder
  // STD: } else {
  // STD: reussir.token.alloc(%arg1 : index)
  // LLVM-LABEL: llvm.func @ensure_dynamic(
  // LLVM: llvm.call @__reussir_allocate({{.*}}, %arg1)
  func.func @ensure_dynamic(%old: !nullable, %bytes: index) -> !token {
    %new = reussir.token.ensure(%old : !nullable) size(%bytes : index) : !token
    return %new : !token
  }

  // A proven compatible static donor can ensure a dynamic result too.
  // STD-LABEL: func.func @ensure_static_donor(
  // STD: reussir.token.launder({{.*}} : <align : 8, size : 16>) : <align : 8, size : ?>
  // LLVM-LABEL: llvm.func @ensure_static_donor(
  // LLVM: llvm.call @__reussir_allocate({{.*}}, %arg1)
  func.func @ensure_static_donor(%old: !reussir.nullable<!reussir.token<align: 8, size: 16>>, %bytes: index) -> !token {
    %new = reussir.token.ensure(%old : !reussir.nullable<!reussir.token<align: 8, size: 16>>) size(%bytes : index) : !token
    return %new : !token
  }

  // Both runtime entry points receive the requested SSA byte size.
  // LLVM-LABEL: llvm.func @resize_dynamic(
  // LLVM: llvm.call @__reussir_realloc_unsized(%arg0, {{.*}}, %arg1)
  func.func @resize_dynamic(%old: !nullable, %bytes: index) -> !token {
    %new = reussir.token.realloc(%old : !nullable) size(%bytes : index) : !token
    return %new : !token
  }

  // LLVM-LABEL: llvm.func @resize_static(
  // LLVM: llvm.call @__reussir_reallocate(%arg0, {{.*}}, %arg1)
  func.func @resize_static(%old: !reussir.nullable<!reussir.token<align: 8, size: 16>>, %bytes: index) -> !token {
    %new = reussir.token.realloc(%old : !reussir.nullable<!reussir.token<align: 8, size: 16>>) size(%bytes : index) : !token
    return %new : !token
  }
}
