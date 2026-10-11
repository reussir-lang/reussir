// RUN: %reussir-opt %s -verify-diagnostics -split-input-file

// expected-error @+1 {{cell and array references cannot have regional field capability}}
!bad = !reussir.ref<!reussir.cell<i64> field>

// -----

// expected-error @+1 {{cell and array references cannot have regional field capability}}
!bad = !reussir.ref<!reussir.array<4 x i64> field>

// -----

func.func @cell_slot(%ref: !reussir.ref<!reussir.cell<i64>>) {
  // expected-error @+1 {{a cell slot projection must have unspecified capability}}
  %slot = reussir.ref.project(%ref : !reussir.ref<!reussir.cell<i64>>) [0] : !reussir.ref<i64 field>
  return
}

// -----

func.func @array_slot(%view: memref<4xi64>, %index: index) {
  // expected-error @+1 {{projected reference capability must be unspecified}}
  %slot = reussir.array.project(%view : memref<4xi64>)[%index : index] : !reussir.ref<i64 field>
  return
}

// -----

func.func @bridge(%view: memref<i64>) {
  // expected-error @+1 {{a memref cannot produce a regional field reference}}
  %slot = reussir.ref.from_memref(%view : memref<i64>) : !reussir.ref<i64 field>
  return
}

// -----

func.func @token(%token: !reussir.token<align: 4, size: 8>) {
  // expected-error @+1 {{token storage cannot produce a regional field reference}}
  %slot = reussir.token.reinterpret(%token : !reussir.token<align: 4, size: 8>) : !reussir.ref<i64 field>
  return
}

// -----

// expected-error @+1 {{field members require a regional record}}
!bad = !reussir.record<compound "Value" [value] {[field] i64}>

// -----

// expected-error @+1 {{cell and array members cannot be regional field links}}
!bad = !reussir.record<compound "Region" [regional] {[field] !reussir.cell<i64>}>

// -----

func.func @ordinary_store(%slot: !reussir.ref<i64>, %value: i64) {
  // expected-error @+1 {{target reference must have field capability, got: unspecified}}
  reussir.ref.store(%slot : !reussir.ref<i64>)(%value : i64)
  return
}

// -----

func.func @frozen_store(%slot: !reussir.ref<i64 rigid>, %value: i64) {
  // expected-error @+1 {{target reference must have field capability, got: rigid}}
  reussir.ref.store(%slot : !reussir.ref<i64 rigid>)(%value : i64)
  return
}
