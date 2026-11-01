// RUN: %reussir-translate --reussir-to-llvmir %s | %FileCheck %s
module attributes {llvm.target_triple = "x86_64-pc-windows-msvc", reussir.drop_order = "backward", reussir.ffi_symbols = [@owned]} {
  llvm.comdat @groups {
    llvm.comdat_selector @owned any
  }
  llvm.func linkonce_odr @owned() comdat(@groups::@owned) {
    llvm.return
  }
  // CHECK: $owned.backward = comdat any
  // CHECK: define linkonce_odr void @owned.backward() comdat
}
