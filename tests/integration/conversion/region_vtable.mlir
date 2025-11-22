// RUN: %reussir-opt \
// RUN:   -reussir-lowering-region-patterns \
// RUN:   -reussir-drop-expansion \
// RUN:   -reussir-lowering-basic-ops \
// RUN:   -o %t.mlir %s
// RUN: %FileCheck %s --check-prefix=CHECK-MLIR < %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | \
// RUN:   %FileCheck %s --check-prefix=CHECK-LLVM
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | \
// RUN:   %opt -S -O3 | \
// RUN:   %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %S/region_vtable.c %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:    -Wl,-rpath,%library_path %extra_sys_libs
// RUN: %t.exe
!node = !reussir.record<compound "Node" { [field] !reussir.record<compound "Node">, i64, [field] !reussir.record<compound "Node"> }>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @test() {
    %res = reussir.region.run -> !reussir.rc<!node rigid> {
      ^bb0(%reg: !reussir.region):
      %token = reussir.token.alloc : !reussir.token<align: 8, size: 48>
      %c42 = arith.constant 42 : i64
      %null = reussir.nullable.create : !reussir.nullable<!reussir.rc<!node flex>>
      %rec = reussir.record.compound (
        %null, %c42, %null : 
          !reussir.nullable<!reussir.rc<!node flex>>, i64, !reussir.nullable<!reussir.rc<!node flex>>
      ) : !node
      %rc = reussir.rc.create 
        value(%rec : !node) 
        token(%token : !reussir.token<align: 8, size: 48>)
        region(%reg : !reussir.region) : !reussir.rc<!node flex>
      reussir.region.yield %rc : !reussir.rc<!node flex>
    }
    reussir.rc.dec (%res : !reussir.rc<!node rigid>)
    return
  }
}

// CHECK-MLIR-DAG: llvm.func @__reussir_allocate
// CHECK-MLIR-DAG: llvm.func @__reussir_freeze_flex_object
// CHECK-MLIR-DAG: llvm.func @__reussir_cleanup_region
// CHECK-MLIR-DAG: llvm.func @__reussir_release_rigid_object
// CHECK-MLIR-DAG: llvm.call @__reussir_allocate
// CHECK-MLIR-DAG: llvm.call @__reussir_freeze_flex_object
// CHECK-MLIR-DAG: llvm.call @__reussir_cleanup_region
// CHECK-MLIR-DAG: llvm.call @__reussir_release_rigid_object

// CHECK-LLVM-DAG: declare ptr @__reussir_allocate
// CHECK-LLVM-DAG: declare ptr @__reussir_freeze_flex_object
// CHECK-LLVM-DAG: declare void @__reussir_cleanup_region
// CHECK-LLVM-DAG: declare void @__reussir_release_rigid_object
// CHECK-LLVM-DAG: call ptr @__reussir_allocate
// CHECK-LLVM-DAG: call ptr @__reussir_freeze_flex_object
// CHECK-LLVM-DAG: call void @__reussir_cleanup_region
// CHECK-LLVM-DAG: call void @__reussir_release_rigid_object
