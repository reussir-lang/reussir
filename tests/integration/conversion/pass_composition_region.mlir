// RUN: %reussir-opt %s --sccp | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,sccp,reussir-lowering-region-patterns,convert-scf-to-cf,convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

// SCCP must see the return edge and the yielded value. Without them, the
// true arm contributes no value to the join, which incorrectly folds to 1.
module {
  func.func @branch(%flag: i1) -> i32 {
    %one = arith.constant 1 : i32
    %two = arith.constant 2 : i32
    %r = scf.if %flag -> i32 {
      %inner = reussir.region.run -> i32 {
      ^bb0(%region: !reussir.region):
        reussir.region.yield %two : i32
      }
      scf.yield %inner : i32
    } else {
      scf.yield %one : i32
    }
    return %r : i32
  }

  func.func @main() -> i32 {
    %true = arith.constant true
    %two = arith.constant 2 : i32
    %r = call @branch(%true) : (i1) -> i32
    %ok = arith.cmpi eq, %r, %two : i32
    cf.assert %ok, "SCCP lost region.run return edge"
    %zero = arith.constant 0 : i32
    return %zero : i32
  }

  // CHECK-LABEL: func.func @forward_scalar
  // CHECK: %[[C:.*]] = arith.constant 42 : i32
  // CHECK: return %[[C]] : i32
  func.func @forward_scalar() -> i32 {
    %r = reussir.region.run -> i32 {
    ^bb0(%region: !reussir.region):
      %c = arith.constant 42 : i32
      reussir.region.yield %c : i32
    }
    return %r : i32
  }
}
