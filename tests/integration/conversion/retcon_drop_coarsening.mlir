// RUN: %reussir-opt %s --reussir-convert-to-llvm --canonicalize --verify-each -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.mlir -o %t.ll
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,default<O2>,verify' -o /dev/null

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  llvm.func @before(!llvm.ptr)
  llvm.func @after(!llvm.ptr)

  // The root keeps its original body and bounded lazy frame. Its child task
  // targets the worker, not another root wrapper.
  // CHECK-LABEL: llvm.func private @drop.start(
  // CHECK: llvm.mlir.addressof @drop.chunk.routine
  // CHECK: llvm.call @before
  // CHECK: llvm.call @llvm.coro.suspend.retcon.i1
  // CHECK: llvm.call @after
  // CHECK-NOT: llvm.call @before
  // CHECK: llvm.return
  llvm.func linkonce_odr @drop(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    llvm.call @before(%p) : (!llvm.ptr) -> ()
    %child = llvm.load %p : !llvm.ptr -> !llvm.ptr
    %null = llvm.mlir.zero : !llvm.ptr
    %has = llvm.icmp "ne" %child, %null : !llvm.ptr
    llvm.cond_br %has, ^child, ^done
  ^child:
    reussir.dtor.suspend @drop(%child : !llvm.ptr)
    llvm.br ^done
  ^done:
    llvm.call @after(%p) : (!llvm.ptr) -> ()
    llvm.return
  }

  // A worker enters two source levels, but its remaining recursive edge is a
  // suspension. Neither the root nor the worker uses native recursive calls.
  // CHECK-LABEL: llvm.func private @drop.chunk.start(
  // CHECK: llvm.mlir.addressof @drop.chunk.routine
  // CHECK: llvm.call @before
  // CHECK: llvm.call @before
  // CHECK: llvm.call @llvm.coro.suspend.retcon.i1
  // CHECK: llvm.call @after
  // CHECK: llvm.call @after
  // CHECK-NOT: llvm.call @before
  // CHECK-NOT: llvm.call @drop.chunk.start
  // CHECK: llvm.return


  // Freeze both bodies before expanding mutual recursion. Each worker contains
  // one A/B pair and hands control back to the executor at the next pair.
  llvm.func linkonce_odr @left(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    llvm.call @before(%p) : (!llvm.ptr) -> ()
    reussir.dtor.suspend @right(%p : !llvm.ptr)
    llvm.return
  }
  llvm.func linkonce_odr @right(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    llvm.call @after(%p) : (!llvm.ptr) -> ()
    reussir.dtor.suspend @left(%p : !llvm.ptr)
    llvm.return
  }
  // CHECK-LABEL: llvm.func private @left.chunk.start(
  // CHECK: llvm.mlir.addressof @left.chunk.routine
  // CHECK: llvm.call @before
  // CHECK: llvm.call @after
  // CHECK: llvm.call @llvm.coro.suspend.retcon.i1
  // CHECK-NOT: llvm.call @before
  // CHECK-NOT: llvm.call @after
  // CHECK: llvm.return
  // CHECK-LABEL: llvm.func private @right.chunk.start(
  // CHECK: llvm.mlir.addressof @right.chunk.routine
  // CHECK: llvm.call @after
  // CHECK: llvm.call @before
  // CHECK: llvm.call @llvm.coro.suspend.retcon.i1
  // CHECK-NOT: llvm.call @before
  // CHECK-NOT: llvm.call @after
  // CHECK: llvm.return
}
