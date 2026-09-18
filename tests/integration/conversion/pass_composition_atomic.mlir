// RUN: %reussir-opt %s --cse --canonicalize | %FileCheck %s --check-prefix=OPS
// RUN: %reussir-opt %s --cse --canonicalize --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

// Even unused acquire probes synchronize. Both CSE and DCE must preserve
// them before conversion; ordinary count reads remain optimizable.
!atomic = !reussir.rc<i32 shared atomic>
!plain = !reussir.rc<i32>
module {
  // OPS-LABEL: func.func @atomic_probes
  // OPS-COUNT-2: reussir.rc.fetch
  // OPS-COUNT-2: reussir.rc.is_unique
  // LLVM-LABEL: define void @atomic_probes
  // LLVM-COUNT-4: load atomic i32, ptr %{{[0-9]+}} acquire, align 4
  // LLVM: ret void
  func.func @atomic_probes(%rc: !atomic) {
    %a = reussir.rc.fetch (%rc : !atomic) : index
    %b = reussir.rc.fetch (%rc : !atomic) : index
    %c = reussir.rc.is_unique (%rc : !atomic) : i1
    %d = reussir.rc.is_unique (%rc : !atomic) : i1
    return
  }

  // OPS-LABEL: func.func @plain_unused
  // OPS-NOT: reussir.rc.
  // OPS: return
  // LLVM-LABEL: define void @plain_unused
  // LLVM-NOT: load
  // LLVM: ret void
  func.func @plain_unused(%rc: !plain) {
    %a = reussir.rc.fetch (%rc : !plain) : index
    %b = reussir.rc.is_unique (%rc : !plain) : i1
    return
  }

  // OPS-LABEL: func.func @plain_cse
  // OPS: %[[COUNT:.*]] = reussir.rc.fetch
  // OPS-NOT: reussir.rc.fetch
  // OPS: return %[[COUNT]], %[[COUNT]]
  // LLVM-LABEL: define { i64, i64 } @plain_cse
  // LLVM: load i32, ptr %0
  // LLVM-NOT: load
  // LLVM: ret
  func.func @plain_cse(%rc: !plain) -> (index, index) {
    %a = reussir.rc.fetch (%rc : !plain) : index
    %b = reussir.rc.fetch (%rc : !plain) : index
    return %a, %b : index, index
  }

  // LLVM-LABEL: define i1 @plain_unique
  // LLVM: load i32, ptr %{{[0-9]+}}
  // LLVM-NOT: load atomic
  // LLVM: icmp eq i32
  func.func @plain_unique(%rc: !plain) -> i1 {
    %a = reussir.rc.is_unique (%rc : !plain) : i1
    return %a : i1
  }
}
