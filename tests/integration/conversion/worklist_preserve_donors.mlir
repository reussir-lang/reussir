// RUN: %reussir-opt %s --reussir-rc-decrement-expansion='defer-normal=false' --verify-each | %FileCheck %s --check-prefix=EARLY
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-rc-decrement-expansion{defer-normal=false},reussir-acquire-drop-expansion{outline-record=true expand-decrement=true})' --verify-each | %FileCheck %s --check-prefix=FINAL

!node_ = !reussir.record<compound "Node" incomplete>
!node = !reussir.record<compound "Node" { i64, !reussir.nullable<!reussir.rc<!node_ shared>> }>
!rc = !reussir.rc<!node shared>
!token = !reussir.nullable<!reussir.token<align: 8, size: 24>>

module {
  // EARLY-LABEL: func.func @ordinary
  // EARLY: scf.if
  // EARLY: reussir.rc.reinterpret
  // EARLY: reussir.token.free
  // FINAL-LABEL: func.func @ordinary
  // FINAL: reussir.drop.run
  // FINAL: reussir.rc.reinterpret
  // FINAL: reussir.token.free
  // FINAL-NOT: reussir.rc.dec
  func.func @ordinary(%rc: !rc) {
    %token = reussir.rc.dec(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }

  // A used donor keeps its real token and synchronous payload release.
  // EARLY-LABEL: func.func @donor
  // EARLY: scf.if
  // EARLY: reussir.rc.reinterpret
  // FINAL-LABEL: func.func @donor
  // FINAL: reussir.drop.run
  // FINAL: reussir.rc.reinterpret
  func.func @donor(%rc: !rc) -> !token {
    %token = reussir.rc.dec(%rc : !rc) : !token
    return %token : !token
  }
}
