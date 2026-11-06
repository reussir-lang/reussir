// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=backward' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s

!node_ = !reussir.record<compound "PreloadNode" incomplete>
!link = !reussir.nullable<!reussir.rc<!node_ shared>>
!node = !reussir.record<compound "PreloadNode" { !link, !link }>
!rc = !reussir.rc<!node shared>
!token = !reussir.nullable<!reussir.token<align: 8, size: 24>>

module {
  func.func @drop(%rc: !rc) {
    %token = reussir.rc.dec(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }
}

// Sibling pointers load before either RC is read. The children may alias:
// the second RC read must observe any decrement of the first child.
// CHECK-LABEL: func.func private @{{.*}}REUSSIR_DROP_release_
// CHECK: %[[LEFT:[a-zA-Z0-9_]+]] = reussir.ref.load
// CHECK-NEXT: %{{.*}} = reussir.ref.project{{.*}}[1]
// CHECK-NEXT: %[[RIGHT:[a-zA-Z0-9_]+]] = reussir.ref.load
// CHECK-NEXT: %{{.*}} = reussir.nullable.check(%[[LEFT]] :
// CHECK: %[[LEFT_RC:[a-zA-Z0-9_]+]] = reussir.nullable.coerce(%[[LEFT]] :
// CHECK-NEXT: %{{.*}} = reussir.rc.fetch(%[[LEFT_RC]] :
// CHECK: reussir.rc.set(%[[LEFT_RC]] :
// CHECK: reussir.nullable.check(%[[RIGHT]] :
// CHECK: %[[RIGHT_RC:[a-zA-Z0-9_]+]] = reussir.nullable.coerce(%[[RIGHT]] :
// CHECK-NEXT: %{{.*}} = reussir.rc.fetch(%[[RIGHT_RC]] :
// CHECK: reussir.rc.set(%[[RIGHT_RC]] :
// CHECK: reussir.token.free
