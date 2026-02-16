// RUN: %reussir-opt %s -verify-diagnostics

!incomplete = !reussir.record<variant "X" incomplete>
module {
  func.func @test(%ref : !reussir.ref<!incomplete>) {
    // expected-error @+1 {{'reussir.ref.acquire' op cannot acquire incomplete record type}}
    reussir.ref.acquire (%ref : !reussir.ref<!incomplete>)
    return
  }
}
