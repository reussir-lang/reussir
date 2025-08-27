// RUN: %reussir-opt %s -verify-diagnostics

// Define an incomplete variant record type
!incomplete_variant = !reussir.record<variant "Incomplete" incomplete>

module {
  func.func @test_variant_incomplete_record(%ref : !reussir.ref<!incomplete_variant>) {
    // expected-error @+1 {{'reussir.ref.drop' op cannot drop incomplete variant record}}
    reussir.ref.drop(%ref : !reussir.ref<!incomplete_variant>) variant[0]
    return
  }
}




