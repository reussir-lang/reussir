// RUN: %reussir-opt %s -verify-diagnostics

// Define a compound record type (not variant)
!compound_record = !reussir.record<compound "Test" { [value] i32, [value] i64 }>

module {
  func.func @test_variant_not_variant_record(%ref : !reussir.ref<!compound_record>) {
    // expected-error @+1 {{'reussir.ref.drop' op when variant is specified, reference element type must be a variant record type}}
    reussir.ref.drop(%ref : !reussir.ref<!compound_record>) variant[0]
    return
  }
}
