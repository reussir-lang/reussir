// RUN: %reussir-opt %s -verify-diagnostics

// Define a variant record type with 2 variants
!variant_record = !reussir.record<variant "Test" { [value] i32, [value] i64 }>

module {
  func.func @test_variant_out_of_bounds(%ref : !reussir.ref<!variant_record>) {
    // expected-error @+1 {{'reussir.ref.drop' op variant index out of bounds: 2 >= 2}}
    reussir.ref.drop(%ref : !reussir.ref<!variant_record>) variant[2]
    return
  }
}




