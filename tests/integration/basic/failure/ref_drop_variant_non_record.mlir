// RUN: %reussir-opt %s -verify-diagnostics

module {
  func.func @test_variant_non_record(%ref : !reussir.ref<i32>) {
    // expected-error @+1 {{'reussir.ref.drop' op when variant is specified, reference element type must be a record type, got: 'i32'}}
    reussir.ref.drop(%ref : !reussir.ref<i32>) variant[0]
    return
  }
}
