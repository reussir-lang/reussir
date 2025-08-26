// RUN: %reussir-opt %s | %reussir-opt
!complex_variant = !reussir.record<variant "Complex" { 
  !reussir.record<compound "A" { i32 }>, 
  !reussir.record<compound "B" { i64 }> 
}>

module {
  func.func @test_variant_drop_valid(%ref : !reussir.ref<!complex_variant>) {
    reussir.ref.drop(%ref : !reussir.ref<!complex_variant>) variant[0]
    return
  }
}
