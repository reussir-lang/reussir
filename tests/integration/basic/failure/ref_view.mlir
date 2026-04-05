// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  func.func @wrong_to_memref_rank(%ref : !reussir.ref<i64>) {
    // expected-error @+1 {{view result must be a zero-rank memref}}
    %0 = reussir.ref.to_memref (%ref : !reussir.ref<i64>) : memref<1xi64>
    func.return
  }

  func.func @wrong_to_memref_element(%ref : !reussir.ref<i64>) {
    // expected-error @+1 {{view result element type mismatch: expected 'i64', got 'f64'}}
    %0 = reussir.ref.to_memref (%ref : !reussir.ref<i64>) : memref<f64>
    func.return
  }

  func.func @wrong_from_memref_rank(%view : memref<1xi64>) {
    // expected-error @+1 {{view input must be a zero-rank memref}}
    %0 = reussir.ref.from_memref (%view : memref<1xi64>) : !reussir.ref<i64>
    func.return
  }

  func.func @wrong_from_memref_element(%view : memref<f64>) {
    // expected-error @+1 {{view input element type mismatch: expected 'i64', got 'f64'}}
    %0 = reussir.ref.from_memref (%view : memref<f64>) : !reussir.ref<i64>
    func.return
  }
}
