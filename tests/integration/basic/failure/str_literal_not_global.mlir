// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  reussir.str.global @hello = "Hello, World!"

  func.func @test_str_literal_not_global() -> !reussir.str<local> {
    // expected-error @+1 {{literal type must have global lifescope, got: local}}
    %str = reussir.str.literal @hello : !reussir.str<local>
    return %str : !reussir.str<local>
  }
}

