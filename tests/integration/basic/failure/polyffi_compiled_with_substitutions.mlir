// RUN: %reussir-opt %s -verify-diagnostics

// Test that substitutions cannot be specified when compiledModule is used

module @test {
  func.func @polyffi_compiled_with_substitutions() {
    // expected-error @+1 {{substitutions cannot be specified when compiledModule is used}}
    reussir.polyffi substitutions({key = i64}) {compiledModule = dense_resource<blob1> : tensor<24xi8>}
    func.return
  }
}

{-#
dialect_resources: {
    builtin: {
      blob1: "0x08000000010000000000000002000000000000000300000000000000"
    }
  }
#-}
