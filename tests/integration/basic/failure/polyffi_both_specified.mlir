// RUN: %reussir-opt %s -verify-diagnostics

// Test that both moduleTexture and compiledModule cannot be specified

module @test {
  func.func @polyffi_both_specified() {
    // expected-error @+1 {{cannot specify both moduleTexture and compiledModule}}
    reussir.polyffi texture("module content") {compiledModule = dense_resource<blob1> : tensor<24xi8>}
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
