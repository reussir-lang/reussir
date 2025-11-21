// RUN: %reussir-opt %s | %reussir-opt

module @test {
  // Test with moduleTexture and substitutions
  func.func @polyffi_texture_with_substitutions() {
    reussir.polyffi texture("module content") substitutions({key = i64})
    func.return
  }

  // Test with moduleTexture only
  func.func @polyffi_texture_only() {
    reussir.polyffi texture("module content")
    func.return
  }

  // Test with compiledModule only
  func.func @polyffi_compiled_only() {
    reussir.polyffi compiled(dense_resource<blob1> : tensor<24xi8>)
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
