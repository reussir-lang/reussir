// RUN: %reussir-opt %s | %reussir-opt

module @test {
  // Test with moduleTexture and substitutions
  func.func @polyffi_texture_with_substitutions() {
    reussir.polyffi texture("module content") substitutions({key = i64})
    func.return
  }

  // Test with moduleTexture only
  func.func @polyffi_texture_only() {
    reussir.polyffi texture("module content") substitutions({})
    func.return
  }
}


