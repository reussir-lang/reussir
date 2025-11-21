// RUN: %reussir-opt %s -verify-diagnostics

// Test that substitutions cannot be empty

module @test {
  func.func @polyffi_empty_substitutions() {
    // expected-error @+1 {{substitutions cannot be empty}}
    reussir.polyffi texture("module content") substitutions({})
    func.return
  }
}

