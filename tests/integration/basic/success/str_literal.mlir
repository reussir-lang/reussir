// RUN: %reussir-opt %s | %reussir-opt

module @test {
  reussir.str.global @hello = "Hello, World!"
  reussir.str.global @empty = ""

  func.func @test_str_literal() -> !reussir.str<global> {
    %str = reussir.str.literal @hello : !reussir.str<global>
    return %str : !reussir.str<global>
  }

  func.func @test_empty_str_literal() -> !reussir.str<global> {
    %str = reussir.str.literal @empty : !reussir.str<global>
    return %str : !reussir.str<global>
  }
}

