// RUN: %reussir-opt %s \
// RUN:   --reussir-lowering-scf-ops \
// RUN:   --convert-scf-to-cf \
// RUN:   --reussir-lowering-basic-ops | \
// RUN:   %reussir-translate --reussir-to-llvmir | \
// RUN:   %opt -S -O3 | \
// RUN:   %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %S/str_select_e2e.c %t.o -o %t.exe -L%library_path \
// RUN:    %rpath_flag %extra_sys_libs
// RUN: %t.exe

module {
  // Function to test basic pattern matching: matches "foo" (index 0), "bar" (index 1), or none
  func.func @test_basic(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["foo", "bar"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test pattern with common prefix: "abc" (0), "abd" (1), "xyz" (2)
  func.func @test_prefix(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["abc", "abd", "xyz"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test long pattern
  func.func @test_long(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["extremely_long_unique_pattern_that_is_over_32_chars"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }

  // Function to test many patterns with various first bytes
  func.func @test_many(%str: !reussir.str<local>) -> (index, i1) {
    %idx, %found = reussir.str.select (%str) ["apple", "banana", "cherry", "date", "elderberry"] : (!reussir.str<local>) -> (index, i1)
    return %idx, %found : index, i1
  }
}
