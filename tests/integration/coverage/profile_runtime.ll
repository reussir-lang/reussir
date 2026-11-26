; REQUIRES: profile-runtime
; `rrc --instrument-coverage` links the profile runtime of the Rust toolchain
; into executables and fuzzer executables. The runtime writes the counters to
; a `.profraw` file at exit.

; RUN: %rrc %s -o %t.exe --emit executable --instrument-coverage %rrc_linker
; RUN: env LLVM_PROFILE_FILE=%t.profraw %t.exe
; RUN: %llvm-profdata show --function=__reussir_main --counts %t.profraw | %FileCheck %s --check-prefix=ONCE
; RUN: %llvm-profdata show --function=reussir_fuzz_init --counts %t.profraw | %FileCheck %s --check-prefix=NEVER

; The fuzzer stops in its initialization function, before libFuzzer starts.
; RUN: %rrc %s -o %t.fuzz.exe --emit executable --fuzzer --instrument-coverage %rrc_linker
; RUN: env LLVM_PROFILE_FILE=%t.fuzz.profraw %t.fuzz.exe
; RUN: %llvm-profdata show --function=reussir_fuzz_init --counts %t.fuzz.profraw | %FileCheck %s --check-prefix=ONCE

; ONCE: Function count: 1
; NEVER: Function count: 0

@__profn___reussir_main = private constant [14 x i8] c"__reussir_main"
@__profn_reussir_fuzz_init = private constant [17 x i8] c"reussir_fuzz_init"

declare void @exit(i32)

define void @__reussir_main() {
  call void @llvm.instrprof.increment(ptr @__profn___reussir_main, i64 1, i32 1, i32 0)
  ret void
}

define void @reussir_fuzz_init() {
  call void @llvm.instrprof.increment(ptr @__profn_reussir_fuzz_init, i64 1, i32 1, i32 0)
  call void @exit(i32 0)
  unreachable
}
