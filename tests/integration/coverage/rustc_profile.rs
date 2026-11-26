// REQUIRES: profile-runtime
// RUN: %rustc_path -C instrument-coverage %s -o %t.exe
// RUN: env LLVM_PROFILE_FILE=%t.profraw %t.exe
// RUN: %llvm-profdata merge -sparse %t.profraw -o %t.profdata
// RUN: %llvm-cov export -format=lcov %t.exe -instr-profile=%t.profdata | %FileCheck %s

// This test does not use Reussir. It makes sure that %llvm-profdata and
// %llvm-cov read the profile that the runtime of %rustc_path writes. rrc links
// the same runtime. The program runs with no arguments, so `pick` does not
// execute its first return.

// CHECK: FNDA:1,{{.*}}pick
fn pick(x: usize) -> i32 {
    if x > 1 {
        return 1; // CHECK: DA:[[@LINE]],0
    }
    0 // CHECK: DA:[[@LINE]],1
}

fn main() {
    std::process::exit(pick(std::env::args().count()));
}
