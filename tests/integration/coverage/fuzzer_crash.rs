// UNSUPPORTED: windows
// REQUIRES: profile-runtime, reussir-fuzzer
// RUN: %rustc_path --edition 2024 -C instrument-coverage %s -o %t.exe %reussir_fuzzer_externs
// RUN: rm -rf %t.fail %t.abort && mkdir %t.fail %t.abort
// RUN: %python -c "open(r'%t.fail/input', 'wb').write(bytes(8) + b'F')"
// RUN: %python -c "open(r'%t.abort/input', 'wb').write(bytes(8) + b'A')"
// RUN: %not env LLVM_PROFILE_FILE=%t.fail.profraw %t.exe %t.fail -runs=0 -artifact_prefix=%t. 2>&1 | %FileCheck %s --check-prefix=CRASH
// RUN: %llvm-profdata merge -sparse %t.fail.profraw -o %t.fail.profdata
// RUN: %llvm-cov export -format=lcov %t.exe -instr-profile=%t.fail.profdata | %FileCheck %s --check-prefix=FAIL
// RUN: %not env LLVM_PROFILE_FILE=%t.abort.profraw %t.exe %t.abort -runs=0 -artifact_prefix=%t. 2>&1 | %FileCheck %s --check-prefix=CRASH
// RUN: %llvm-profdata merge -sparse %t.abort.profraw -o %t.abort.profdata
// RUN: %llvm-cov export -format=lcov %t.exe -instr-profile=%t.abort.profdata | %FileCheck %s --check-prefix=ABORT

// libFuzzer stops on a crash with `_Exit`, which does not run the exit handler
// of the profile runtime. Like the fuzzer driver of `rrc --instrument-coverage`,
// this driver gives `__llvm_profile_write_file` to the harness. The harness
// writes the profile before libFuzzer stops. A failing case panics in the
// harness. A Reussir panic aborts. Each replay crashes on its only input, and
// its profile counts that input.

// CRASH: SUMMARY: libFuzzer: deadly signal

#![no_main]
extern crate reussir_fuzzer;

use reussir_fuzzer::spec::{CaseSpec, IntWidth, SuiteSpec, ValueSpec};
use std::ffi::c_void;

unsafe extern "C" fn clone_tree(tree: *mut c_void) -> *mut c_void {
    tree
}

unsafe extern "C" fn drop_tree(_tree: *mut c_void) {}

// The byte after the case index selects how the case ends.
unsafe extern "C" fn run(_tree: *mut c_void, data: *const u8, len: usize) -> i32 {
    let bytes = unsafe { std::slice::from_raw_parts(data, len) };
    match bytes.get(8) {
        Some(b'F') => 1,
        // FAIL: DA:[[@LINE-1]],1
        // ABORT: DA:[[@LINE-2]],0
        Some(b'A') => std::process::abort(),
        // FAIL: DA:[[@LINE-1]],0
        // ABORT: DA:[[@LINE-2]],1
        _ => 0,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn reussir_fuzz_init() {
    let byte = ValueSpec::UInt {
        width: IntWidth::I8,
        min: 0,
        max: 255,
    };
    let spec = SuiteSpec {
        cases: vec![CaseSpec {
            path: vec!["crash".into()],
            input: ValueSpec::Array {
                elem: Box::new(byte),
                min_len: 0,
                max_len: 8,
            },
        }],
    };
    let spec = spec.to_bytes();
    unsafe {
        reussir_fuzzer::__reussir_fuzzer_register(
            std::ptr::dangling_mut(),
            clone_tree,
            drop_tree,
            run,
            spec.as_ptr(),
            spec.len(),
        )
    };
}

unsafe extern "C" {
    fn __llvm_profile_write_file() -> std::ffi::c_int;
}

reussir_fuzzer::fuzz_target!(reussir_fuzz_init, write_profile = __llvm_profile_write_file);
