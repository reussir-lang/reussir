#![no_main]

extern crate reussir_fuzzer;
extern crate reussir_rt;

reussir_fuzzer::fuzz_target!();

