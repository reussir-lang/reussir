//! Bounds of the SanitizerCoverage sections on COFF.
//!
//! ELF and Mach-O linkers synthesize the start and stop symbols of the
//! counter, guard, and PC-table sections. COFF linkers do not: the runtime
//! defines them in grouped sections that sort around the instrumented data
//! (`$A` < `$M` < `$Z`), as compiler-rt's `sanitizer_coverage_win_sections.cpp`
//! does. The vendored libFuzzer omits that file, so the fuzzer bridge supplies
//! the same definitions. Each start symbol is 8 bytes so the data after it
//! needs no padding; each stop symbol is 1 byte so the linker does not align
//! its section.
#![allow(non_upper_case_globals)]

#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOV$CA")]
static mut __start___sancov_cntrs: u64 = 0;
#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOV$CZ")]
static mut __stop___sancov_cntrs: u8 = 0;

#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOV$GA")]
static mut __start___sancov_guards: u64 = 0;
#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOV$GZ")]
static mut __stop___sancov_guards: u8 = 0;

#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOVP$A")]
static __start___sancov_pcs: u64 = 0;
#[unsafe(no_mangle)]
#[unsafe(link_section = ".SCOVP$Z")]
static __stop___sancov_pcs: u8 = 0;
