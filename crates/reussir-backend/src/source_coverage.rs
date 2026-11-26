//! Source-based code coverage for the AOT compiler (`rrc`), in the format of
//! clang `-fprofile-instr-generate -fcoverage-mapping` and rustc
//! `-C instrument-coverage`.

use llvm_sys::prelude::LLVMModuleRef;
use reussir_backend_sys as sys;

/// Runs LLVM's `InstrProfilingLoweringPass` over `module`: it changes the
/// counter increments into counter updates and adds the profile data that the
/// profile runtime writes. Call after [`crate::llvm::run_backend_llvm_pipeline`].
///
/// # Safety
/// `module` must be a valid `LLVMModuleRef`.
pub unsafe fn run_instr_profiling_lowering_pass(module: LLVMModuleRef) {
    unsafe {
        sys::reussirRunInstrProfilingLoweringPass(module as sys::LLVMModuleRef);
    }
}
