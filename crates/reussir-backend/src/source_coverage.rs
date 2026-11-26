//! Source-based code coverage for the AOT compiler (`rrc`), in the format of
//! clang `-fprofile-instr-generate -fcoverage-mapping` and rustc
//! `-C instrument-coverage`.

use llvm_sys::prelude::LLVMModuleRef;
use melior::Context;
use melior::ir::Module;
use melior::pass::{Pass, PassManager};
use reussir_backend_sys as sys;

/// Runs `reussir-instrument-coverage` and then `reussir-emit-coverage-mapping`
/// over `module`. They add the counter increments and the coverage mapping.
/// Call before [`crate::pipeline::run_lowering_pipeline`], so that the passes
/// see the module before inlining and optimization.
pub fn instrument_module(context: &Context, module: &mut Module) -> Result<(), String> {
    let manager = PassManager::new(context);
    // SAFETY: each factory returns a new pass, and the manager owns it.
    unsafe {
        manager.add_pass(Pass::from_raw(sys::reussirCreateInstrumentCoveragePass()));
        manager.add_pass(Pass::from_raw(sys::reussirCreateEmitCoverageMappingPass()));
    }
    manager
        .run(module)
        .map_err(|e| format!("source coverage instrumentation failed: {e}"))
}

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
