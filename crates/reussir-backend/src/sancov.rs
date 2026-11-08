//! SanitizerCoverage and fuzzing instrumentation for the AOT compiler (`rrc`).
//!
//! Normal SanitizerCoverage variants (`trace-pc-guard`, `inline-8bit-counters`,
//! `inline-bool-flag`, `trace-pc`, etc.) configure only the post-optimization
//! `SanitizerCoveragePass`. Fuzzing (`fuzzer` preset or `--emit fuzzer`)
//! additionally sets [`SanCovOptions::opt_for_fuzzing`], which stamps
//! `optforfuzzing` and `no-builtin-*` onto every function definition *before*
//! the optimization pipeline runs so comparison structure and call sites are
//! preserved for value-profile hooks.

use std::ffi::c_int;

use llvm_sys::prelude::LLVMModuleRef;
use melior::ir::{
    Module,
    attribute::{ArrayAttribute, StringAttribute},
    operation::{OperationLike, OperationMutLike},
};
use reussir_backend_sys as sys;

/// SanitizerCoverage granularity, mirroring [`sys::ReussirSanCovType`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum SanCovType {
    #[default]
    None,
    Function,
    BB,
    Edge,
}

impl SanCovType {
    fn to_sys(&self) -> sys::ReussirSanCovType {
        match self {
            SanCovType::None => sys::ReussirSanCovType::None,
            SanCovType::Function => sys::ReussirSanCovType::Function,
            SanCovType::BB => sys::ReussirSanCovType::BB,
            SanCovType::Edge => sys::ReussirSanCovType::Edge,
        }
    }
}

/// SanitizerCoverage and fuzzing instrumentation options.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct SanCovOptions {
    pub coverage_type: SanCovType,
    /// Demangled package, module, or function prefixes to instrument.
    pub include: Vec<String>,
    /// Demangled prefixes to exclude, taking precedence over includes.
    pub exclude: Vec<String>,
    pub opt_for_fuzzing: bool,
    pub indirect_calls: bool,
    pub trace_bb: bool,
    pub trace_cmp: bool,
    pub trace_div: bool,
    pub trace_gep: bool,
    pub use_8bit_counters: bool,
    pub trace_pc: bool,
    pub trace_pc_guard: bool,
    pub inline_8bit_counters: bool,
    pub inline_bool_flag: bool,
    pub pc_table: bool,
    pub no_prune: bool,
    pub stack_depth: bool,
    pub trace_loads: bool,
    pub trace_stores: bool,
    pub collect_control_flow: bool,
}

impl SanCovOptions {
    /// The fuzzing default excludes the test package; explicit includes replace it.
    pub fn excluded_scopes(&self) -> Vec<String> {
        let mut scopes = self.exclude.clone();
        if self.opt_for_fuzzing && self.include.is_empty() {
            scopes.push("test".into());
        }
        scopes
    }

    /// Returns the instrumentation preset required by libFuzzer
    /// (matching clang `-fsanitize=fuzzer-no-link`): pre-optimization
    /// `optforfuzzing` + `no-builtin-*` attributes, plus edge-level
    /// `inline-8bit-counters`, `pc-table`, `trace-cmp`, and `indirect-calls`.
    /// [`Self::for_target`] adds `stack-depth` on Linux.
    pub const fn fuzzer() -> Self {
        Self {
            coverage_type: SanCovType::Edge,
            include: Vec::new(),
            exclude: Vec::new(),
            opt_for_fuzzing: true,
            indirect_calls: true,
            trace_bb: false,
            trace_cmp: true,
            trace_div: false,
            trace_gep: false,
            use_8bit_counters: false,
            trace_pc: false,
            trace_pc_guard: false,
            inline_8bit_counters: true,
            inline_bool_flag: false,
            pc_table: true,
            no_prune: false,
            stack_depth: false,
            trace_loads: false,
            trace_stores: false,
            collect_control_flow: false,
        }
    }

    /// Parses a sequence of `--sancov` flags (each flag may also be
    /// comma-separated, e.g. `edge,trace-pc-guard`). Returns `Ok(None)` when no
    /// flags are supplied.
    pub fn from_flags<I, S>(flags: I) -> Result<Option<Self>, String>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let mut opts = Self::default();
        let mut explicit_granularity: Option<SanCovType> = None;
        let mut saw_any = false;

        for raw in flags {
            for token in raw.as_ref().split(',').map(str::trim) {
                if token.is_empty() {
                    continue;
                }
                saw_any = true;
                let mut set_granularity = |g: SanCovType| -> Result<(), String> {
                    if let Some(prev) = explicit_granularity
                        && prev != g
                    {
                        return Err(format!(
                            "conflicting SanitizerCoverage granularities: {prev:?} vs {g:?}"
                        ));
                    }
                    explicit_granularity = Some(g);
                    opts.coverage_type = g;
                    Ok(())
                };
                match token {
                    "fuzzer" => {
                        opts.opt_for_fuzzing = true;
                        opts.indirect_calls = true;
                        opts.trace_cmp = true;
                        opts.inline_8bit_counters = true;
                        opts.pc_table = true;
                        if opts.coverage_type == SanCovType::None {
                            opts.coverage_type = SanCovType::Edge;
                        }
                    }
                    "func" | "function" => set_granularity(SanCovType::Function)?,
                    "bb" => set_granularity(SanCovType::BB)?,
                    "edge" => set_granularity(SanCovType::Edge)?,
                    "indirect-calls" => opts.indirect_calls = true,
                    "trace-bb" => opts.trace_bb = true,
                    "trace-cmp" => opts.trace_cmp = true,
                    "trace-div" => opts.trace_div = true,
                    "trace-gep" => opts.trace_gep = true,
                    "use-8bit-counters" => opts.use_8bit_counters = true,
                    "trace-pc" => opts.trace_pc = true,
                    "trace-pc-guard" => opts.trace_pc_guard = true,
                    "inline-8bit-counters" => opts.inline_8bit_counters = true,
                    "inline-bool-flag" => opts.inline_bool_flag = true,
                    "pc-table" => opts.pc_table = true,
                    "no-prune" => opts.no_prune = true,
                    "stack-depth" => opts.stack_depth = true,
                    "trace-loads" => opts.trace_loads = true,
                    "trace-stores" => opts.trace_stores = true,
                    "control-flow" => opts.collect_control_flow = true,
                    "opt-for-fuzzing" => opts.opt_for_fuzzing = true,
                    other => {
                        return Err(format!(
                            "unknown `--sancov` option `{other}` (expected `fuzzer`, `func`, `bb`, \
                             `edge`, `trace-pc-guard`, `inline-8bit-counters`, `inline-bool-flag`, \
                             `trace-pc`, `pc-table`, `trace-cmp`, `trace-div`, `trace-gep`, \
                             `indirect-calls`, `stack-depth`, `no-prune`, `trace-loads`, \
                             `trace-stores`, `control-flow`, or `opt-for-fuzzing`)"
                        ));
                    }
                }
            }
        }

        if !saw_any {
            return Ok(None);
        }

        if opts.coverage_type == SanCovType::None && opts.has_coverage_pass_action() {
            opts.coverage_type = SanCovType::Edge;
        }

        if !opts.has_coverage_pass_action() && !opts.opt_for_fuzzing {
            return Err(
                "`--sancov` granularity (`func`, `bb`, `edge`) requires at least one \
                 instrumentation mode (e.g. `trace-pc-guard`, `inline-8bit-counters`, \
                 `inline-bool-flag`, `trace-pc`, or `trace-cmp`)"
                    .into(),
            );
        }

        if opts.pc_table
            && !opts.inline_8bit_counters
            && !opts.inline_bool_flag
            && !opts.trace_pc
            && !opts.trace_pc_guard
        {
            return Err(
                "`--sancov pc-table` requires one of `inline-8bit-counters`, `inline-bool-flag`, \
                 `trace-pc`, or `trace-pc-guard`"
                    .into(),
            );
        }

        Ok(Some(opts))
    }

    /// Merges the libFuzzer requirements into `self` (used when `--emit fuzzer`
    /// is combined with additional `--sancov` flags such as `trace-div` or
    /// `no-prune`).
    pub fn with_fuzzer_defaults(mut self) -> Self {
        let fuzzer = Self::fuzzer();
        if self.coverage_type == SanCovType::None {
            self.coverage_type = fuzzer.coverage_type;
        }
        self.opt_for_fuzzing = true;
        self.indirect_calls |= fuzzer.indirect_calls;
        self.trace_cmp |= fuzzer.trace_cmp;
        self.inline_8bit_counters |= fuzzer.inline_8bit_counters;
        self.pc_table |= fuzzer.pc_table;
        self
    }

    /// Completes the fuzzer preset for `triple`. Like clang, it tracks stack
    /// depth only on Linux: libFuzzer's thread-local `__sancov_lowest_stack`
    /// is not available to instrumented code on other targets.
    pub fn for_target(mut self, triple: &str) -> Self {
        let linux = triple
            .split('-')
            .nth(2)
            .is_some_and(|os| os.starts_with("linux"));
        self.stack_depth |= self.opt_for_fuzzing && linux;
        self
    }

    fn has_coverage_pass_action(&self) -> bool {
        self.indirect_calls
            || self.trace_bb
            || self.trace_cmp
            || self.trace_div
            || self.trace_gep
            || self.use_8bit_counters
            || self.trace_pc
            || self.trace_pc_guard
            || self.inline_8bit_counters
            || self.inline_bool_flag
            || self.pc_table
            || self.stack_depth
            || self.trace_loads
            || self.trace_stores
            || self.collect_control_flow
    }

    fn to_sys(&self) -> sys::ReussirSanCovOptions {
        sys::ReussirSanCovOptions {
            coverage_type: self.coverage_type.to_sys(),
            indirect_calls: c_int::from(self.indirect_calls),
            trace_bb: c_int::from(self.trace_bb),
            trace_cmp: c_int::from(self.trace_cmp),
            trace_div: c_int::from(self.trace_div),
            trace_gep: c_int::from(self.trace_gep),
            use_8bit_counters: c_int::from(self.use_8bit_counters),
            trace_pc: c_int::from(self.trace_pc),
            trace_pc_guard: c_int::from(self.trace_pc_guard),
            inline_8bit_counters: c_int::from(self.inline_8bit_counters),
            inline_bool_flag: c_int::from(self.inline_bool_flag),
            pc_table: c_int::from(self.pc_table),
            no_prune: c_int::from(self.no_prune),
            stack_depth: c_int::from(self.stack_depth),
            trace_loads: c_int::from(self.trace_loads),
            trace_stores: c_int::from(self.trace_stores),
            collect_control_flow: c_int::from(self.collect_control_flow),
        }
    }
}

/// Records the scope in MLIR before inlining and closure lowering.
pub fn prepare_module_scope<'c>(
    context: &'c melior::Context,
    module: &mut Module<'c>,
    options: &SanCovOptions,
) {
    if options.include.is_empty()
        && options.exclude.is_empty()
        && module
            .as_operation()
            .attribute("reussir.coverage.include")
            .is_ok()
    {
        unsafe {
            sys::reussirPrepareCoverage(module.to_raw());
        }
        return;
    }
    let excluded = options.excluded_scopes();
    if options.include.is_empty() && excluded.is_empty() {
        return;
    }
    for (name, scopes) in [
        ("reussir.coverage.include", &options.include),
        ("reussir.coverage.exclude", &excluded),
    ] {
        let strings: Vec<_> = scopes
            .iter()
            .map(|s| StringAttribute::new(context, s).into())
            .collect();
        module
            .as_operation_mut()
            .set_attribute(name, ArrayAttribute::new(context, &strings).into());
    }
    unsafe {
        sys::reussirPrepareCoverage(module.to_raw());
    }
}

/// Applies scope to linked LLVM definitions and preserves cross-scope calls.
///
/// # Safety
/// `module` must be a valid LLVM module.
pub unsafe fn prepare_llvm_scope(module: LLVMModuleRef, options: &SanCovOptions) {
    let excluded = options.excluded_scopes();
    let refs = |scopes: &[String]| {
        scopes
            .iter()
            .map(|s| sys::mlir_sys::MlirStringRef {
                data: s.as_ptr().cast(),
                length: s.len(),
            })
            .collect::<Vec<_>>()
    };
    let includes = refs(&options.include);
    let excludes = refs(&excluded);
    unsafe {
        sys::reussirPrepareCoverageLLVM(
            module.cast(),
            includes.as_ptr(),
            includes.len(),
            excludes.as_ptr(),
            excludes.len(),
        );
    }
}

/// Stamps `optforfuzzing` and `no-builtin-*` string/memory comparison
/// exclusions onto every function definition in `module`. Call before
/// [`crate::llvm::run_backend_llvm_pipeline`] when
/// [`SanCovOptions::opt_for_fuzzing`] is set.
///
/// # Safety
/// `module` must be a valid `LLVMModuleRef`.
pub unsafe fn prepare_module_for_fuzzing(module: LLVMModuleRef) {
    unsafe {
        sys::reussirPrepareModuleForFuzzing(module as sys::LLVMModuleRef);
    }
}

/// Runs LLVM's `SanitizerCoveragePass` over `module` with `options`. Call after
/// [`crate::llvm::run_backend_llvm_pipeline`].
///
/// # Safety
/// `module` must be a valid `LLVMModuleRef`, and `machine` must be a valid
/// `LLVMTargetMachineRef` or null.
pub unsafe fn run_sanitizer_coverage_pass(
    module: LLVMModuleRef,
    machine: llvm_sys::target_machine::LLVMTargetMachineRef,
    options: &SanCovOptions,
) {
    let sys_opts = options.to_sys();
    unsafe {
        sys::reussirRunSanitizerCoveragePass(
            module as sys::LLVMModuleRef,
            machine as sys::LLVMTargetMachineRef,
            &raw const sys_opts,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_normal_sancov_without_opt_for_fuzzing() {
        let opts = SanCovOptions::from_flags(["trace-pc-guard", "trace-cmp"])
            .expect("valid flags")
            .expect("some options");
        assert_eq!(opts.coverage_type, SanCovType::Edge);
        assert!(!opts.opt_for_fuzzing);
        assert!(opts.trace_pc_guard);
        assert!(opts.trace_cmp);
        assert!(!opts.inline_8bit_counters);
    }

    #[test]
    fn parses_fuzzer_preset_with_opt_for_fuzzing() {
        let opts = SanCovOptions::from_flags(["fuzzer"])
            .expect("valid flags")
            .expect("some options");
        assert_eq!(opts, SanCovOptions::fuzzer());
        assert!(opts.opt_for_fuzzing);
    }

    #[test]
    fn fuzzer_preset_tracks_stack_depth_only_on_linux() {
        let fuzzer = SanCovOptions::fuzzer();
        assert!(fuzzer.for_target("x86_64-unknown-linux-gnu").stack_depth);
        assert!(!fuzzer.for_target("arm64-apple-macosx15.0.0").stack_depth);
        assert!(!fuzzer.for_target("x86_64-pc-windows-msvc").stack_depth);
        let explicit = SanCovOptions::from_flags(["trace-pc-guard,stack-depth"])
            .expect("valid flags")
            .expect("some options");
        assert!(explicit.for_target("x86_64-pc-windows-msvc").stack_depth);
    }

    #[test]
    fn rejects_granularity_without_mode_and_orphan_pc_table() {
        assert!(SanCovOptions::from_flags(["edge"]).is_err());
        assert!(SanCovOptions::from_flags(["pc-table"]).is_err());
        assert!(SanCovOptions::from_flags(["func,bb,trace-pc-guard"]).is_err());
    }
}
