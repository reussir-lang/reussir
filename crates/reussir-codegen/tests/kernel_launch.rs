//! Source kernels must lower exclusively to tensor SSA and StableHLO control flow.
use reussir_backend::dialect::ty::stablehlo_token;
use reussir_backend::melior::ir::operation::OperationLike;
use reussir_codegen::lower::{LinkagePolicy, lower_program};
use reussir_core::full::mono::monomorphize;
use reussir_core::{in_arena, semi::elaborate, surface};
#[path = "../src/testing.rs"]
mod testing;

fn lower(source: &str) -> Result<String, String> {
    let context = testing::context();
    in_arena(|tcx| {
        let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
        let parse = reussir_syntax::parse_with_interner(source, interner.clone());
        assert!(parse.ok(), "parse errors: {:?}", parse.errors);
        let program = surface::program(&parse.root);
        let elab = elaborate(tcx, &program, &interner);
        assert!(!elab.has_errors(), "elaboration errors: {:?}", elab.reports);
        let (program, reports) = monomorphize(&elab.mono_input());
        assert!(reports.is_empty(), "mono errors: {reports:?}");
        let module = lower_program(&context, tcx, &program, None, None, LinkagePolicy::Jit, &[])
            .map_err(|e| e.to_string())?;
        assert!(
            module.as_operation().verify(),
            "invalid kernel IR: {}",
            module.as_operation()
        );
        Ok(module.as_operation().to_string())
    })
}
fn enabled() -> bool {
    stablehlo_token(&testing::context()).is_some()
}

#[test]
fn outlines_launches_and_preserves_configuration() {
    let source = include_str!("../../../tests/integration/frontend/kernel_launch.rr");
    let result = lower(source);
    if !enabled() {
        assert!(result.unwrap_err().contains("REUSSIR_ENABLE_OPENXLA"));
        return;
    }
    let text = result.unwrap();
    for expected in [
        "ifrt.Call",
        "reussir.array.to_ifrt",
        "reussir.array.from_ifrt",
        "reussir.disable_donation",
        "reussir.backend_optimization_level",
        "reussir.constant_inputs = array<i32: 2>",
        "tensor<ui64>",
    ] {
        assert!(text.contains(expected), "missing {expected}: {text}");
    }
}
