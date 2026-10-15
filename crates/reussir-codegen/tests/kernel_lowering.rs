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
fn conditional_products_calls_and_scalars() {
    let result = lower(
        r#"
        #[kernel] fn identity<T>(x: T) -> T { x }
        #[kernel] pub fn choose(c: bool, x: Tensor<[f32; 2]>, y: Tensor<[f32; 2]>, n: i64) -> (Tensor<[f32; 2]>, (i64, ())) {
            let (a, b) = if c { (identity(x), (n + 1, ())) } else { (y, (n - 1, ())) };
            if n < 0 { (a, b) } else { (a, (b.0 * 2, ())) }
        }
        #[kernel] pub fn scalar(x: u64) -> u64 {
            if x > 9223372036854775808 { x / 2 } else { 18446744073709551615 }
        }
        #[kernel] pub fn empty(c: bool) { if !c { () } }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("REUSSIR_ENABLE_OPENXLA"));
        return;
    }
    let text = result.unwrap();
    for operation in [
        "stablehlo.if",
        "stablehlo.compare",
        "stablehlo.add",
        "stablehlo.subtract",
        "stablehlo.multiply",
        "stablehlo.divide",
        "stablehlo.not",
        "call",
        "reussir.kernel_function",
        "tensor<ui64>",
    ] {
        assert!(text.contains(operation), "missing {operation}: {text}");
    }
    for host in ["scf.if", "arith.", "reussir.record", "reussir.rc"] {
        assert!(!text.contains(host), "host instruction {host}: {text}");
    }
}

#[test]
fn rank_zero_tensor_predicate_and_private_token_helpers() {
    let result = lower(
        r#"
        #[kernel] pub fn choose(c: Tensor<bool>, x: Tensor<[f32; ?, 0]>, y: Tensor<[f32; ?, 0]>) -> Tensor<[f32; ?, 0]> {
            if c { x } else { y }
        }
        #[kernel] fn token(c: bool, t: Token) -> Token { if c { t } else { t } }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("REUSSIR_ENABLE_OPENXLA"));
        return;
    }
    let text = result.unwrap();
    assert!(text.contains("tensor<?x0xf32>"));
    assert!(text.contains("!stablehlo.token"));
    assert!(text.contains("stablehlo.if"));
}

#[test]
fn rejects_future_control_results_and_public_non_tensor_abi() {
    if !enabled() {
        return;
    }
    let future = lower(r#"#[kernel] fn choose(c: bool, x: Future<Tensor<f32>>) -> Future<Tensor<f32>> { if c { x } else { x } }"#).unwrap_err();
    assert!(
        future.contains("control-flow results cannot contain Future"),
        "{future}"
    );
    let public = lower(r#"#[kernel] pub fn token(t: Token) -> Token { t }"#).unwrap_err();
    assert!(
        public.contains("public kernel entry parameters and results must flatten to tensors"),
        "{public}"
    );
}
