//! Compile source match expressions and verify their real StableHLO regions.
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
fn wide_signed_guarded_nested_and_tuple_matches_verify() {
    let result = lower(include_str!(
        "../../../tests/integration/frontend/kernel_integer_match.rr"
    ));
    if !enabled() {
        assert!(result.unwrap_err().contains("REUSSIR_ENABLE_OPENXLA"));
        return;
    }
    let text = result.unwrap();
    for required in [
        "stablehlo.case",
        "stablehlo.if",
        "stablehlo.compare",
        "stablehlo.select",
        "tensor<ui64>",
        "4294967296",
        "-9223372036854775808",
    ] {
        assert!(text.contains(required), "missing {required}: {text}");
    }
    for forbidden in [
        "stablehlo.convert",
        "scf.index_switch",
        "arith.",
        "reussir.record",
        "reussir.rc",
    ] {
        assert!(!text.contains(forbidden), "unexpected {forbidden}: {text}");
    }
}

#[test]
fn match_execution_demo_verifies() {
    let result = lower(include_str!(
        "../../../tests/integration/frontend/kernel_match_execute.rr"
    ));
    if enabled() {
        result.unwrap();
    } else {
        assert!(result.unwrap_err().contains("REUSSIR_ENABLE_OPENXLA"));
    }
}

#[test]
fn integer_case_can_yield_tokens_but_not_futures() {
    if !enabled() {
        return;
    }
    let token =
        lower("#[kernel] fn choose(key: i32, t: Token) -> Token { match key { 0 => t, _ => t } }")
            .unwrap();
    assert!(token.contains("!stablehlo.token"), "{token}");
    let future = lower("#[kernel] fn choose(key: i32, f: Future<Tensor<f32>>) -> Future<Tensor<f32>> { match key { 0 => f, _ => f } }").unwrap_err();
    assert!(
        future.contains("control-flow results cannot contain Future"),
        "{future}"
    );
    let nested = lower("#[kernel] fn choose(key: i32, f: Future<Tensor<f32>>) -> ((), Future<Tensor<f32>>) { match key { 0 => ((), f), _ => ((), f) } }").unwrap_err();
    assert!(
        nested.contains("control-flow results cannot contain Future"),
        "{nested}"
    );
}

#[test]
fn serialized_integer_switches_reject_keys_outside_the_scrutinee_type() {
    for kernel in [false, true] {
        if kernel && !enabled() {
            continue;
        }
        for (ty, key) in [("u64", "-1"), ("i8", "128")] {
            let text = format!(
                "{}pub fn @bad(v0 (x): {ty}) -> i32 {{ match v0 : {ty} {{ switch scrut {{ {key} => {{ => 1 : i32 }} _ => {{ => 2 : i32 }} }} }} : i32 }}",
                if kernel { "#[kernel] " } else { "" }
            );
            let context = testing::context();
            in_arena(|tcx| {
                let parsed = reussir_core::full::mir::build::parse_program(tcx, &text)
                    .expect("parse malformed-key MIR");
                let error = lower_program(
                    &context,
                    tcx,
                    &parsed.program,
                    None,
                    None,
                    LinkagePolicy::Jit,
                    &[],
                )
                .unwrap_err()
                .to_string();
                assert!(
                    error.contains("integer pattern") && error.contains("out of range"),
                    "{error}"
                );
            });
        }
    }
}
