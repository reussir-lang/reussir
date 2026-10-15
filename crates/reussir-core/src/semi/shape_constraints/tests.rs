use crate::{
    semi::{Severity, elaborate},
    surface, with_tcx,
};

fn reports(source: &str) -> Vec<String> {
    with_tcx(|tcx| {
        let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
        let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
        assert!(parsed.ok(), "parse errors: {:?}", parsed.errors);
        let program = surface::program(&parsed.root);
        let elab = elaborate(tcx, &program, &interner);
        let mut reports = elab
            .reports
            .iter()
            .filter(|r| r.severity == Severity::Error)
            .map(|r| r.message.to_string())
            .collect::<Vec<_>>();
        if reports.is_empty() {
            let (_, errors) = crate::full::mono::monomorphize(&elab.mono_input());
            reports.extend(errors.into_iter().map(|r| r.message.to_string()));
        }
        reports
    })
}

#[test]
fn contextual_shapes_flow_through_literals_operators_helpers_and_later_uses() {
    let source =
        include_str!("../../../../../tests/integration/frontend/shape_inference_elementwise.rr");
    assert!(reports(source).is_empty(), "{:#?}", reports(source));
}

#[test]
fn backward_shape_rules_do_not_guess_shapes_or_select_predicates() {
    for source in [
        "#[kernel] pub fn bad(x:Tensor<[f32;3]>)->Tensor<[f32;3]> { core::intrinsic::tensor::select(core::intrinsic::tensor::fill(true),x,x) }",
        "#[kernel] pub fn bad() { let x:Tensor<[f32;_]> = core::intrinsic::tensor::fill(1.0); }",
        "#[kernel] pub fn bad() { let x = core::intrinsic::tensor::iota(0); }",
    ] {
        let errors = reports(source);
        assert!(!errors.is_empty(), "incorrectly accepted: {source}");
        assert!(
            errors
                .iter()
                .any(|s| s.contains("annotations needed") || s.contains("cannot infer")),
            "{source}: {errors:#?}"
        );
    }
}

#[test]
fn inferred_shapes_keep_concrete_contradictions_and_rank_constraints() {
    for source in [
        "#[kernel] pub fn bad()->Tensor<[i32;2,3]> { core::intrinsic::tensor::iota(2) }",
        "#[kernel] pub fn bad()->Tensor<[f32;3]> { let x:i64=1; core::intrinsic::tensor::fill(x) }",
        "#[kernel] pub fn bad(x:Tensor<[f32;2,3]>)->Tensor<[f32;2,3]> { let y:Tensor<[f32;_,4]> = x; y }",
    ] {
        assert!(
            !reports(source).is_empty(),
            "incorrectly accepted: {source}"
        );
    }
}

#[test]
fn scalar_and_tensor_families_remain_distinct_after_late_context() {
    let source = r#"
    import core::intrinsic::tensor;
    #[kernel] pub fn scalar(a:f32)->Tensor<[f32;2]> { tensor::fill(a+1.0) }
    #[kernel] pub fn tensor_value(a:Tensor<f32>)->Tensor<[f32;2]> { tensor::fill(a) }
    #[kernel] pub fn late_tensor(x:Tensor<[f32;2]>)->Tensor<[f32;2]> {
        let a=tensor::fill(1.0);
        let b=a+x;
        b
    }
    "#;
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn generic_fill_wrappers_preserve_scalar_evidence_for_forwarded_computations() {
    let source = r#"
    import core::intrinsic::tensor;
    #[kernel]
    fn filled<T, S>(value: S) -> Tensor<T> { tensor::fill(value) }
    #[kernel]
    pub fn computed() -> Tensor<[f32; 2]> { filled(1.0 + 2.0) }
    #[kernel]
    pub fn local() -> Tensor<[f32; 2]> {
        let scalar = 1.0 + 2.0;
        filled(scalar)
    }
    #[kernel]
    pub fn annotated_local() -> Tensor<[f32; 2]> {
        let scalar: f32 = 1.0 + 2.0;
        filled(scalar)
    }
    #[kernel]
    pub fn projection() -> Tensor<[f32; 2]> {
        let pair = (1.0 + 2.0, true);
        filled(pair.0)
    }
    #[kernel]
    pub fn separate_instance() -> Tensor<[f64; 3]> { filled(3.0 + 4.0) }
    "#;
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn generic_fill_preserves_numeric_evidence_from_tuple_loop_state() {
    let source = r#"
    import core::intrinsic::tensor;
    #[kernel]
    fn filled<T, S>(value: S) -> Tensor<T> { tensor::fill(value) }
    #[kernel]
    pub fn fibonacci(limit: i64) -> Tensor<[i64; 2]> {
        let (a, b, step) = while [a = 0, b = 1, step = 0] (step < limit) {
            (b, a + b, step + 1)
        };
        filled(a)
    }
    "#;
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn linalg_relations_flow_backward_through_direct_and_wrapped_calls() {
    let source =
        include_str!("../../../../../tests/integration/frontend/shape_inference_linalg.rr");
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}
#[test]
fn linalg_relations_reject_contradictions_and_ambiguous_ranks() {
    for source in [
        "#[kernel] pub fn bad()->Tensor<[f32;7]> { core::intrinsic::tensor::dot(core::intrinsic::tensor::fill(1.0),core::intrinsic::tensor::fill(2.0)) }",
        "#[kernel] pub fn bad(x:Tensor<[f32;2,3]>)->Tensor<[f32;4,2]> { core::intrinsic::tensor::transpose(x) }",
        "#[kernel] pub fn bad(x:Tensor<[f32;2,3]>,y:Tensor<[f32;4,5]>)->Tensor<[f32;2,5]> { core::intrinsic::tensor::dot(x,y) }",
    ] {
        assert!(
            !reports(source).is_empty(),
            "incorrectly accepted: {source}"
        );
    }
}

#[test]
fn mandelbrot_needs_only_one_scalar_float_anchor_and_the_kernel_result() {
    let source =
        include_str!("../../../../../tests/integration/frontend/shape_inference_mandelbrot.rr");
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn dynamic_fill_infers_computed_numeric_shape_metadata() {
    let source = r#"
    import core::intrinsic::tensor;
    #[kernel]
    pub fn computed_size() -> Tensor<[f32; 3]> {
        tensor::dynamic_fill(1.0, (1 + 2,))
    }
    #[kernel]
    fn dynamically_filled<T, S>(value: S, size: i64) -> Tensor<T> {
        tensor::dynamic_fill(value, (size,))
    }
    #[kernel]
    pub fn wrapped_computed_size() -> Tensor<[f32; 3]> {
        dynamically_filled(1.0, 1 + 2)
    }
    #[kernel]
    pub fn local_computed_size() -> Tensor<[f32; 3]> {
        let sizes = (1 + 2,);
        tensor::dynamic_fill(1.0, sizes)
    }
    "#;
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}

#[test]
fn runtime_shapes_wait_for_context_then_default_conservatively() {
    let source =
        include_str!("../../../../../tests/integration/frontend/shape_inference_runtime.rr");
    let errors = reports(source);
    assert!(errors.is_empty(), "{errors:#?}");
}
