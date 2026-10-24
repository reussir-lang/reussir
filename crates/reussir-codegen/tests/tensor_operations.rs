//! Source kernels must lower exclusively to tensor SSA and StableHLO control flow.
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
    reussir_backend::features::enabled("openxla")
}

#[test]
fn initialization_arithmetic_comparison_and_dot_verify() {
    let result = lower(
        r#"
        import core::intrinsic::tensor;
        #[kernel] pub fn run(a:Tensor<[f32;2,3]>, b:Tensor<[f32;3,4]>) -> (Tensor<[f32;2,4]>, Tensor<[f32;2,3]>, Tensor<[bool;2,3]>) {
            let filled:Tensor<[f32;2,3]> =tensor::fill(2.0);
            let ramp=tensor::iota<Tensor<[f32;2,3]>>(1);
            let x=tensor::max(tensor::abs(-((a+filled)*ramp-filled)/filled),a%filled);
            let pred=x>=a;
            (tensor::dot(x,b),tensor::select(pred,x,a),pred)
        }
        #[kernel] pub fn vv(a:Tensor<[i32;3]>, b:Tensor<[i32;3]>) -> Tensor<i32> { tensor::dot(a,b) }
        #[kernel] pub fn vm(a:Tensor<[f32;3]>, b:Tensor<[f32;3,4]>) -> Tensor<[f32;4]> { tensor::dot(a,b) }
        #[kernel] pub fn mv(a:Tensor<[f32;2,3]>, b:Tensor<[f32;3]>) -> Tensor<[f32;2]> { tensor::dot(a,b) }
        #[kernel] pub fn tr(a:Tensor<[f32;2,3]>) -> Tensor<[f32;3,2]> { tensor::transpose(a) }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("the `openxla` feature"));
        return;
    }
    let text = result.unwrap();
    for name in [
        "broadcast_in_dim",
        "iota",
        "add",
        "subtract",
        "multiply",
        "divide",
        "remainder",
        "negate",
        "abs",
        "maximum",
        "compare",
        "select",
        "dot_general",
        "transpose",
    ] {
        assert!(
            text.contains(&format!("stablehlo.{name}")),
            "{name}: {text}"
        );
    }
}

#[test]
fn dynamic_shapes_rank_zero_zero_extent_and_shape_generics_verify() {
    let result = lower(
        r#"
        import core::intrinsic::tensor;
        #[kernel] fn twice<T>(x:Tensor<T>) -> Tensor<T> { tensor::add(x,x) }
        #[kernel] pub fn dynamic(n:Tensor<i64>,x:Tensor<[f32;?,3]>) -> (Tensor<[f32;?,3]>,Tensor<[i64;?,3]>,Tensor<[f32;?,3]>) {
            (tensor::dynamic_fill(1.0,(n,3)),tensor::dynamic_iota((n,3),1),twice(x))
        }
        #[kernel] pub fn zero() -> (Tensor<f32>,Tensor<[bool;0,3]>,Tensor<[u64;0]>) {
            (tensor::fill(2.0),tensor::fill(true),tensor::iota(0))
        }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("the `openxla` feature"));
        return;
    }
    let text = result.unwrap();
    assert!(text.contains("stablehlo.dynamic_broadcast_in_dim"));
    assert!(text.contains("stablehlo.dynamic_iota"));
    assert!(text.contains("tensor<?x3xf32>"));
    assert!(text.contains("tensor<0x3xi1>"));
}

#[test]
fn every_supported_numeric_element_verifies_without_changing_signedness() {
    for ty in [
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "bfloat16", "f32", "f64",
    ] {
        let source = format!(
            "#[kernel] pub fn run(x:Tensor<[{ty};2,2]>)->Tensor<[{ty};2,2]> {{ core::intrinsic::tensor::dot(x,x) + core::intrinsic::tensor::iota<Tensor<[{ty};2,2]>>(0) }}"
        );
        let result = lower(&source);
        if enabled() {
            assert!(result.is_ok(), "{ty}: {result:?}");
        } else {
            assert!(result.unwrap_err().contains("the `openxla` feature"));
        }
    }
}
