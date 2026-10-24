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
fn isolated_async_slice_roundtrips_real_kernel_values() {
    let result = lower(
        r#"
        #[kernel] fn start<T>(x: Tensor<[T; ?, 4]>, indices: (i32, i32)) -> Future<Tensor<[T; 2, 3]>> {
            async { let sliced = core::intrinsic::tensor::dynamic_slice<Tensor<[T; 2, 3]>>(x, indices); sliced }
        }
        #[kernel] pub fn sliced(x: Tensor<[f32; ?, 4]>, i: i32, j: i32) -> Tensor<[f32; 2, 3]> {
            let pending = start(x, (i, j));
            core::intrinsic::tensor::async_done(pending)
        }
        #[kernel] fn product(x: (Tensor<[f32; 8]>, (i32, ()))) -> Future<Tensor<[f32; 2]>> {
            async {
                let (input, (index, ())) = x;
                let pair = (core::intrinsic::tensor::dynamic_slice<Tensor<[f32; 2]>>(input, (index,)), ());
                pair.0
            }
        }
        #[kernel] pub fn repeat(x: Tensor<[f32; 8]>, n: i64) -> Tensor<[f32; 8]> {
            let (i, value) = while [i = 0, value = x] (i < n) {
                let pending = async { core::intrinsic::tensor::collective_permute(value, ((0, 0),)) };
                (i + 1, core::intrinsic::tensor::async_done(pending))
            };
            value
        }
        #[kernel] pub fn static_slice(x: Tensor<[f32; 8]>) -> Tensor<[f32; 3]> {
            core::intrinsic::tensor::async_done(async { core::intrinsic::tensor::slice<Tensor<[f32; 3]>>(x,(2,)) })
        }
        #[kernel] fn permute(x: Tensor<[f32; 8]>) -> Future<Tensor<[f32; 8]>> {
            async { core::intrinsic::tensor::collective_permute(x, ((0,1),(1,0))) }
        }
        #[kernel] pub fn no_routes(x: Tensor<i32>) -> Tensor<i32> {
            core::intrinsic::tensor::async_done(async { core::intrinsic::tensor::collective_permute(x, ()) })
        }
        #[kernel] pub fn scalar(x: Tensor<u64>) -> Tensor<u64> {
            core::intrinsic::tensor::async_done(async { core::intrinsic::tensor::dynamic_slice<Tensor<u64>>(x, ()) })
        }
        #[kernel] pub fn empty(x: Tensor<[i16; 3]>, i: Tensor<i64>) -> Tensor<[i16; 0]> {
            core::intrinsic::tensor::async_done(async { core::intrinsic::tensor::dynamic_slice<Tensor<[i16; 0]>>(x, (i,)) })
        }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("the `openxla` feature"));
        return;
    }
    let text = result.unwrap();
    for expected in [
        "stablehlo.while",
        "stablehlo.async_start",
        "stablehlo.async_done",
        "stablehlo.dynamic_slice",
        "stablehlo.slice",
        "stablehlo.collective_permute",
        "tensor<0x2xi64>",
        "!stablehlo.future<tensor<2x3xf32>>",
        "tensor<ui64>",
        "tensor<0xi16>",
        "^bb0(",
    ] {
        assert!(text.contains(expected), "missing {expected}: {text}");
    }
    for host in ["reussir.closure", "reussir.rc", "scf.", "arith."] {
        assert!(!text.contains(host), "host operation {host}: {text}");
    }
}

#[test]
fn ordinary_future_done_method_is_a_private_kernel_specialization() {
    let result = lower(
        r#"
        #[lang("tensor_future")] pub struct Future<T> {}
        impl<T> Future<T> {
            #[kernel] pub fn done(self: Self) -> T {
                core::intrinsic::tensor::async_done(self)
            }
        }
        #[kernel] pub fn run(x: Tensor<f32>) -> Tensor<f32> {
            let pending = async { core::intrinsic::tensor::collective_permute(x, ((0, 0),)) };
            pending.done()
        }
    "#,
    );
    if !enabled() {
        assert!(result.unwrap_err().contains("the `openxla` feature"));
        return;
    }
    let text = result.unwrap();
    assert!(text.contains("func.func private @_RI"), "{text}");
    assert!(
        !text.contains("call @_RI"),
        "simple intrinsic member must inline during codegen: {text}"
    );
    assert!(text.contains("stablehlo.async_done"), "{text}");
}

#[test]
fn public_nongeneric_future_entry_is_still_rejected() {
    let result = lower(
        "#[kernel] pub fn bad(x: Future<Tensor<f32>>) -> Tensor<f32> { core::intrinsic::tensor::async_done(x) }",
    );
    let error = result.unwrap_err();
    assert!(
        error.contains(if enabled() {
            "public kernel entry"
        } else {
            "the `openxla` feature"
        }),
        "{error}"
    );
}
