use std::sync::Arc;

use reussir_syntax::source::FileId;
use reussir_syntax::{Interner, SessionInterner};

use super::*;
use crate::semi::hir::{build::parse_program, print::Printer};
use crate::semi::infer::InferCtxt;
use crate::semi::ty::{FpTy, IntTy};
use crate::semi::{Elaborator, ExternPackage, PackageFile};

pub(super) fn imported<'a, 'tcx>(
    tcx: &'a TyCtxt<'tcx>,
    source: &str,
    interner: &'a SessionInterner,
) -> Elaborator<'a, 'tcx> {
    let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
    assert!(parsed.ok(), "{:?}", parsed.errors);
    let program = crate::surface::program(&parsed.root);
    let mut keys = interner.clone();
    let files = [PackageFile {
        file: FileId::ROOT,
        module: vec![keys.get_or_intern("dep")],
        program: &program,
    }];
    let producer = crate::semi::elaborate_package(tcx, &files, interner);
    assert!(!producer.has_errors(), "{:?}", producer.reports);
    let text = Printer::new(&producer.defs, producer.resolver).program(
        &producer.elaborated,
        &producer.strings.entries(),
        &producer.records,
        &producer.trampolines,
    );
    let text = format!("interface 1 package \"dep\" producer \"rrc 0.1.0\";\n{text}");
    let parsed = parse_program(tcx, &text).expect("RRI body roundtrip");
    let mut consumer = Elaborator::new(tcx, interner);
    consumer.declare_extern_package(
        &mut keys,
        &ExternPackage {
            name: "dep",
            parsed: &parsed,
            files: &[],
        },
    );
    assert!(!consumer.has_errors(), "{:?}", consumer.reports);
    consumer
}

#[test]
fn imported_ordinary_wrapper_graph_preserves_all_tensor_relations() {
    crate::with_tcx(|tcx| {
        let interner = Arc::new(reussir_syntax::new_threaded_interner());
        let elab = imported(
            tcx,
            r#"
            #[kernel] fn matrix<T>(x: T) -> Tensor<[T; 2, 3]> {
                let filled: Tensor<[T; 3, 2]> = core::intrinsic::tensor::fill(x);
                core::intrinsic::tensor::transpose(filled)
            }
            #[kernel] fn middle<T>(x: T) -> Tensor<[T; 2, 3]> { matrix(x) }
            #[kernel] pub fn outer<T>(x: T) -> Tensor<[T; 2, 3]> { middle(x) }
            #[kernel] pub fn product<T>(a: Tensor<[T; 2, 3]>, b: Tensor<[T; 3, 4]>,
                                       p: Tensor<[bool; 2, 4]>) -> Tensor<[T; 2, 4]> {
                let d = core::intrinsic::tensor::dot(a, b);
                core::intrinsic::tensor::select(p, d, d)
            }
        "#,
            &interner,
        );
        let functions = &elab.extern_functions;
        let find = |name: &str| {
            functions
                .iter()
                .find(|f| elab.defs.path(f.def).display(elab.resolver) == name)
                .unwrap()
        };
        for (caller, callee) in [
            ("dep::outer", "dep::middle"),
            ("dep::middle", "dep::matrix"),
        ] {
            let summary = Summary::derive(tcx, find(caller));
            assert!(summary.relations.iter().any(|relation| matches!(relation,
                Relation::Call {target, ..} if *target == find(callee).def)));
        }
        let matrix = Summary::derive(tcx, find("dep::matrix"));
        let ops: Vec<_> = matrix
            .relations
            .iter()
            .filter_map(|r| match r {
                Relation::Tensor { op, .. } => Some(*op),
                _ => None,
            })
            .collect();
        assert_eq!(ops, [TensorFn::Fill, TensorFn::Transpose]);
        let product = Summary::derive(tcx, find("dep::product"));
        let ops: Vec<_> = product
            .relations
            .iter()
            .filter_map(|r| match r {
                Relation::Tensor { op, .. } => Some(*op),
                _ => None,
            })
            .collect();
        assert_eq!(ops, [TensorFn::Dot, TensorFn::Select]);
    });
}

#[test]
fn two_instantiations_do_not_share_type_holes_or_parameter_values() {
    crate::with_tcx(|tcx| {
        let interner = Arc::new(reussir_syntax::new_threaded_interner());
        let elab = imported(
            tcx,
            r#"
            #[kernel] pub fn grid<T>(axis: (i64,)) -> Tensor<[T; ?, 3]> {
                let a = axis.0;
                core::intrinsic::tensor::dynamic_iota((a, 3), 0)
            }
        "#,
            &interner,
        );
        let summary = Summary::derive(tcx, &elab.extern_functions[0]);
        let mut infer = InferCtxt::new(tcx);
        let first = infer.instantiate(&summary.generics);
        let second = infer.instantiate(&summary.generics);
        let elem = tcx.mk(TyKind::Generic(summary.generics[0]));
        let first_elem = infer.instantiate_ty(elem, &first);
        let second_elem = infer.instantiate_ty(elem, &second);
        assert_ne!(first_elem, second_elem);
        infer.unify(first_elem, tcx.mk_fp(FpTy::Ieee(32))).unwrap();
        infer.unify(second_elem, tcx.mk_fp(FpTy::Ieee(64))).unwrap();

        let i64_ty = tcx.mk_int(IntTy::Signed(64));
        let axis = |n| Argument {
            ty: tcx.mk_tuple(&[i64_ty]),
            value: Some(Value::Tuple(vec![Argument {
                ty: i64_ty,
                value: Some(Value::Integer(tcx.alloc(Integer::from(n)))),
            }])),
        };
        for (inst, n) in [(&first, 0i64), (&second, 1i64)] {
            let relations = summary.instantiate(|t| infer.instantiate_ty(t, inst), &[axis(n)]);
            let argument = relations
                .iter()
                .find_map(|r| match r {
                    Relation::Tensor {
                        op: TensorFn::DynamicIota,
                        args,
                        ..
                    } => args.first(),
                    _ => None,
                })
                .unwrap();
            match &argument.value {
                Some(Value::Tuple(fields)) => match fields[0].value {
                    Some(Value::Integer(value)) => assert_eq!(value, &Integer::from(n)),
                    _ => panic!("tuple parameter value did not reach dynamic_iota"),
                },
                _ => panic!("dynamic_iota shape tuple was lost"),
            }
        }
        assert_eq!(infer.resolve(first_elem), tcx.mk_fp(FpTy::Ieee(32)));
        assert_eq!(infer.resolve(second_elem), tcx.mk_fp(FpTy::Ieee(64)));
    });
}

#[test]
fn nested_control_flow_is_traversed_without_executing_it() {
    crate::with_tcx(|tcx| {
        let interner = Arc::new(reussir_syntax::new_threaded_interner());
        let elab = imported(
            tcx,
            r#"
            #[kernel] pub fn state<T>(x: Tensor<[T; 2, 2]>) -> (Tensor<[T; 2, 2]>, ()) {
                let y = while [s = x] (false) {
                    if true { core::intrinsic::tensor::transpose(s) } else { s }
                };
                (y, ())
            }
        "#,
            &interner,
        );
        let summary = Summary::derive(tcx, &elab.extern_functions[0]);
        assert_eq!(
            summary
                .relations
                .iter()
                .filter(|r| matches!(r, Relation::Condition { .. }))
                .count(),
            2
        );
        let mapped = summary.instantiate(
            |ty| {
                if ty == tcx.mk_bool() {
                    tcx.mk_tensor(ty, &[])
                } else {
                    ty
                }
            },
            &[],
        );
        assert!(
            mapped
                .iter()
                .filter_map(|r| match r {
                    Relation::Condition { ty } => Some(*ty),
                    _ => None,
                })
                .all(|ty| ty == tcx.mk_tensor(tcx.mk_bool(), &[]))
        );
        assert!(summary.relations.iter().any(|r| matches!(
            r,
            Relation::Tensor {
                op: TensorFn::Transpose,
                ..
            }
        )));
    });
}

#[test]
fn nested_block_and_tuple_projection_preserve_scalar_literal_facts() {
    crate::with_tcx(|tcx| {
        let interner = Arc::new(reussir_syntax::new_threaded_interner());
        let elab = imported(
            tcx,
            r#"
            #[kernel] pub fn filled() -> Tensor<[f32; 2]> {
                core::intrinsic::tensor::fill({
                    let pair = (1.0, ());
                    let value = pair.0;
                    value
                })
            }
        "#,
            &interner,
        );
        let summary = Summary::derive(tcx, &elab.extern_functions[0]);
        let args = summary
            .relations
            .iter()
            .find_map(|r| match r {
                Relation::Tensor {
                    op: TensorFn::Fill,
                    args,
                    ..
                } => Some(args),
                _ => None,
            })
            .expect("fill relation");
        assert!(matches!(args[0].value, Some(Value::Scalar)));
    });
}

#[test]
fn comparison_summary_keeps_operator_family_and_freshens_its_result() {
    crate::with_tcx(|tcx| {
        let interner = Arc::new(reussir_syntax::new_threaded_interner());
        let elab = imported(
            tcx,
            r#"
            #[kernel] pub fn compare(x: i64, y: i64) -> bool { x < y }
        "#,
            &interner,
        );
        let summary = Summary::derive(tcx, &elab.extern_functions[0]);
        assert!(summary.relations.iter().any(|r| matches!(
            r,
            Relation::Operator {
                op: TensorFn::Lt,
                scalar_bound: ScalarBound::Ordering,
                ..
            }
        )));
        let mut infer = InferCtxt::new(tcx);
        let result = infer.new_hole_ty();
        let relations =
            summary.instantiate(|ty| if ty == tcx.mk_bool() { result } else { ty }, &[]);
        assert!(relations.iter().any(|r| matches!(r,
            Relation::Operator { result: r, .. } if *r == result)));
        assert!(!relations.iter().any(|r| matches!(r,
            Relation::Equal { left, right } if (*left == result && *right == tcx.mk_bool()) || (*right == result && *left == tcx.mk_bool()))));
    });
}
