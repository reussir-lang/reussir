//! Shape and dtype rules for the basic StableHLO tensor operations. These rules
//! are shared by source inference, specialization, and serialized IR loading.
use crate::intrinsic::TensorFn;
use crate::semi::ty::{DYNAMIC_EXTENT, IntTy, Ty, TyKind};

fn open(t: Ty<'_>) -> bool {
    matches!(t.kind(), TyKind::Generic(_) | TyKind::Hole(_))
}
fn tensor(t: Ty<'_>) -> Result<Option<(Ty<'_>, &[u64])>, String> {
    match t.kind() {
        TyKind::Tensor { elem, dims } if dims.is_empty() && open(*elem) => Ok(None),
        TyKind::Tensor { elem, dims } => {
            if let Some(reason) = crate::semi::ty_eval::tensor_element_rejection(*elem) {
                return Err(reason.into());
            }
            Ok(Some((*elem, dims)))
        }
        _ if open(t) => Ok(None),
        _ => Err("tensor operation requires Tensor operands and result".into()),
    }
}
fn same(a: Ty<'_>, b: Ty<'_>) -> bool {
    a == b || open(a) || open(b)
}
fn shape(a: &[u64], b: &[u64]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(a, b)| a == b || *a == DYNAMIC_EXTENT || *b == DYNAMIC_EXTENT)
}
fn numeric(t: Ty<'_>) -> bool {
    open(t) || matches!(t.kind(), TyKind::Int(_) | TyKind::Fp(_))
}
fn scalar(t: Ty<'_>) -> Option<Ty<'_>> {
    match t.kind() {
        TyKind::Tensor { elem, dims } if dims.is_empty() => Some(*elem),
        TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool | TyKind::Generic(_) | TyKind::Hole(_) => {
            Some(t)
        }
        _ => None,
    }
}
pub fn operation(op: TensorFn, result: Ty<'_>, args: &[Ty<'_>]) -> Result<(), String> {
    use TensorFn::*;
    if !op.is_basic() {
        return Ok(());
    }
    if args.len() != op.arity() {
        return Err(format!(
            "{} expects exactly {} arguments",
            op.as_str(),
            op.arity()
        ));
    }
    let output = tensor(result)?;
    match op {
        Fill | DynamicFill | Iota | DynamicIota => {
            if matches!(op, Iota | DynamicIota) {
                let axis = args[usize::from(op == DynamicIota)];
                if !open(axis) && !matches!(axis.kind(), TyKind::Int(IntTy::Signed(64))) {
                    return Err("iota axis must have type i64".into());
                }
            }
            if let Some((elem, dims)) = output {
                if matches!(op, Iota | DynamicIota) && !numeric(elem) {
                    return Err("iota requires a numeric element type".into());
                }
                if matches!(op, Fill | Iota) && dims.contains(&DYNAMIC_EXTENT) {
                    return Err(
                        "fill/iota requires a static result shape; use dynamic_fill/dynamic_iota"
                            .into(),
                    );
                }
                if matches!(op, Fill | DynamicFill) {
                    let value =
                        scalar(args[0]).ok_or("fill value must be a scalar or rank-zero Tensor")?;
                    if !same(value, elem) {
                        return Err("fill value and result element types must match".into());
                    }
                }
                if matches!(op, DynamicFill | DynamicIota) {
                    let shape_ty = args[if op == DynamicFill { 1 } else { 0 }];
                    let fields =
                        match shape_ty.kind() {
                            TyKind::Tuple(fs) => *fs,
                            TyKind::Unit => &[],
                            _ if open(shape_ty) => return Ok(()),
                            _ => return Err(
                                "dynamic tensor shape must be a rank-sized tuple of i64 dimensions"
                                    .into(),
                            ),
                        };
                    if fields.len() != dims.len() {
                        return Err(
                            "dynamic tensor shape needs one dimension per result axis".into()
                        );
                    }
                    for field in fields {
                        let elem = scalar(*field)
                            .ok_or("dynamic tensor dimension must be i64 or Tensor<i64>")?;
                        if !open(elem) && !matches!(elem.kind(), TyKind::Int(IntTy::Signed(64))) {
                            return Err(
                                "dynamic tensor dimension must be i64 or Tensor<i64>".into()
                            );
                        }
                    }
                }
            }
        }
        Dot => {
            let left = tensor(args[0])?;
            let right = tensor(args[1])?;
            if let (Some((le, ld)), Some((re, rd)), Some((oe, od))) = (left, right, output) {
                if !numeric(le) || !same(le, re) || !same(le, oe) {
                    return Err("dot requires matching numeric element types".into());
                }
                if !(1..=2).contains(&ld.len()) || !(1..=2).contains(&rd.len()) {
                    return Err("dot requires vector or matrix operands (rank one or two)".into());
                }
                if !shape(&ld[ld.len() - 1..], &rd[..1]) {
                    return Err("dot contracting dimensions must match".into());
                }
                let expected: Vec<_> = ld[..ld.len() - 1]
                    .iter()
                    .chain(rd[1..].iter())
                    .copied()
                    .collect();
                if !shape(&expected, od) {
                    return Err(
                        "dot result shape does not match the non-contracting dimensions".into(),
                    );
                }
            }
        }
        Transpose => {
            if let (Some((ie, id)), Some((oe, od))) = (tensor(args[0])?, output) {
                if id.len() != 2 || od.len() != 2 || !same(ie, oe) || !shape(&[id[1], id[0]], od) {
                    return Err("transpose requires a matrix and its transposed result type".into());
                }
            }
        }
        _ => {
            let index = usize::from(op == Select);
            let input = tensor(args[index])?;
            for arg in &args[index + 1..] {
                if let (Some((ae, ad)), Some((be, bd))) = (input, tensor(*arg)?) {
                    if !same(ae, be) || ad != bd {
                        return Err("elementwise tensor operands must have identical shapes and element types".into());
                    }
                }
            }
            if let (Some((ie, id)), Some((oe, od))) = (input, output) {
                if id != od {
                    return Err("elementwise tensor result must preserve the operand shape".into());
                }
                if op.is_comparison() {
                    if !matches!(oe.kind(), TyKind::Bool) {
                        return Err("tensor comparison result must have bool elements".into());
                    }
                } else if !same(ie, oe) {
                    return Err("elementwise tensor result must preserve the element type".into());
                }
                // StableHLO permits boolean add/multiply/min/max and comparisons.
                if !matches!(
                    op,
                    Add | Mul | Min | Max | Select | Eq | Ne | Lt | Le | Gt | Ge
                ) && !numeric(ie)
                {
                    return Err(format!("{} requires numeric elements", op.as_str()));
                }
                if op == Select {
                    if let Some((pe, pd)) = tensor(args[0])? {
                        if !matches!(pe.kind(), TyKind::Bool) || (!pd.is_empty() && pd != id) {
                            return Err(
                                "select predicate must be a scalar or same-shape Tensor<bool>"
                                    .into(),
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

macro_rules! expressions {
    ($name:ident, $axis:ident, $integer:ident, $expr:path, $kind:path) => {
        fn $integer(expr: &$expr) -> Option<crate::literal::Integer> {
            use $kind as K;
            match &expr.kind {
                K::ConstInt(n) => Some((*n).clone()),
                K::Negate(value) => $integer(value).map(|n| -n),
                _ => None,
            }
        }
        pub fn $axis(op: TensorFn, result: Ty<'_>, args: &[$expr]) -> Result<i64,String> {
            let index = usize::from(op == TensorFn::DynamicIota);
            let arg = args.get(index).ok_or("iota requires a literal axis")?;
            let n = $integer(arg).ok_or("iota axis must be a compile-time integer literal")?;
            let axis = i64::try_from(&n).map_err(|_| "iota axis is outside the signed 64-bit range")?;
            if axis < 0 || matches!(result.kind(),TyKind::Tensor{elem,dims} if !(dims.is_empty() && open(*elem)) && axis as usize >= dims.len()) { return Err("iota axis must be within the result rank".into()); }
            Ok(axis)
        }
        pub fn $name(op:TensorFn,result:Ty<'_>,args:&[$expr]) -> Result<(),String> {
            use $kind as K;
            operation(op,result,&args.iter().map(|a|a.ty).collect::<Vec<_>>())?;
            if matches!(op,TensorFn::Iota|TensorFn::DynamicIota) { $axis(op,result,args)?; }
            if matches!(op,TensorFn::DynamicFill|TensorFn::DynamicIota) {
                let index=usize::from(op==TensorFn::DynamicFill);
                if let (Some(arg),TyKind::Tensor{dims,..})=(args.get(index),result.kind()) {
                    if let K::Tuple(fields)=&arg.kind {
                        for (field,dim) in fields.iter().zip(dims.iter()) {
                            if let Some(n)=$integer(field) {
                                let value=u64::try_from(&n).map_err(|_|"tensor dimensions must be nonnegative signed 64-bit integers")?;
                                if value>i64::MAX as u64 || (*dim!=DYNAMIC_EXTENT && *dim!=value) { return Err("runtime tensor shape disagrees with the static result extent".into()); }
                            }
                        }
                    }
                }
            }
            Ok(())
        }
    };
}
expressions!(
    hir,
    hir_axis,
    hir_integer,
    crate::semi::hir::Expr<'_>,
    crate::semi::hir::ExprKind
);
expressions!(
    mir,
    mir_axis,
    mir_integer,
    crate::full::mir::Expr<'_>,
    crate::full::mir::ExprKind
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{in_arena, semi::elaborate, surface};

    fn reports(source: &str) -> Vec<String> {
        in_arena(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = surface::program(&parsed.root);
            let elab = elaborate(tcx, &program, &interner);
            let mut reports: Vec<_> = elab
                .reports
                .iter()
                .filter(|r| r.severity == crate::semi::Severity::Error)
                .map(|r| r.message.to_string())
                .collect();
            if reports.is_empty() {
                let (_, mono) = crate::full::mono::monomorphize(&elab.mono_input());
                reports.extend(mono.iter().map(|r| r.message.to_string()));
            }
            reports
        })
    }
    #[test]
    fn constructors_and_operations_reject_invalid_shapes_types_and_immediates() {
        for (source, needle) in [
            (
                "#[kernel] pub fn bad()->Tensor<[f32;?]> { core::intrinsic::tensor::fill(1.0) }",
                "static",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<f32> { core::intrinsic::tensor::iota(0) }",
                "rank",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<[bool;3]> { core::intrinsic::tensor::iota(0) }",
                "numeric",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<[i32;3]> { core::intrinsic::tensor::iota(-1) }",
                "rank",
            ),
            (
                "#[kernel] pub fn bad(i:i64)->Tensor<[i32;3]> { core::intrinsic::tensor::iota(i) }",
                "literal",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<[i32;2,3]> { core::intrinsic::tensor::dynamic_fill(1,(2,)) }",
                "dimension",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<[i32;2]> { core::intrinsic::tensor::dynamic_fill(1,(-2,)) }",
                "nonnegative",
            ),
            (
                "#[kernel] pub fn bad()->Tensor<[i32;2]> { core::intrinsic::tensor::dynamic_fill(1,(3,)) }",
                "static result",
            ),
            (
                "#[kernel] pub fn bad(x:Tensor<[bool;2]>)->Tensor<[bool;2]> { core::intrinsic::tensor::div(x,x) }",
                "numeric",
            ),
            (
                "#[kernel] pub fn bad(x:Tensor<[f32;2]>,y:Tensor<[f32;3]>)->Tensor<f32> { core::intrinsic::tensor::dot(x,y) }",
                "contracting",
            ),
            (
                "#[kernel] pub fn bad(x:Tensor<[f32;2,3]>)->Tensor<[f32;2,3]> { core::intrinsic::tensor::transpose(x) }",
                "type mismatch",
            ),
            (
                "#[kernel] pub fn bad(x:Tensor<[i32;2]>)->Tensor<[i32;2]> { core::intrinsic::tensor::select(x,x,x) }",
                "predicate",
            ),
            (
                "#[kernel] fn helper<T>(x:Tensor<T>)->Tensor<T> { core::intrinsic::tensor::div(x,x) } #[kernel] pub fn bad(x:Tensor<[bool;2]>)->Tensor<[bool;2]> { helper(x) }",
                "numeric",
            ),
        ] {
            let errors = reports(source);
            assert!(
                errors.iter().any(|e| e.contains(needle)),
                "{source}: {errors:?}"
            );
        }
    }
    #[test]
    fn shape_argument_and_element_argument_specialize_independently() {
        let errors = reports(
            r#"
            #[kernel] fn shape<T>(x:Tensor<T>)->Tensor<T> { core::intrinsic::tensor::add(x,x) }
            #[kernel] fn element<T>(x:Tensor<[T;2,3]>)->Tensor<[T;2,3]> { core::intrinsic::tensor::mul(x,x) }
            #[kernel] pub fn run(a:Tensor<[f32;2,3]>,b:Tensor<f32>)->(Tensor<[f32;2,3]>,Tensor<f32>) { (shape(element(a)),shape(b)) }
            #[kernel] pub fn inferred()->Tensor<[f32;2]> { let x=core::intrinsic::tensor::fill(1.0); x }
        "#,
        );
        assert!(errors.is_empty(), "{errors:?}");
        let errors = reports(
            "#[kernel] fn f<T>(x:Tensor<T>)->Tensor<T> { x } #[kernel] pub fn bad(x:Tensor<[f32;2]>)->Tensor<f32> { f<f32>(x) }",
        );
        assert!(
            errors.iter().any(|e| e.contains("type mismatch")),
            "{errors:?}"
        );
    }
    #[test]
    fn tensor_metadata_rules_are_checked_without_source_inference() {
        in_arena(|tcx| {
            let f = tcx.mk_fp(crate::semi::ty::FpTy::Ieee(32));
            let a = tcx.mk_tensor(f, &[2, 3]);
            let b = tcx.mk_tensor(f, &[3, 4]);
            let result = tcx.mk_tensor(f, &[2, 4]);
            assert!(operation(TensorFn::Dot, result, &[a, b]).is_ok());
            assert!(operation(TensorFn::Dot, a, &[a, b]).is_err());
            assert!(operation(TensorFn::Add, a, &[a, b]).is_err());
            assert!(operation(TensorFn::Neg, a, &[]).is_err());
            assert!(operation(TensorFn::Fill, result, &[a]).is_err());
        });
    }
    #[test]
    fn serialized_ir_rejects_malformed_tensor_operations() {
        for (body, result, needle) in [
            ("tensor#fill(1 : i32)", "Tensor<[f32;2]>", "element types"),
            ("tensor#iota(0 : bool)", "Tensor<[i32;2]>", "type i64"),
            ("tensor#iota(4 : i64)", "Tensor<[i32;2]>", "rank"),
            (
                "tensor#dynamic_fill(1 : i32, (-1 : i64,) : (i64,))",
                "Tensor<[i32;?]>",
                "nonnegative",
            ),
            ("tensor#add()", "Tensor<i32>", "arguments"),
        ] {
            in_arena(|tcx| {
                let mir = format!("#[kernel] pub fn @bad() -> {result} {{ {body} : {result} }}");
                let error = crate::full::mir::build::parse_program(tcx, &mir)
                    .err()
                    .expect("invalid MIR should fail");
                assert!(error.contains(needle), "{mir}: {error}");
                let hir = format!("#[kernel] pub fn #bad() -> {result} {{ {body} : {result} }}");
                let error = crate::semi::hir::build::parse_program(tcx, &hir)
                    .err()
                    .expect("invalid HIR should fail");
                assert!(error.contains(needle), "{hir}: {error}");
            });
        }
    }
}
