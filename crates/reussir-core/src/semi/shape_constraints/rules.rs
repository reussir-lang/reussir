//! Tensor-specific relations over inference-only shapes.
//!
//! This is the only operation dispatch in the shape solver. The controller
//! schedules these monotone rules; it knows neither tensor ranks nor op names.
//! Alternatives are probed transactionally and committed only when unique.
//!
//! To add an operation, extend the exhaustive `apply` dispatch here and put its
//! rank, element-type, and dimension relations in a helper in this module.
//! Return `Deferred` while any required fact is unresolved; do not choose a
//! rank or allocate fresh variables on every retry. Runtime metadata defaults
//! belong only to `Phase::DefaultRuntime`. Add rule tests here and source-level
//! direct/wrapper cases in `tests.rs`. The scheduler and summary composition
//! should not need operation-specific branches.
use crate::intrinsic::TensorFn;
use crate::semi::infer::{
    InferCtxt,
    shape::{Dim, ShapeId, TensorView},
};
use crate::semi::shape_summary::{Argument, Value};
use crate::semi::ty::{DYNAMIC_EXTENT, IntTy, Ty, TyCtxt, TyKind};

/// Runtime dimensions are a finalization choice, never an eager substitute for
/// an unknown static dimension while later source constraints can still arrive.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Phase {
    Propagate,
    DefaultRuntime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum RuleStatus {
    Solved,
    Deferred,
}

fn equal<'tcx>(i: &mut InferCtxt<'_, 'tcx>, a: Ty<'tcx>, b: Ty<'tcx>) -> Result<(), String> {
    // Rigid declaration formals are relations to replay at instantiation, not
    // assignable inference variables. Concrete disagreements are still errors.
    let left = i.shallow_resolve(a);
    let right = i.shallow_resolve(b);
    if (matches!(left.kind(), TyKind::Generic(_)) && !matches!(right.kind(), TyKind::Hole(_)))
        || (matches!(right.kind(), TyKind::Generic(_)) && !matches!(left.kind(), TyKind::Hole(_)))
    {
        return Ok(());
    }
    i.unify(a, b)
        .map_err(|_| "tensor relation has incompatible element or operand types".into())
}
fn exact(i: &mut InferCtxt<'_, '_>, a: ShapeId, b: ShapeId) -> Result<(), String> {
    i.unify_shapes(a, b).map_err(|e| e.to_string())
}
fn rank(i: &mut InferCtxt<'_, '_>, shape: ShapeId, n: usize) -> Result<Vec<Dim>, String> {
    if let Some(ds) = i.shape_dims(shape) {
        return if ds.len() == n {
            Ok(ds)
        } else {
            Err(format!("expected tensor rank {n}, found {}", ds.len()))
        };
    }
    let ds = (0..n).map(|_| i.new_dim()).collect::<Vec<_>>();
    let other = i.ranked_shape(ds.clone());
    exact(i, shape, other)?;
    Ok(ds)
}
fn tensor<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    t: Ty<'tcx>,
) -> Result<Option<TensorView<'tcx>>, String> {
    let resolved = i.shallow_resolve(t);
    // A rigid whole-shape formal has no rank until the summary is instantiated.
    if matches!(resolved.kind(), TyKind::Generic(_) | TyKind::Bottom)
        || matches!(resolved.kind(), TyKind::Tensor { elem, dims } if dims.is_empty() && matches!(elem.kind(), TyKind::Generic(_)))
    {
        return Ok(None);
    }
    if let Some(view) = i.tensor_view(t) {
        return Ok(Some(view));
    }
    if matches!(resolved.kind(), TyKind::Hole(_)) {
        let elem = i.new_hole_ty();
        let shape = i.new_shape();
        let candidate = i.new_tensor_ty(elem, shape);
        equal(i, t, candidate)?;
        return Ok(Some(TensorView { elem, shape }));
    }
    Err("tensor operation requires Tensor operands and result".into())
}
fn array<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    t: Ty<'tcx>,
) -> Result<Option<TensorView<'tcx>>, String> {
    if let Some(view) = i.array_view(t) {
        return Ok(Some(view));
    }
    match i.shallow_resolve(t).kind() {
        TyKind::Generic(_) | TyKind::Bottom => Ok(None),
        TyKind::Hole(_) => {
            let elem = i.new_hole_ty();
            let shape = i.new_shape();
            let candidate = i.new_array_ty(elem, shape);
            equal(i, t, candidate)?;
            Ok(Some(TensorView { elem, shape }))
        }
        _ => Err("tensor host boundary requires an Array".into()),
    }
}
fn matching<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    a: TensorView<'tcx>,
    b: TensorView<'tcx>,
) -> Result<(), String> {
    equal(i, a.elem, b.elem)?;
    exact(i, a.shape, b.shape)
}

/// Runtime compatibility is not equality: `?` never proves a static extent.
fn compatible(i: &mut InferCtxt<'_, '_>, a: Dim, b: Dim) -> Result<(), String> {
    match (i.resolve_dim(a), i.resolve_dim(b)) {
        (Dim::Known(DYNAMIC_EXTENT), _) | (_, Dim::Known(DYNAMIC_EXTENT)) => Ok(()),
        (a, b) => i.unify_dims(a, b).map_err(|e| e.to_string()),
    }
}
/// Copy input metadata to an unknown result; explicit result refinements are
/// checked for compatibility rather than replacing an input's runtime extent.
fn output_dim(
    i: &mut InferCtxt<'_, '_>,
    input: Dim,
    output: Dim,
    phase: Phase,
) -> Result<(), String> {
    match (i.resolve_dim(input), i.resolve_dim(output)) {
        (Dim::Known(DYNAMIC_EXTENT), Dim::Var(_)) if phase == Phase::Propagate => Ok(()),
        (Dim::Known(n), Dim::Var(_)) => i
            .unify_dims(output, Dim::Known(n))
            .map_err(|e| e.to_string()),
        (a, b) => compatible(i, a, b),
    }
}
fn viable<'a, 'tcx>(
    i: &mut InferCtxt<'a, 'tcx>,
    f: impl FnOnce(&mut InferCtxt<'a, 'tcx>) -> Result<(), String>,
) -> bool {
    // probe commits Ok; wrapping the outcome in Err rolls back every candidate.
    i.probe(|i| Err::<(), _>(f(i))).unwrap_err().is_ok()
}
fn dot_case(
    i: &mut InferCtxt<'_, '_>,
    a: ShapeId,
    b: ShapeId,
    out: ShapeId,
    ar: usize,
    br: usize,
    phase: Phase,
) -> Result<(), String> {
    let ad = rank(i, a, ar)?;
    let bd = rank(i, b, br)?;
    let od = rank(i, out, ar + br - 2)?;
    compatible(i, ad[ar - 1], bd[0])?;
    for (input, output) in ad[..ar - 1].iter().chain(bd[1..].iter()).zip(od) {
        output_dim(i, *input, output, phase)?;
    }
    Ok(())
}
fn dot(
    i: &mut InferCtxt<'_, '_>,
    a: ShapeId,
    b: ShapeId,
    out: ShapeId,
    phase: Phase,
) -> Result<RuleStatus, String> {
    i.require_rank_at_least(a, 1).map_err(|e| e.to_string())?;
    i.require_rank_at_least(b, 1).map_err(|e| e.to_string())?;
    let candidates = [(1, 1), (1, 2), (2, 1), (2, 2)]
        .into_iter()
        .filter(|&(ar, br)| viable(i, |i| dot_case(i, a, b, out, ar, br, phase)))
        .collect::<Vec<_>>();
    match candidates.as_slice() {
        [] => Err("dot has no compatible vector/matrix rank and dimension combination".into()),
        &[(ar, br)] => {
            dot_case(i, a, b, out, ar, br, phase)?;
            Ok(RuleStatus::Solved)
        }
        _ => Ok(RuleStatus::Deferred),
    }
}
fn select_predicate(
    i: &mut InferCtxt<'_, '_>,
    predicate: ShapeId,
    value: ShapeId,
) -> Result<RuleStatus, String> {
    if let Some(ds) = i.shape_dims(predicate) {
        if ds.is_empty() {
            return Ok(RuleStatus::Solved);
        }
        exact(i, predicate, value)?;
        return Ok(RuleStatus::Solved);
    }
    if i.shape_dims(value).is_some_and(|ds| ds.is_empty()) {
        rank(i, predicate, 0)?;
        return Ok(RuleStatus::Solved);
    }
    let scalar = viable(i, |i| rank(i, predicate, 0).map(|_| ()));
    let shaped = viable(i, |i| exact(i, predicate, value));
    match (scalar, shaped) {
        (true, false) => {
            rank(i, predicate, 0)?;
            Ok(RuleStatus::Solved)
        }
        (false, true) => {
            exact(i, predicate, value)?;
            Ok(RuleStatus::Solved)
        }
        (true, true) => Ok(RuleStatus::Deferred),
        (false, false) => Err("select predicate must be scalar or match the value shape".into()),
    }
}
fn scalar_element<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    arg: &Argument<'tcx>,
) -> Result<Option<Ty<'tcx>>, String> {
    let resolved = i.shallow_resolve(arg.ty);
    if matches!(resolved.kind(), TyKind::Tensor { elem, dims } if dims.is_empty() && matches!(elem.kind(), TyKind::Generic(_)))
    {
        return Ok(None);
    }
    if let Some(view) = i.tensor_view(arg.ty) {
        rank(i, view.shape, 0)?;
        return Ok(Some(view.elem));
    }
    let ty = i.shallow_resolve(arg.ty);
    match ty.kind() {
        TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool => Ok(Some(ty)),
        TyKind::Hole(_)
            if matches!(
                arg.value,
                Some(
                    Value::Integer(_)
                        | Value::Bool(_)
                        | Value::Scalar
                        | Value::Opaque { scalar: true, .. }
                )
            ) =>
        {
            Ok(Some(ty))
        }
        TyKind::Hole(_) | TyKind::Generic(_) | TyKind::Bottom => Ok(None),
        _ => Err("expected a scalar or rank-zero Tensor".into()),
    }
}
fn fields<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    arg: &Argument<'tcx>,
) -> Result<Option<Vec<Argument<'tcx>>>, String> {
    if let Some(Value::Tuple(fields)) = &arg.value {
        return Ok(Some(fields.clone()));
    }
    match i.shallow_resolve(arg.ty).kind() {
        TyKind::Unit => Ok(Some(Vec::new())),
        TyKind::Tuple(fields) => Ok(Some(
            fields.iter().copied().map(Argument::unknown).collect(),
        )),
        TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => Ok(None),
        _ => Err("tensor shape or indices must be a tuple".into()),
    }
}
fn integer(arg: &Argument<'_>) -> Result<Option<u64>, String> {
    match arg.value {
        Some(Value::Integer(n)) => u64::try_from(n)
            .ok()
            .filter(|&n| n <= i64::MAX as u64)
            .map(Some)
            .ok_or_else(|| {
                "tensor dimension or axis must be a nonnegative signed 64-bit integer".into()
            }),
        _ => Ok(None),
    }
}
fn dynamic_shape<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    tcx: &TyCtxt<'tcx>,
    output: ShapeId,
    arg: &Argument<'tcx>,
    phase: Phase,
) -> Result<RuleStatus, String> {
    let Some(fields) = fields(i, arg)? else {
        return Ok(RuleStatus::Deferred);
    };
    let dims = rank(i, output, fields.len())?;
    let mut status = RuleStatus::Solved;
    for (field, dim) in fields.iter().zip(dims) {
        match scalar_element(i, field)? {
            Some(elem) => equal(i, elem, tcx.mk_int(IntTy::Signed(64)))?,
            None => status = RuleStatus::Deferred,
        }
        if let Some(value) = integer(field)? {
            compatible(i, Dim::Known(value), dim)?;
        } else if matches!(i.resolve_dim(dim), Dim::Var(_)) {
            // Keep the original dimension meta live throughout propagation:
            // a later result annotation may still constrain this runtime size.
            // Only the final fallback can choose the conservative dynamic
            // representation, and only for an actual runtime size operand.
            if phase == Phase::DefaultRuntime {
                i.unify_dims(dim, Dim::Known(DYNAMIC_EXTENT))
                    .map_err(|e| e.to_string())?;
            } else {
                status = RuleStatus::Deferred;
            }
        }
    }
    Ok(status)
}
fn open(t: Ty<'_>) -> bool {
    match t.kind() {
        TyKind::Hole(_) | TyKind::Generic(_) => true,
        TyKind::Tensor { elem, .. } | TyKind::Array { elem, .. } => open(*elem),
        TyKind::Future(ts) | TyKind::Tuple(ts) => ts.iter().any(|&t| open(t)),
        _ => false,
    }
}

pub(super) fn apply<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    tcx: &TyCtxt<'tcx>,
    op: TensorFn,
    result: Ty<'tcx>,
    args: &[Argument<'tcx>],
    phase: Phase,
) -> Result<RuleStatus, String> {
    use TensorFn::*;
    let count = match op {
        Of | Materialize | AsyncDone => 1,
        _ => op.arity(),
    };
    if args.len() != count {
        return Err(format!("{} expects {count} operands", op.as_str()));
    }
    let mut status = RuleStatus::Solved;
    // Preserve exact result identities before introducing a Tensor family.
    // In a polymorphic body the operand may still be a rigid whole type V;
    // assigning its identity to a fresh result is valid without guessing V.
    match op {
        Add | Sub | Mul | Div | Rem | Neg | Abs | Min | Max | CollectivePermute => {
            equal(i, result, args[0].ty)?
        }
        Select => equal(i, result, args[1].ty)?,
        Of | Materialize | Dim | DynamicSlice | Slice | AsyncDone | Fill | DynamicFill | Iota
        | DynamicIota | Eq | Ne | Lt | Le | Gt | Ge | Dot | Transpose => {}
    }
    match op {
        Of | Materialize => {
            let (a, b) = if op == Of {
                (array(i, args[0].ty)?, tensor(i, result)?)
            } else {
                (tensor(i, args[0].ty)?, array(i, result)?)
            };
            if let (Some(a), Some(b)) = (a, b) {
                matching(i, a, b)?;
            } else {
                status = RuleStatus::Deferred;
            }
        }
        Dim => {
            tensor(i, args[0].ty)?;
            equal(i, result, tcx.mk_int(IntTy::Unsigned(64)))?;
            equal(i, args[1].ty, tcx.mk_int(IntTy::Unsigned(64)))?;
        }
        AsyncDone => {
            tensor(i, result)?;
            match i.shallow_resolve(args[0].ty).kind() {
                TyKind::Future(payload) if payload.len() == 1 => equal(i, result, payload[0])?,
                TyKind::Hole(_) => equal(i, args[0].ty, tcx.mk_future(&[result]))?,
                TyKind::Generic(_) | TyKind::Bottom => status = RuleStatus::Deferred,
                _ => {
                    return Err(
                        "async_done requires a Future with exactly one Tensor payload".into(),
                    );
                }
            }
        }
        Fill | DynamicFill | Iota | DynamicIota => {
            // Metadata has a type independently of the result's opaque generic
            // shape. Core wrappers must type their literal axes and sizes too.
            if matches!(op, Iota | DynamicIota) {
                equal(
                    i,
                    args[usize::from(op == DynamicIota)].ty,
                    tcx.mk_int(IntTy::Signed(64)),
                )?;
            }
            if matches!(op, DynamicFill | DynamicIota) {
                if let Some(fields) = fields(i, &args[usize::from(op == DynamicFill)])? {
                    for field in fields {
                        if let Some(elem) = scalar_element(i, &field)? {
                            equal(i, elem, tcx.mk_int(IntTy::Signed(64)))?;
                        }
                        integer(&field)?;
                    }
                }
            }
            let Some(output) = tensor(i, result)? else {
                return Ok(RuleStatus::Deferred);
            };
            if matches!(op, Fill | DynamicFill) {
                match scalar_element(i, &args[0])? {
                    Some(elem) => equal(i, elem, output.elem)?,
                    None => status = RuleStatus::Deferred,
                }
            }
            if matches!(op, DynamicFill | DynamicIota) {
                if dynamic_shape(
                    i,
                    tcx,
                    output.shape,
                    &args[usize::from(op == DynamicFill)],
                    phase,
                )? == RuleStatus::Deferred
                {
                    status = RuleStatus::Deferred;
                }
            }
            if matches!(op, Iota | DynamicIota) {
                let axis = &args[usize::from(op == DynamicIota)];
                equal(i, axis.ty, tcx.mk_int(IntTy::Signed(64)))?;
                if let Some(axis) = integer(axis)? {
                    let minimum = usize::try_from(axis)
                        .ok()
                        .and_then(|n| n.checked_add(1))
                        .ok_or("iota axis is too large")?;
                    i.require_rank_at_least(output.shape, minimum)
                        .map_err(|_| "iota axis must be within the result rank".to_owned())?;
                } else {
                    status = RuleStatus::Deferred;
                }
            }
        }
        Dot => {
            let a = tensor(i, args[0].ty)?;
            let b = tensor(i, args[1].ty)?;
            let out = if a.is_some() && b.is_some() {
                tensor(i, result)?
            } else {
                None
            };
            if let (Some(a), Some(b), Some(out)) = (a, b, out) {
                equal(i, a.elem, b.elem)?;
                equal(i, a.elem, out.elem)?;
                status = dot(i, a.shape, b.shape, out.shape, phase)?;
            } else {
                status = RuleStatus::Deferred;
            }
        }
        Transpose => {
            let a = tensor(i, args[0].ty)?;
            let out = if a.is_some() {
                tensor(i, result)?
            } else {
                None
            };
            if let (Some(a), Some(out)) = (a, out) {
                equal(i, a.elem, out.elem)?;
                let ad = rank(i, a.shape, 2)?;
                let od = rank(i, out.shape, 2)?;
                output_dim(i, ad[0], od[1], phase)?;
                output_dim(i, ad[1], od[0], phase)?;
            } else {
                status = RuleStatus::Deferred;
            }
        }
        DynamicSlice | Slice => {
            let a = tensor(i, args[0].ty)?;
            let out = if a.is_some() {
                tensor(i, result)?
            } else {
                None
            };
            if let (Some(a), Some(out)) = (a, out) {
                equal(i, a.elem, out.elem)?;
                if let Some(indices) = fields(i, &args[1])? {
                    rank(i, a.shape, indices.len())?;
                    rank(i, out.shape, indices.len())?;
                } else if let Some(ds) = i.shape_dims(a.shape) {
                    rank(i, out.shape, ds.len())?;
                } else if let Some(ds) = i.shape_dims(out.shape) {
                    rank(i, a.shape, ds.len())?;
                } else {
                    status = RuleStatus::Deferred;
                }
            } else {
                status = RuleStatus::Deferred;
            }
        }
        CollectivePermute | Add | Sub | Mul | Div | Rem | Neg | Abs | Min | Max | Eq | Ne | Lt
        | Le | Gt | Ge | Select => {
            let first = usize::from(op == Select);
            let a = tensor(i, args[first].ty)?;
            let out = if a.is_some() {
                tensor(i, result)?
            } else {
                None
            };
            if let (Some(a), Some(out)) = (a, out) {
                exact(i, a.shape, out.shape)?;
                if op.is_comparison() {
                    equal(i, out.elem, tcx.mk_bool())?;
                } else {
                    equal(i, a.elem, out.elem)?;
                }
                // Collective-permute's second operand is replica-pair metadata.
                if op != CollectivePermute {
                    for arg in &args[first + 1..] {
                        if let Some(b) = tensor(i, arg.ty)? {
                            matching(i, a, b)?;
                        } else {
                            status = RuleStatus::Deferred;
                        }
                    }
                }
                if op == Select {
                    if let Some(p) = tensor(i, args[0].ty)? {
                        equal(i, p.elem, tcx.mk_bool())?;
                        status = select_predicate(i, p.shape, a.shape)?;
                    } else {
                        status = RuleStatus::Deferred;
                    }
                }
            } else {
                status = RuleStatus::Deferred;
            }
        }
    }
    let result = i.resolve(result);
    let types = args.iter().map(|a| i.resolve(a.ty)).collect::<Vec<_>>();
    crate::tensor_check::operation(op, result, &types)?;
    if open(result) || types.iter().copied().any(open) {
        status = RuleStatus::Deferred;
    }
    Ok(status)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::ty::FpTy;
    use crate::{literal::Integer, with_tcx};

    fn apply<'tcx>(
        i: &mut InferCtxt<'_, 'tcx>,
        tcx: &TyCtxt<'tcx>,
        op: TensorFn,
        result: Ty<'tcx>,
        args: &[Argument<'tcx>],
    ) -> Result<RuleStatus, String> {
        super::apply(i, tcx, op, result, args, Phase::Propagate)
    }

    fn literal<'tcx>(tcx: &TyCtxt<'tcx>, n: i64) -> Argument<'tcx> {
        Argument {
            ty: tcx.mk_int(IntTy::Signed(64)),
            value: Some(Value::Integer(tcx.alloc(Integer::from(n)))),
        }
    }
    fn unknowns<'tcx>(i: &mut InferCtxt<'_, 'tcx>, n: usize) -> Vec<Argument<'tcx>> {
        (0..n).map(|_| Argument::unknown(i.new_hole_ty())).collect()
    }
    fn run<'tcx>(
        i: &mut InferCtxt<'_, 'tcx>,
        tcx: &TyCtxt<'tcx>,
        op: TensorFn,
        result: Ty<'tcx>,
        args: &[Ty<'tcx>],
    ) -> Result<RuleStatus, String> {
        apply(
            i,
            tcx,
            op,
            result,
            &args
                .iter()
                .copied()
                .map(Argument::unknown)
                .collect::<Vec<_>>(),
        )
    }

    #[test]
    fn elementwise_rules_flow_backward_and_reach_a_fixed_point() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let out = tcx.mk_tensor(f, &[2, 3]);
            for op in [
                TensorFn::Add,
                TensorFn::Sub,
                TensorFn::Mul,
                TensorFn::Div,
                TensorFn::Rem,
                TensorFn::Min,
                TensorFn::Max,
                TensorFn::Neg,
                TensorFn::Abs,
            ] {
                let mut i = InferCtxt::new(tcx);
                let args = unknowns(&mut i, op.arity());
                assert_eq!(
                    apply(&mut i, tcx, op, out, &args).unwrap(),
                    RuleStatus::Solved
                );
                for arg in &args {
                    assert_eq!(i.resolve(arg.ty), out);
                }
                let revision = i.progress();
                for _ in 0..4 {
                    apply(&mut i, tcx, op, out, &args).unwrap();
                }
                assert_eq!(i.progress(), revision);
            }
        });
    }
    #[test]
    fn comparison_preserves_shape_without_conflating_bool_and_input_dtype() {
        with_tcx(|tcx| {
            for op in [
                TensorFn::Eq,
                TensorFn::Ne,
                TensorFn::Lt,
                TensorFn::Le,
                TensorFn::Gt,
                TensorFn::Ge,
            ] {
                let mut i = InferCtxt::new(tcx);
                let args = unknowns(&mut i, 2);
                let output = tcx.mk_tensor(tcx.mk_bool(), &[4]);
                assert_eq!(
                    apply(&mut i, tcx, op, output, &args).unwrap(),
                    RuleStatus::Deferred
                );
                let input = tcx.mk_tensor(tcx.mk_fp(FpTy::Ieee(32)), &[4]);
                i.unify(args[0].ty, input).unwrap();
                assert_eq!(
                    apply(&mut i, tcx, op, output, &args).unwrap(),
                    RuleStatus::Solved
                );
                assert_eq!(i.resolve(args[1].ty), input);
            }
        });
    }
    #[test]
    fn fill_scalar_or_rank_zero_tensor_stays_ambiguous_without_evidence() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let output = tcx.mk_tensor(f, &[2, 3]);
            let args = unknowns(&mut i, 1);
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Fill, output, &args).unwrap(),
                RuleStatus::Deferred
            );
            assert!(matches!(i.resolve(args[0].ty).kind(), TyKind::Hole(_)));
            i.unify(args[0].ty, tcx.mk_tensor(f, &[])).unwrap();
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Fill, output, &args).unwrap(),
                RuleStatus::Solved
            );
            let scalar = i.new_hole_ty();
            apply(
                &mut i,
                tcx,
                TensorFn::Fill,
                output,
                &[Argument {
                    ty: scalar,
                    value: Some(Value::Scalar),
                }],
            )
            .unwrap();
            assert_eq!(i.resolve(scalar), f);
        });
    }
    #[test]
    fn dynamic_constructor_infers_literal_and_runtime_dimensions() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let it = tcx.mk_int(IntTy::Signed(64));
            let mut i = InferCtxt::new(tcx);
            let out = i.new_hole_ty();
            let shape = Argument {
                ty: tcx.mk_tuple(&[it, it]),
                value: Some(Value::Tuple(vec![literal(tcx, 3), Argument::unknown(it)])),
            };
            assert_eq!(
                super::apply(
                    &mut i,
                    tcx,
                    TensorFn::DynamicFill,
                    out,
                    &[Argument::unknown(f), shape.clone()],
                    Phase::DefaultRuntime,
                )
                .unwrap(),
                RuleStatus::Solved
            );
            assert_eq!(i.resolve(out), tcx.mk_tensor(f, &[3, DYNAMIC_EXTENT]));
            let explicit = tcx.mk_tensor(f, &[3, 4]);
            apply(
                &mut i,
                tcx,
                TensorFn::DynamicFill,
                explicit,
                &[Argument::unknown(f), shape],
            )
            .unwrap();
            let bad = Argument {
                ty: tcx.mk_tuple(&[it]),
                value: Some(Value::Tuple(vec![literal(tcx, -1)])),
            };
            assert!(
                apply(
                    &mut i,
                    tcx,
                    TensorFn::DynamicFill,
                    out,
                    &[Argument::unknown(f), bad]
                )
                .is_err()
            );
        });
    }
    #[test]
    fn runtime_dimensions_wait_for_late_static_context_without_freshening() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let index = tcx.mk_int(IntTy::Signed(64));
            let result = i.new_hole_ty();
            let args = [
                Argument::unknown(f),
                Argument::unknown(tcx.mk_tuple(&[index])),
            ];
            assert_eq!(
                apply(&mut i, tcx, TensorFn::DynamicFill, result, &args).unwrap(),
                RuleStatus::Deferred
            );
            let view = i.tensor_view(result).unwrap();
            let dims = i.shape_dims(view.shape).unwrap();
            assert!(matches!(dims.as_slice(), [Dim::Var(_)]));
            let revision = i.progress();
            for _ in 0..4 {
                assert_eq!(
                    apply(&mut i, tcx, TensorFn::DynamicFill, result, &args).unwrap(),
                    RuleStatus::Deferred
                );
                assert_eq!(i.shape_dims(view.shape).unwrap(), dims);
                assert_eq!(i.progress(), revision);
            }
            let expected = tcx.mk_tensor(f, &[4]);
            i.unify(result, expected).unwrap();
            assert_eq!(
                apply(&mut i, tcx, TensorFn::DynamicFill, result, &args).unwrap(),
                RuleStatus::Solved
            );
            super::apply(
                &mut i,
                tcx,
                TensorFn::DynamicFill,
                result,
                &args,
                Phase::DefaultRuntime,
            )
            .unwrap();
            assert_eq!(i.resolve(result), expected);
        });
    }

    #[test]
    fn final_runtime_defaulting_does_not_guess_unrelated_shapes_or_dtypes() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let index = tcx.mk_int(IntTy::Signed(64));
            let result = i.new_hole_ty();
            let args = [Argument::unknown(tcx.mk_tuple(&[index])), literal(tcx, 0)];
            assert_eq!(
                apply(&mut i, tcx, TensorFn::DynamicIota, result, &args).unwrap(),
                RuleStatus::Deferred
            );
            assert_eq!(
                super::apply(
                    &mut i,
                    tcx,
                    TensorFn::DynamicIota,
                    result,
                    &args,
                    Phase::DefaultRuntime
                )
                .unwrap(),
                RuleStatus::Deferred
            );
            let view = i.tensor_view(result).unwrap();
            assert_eq!(
                i.shape_dims(view.shape),
                Some(vec![Dim::Known(DYNAMIC_EXTENT)])
            );
            assert!(matches!(i.resolve(view.elem).kind(), TyKind::Hole(_)));
            i.unify(view.elem, index).unwrap();
            assert_eq!(
                apply(&mut i, tcx, TensorFn::DynamicIota, result, &args).unwrap(),
                RuleStatus::Solved
            );
            assert_eq!(i.resolve(result), tcx.mk_tensor(index, &[DYNAMIC_EXTENT]));
            let unrelated = i.new_hole_ty();
            assert_eq!(
                super::apply(
                    &mut i,
                    tcx,
                    TensorFn::Fill,
                    unrelated,
                    &[Argument::unknown(index)],
                    Phase::DefaultRuntime
                )
                .unwrap(),
                RuleStatus::Deferred
            );
            let view = i.tensor_view(unrelated).unwrap();
            assert_eq!(i.shape_dims(view.shape), None);
        });
    }

    #[test]
    fn iota_axis_only_imposes_minimum_rank_and_never_guesses_shape() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let out = i.new_hole_ty();
            let arg = literal(tcx, 2);
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, out, &[arg.clone()]).unwrap(),
                RuleStatus::Deferred
            );
            let shape = i.tensor_view(out).unwrap().shape;
            assert_eq!(i.shape_rank_min(shape), 3);
            assert_eq!(i.shape_dims(shape), None);
            let revision = i.progress();
            apply(&mut i, tcx, TensorFn::Iota, out, &[arg.clone()]).unwrap();
            assert_eq!(revision, i.progress());
            assert!(
                i.unify(out, tcx.mk_tensor(tcx.mk_int(IntTy::Signed(32)), &[2, 3]))
                    .is_err()
            );
            i.unify(
                out,
                tcx.mk_tensor(tcx.mk_int(IntTy::Signed(32)), &[2, 3, 4]),
            )
            .unwrap();
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, out, &[arg]).unwrap(),
                RuleStatus::Solved
            );
            let mut i = InferCtxt::new(tcx);
            let out = i.new_hole_ty();
            let shape = Argument {
                ty: tcx.mk_tuple(&[tcx.mk_int(IntTy::Signed(64))]),
                value: Some(Value::Tuple(vec![literal(tcx, 4)])),
            };
            apply(
                &mut i,
                tcx,
                TensorFn::DynamicIota,
                out,
                &[shape, literal(tcx, 0)],
            )
            .unwrap();
            let shape = i.tensor_view(out).unwrap().shape;
            assert_eq!(i.shape_dims(shape), Some(vec![Dim::Known(4)]));
        });
    }
    #[test]
    fn all_four_dot_rank_relations_infer_forward() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            for (a, b, out) in [
                (vec![3], vec![3], vec![]),
                (vec![3], vec![3, 4], vec![4]),
                (vec![2, 3], vec![3], vec![2]),
                (vec![2, 3], vec![3, 4], vec![2, 4]),
            ] {
                let mut i = InferCtxt::new(tcx);
                let result = i.new_hole_ty();
                assert_eq!(
                    run(
                        &mut i,
                        tcx,
                        TensorFn::Dot,
                        result,
                        &[tcx.mk_tensor(f, &a), tcx.mk_tensor(f, &b)]
                    )
                    .unwrap(),
                    RuleStatus::Solved
                );
                assert_eq!(i.resolve(result), tcx.mk_tensor(f, &out));
            }
        });
    }
    #[test]
    fn dot_backward_inference_commits_only_unique_rank_alternative() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let mut i = InferCtxt::new(tcx);
            let args = unknowns(&mut i, 2);
            let out = tcx.mk_tensor(f, &[7]);
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Dot, out, &args).unwrap(),
                RuleStatus::Deferred
            );
            for arg in &args {
                let v = i.tensor_view(arg.ty).unwrap();
                assert_eq!(i.shape_dims(v.shape), None);
            }
            i.unify(args[0].ty, tcx.mk_tensor(f, &[4])).unwrap();
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Dot, out, &args).unwrap(),
                RuleStatus::Solved
            );
            assert_eq!(i.resolve(args[1].ty), tcx.mk_tensor(f, &[4, 7]));
            let mut i = InferCtxt::new(tcx);
            let a = i.new_hole_ty();
            run(
                &mut i,
                tcx,
                TensorFn::Dot,
                tcx.mk_tensor(f, &[2, 4]),
                &[a, tcx.mk_tensor(f, &[3, 4])],
            )
            .unwrap();
            assert_eq!(i.resolve(a), tcx.mk_tensor(f, &[2, 3]));
        });
    }
    #[test]
    fn contradictory_dot_candidates_do_not_leave_speculative_dimensions() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let mut i = InferCtxt::new(tcx);
            let out = i.new_hole_ty();
            assert!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Dot,
                    out,
                    &[tcx.mk_tensor(f, &[3]), tcx.mk_tensor(f, &[4])]
                )
                .is_err()
            );
            let shape = i.tensor_view(out).unwrap().shape;
            assert_eq!(i.shape_dims(shape), None);
        });
    }
    #[test]
    fn runtime_contraction_does_not_solve_static_dimension_meta() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let mut i = InferCtxt::new(tcx);
            let d = i.new_dim();
            let s = i.ranked_shape(vec![d, Dim::Known(4)]);
            let right = i.new_tensor_ty(f, s);
            assert_eq!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Dot,
                    tcx.mk_tensor(f, &[2, 4]),
                    &[tcx.mk_tensor(f, &[2, DYNAMIC_EXTENT]), right]
                )
                .unwrap(),
                RuleStatus::Deferred
            );
            assert_eq!(i.resolve_dim(d), d);
        });
    }
    #[test]
    fn transpose_flows_both_directions_and_preserves_dynamic_metadata() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let mut i = InferCtxt::new(tcx);
            let input = i.new_hole_ty();
            run(
                &mut i,
                tcx,
                TensorFn::Transpose,
                tcx.mk_tensor(f, &[3, 2]),
                &[input],
            )
            .unwrap();
            assert_eq!(i.resolve(input), tcx.mk_tensor(f, &[2, 3]));
            let out = i.new_hole_ty();
            let input = tcx.mk_tensor(f, &[DYNAMIC_EXTENT, 3]);
            run(&mut i, tcx, TensorFn::Transpose, out, &[input]).unwrap();
            let view = i.tensor_view(out).unwrap();
            assert!(matches!(
                i.shape_dims(view.shape).unwrap().as_slice(),
                [Dim::Known(3), Dim::Var(_)]
            ));
            super::apply(
                &mut i,
                tcx,
                TensorFn::Transpose,
                out,
                &[Argument::unknown(input)],
                Phase::DefaultRuntime,
            )
            .unwrap();
            assert_eq!(i.resolve(out), tcx.mk_tensor(f, &[3, DYNAMIC_EXTENT]));
            run(
                &mut i,
                tcx,
                TensorFn::Transpose,
                tcx.mk_tensor(f, &[3, 4]),
                &[input],
            )
            .unwrap();
        });
    }
    #[test]
    fn shape_copying_waits_for_later_static_context_before_dynamic_defaulting() {
        with_tcx(|tcx| {
            let f = tcx.mk_fp(FpTy::Ieee(32));
            for op in [TensorFn::Transpose, TensorFn::Dot] {
                let mut i = InferCtxt::new(tcx);
                let result = i.new_hole_ty();
                let (args, expected) = if op == TensorFn::Transpose {
                    (
                        vec![tcx.mk_tensor(f, &[DYNAMIC_EXTENT, 3])],
                        tcx.mk_tensor(f, &[3, 4]),
                    )
                } else {
                    (
                        vec![
                            tcx.mk_tensor(f, &[DYNAMIC_EXTENT, 3]),
                            tcx.mk_tensor(f, &[3, 4]),
                        ],
                        tcx.mk_tensor(f, &[2, 4]),
                    )
                };
                assert_eq!(
                    run(&mut i, tcx, op, result, &args).unwrap(),
                    RuleStatus::Deferred
                );
                let revision = i.progress();
                assert_eq!(
                    run(&mut i, tcx, op, result, &args).unwrap(),
                    RuleStatus::Deferred
                );
                assert_eq!(revision, i.progress());
                i.unify(result, expected).unwrap();
                let args = args.into_iter().map(Argument::unknown).collect::<Vec<_>>();
                assert_eq!(
                    super::apply(&mut i, tcx, op, result, &args, Phase::DefaultRuntime).unwrap(),
                    RuleStatus::Solved
                );
                assert_eq!(i.resolve(result), expected);
            }
        });
    }

    #[test]
    fn select_does_not_guess_scalar_predicate_and_uses_later_rank_evidence() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let value = tcx.mk_tensor(tcx.mk_bool(), &[3]);
            let predicate = i.new_hole_ty();
            assert_eq!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Select,
                    value,
                    &[predicate, value, value]
                )
                .unwrap(),
                RuleStatus::Deferred
            );
            let s = i.tensor_view(predicate).unwrap().shape;
            assert_eq!(i.shape_dims(s), None);
            i.require_rank_at_least(s, 1).unwrap();
            assert_eq!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Select,
                    value,
                    &[predicate, value, value]
                )
                .unwrap(),
                RuleStatus::Solved
            );
            assert_eq!(i.resolve(predicate), value);
        });
    }
    #[test]
    fn boundaries_async_and_slice_have_explicit_relations() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let f = tcx.mk_fp(FpTy::Ieee(32));
            let t = tcx.mk_tensor(f, &[5, 6]);
            let a = tcx.mk_array(f, &[5, 6]);
            let out = i.new_hole_ty();
            run(&mut i, tcx, TensorFn::Of, out, &[a]).unwrap();
            assert_eq!(i.resolve(out), t);
            let out = i.new_hole_ty();
            run(&mut i, tcx, TensorFn::Materialize, out, &[t]).unwrap();
            assert_eq!(i.resolve(out), a);
            let out = i.new_hole_ty();
            run(
                &mut i,
                tcx,
                TensorFn::AsyncDone,
                out,
                &[tcx.mk_future(&[t])],
            )
            .unwrap();
            assert_eq!(i.resolve(out), t);
            let dim = i.new_hole_ty();
            run(
                &mut i,
                tcx,
                TensorFn::Dim,
                dim,
                &[t, tcx.mk_int(IntTy::Unsigned(64))],
            )
            .unwrap();
            assert_eq!(i.resolve(dim), tcx.mk_int(IntTy::Unsigned(64)));
            for op in [TensorFn::Slice, TensorFn::DynamicSlice] {
                let input = i.new_hole_ty();
                let out = tcx.mk_tensor(f, &[2, 3]);
                run(
                    &mut i,
                    tcx,
                    op,
                    out,
                    &[input, tcx.mk_tuple(&[tcx.mk_int(IntTy::Signed(64)); 2])],
                )
                .unwrap();
                let view = i.tensor_view(input).unwrap();
                assert_eq!(i.shape_dims(view.shape).unwrap().len(), 2);
                assert_eq!(i.resolve(view.elem), f);
            }
            let out = i.new_hole_ty();
            run(
                &mut i,
                tcx,
                TensorFn::CollectivePermute,
                out,
                &[t, tcx.mk_unit()],
            )
            .unwrap();
            assert_eq!(i.resolve(out), t);
        });
    }
    #[test]
    fn opaque_generic_results_still_constrain_metadata_and_exact_identities() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let generic = tcx.mk_generic(crate::semi::ty::GenericId(0));
            let tensor = tcx.mk_tensor(generic, &[]);
            let axis_ty = i.new_hole_ty();
            let axis = Argument {
                ty: axis_ty,
                value: Some(Value::Integer(tcx.alloc(Integer::from(0)))),
            };
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, tensor, &[axis]).unwrap(),
                RuleStatus::Deferred
            );
            assert_eq!(i.resolve(axis_ty), tcx.mk_int(IntTy::Signed(64)));
            let result = i.new_hole_ty();
            assert_eq!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Select,
                    result,
                    &[tcx.mk_tensor(tcx.mk_bool(), &[]), generic, generic]
                )
                .unwrap(),
                RuleStatus::Deferred
            );
            assert_eq!(i.resolve(result), generic);
            let comparison = i.new_hole_ty();
            assert_eq!(
                run(&mut i, tcx, TensorFn::Eq, comparison, &[tensor, tensor]).unwrap(),
                RuleStatus::Deferred
            );
            // The deferred shape contract must not prevent a later rigid
            // return annotation from naming the result in a generic wrapper.
            i.unify(comparison, generic).unwrap();
        });
    }

    #[test]
    fn ranked_generic_dtype_relations_wait_for_instantiation() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let generic = tcx.mk_generic(crate::semi::ty::GenericId(0));
            let output = tcx.mk_tensor(generic, &[3]);
            let scalar = tcx.mk_fp(FpTy::Ieee(32));
            assert_eq!(
                run(&mut i, tcx, TensorFn::Fill, output, &[scalar]).unwrap(),
                RuleStatus::Deferred
            );
            let input = tcx.mk_tensor(generic, &[]);
            assert_eq!(
                run(&mut i, tcx, TensorFn::Fill, output, &[input]).unwrap(),
                RuleStatus::Deferred
            );
            assert!(
                run(
                    &mut i,
                    tcx,
                    TensorFn::Fill,
                    tcx.mk_tensor(tcx.mk_bool(), &[3]),
                    &[scalar]
                )
                .is_err()
            );
        });
    }

    #[test]
    fn rigid_generic_formers_remain_deferred_and_wrong_families_are_rejected() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let generic = tcx.mk_generic(crate::semi::ty::GenericId(0));
            let t = tcx.mk_tensor(generic, &[]);
            assert_eq!(
                run(&mut i, tcx, TensorFn::Neg, t, &[t]).unwrap(),
                RuleStatus::Deferred
            );
            let s = i.new_shape();
            let array = i.new_array_ty(tcx.mk_bool(), s);
            let result = i.new_hole_ty();
            assert!(run(&mut i, tcx, TensorFn::Neg, result, &[array]).is_err());
        });
    }
}
