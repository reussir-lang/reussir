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
    shape::{Dim, ShapeError, ShapeId, TensorView},
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

/// Structured failures from shape-rule propagation. Preserve shape conflicts
/// so callers can inspect their dimensions or ranks without parsing messages.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub(super) enum RuleError {
    #[error("tensor relation has incompatible element or operand types")]
    IncompatibleTypes,
    #[error(transparent)]
    Shape(#[from] ShapeError),
    #[error("expected tensor rank {expected}, found {actual}")]
    Rank { expected: usize, actual: usize },
    #[error("tensor operation requires Tensor operands and result")]
    ExpectedTensor,
    #[error("dot has no compatible vector/matrix rank and dimension combination")]
    IncompatibleDotShapes,
    #[error("cannot infer shape for {operation}: operation has no inference rule")]
    CannotInfer { operation: &'static str },
    #[error("select predicate must be scalar or match the value shape")]
    PredicateShape,
    #[error("expected a scalar or rank-zero Tensor")]
    ExpectedScalar,
    #[error("tensor dimension or axis must be a nonnegative signed 64-bit integer")]
    InvalidInteger,
    #[error("{operation} expects {expected} operands")]
    OperandCount {
        operation: &'static str,
        expected: usize,
        actual: usize,
    },
    #[error("iota axis is too large")]
    IotaAxisOverflow { axis: u64 },
    #[error("iota axis must be within the result rank")]
    IotaAxisOutOfBounds {
        axis: u64,
        #[source]
        source: ShapeError,
    },
    // The shared concrete validator still exposes text diagnostics.
    #[error("{0}")]
    Validation(String),
}

fn equal<'tcx>(i: &mut InferCtxt<'_, 'tcx>, a: Ty<'tcx>, b: Ty<'tcx>) -> Result<(), RuleError> {
    // Rigid declaration formals are relations to replay at instantiation, not
    // assignable inference variables. Concrete disagreements are still errors.
    let left = i.shallow_resolve(a);
    let right = i.shallow_resolve(b);
    if (matches!(left.kind(), TyKind::Generic(_)) && !matches!(right.kind(), TyKind::Hole(_)))
        || (matches!(right.kind(), TyKind::Generic(_)) && !matches!(left.kind(), TyKind::Hole(_)))
    {
        return Ok(());
    }
    i.unify(a, b).map_err(|_| RuleError::IncompatibleTypes)
}
fn exact(i: &mut InferCtxt<'_, '_>, a: ShapeId, b: ShapeId) -> Result<(), RuleError> {
    i.unify_shapes(a, b).map_err(RuleError::from)
}
fn rank(i: &mut InferCtxt<'_, '_>, shape: ShapeId, n: usize) -> Result<Vec<Dim>, RuleError> {
    if let Some(ds) = i.shape_dims(shape) {
        return if ds.len() == n {
            Ok(ds)
        } else {
            Err(RuleError::Rank {
                expected: n,
                actual: ds.len(),
            })
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
) -> Result<Option<TensorView<'tcx>>, RuleError> {
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
    Err(RuleError::ExpectedTensor)
}
fn matching<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    a: TensorView<'tcx>,
    b: TensorView<'tcx>,
) -> Result<(), RuleError> {
    equal(i, a.elem, b.elem)?;
    exact(i, a.shape, b.shape)
}

fn compatible(i: &mut InferCtxt<'_, '_>, a: Dim, b: Dim) -> Result<(), RuleError> {
    match (i.resolve_dim(a), i.resolve_dim(b)) {
        (Dim::Known(DYNAMIC_EXTENT), _) | (_, Dim::Known(DYNAMIC_EXTENT)) => Ok(()),
        (a, b) => i.unify_dims(a, b).map_err(RuleError::from),
    }
}
/// Copy input metadata to an unknown result; explicit result refinements are
/// checked for compatibility rather than replacing an input's runtime extent.
fn output_dim(
    i: &mut InferCtxt<'_, '_>,
    input: Dim,
    output: Dim,
    phase: Phase,
) -> Result<(), RuleError> {
    match (i.resolve_dim(input), i.resolve_dim(output)) {
        (Dim::Known(DYNAMIC_EXTENT), Dim::Var(_)) if phase == Phase::Propagate => Ok(()),
        (Dim::Known(n), Dim::Var(_)) => {
            i.unify_dims(output, Dim::Known(n)).map_err(RuleError::from)
        }
        (a, b) => compatible(i, a, b),
    }
}
fn dot_case(
    i: &mut InferCtxt<'_, '_>,
    a: ShapeId,
    b: ShapeId,
    out: ShapeId,
    ar: usize,
    br: usize,
    phase: Phase,
) -> Result<(), RuleError> {
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
) -> Result<RuleStatus, RuleError> {
    i.require_rank_at_least(a, 1).map_err(RuleError::from)?;
    i.require_rank_at_least(b, 1).map_err(RuleError::from)?;
    let candidates = [(1, 1), (1, 2), (2, 1), (2, 2)]
        .into_iter()
        .filter(|&(ar, br)| viable(i, |i| dot_case(i, a, b, out, ar, br, phase)))
        .collect::<Vec<_>>();
    match candidates.as_slice() {
        [] => Err(RuleError::IncompatibleDotShapes),
        &[(ar, br)] => {
            dot_case(i, a, b, out, ar, br, phase)?;
            Ok(RuleStatus::Solved)
        }
        _ => Ok(RuleStatus::Deferred),
    }
}
fn viable<'a, 'tcx>(
    i: &mut InferCtxt<'a, 'tcx>,
    f: impl FnOnce(&mut InferCtxt<'a, 'tcx>) -> Result<(), RuleError>,
) -> bool {
    // probe commits Ok; wrapping the outcome in Err rolls back every candidate.
    i.probe(|i| Err::<(), _>(f(i))).unwrap_err().is_ok()
}
fn select_predicate(
    i: &mut InferCtxt<'_, '_>,
    predicate: ShapeId,
    value: ShapeId,
) -> Result<RuleStatus, RuleError> {
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
        (false, false) => Err(RuleError::PredicateShape),
    }
}
fn scalar_element<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    arg: &Argument<'tcx>,
) -> Result<Option<Ty<'tcx>>, RuleError> {
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
        _ => Err(RuleError::ExpectedScalar),
    }
}
fn integer(arg: &Argument<'_>) -> Result<Option<u64>, RuleError> {
    match arg.value {
        Some(Value::Integer(n)) => u64::try_from(n)
            .ok()
            .filter(|&n| n <= i64::MAX as u64)
            .map(Some)
            .ok_or(RuleError::InvalidInteger),
        _ => Ok(None),
    }
}
fn open(t: Ty<'_>) -> bool {
    match t.kind() {
        TyKind::Hole(_) | TyKind::Generic(_) => true,
        TyKind::Tensor { elem, .. } | TyKind::Array { elem, .. } => open(*elem),
        TyKind::Future(ts) | TyKind::Tuple(ts) => ts.iter().any(|&t| open(t)),
        _ => false,
    }
}

/// Existing operations without a shape rule keep their declared type checking.
/// New domain rules opt in here as they are introduced.
pub(in crate::semi) fn supports(op: TensorFn) -> bool {
    use TensorFn::*;
    matches!(
        op,
        Dot | Transpose
            | Fill
            | Iota
            | Add
            | Sub
            | Mul
            | Div
            | Rem
            | Neg
            | Abs
            | Min
            | Max
            | Eq
            | Ne
            | Lt
            | Le
            | Gt
            | Ge
            | Select
    )
}

pub(super) fn apply<'tcx>(
    i: &mut InferCtxt<'_, 'tcx>,
    tcx: &TyCtxt<'tcx>,
    op: TensorFn,
    result: Ty<'tcx>,
    args: &[Argument<'tcx>],
    phase: Phase,
) -> Result<RuleStatus, RuleError> {
    use TensorFn::*;
    let count = match op {
        Of | Materialize | AsyncDone => 1,
        _ => op.arity(),
    };
    if args.len() != count {
        return Err(RuleError::OperandCount {
            operation: op.as_str(),
            expected: count,
            actual: args.len(),
        });
    }
    let mut status = RuleStatus::Solved;
    // Preserve exact result identities before introducing a Tensor family.
    // In a polymorphic body the operand may still be a rigid whole type V;
    // assigning its identity to a fresh result is valid without guessing V.
    match op {
        Add | Sub | Mul | Div | Rem | Neg | Abs | Min | Max => equal(i, result, args[0].ty)?,
        Select => equal(i, result, args[1].ty)?,
        Of | Materialize | Dim | DynamicSlice | Slice | AsyncDone | Fill | DynamicFill | Iota
        | DynamicIota | Eq | Ne | Lt | Le | Gt | Ge | Dot | Transpose | CollectivePermute => {}
    }
    match op {
        Fill | Iota => {
            if op == Iota {
                equal(i, args[0].ty, tcx.mk_int(IntTy::Signed(64)))?;
            }
            let Some(output) = tensor(i, result)? else {
                return Ok(RuleStatus::Deferred);
            };
            if op == Fill {
                match scalar_element(i, &args[0])? {
                    Some(elem) => equal(i, elem, output.elem)?,
                    None => status = RuleStatus::Deferred,
                }
            } else if let Some(axis) = integer(&args[0])? {
                let minimum = usize::try_from(axis)
                    .ok()
                    .and_then(|n| n.checked_add(1))
                    .ok_or(RuleError::IotaAxisOverflow { axis })?;
                i.require_rank_at_least(output.shape, minimum)
                    .map_err(|source| RuleError::IotaAxisOutOfBounds { axis, source })?;
            } else {
                status = RuleStatus::Deferred;
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
        Add | Sub | Mul | Div | Rem | Neg | Abs | Min | Max | Eq | Ne | Lt | Le | Gt | Ge
        | Select => {
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
                for arg in &args[first + 1..] {
                    if let Some(b) = tensor(i, arg.ty)? {
                        matching(i, a, b)?;
                    } else {
                        status = RuleStatus::Deferred;
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
        Of | Materialize | Dim | AsyncDone | DynamicFill | DynamicIota | DynamicSlice | Slice
        | CollectivePermute => {
            return Err(RuleError::CannotInfer {
                operation: op.as_str(),
            });
        }
    }
    let result = i.resolve(result);
    let types = args.iter().map(|a| i.resolve(a.ty)).collect::<Vec<_>>();
    crate::tensor_check::operation(op, result, &types).map_err(RuleError::Validation)?;
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
    ) -> Result<RuleStatus, RuleError> {
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
    ) -> Result<RuleStatus, RuleError> {
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
    fn unsupported_operations_return_cannot_infer_without_changing_inference() {
        with_tcx(|tcx| {
            use TensorFn::*;
            for op in [
                Of,
                Materialize,
                Dim,
                AsyncDone,
                DynamicFill,
                DynamicIota,
                DynamicSlice,
                Slice,
                CollectivePermute,
            ] {
                assert!(!supports(op));
                for phase in [Phase::Propagate, Phase::DefaultRuntime] {
                    let mut i = InferCtxt::new(tcx);
                    let result = i.new_hole_ty();
                    let count = match op {
                        Of | Materialize | AsyncDone => 1,
                        _ => op.arity(),
                    };
                    let args = unknowns(&mut i, count);
                    let revision = i.progress();
                    let err = super::apply(&mut i, tcx, op, result, &args, phase).unwrap_err();
                    assert_eq!(
                        err,
                        RuleError::CannotInfer {
                            operation: op.as_str()
                        }
                    );
                    assert_eq!(
                        err.to_string(),
                        format!(
                            "cannot infer shape for {}: operation has no inference rule",
                            op.as_str()
                        ),
                    );
                    assert_eq!(i.progress(), revision);
                    assert_eq!(i.resolve(result), result);
                    for arg in args {
                        assert_eq!(i.resolve(arg.ty), arg.ty);
                    }
                }
            }
        });
    }

    #[test]
    fn operation_errors_preserve_operand_counts_and_invalid_axes() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let output = tcx.mk_tensor(tcx.mk_int(IntTy::Signed(32)), &[4]);
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, output, &[]),
                Err(RuleError::OperandCount {
                    operation: TensorFn::Iota.as_str(),
                    expected: 1,
                    actual: 0,
                }),
            );
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, output, &[literal(tcx, -1)]),
                Err(RuleError::InvalidInteger),
            );
            assert_eq!(
                apply(&mut i, tcx, TensorFn::Iota, output, &[literal(tcx, 1)]),
                Err(RuleError::IotaAxisOutOfBounds {
                    axis: 1,
                    source: ShapeError::MinimumRank {
                        minimum: 2,
                        actual: 1
                    },
                }),
            );
        });
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
}

#[cfg(test)]
mod framework_tests {
    use super::*;
    use crate::{semi::ty::GenericId, with_tcx};

    #[test]
    fn rigid_contracts_defer_but_concrete_disagreements_fail() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let generic = tcx.mk_generic(GenericId(0));
            let hole = i.new_hole_ty();
            equal(&mut i, hole, generic).unwrap();
            assert_eq!(i.resolve(hole), generic);
            equal(&mut i, generic, tcx.mk_bool()).unwrap();
            assert!(open(i.resolve(hole)));
            assert_eq!(
                equal(&mut i, tcx.mk_bool(), tcx.mk_unit()),
                Err(RuleError::IncompatibleTypes)
            );
        });
    }

    #[test]
    fn tensor_refinement_preserves_unknown_rank_and_reuses_variables() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let ty = i.new_hole_ty();
            let view = tensor(&mut i, ty).unwrap().unwrap();
            assert_eq!(i.shape_dims(view.shape), None);
            let revision = i.progress();
            tensor(&mut i, ty).unwrap().unwrap();
            assert_eq!(i.progress(), revision);

            let concrete = tcx.mk_tensor(tcx.mk_bool(), &[2, 3]);
            let other = tensor(&mut i, concrete).unwrap().unwrap();
            matching(&mut i, view, other).unwrap();
            assert_eq!(i.resolve(ty), concrete);
            assert!(!open(i.resolve(ty)));
            assert_eq!(
                tensor(&mut i, tcx.mk_unit()),
                Err(RuleError::ExpectedTensor)
            );
            assert!(
                tensor(&mut i, tcx.mk_generic(GenericId(0)))
                    .unwrap()
                    .is_none()
            );
        });
    }

    #[test]
    fn rank_refinement_is_stable_and_respects_minimum_rank() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let shape = i.new_shape();
            i.require_rank_at_least(shape, 2).unwrap();
            assert!(!viable(&mut i, |i| rank(i, shape, 1).map(|_| ())));
            assert_eq!(i.shape_dims(shape), None);
            rank(&mut i, shape, 2).unwrap();
            let revision = i.progress();
            rank(&mut i, shape, 2).unwrap();
            assert_eq!(i.progress(), revision);
            assert_eq!(
                rank(&mut i, shape, 3),
                Err(RuleError::Rank {
                    expected: 3,
                    actual: 2
                })
            );
        });
    }

    #[test]
    fn shape_conflicts_preserve_their_category_and_dimensions() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let left = i.ranked_shape(vec![Dim::Known(2)]);
            let right = i.ranked_shape(vec![Dim::Known(3)]);
            let err = exact(&mut i, left, right).unwrap_err();
            assert_eq!(
                err,
                RuleError::Shape(ShapeError::Dimension { left: 2, right: 3 })
            );
            assert_eq!(err.to_string(), "incompatible dimensions 2 and 3");
        });
    }

    #[test]
    fn alternatives_roll_back_type_and_shape_effects_on_success_and_failure() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let ty = i.new_hole_ty();
            let shape = i.new_shape();
            let revision = i.progress();
            for succeeds in [true, false] {
                assert_eq!(
                    viable(&mut i, |i| {
                        equal(i, ty, tcx.mk_bool())?;
                        rank(i, shape, 2)?;
                        if succeeds {
                            Ok(())
                        } else {
                            Err(RuleError::IncompatibleTypes)
                        }
                    }),
                    succeeds,
                );
                assert_eq!(i.resolve(ty), ty);
                assert_eq!(i.shape_dims(shape), None);
                assert_eq!(i.progress(), revision);
            }
        });
    }
}
