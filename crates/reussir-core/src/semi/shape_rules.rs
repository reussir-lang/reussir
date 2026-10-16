//! Shared primitives for deferred tensor shape inference rules.
//!
//! Rules report whether their constraints are solved or need more evidence.
//! These helpers preserve rigid generic contracts, refine inference-only
//! tensor shapes, and probe alternatives without committing their effects.
//! Operation dispatch and operation-specific constraints are added separately.
use crate::semi::infer::{
    InferCtxt,
    shape::{Dim, ShapeError, ShapeId, TensorView},
};
use crate::semi::ty::{Ty, TyKind};

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

fn viable<'a, 'tcx>(
    i: &mut InferCtxt<'a, 'tcx>,
    f: impl FnOnce(&mut InferCtxt<'a, 'tcx>) -> Result<(), RuleError>,
) -> bool {
    // probe commits Ok; wrapping the outcome in Err rolls back every candidate.
    i.probe(|i| Err::<(), _>(f(i))).unwrap_err().is_ok()
}
fn open(t: Ty<'_>) -> bool {
    match t.kind() {
        TyKind::Hole(_) | TyKind::Generic(_) => true,
        TyKind::Tensor { elem, .. } | TyKind::Array { elem, .. } => open(*elem),
        TyKind::Future(ts) | TyKind::Tuple(ts) => ts.iter().any(|&t| open(t)),
        _ => false,
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
