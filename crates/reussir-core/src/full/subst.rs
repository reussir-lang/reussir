//! Type substitution: ground a Semi [`Ty`] at a generic→type assignment.
//!
//! Monomorphization specializes a polymorphic function by mapping each generic
//! parameter ([`GenericId`]) to a concrete type. Because Full types *are* Semi
//! [`Ty`]s (just ground), substitution re-interns through the same [`TyCtxt`], so
//! the result keeps pointer-identity equality and feeds straight into mangling.
//!
//! Only *types* are substituted here; the expression tree is not rewritten in
//! place — [`crate::full::mono`] *lowers* it into the [`crate::full::mir`],
//! grounding each node's type with [`subst_ty`] as it goes.

use rustc_hash::FxHashMap;

use crate::semi::ty::{GenericId, Ty, TyCtxt, TyKind};

/// A ground assignment for a function's generic parameters, and whether the
/// instance is the atomic lifting of a built-in integer method: such an
/// instance colors every `Nat`/`Int` it mentions as `Arc<Nat>`/`Arc<Int>`.
#[derive(Clone, Default, Debug)]
pub struct Subst<'tcx> {
    generics: FxHashMap<GenericId, Ty<'tcx>>,
    atomic_bigints: bool,
}

impl<'tcx> Subst<'tcx> {
    /// The substitution of an atomic lifted instance (see [`Subst`]).
    pub fn atomic() -> Self {
        Subst {
            generics: FxHashMap::default(),
            atomic_bigints: true,
        }
    }

    pub fn insert(&mut self, id: GenericId, ty: Ty<'tcx>) {
        self.generics.insert(id, ty);
    }

    pub fn get(&self, id: &GenericId) -> Option<&Ty<'tcx>> {
        self.generics.get(id)
    }

    pub fn is_empty(&self) -> bool {
        self.generics.is_empty() && !self.atomic_bigints
    }
}

impl<'tcx> FromIterator<(GenericId, Ty<'tcx>)> for Subst<'tcx> {
    fn from_iter<I: IntoIterator<Item = (GenericId, Ty<'tcx>)>>(iter: I) -> Self {
        Subst {
            generics: iter.into_iter().collect(),
            atomic_bigints: false,
        }
    }
}

/// Substitute generics in a type, re-interning into `tcx`. An empty `subst`
/// short-circuits (the type is already ground).
pub fn subst_ty<'tcx>(tcx: &TyCtxt<'tcx>, ty: Ty<'tcx>, subst: &Subst<'tcx>) -> Ty<'tcx> {
    if subst.is_empty() {
        return ty;
    }
    match *ty.kind() {
        TyKind::Generic(id) => *subst
            .get(&id)
            .unwrap_or_else(|| panic!("subst: generic {id:?} is not bound by this instance")),
        TyKind::BigInt(_) if subst.atomic_bigints => tcx.mk_arc(ty),
        // Already atomic: lifting does not color it twice.
        TyKind::Arc(inner) if subst.atomic_bigints && matches!(inner.kind(), TyKind::BigInt(_)) => {
            ty
        }
        TyKind::Record { def, args, flex } => {
            let args: Vec<Ty<'tcx>> = args.iter().map(|&a| subst_ty(tcx, a, subst)).collect();
            tcx.mk_record(def, &args, flex)
        }
        TyKind::Closure { params, ret } => {
            let params: Vec<Ty<'tcx>> = params.iter().map(|&p| subst_ty(tcx, p, subst)).collect();
            tcx.mk_closure(&params, subst_ty(tcx, ret, subst))
        }
        TyKind::Nullable(inner) => tcx.mk_nullable(subst_ty(tcx, inner, subst)),
        TyKind::Cell { elem, kind } => tcx.mk_cell(subst_ty(tcx, elem, subst), kind),
        TyKind::Arc(inner) => tcx.mk_arc(subst_ty(tcx, inner, subst)),
        TyKind::ConstSeq(elem) => tcx.mk_const_seq(subst_ty(tcx, elem, subst)),
        TyKind::Array { elem, dims } => tcx.mk_array(subst_ty(tcx, elem, subst), dims),
        TyKind::DevArray {
            elem,
            dims,
            placement,
        } => tcx.mk_devarray(
            subst_ty(tcx, elem, subst),
            dims,
            Some(subst_ty(tcx, placement, subst)),
        ),
        TyKind::Future(ps) => tcx.mk_future(
            &ps.iter()
                .map(|&p| subst_ty(tcx, p, subst))
                .collect::<Vec<_>>(),
        ),
        TyKind::Tuple(ps) => tcx.mk_tuple(
            &ps.iter()
                .map(|&p| subst_ty(tcx, p, subst))
                .collect::<Vec<_>>(),
        ),
        TyKind::Token => ty,
        TyKind::Tensor { elem, dims } => tcx.mk_tensor(subst_ty(tcx, elem, subst), dims),
        TyKind::DefaultPlacement
        | TyKind::Int(_)
        | TyKind::BigInt(_)
        | TyKind::Fp(_)
        | TyKind::Bool
        | TyKind::Str
        | TyKind::Char
        | TyKind::Unit
        | TyKind::Bottom => ty,
        TyKind::Hole(_) => panic!("subst: an inference hole survived zonking"),
    }
}
