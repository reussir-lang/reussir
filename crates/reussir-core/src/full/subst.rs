//! Type substitution: ground a Semi [`Ty`] at a generic→type assignment.
//!
//! Monomorphization specializes a polymorphic function by mapping each generic
//! parameter ([`GenericId`]) to a concrete type. Because Full types *are* Semi
//! [`Ty`]s (just ground), substitution re-interns through the same [`TyCtxt`], so
//! the result keeps pointer-identity equality and feeds straight into mangling.
//!
//! Only *types* are substituted here; the expression tree is not rewritten in
//! place — [`crate::full::mono`] *lowers* it into the [`crate::full::mir`],
//! grounding each node's type with [`Grounding::ty`] as it goes.

use std::cell::RefCell;

use rustc_hash::FxHashMap;

use crate::semi::traits::{AssocError, TraitDb};
use crate::semi::ty::{GenericId, Ty, TyCtxt, TyKind, mentions_assoc};

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

/// Substitution for monomorphization, where every trait reference is ground
/// once the generics are replaced: [`subst_ty`], then each associated type
/// replaced by the type its impl binds.
pub struct Grounding<'a, 'tcx> {
    pub tcx: &'a TyCtxt<'tcx>,
    /// `None` for an input that carries no trait tables.
    pub traits: Option<&'a TraitDb<'tcx>>,
    /// The first associated type no impl decided, for the caller to report.
    failed: RefCell<Option<(Ty<'tcx>, AssocError<'tcx>)>>,
}

impl<'a, 'tcx> Grounding<'a, 'tcx> {
    pub fn new(tcx: &'a TyCtxt<'tcx>, traits: Option<&'a TraitDb<'tcx>>) -> Self {
        Self {
            tcx,
            traits,
            failed: RefCell::new(None),
        }
    }

    /// `ty` under `subst`, ground. An associated type no impl decides
    /// makes it the error type and is kept for [`Self::take_failure`].
    pub fn ty(&self, ty: Ty<'tcx>, subst: &Subst<'tcx>) -> Ty<'tcx> {
        let ty = subst_ty(self.tcx, ty, subst);
        if !mentions_assoc(ty) {
            return ty;
        }
        let normalized = match self.traits {
            Some(db) => db.normalize(self.tcx, &[], ty),
            None => Err((ty, AssocError::Undeclared)),
        };
        normalized.unwrap_or_else(|failure| {
            self.failed.borrow_mut().get_or_insert(failure);
            self.tcx.mk(TyKind::Bottom)
        })
    }

    pub fn take_failure(&self) -> Option<(Ty<'tcx>, AssocError<'tcx>)> {
        self.failed.borrow_mut().take()
    }
}

/// Substitute generics in a type, re-interning into `tcx`. An empty `subst`
/// short-circuits (the type is already ground). Associated types keep their
/// form with their arguments substituted; [`Grounding::ty`] resolves them.
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
        TyKind::Hole(_) => panic!("subst: an inference hole survived zonking"),
        _ => ty.map_children(tcx, |child| subst_ty(tcx, child, subst)),
    }
}
