//! A total structural substituter over `Ty` for trait machinery.
//!
//! Unlike `full::subst::subst_ty` — which panics on an unmapped generic
//! (monomorphization must ground everything) — this walker lets unmapped
//! generics pass through: conformance checking substitutes only the trait's
//! binder while the member's own generics stay rigid.

use rustc_hash::FxHashMap;

use reussir_syntax::kind::TokenKey;

use crate::semi::ty::{DefId, GenericId, Ty, TyCtxt, TyKind};

pub fn replace_generics<'tcx>(
    tcx: &TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    map: &FxHashMap<GenericId, Ty<'tcx>>,
) -> Ty<'tcx> {
    rebuild(tcx, ty, &mut |g| map.get(&g).copied(), &mut |_, _, _| None)
}

/// Rebuild `ty` bottom-up: a type parameter becomes `generic(g)` when that
/// is `Some`, and an associated type, once its arguments are rebuilt,
/// becomes `assoc(trait, name, arguments)` when that is `Some`.
pub fn rebuild<'tcx>(
    tcx: &TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    generic: &mut impl FnMut(GenericId) -> Option<Ty<'tcx>>,
    assoc: &mut impl FnMut(DefId, TokenKey, &'tcx [Ty<'tcx>]) -> Option<Ty<'tcx>>,
) -> Ty<'tcx> {
    if let TyKind::Generic(g) = *ty.kind() {
        return generic(g).unwrap_or(ty);
    }
    let rebuilt = ty.map_children(tcx, |child| rebuild(tcx, child, generic, assoc));
    match *rebuilt.kind() {
        TyKind::Assoc {
            trait_def,
            name,
            args,
        } => assoc(trait_def, name, args).unwrap_or(rebuilt),
        _ => rebuilt,
    }
}

/// Collect every generic occurring in `ty` — the E0207 constrainedness walk.
/// An associated type's arguments do not count: naming `T::Name` does not
/// determine `T`.
pub fn collect_generics_of(ty: Ty<'_>, out: &mut rustc_hash::FxHashSet<GenericId>) {
    match *ty.kind() {
        TyKind::Generic(g) => {
            out.insert(g);
        }
        TyKind::Assoc { .. } => {}
        _ => ty
            .children()
            .for_each(|child| collect_generics_of(child, out)),
    }
}
