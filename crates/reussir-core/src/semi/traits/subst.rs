//! A total structural substituter over `Ty` for trait machinery.
//!
//! Unlike `full::subst::subst_ty` — which panics on an unmapped generic
//! (monomorphization must ground everything) — this walker lets unmapped
//! generics pass through: conformance checking substitutes only the trait's
//! binder while the member's own generics stay rigid.

use rustc_hash::FxHashMap;

use crate::semi::ty::{GenericId, Ty, TyCtxt, TyKind};

pub fn replace_generics<'tcx>(
    tcx: &TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    map: &FxHashMap<GenericId, Ty<'tcx>>,
) -> Ty<'tcx> {
    rebuild(tcx, ty, &mut |g| map.get(&g).copied(), &mut |assoc| assoc)
}

/// Rebuild `ty` bottom-up: a type parameter becomes `generic(g)` when that
/// is `Some`, and an associated type, once its arguments are rebuilt,
/// becomes `assoc(it)`.
pub fn rebuild<'tcx>(
    tcx: &TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    generic: &mut impl FnMut(GenericId) -> Option<Ty<'tcx>>,
    assoc: &mut impl FnMut(Ty<'tcx>) -> Ty<'tcx>,
) -> Ty<'tcx> {
    match *ty.kind() {
        TyKind::Generic(g) => generic(g).unwrap_or(ty),
        TyKind::Assoc {
            trait_def,
            name,
            args,
        } => {
            let new: Vec<Ty> = args
                .iter()
                .map(|&a| rebuild(tcx, a, generic, assoc))
                .collect();
            assoc(tcx.mk_assoc(trait_def, name, &new))
        }
        TyKind::Nullable(inner) => {
            let n = rebuild(tcx, inner, generic, assoc);
            if n == inner { ty } else { tcx.mk_nullable(n) }
        }
        TyKind::Arc(inner) => {
            let n = rebuild(tcx, inner, generic, assoc);
            if n == inner { ty } else { tcx.mk_arc(n) }
        }
        TyKind::ConstSeq(elem) => {
            let n = rebuild(tcx, elem, generic, assoc);
            if n == elem { ty } else { tcx.mk_const_seq(n) }
        }
        TyKind::Ptr { space, kind } => {
            let n = rebuild(tcx, space, generic, assoc);
            if n == space { ty } else { tcx.mk_ptr(n, kind) }
        }
        TyKind::Cell { elem, kind } => {
            let n = rebuild(tcx, elem, generic, assoc);
            if n == elem { ty } else { tcx.mk_cell(n, kind) }
        }
        TyKind::Record { def, args, flex } => {
            let new: Vec<Ty> = args
                .iter()
                .map(|&a| rebuild(tcx, a, generic, assoc))
                .collect();
            if new.iter().zip(args.iter()).all(|(a, b)| a == b) {
                ty
            } else {
                tcx.mk_record(def, &new, flex)
            }
        }
        TyKind::Closure { params, ret } => {
            let new: Vec<Ty> = params
                .iter()
                .map(|&p| rebuild(tcx, p, generic, assoc))
                .collect();
            let r = rebuild(tcx, ret, generic, assoc);
            if r == ret && new.iter().zip(params.iter()).all(|(a, b)| a == b) {
                ty
            } else {
                tcx.mk_closure(&new, r)
            }
        }
        TyKind::DevArray {
            elem,
            dims,
            placement,
        } => {
            let elem = rebuild(tcx, elem, generic, assoc);
            let placement = Some(rebuild(tcx, placement, generic, assoc));
            tcx.mk_devarray(elem, dims, placement)
        }
        TyKind::Tuple(fields) => tcx.mk_tuple(
            &fields
                .iter()
                .map(|&t| rebuild(tcx, t, generic, assoc))
                .collect::<Vec<_>>(),
        ),
        TyKind::Future(ps) => tcx.mk_future(
            &ps.iter()
                .map(|&p| rebuild(tcx, p, generic, assoc))
                .collect::<Vec<_>>(),
        ),
        TyKind::Tensor { elem, dims } => {
            let elem = rebuild(tcx, elem, generic, assoc);
            tcx.mk_tensor(elem, dims)
        }
        TyKind::Array { elem, dims } => {
            let n = rebuild(tcx, elem, generic, assoc);
            if n == elem { ty } else { tcx.mk_array(n, dims) }
        }
        _ => ty,
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
        TyKind::Nullable(inner) | TyKind::Arc(inner) | TyKind::ConstSeq(inner) => {
            collect_generics_of(inner, out)
        }
        TyKind::Cell { elem, .. } => collect_generics_of(elem, out),
        TyKind::Ptr { space, .. } => collect_generics_of(space, out),
        TyKind::Record { args, .. } => args.iter().for_each(|&a| collect_generics_of(a, out)),
        TyKind::Closure { params, ret } => {
            params.iter().for_each(|&p| collect_generics_of(p, out));
            collect_generics_of(ret, out);
        }
        TyKind::DevArray {
            elem, placement, ..
        } => {
            collect_generics_of(elem, out);
            collect_generics_of(placement, out);
        }
        TyKind::Future(ps) | TyKind::Tuple(ps) => {
            ps.iter().for_each(|&p| collect_generics_of(p, out))
        }
        TyKind::Array { elem, .. } | TyKind::Tensor { elem, .. } => collect_generics_of(elem, out),
        _ => {}
    }
}
