//! Coherence: the head keying and the unification engine behind the overlap
//! and orphan gates (*agentic-note*: design-trait-system-s5-2).
//!
//! One matching engine serves two callers with different variable sets:
//! coherence's overlap check passes BOTH impls' generics as unifiable —
//! `Pair<T, i32>` vs `Pair<u8, U>` overlap exactly because a two-sided
//! unifier maps `T ↦ u8, U ↦ i32`, which one-way matching in either
//! direction misses — while selection passes only the candidate's generics,
//! making the same routine a one-way matcher:
//!
//! ```text
//!   head(I₁.self) = head(I₂.self)     I₁.trait = I₂.trait
//!   unify_{ᾱ₁ ∪ ᾱ₂}(I₁.args, I₂.args) ⇝ θ
//!  ──────────────────────────────────────────────────────── (overlap)
//!   ᾱ₁. I₁  ♯  ᾱ₂. I₂        — rejected at declaration
//! ```
//!
//! Rejecting every (overlap)-related pair is what entitles selection's
//! (impl) rule (`select.rs`) to commit to its first one-way match: with the
//! goal's generics rigid, two committed candidates for one goal would be a
//! two-sided unifier for their heads.

use rustc_hash::{FxHashMap, FxHashSet};

use crate::semi::ty::{BigIntTy, CellKind, DefId, FpTy, GenericId, IntTy, PtrKind, Ty, TyKind};

/// The head constructor an impl's self type is keyed under —
/// `(TraitId, HeadKey)` buckets make candidate lookup and overlap checking
/// per-head instead of per-program. `None` (no key) means the self type has
/// no ground head: a bare generic (a blanket impl, rejected) or an
/// error-poisoned `Bottom`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum HeadKey {
    Record(DefId),
    Int(IntTy),
    BigInt(BigIntTy),
    Fp(FpTy),
    Bool,
    Str,
    Char,
    Unit,
    Closure,
    Array,
    Tensor,
    Token,
    Future,
    Tuple(usize),
    DevArray,
    DefaultPlacement,
    Cell(CellKind),
    Nullable,
    Arc,
    ConstSeq,
    Ptr(PtrKind),
    Simd,
    GenericAddrSpace,
}

/// The head key of a self type, `None` for headless types
/// (`Generic`/`Hole`/`Bottom`).
pub fn head_key(ty: Ty<'_>) -> Option<HeadKey> {
    Some(match *ty.kind() {
        TyKind::Record { def, .. } => HeadKey::Record(def),
        TyKind::Int(i) => HeadKey::Int(i),
        TyKind::BigInt(b) => HeadKey::BigInt(b),
        TyKind::Fp(f) => HeadKey::Fp(f),
        TyKind::Bool => HeadKey::Bool,
        TyKind::Str => HeadKey::Str,
        TyKind::Char => HeadKey::Char,
        TyKind::Unit => HeadKey::Unit,
        TyKind::Closure { .. } => HeadKey::Closure,
        TyKind::Array { .. } => HeadKey::Array,
        TyKind::Tensor { .. } => HeadKey::Tensor,
        TyKind::Token => HeadKey::Token,
        TyKind::Future(_) => HeadKey::Future,
        TyKind::Tuple(fields) => HeadKey::Tuple(fields.len()),
        TyKind::DevArray { .. } => HeadKey::DevArray,
        TyKind::DefaultPlacement => HeadKey::DefaultPlacement,
        TyKind::Cell { kind, .. } => HeadKey::Cell(kind),
        TyKind::Nullable(_) => HeadKey::Nullable,
        TyKind::Arc(_) => HeadKey::Arc,
        TyKind::ConstSeq(_) => HeadKey::ConstSeq,
        TyKind::Ptr { kind, .. } => HeadKey::Ptr(kind),
        TyKind::GenericAddrSpace => HeadKey::GenericAddrSpace,
        TyKind::Simd { .. } => HeadKey::Simd,
        TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => return None,
    })
}

/// First-order syntactic unification of two argument lists, assigning only
/// the generics in `vars`. `subst` accumulates the assignment
/// (resolve-through on both sides, occurs check on binding). Returns whether
/// the lists unify — for overlap, a `true` is a conflict.
pub fn unify_args<'tcx>(
    a: &[Ty<'tcx>],
    b: &[Ty<'tcx>],
    vars: &FxHashSet<GenericId>,
    subst: &mut FxHashMap<GenericId, Ty<'tcx>>,
) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(&x, &y)| unify_ty(x, y, vars, subst))
}

/// The renaming from `a_vars` to `b_vars` under which `a` equals `b`, or
/// `None` when the lists differ by more than a bijective renaming of their
/// generics. Identifies the same impl head loaded twice under fresh generic
/// ids.
pub fn alpha_renaming<'tcx>(
    a: &[Ty<'tcx>],
    b: &[Ty<'tcx>],
    a_vars: &FxHashSet<GenericId>,
    b_vars: &FxHashSet<GenericId>,
) -> Option<FxHashMap<GenericId, GenericId>> {
    let vars: FxHashSet<GenericId> = a_vars.union(b_vars).copied().collect();
    let mut subst = FxHashMap::default();
    if !unify_args(a, b, &vars, &mut subst) {
        return None;
    }
    // A renaming binds each generic to a generic of the other side, and no
    // two generics to the same one.
    let mut renaming = FxHashMap::default();
    let mut targets = FxHashSet::default();
    for (&g, &t) in &subst {
        let TyKind::Generic(h) = *t.kind() else {
            return None;
        };
        let (from, to) = if a_vars.contains(&g) && b_vars.contains(&h) {
            (g, h)
        } else if b_vars.contains(&g) && a_vars.contains(&h) {
            (h, g)
        } else {
            return None;
        };
        if !targets.insert(to) || renaming.insert(from, to).is_some() {
            return None;
        }
    }
    Some(renaming)
}

/// Chase a binding chain to its representative. Shallow: only a `Generic`
/// *head* is followed; selection grounds whole types by feeding the resolved
/// bindings through `replace_generics`.
pub(crate) fn resolve_binding<'tcx>(
    mut ty: Ty<'tcx>,
    subst: &FxHashMap<GenericId, Ty<'tcx>>,
) -> Ty<'tcx> {
    while let TyKind::Generic(g) = *ty.kind() {
        match subst.get(&g) {
            Some(&next) => ty = next,
            None => break,
        }
    }
    ty
}

fn occurs(g: GenericId, ty: Ty<'_>, subst: &FxHashMap<GenericId, Ty<'_>>) -> bool {
    let ty = resolve_binding(ty, subst);
    match *ty.kind() {
        TyKind::Generic(h) => g == h,
        TyKind::DevArray {
            elem, placement, ..
        } => occurs(g, elem, subst) || occurs(g, placement, subst),
        TyKind::Nullable(inner) | TyKind::Arc(inner) | TyKind::ConstSeq(inner) => {
            occurs(g, inner, subst)
        }
        TyKind::Cell { elem, .. } | TyKind::Array { elem, .. } | TyKind::Tensor { elem, .. } => {
            occurs(g, elem, subst)
        }
        TyKind::Ptr { space, .. } => occurs(g, space, subst),
        TyKind::Future(ps) | TyKind::Tuple(ps) => ps.iter().any(|&p| occurs(g, p, subst)),
        TyKind::Record { args, .. } => args.iter().any(|&a| occurs(g, a, subst)),
        TyKind::Closure { params, ret } => {
            params.iter().any(|&p| occurs(g, p, subst)) || occurs(g, ret, subst)
        }
        _ => false,
    }
}

fn unify_ty<'tcx>(
    a: Ty<'tcx>,
    b: Ty<'tcx>,
    vars: &FxHashSet<GenericId>,
    subst: &mut FxHashMap<GenericId, Ty<'tcx>>,
) -> bool {
    let a = resolve_binding(a, subst);
    let b = resolve_binding(b, subst);
    if a == b {
        return true;
    }
    match (a.kind(), b.kind()) {
        (&TyKind::Generic(g), _) if vars.contains(&g) => {
            if occurs(g, b, subst) {
                return false;
            }
            subst.insert(g, b);
            true
        }
        (_, &TyKind::Generic(g)) if vars.contains(&g) => {
            if occurs(g, a, subst) {
                return false;
            }
            subst.insert(g, a);
            true
        }
        (TyKind::Nullable(x), TyKind::Nullable(y))
        | (TyKind::Arc(x), TyKind::Arc(y))
        | (TyKind::ConstSeq(x), TyKind::ConstSeq(y)) => unify_ty(*x, *y, vars, subst),
        (TyKind::Cell { elem: x, kind: kx }, TyKind::Cell { elem: y, kind: ky }) => {
            kx == ky && unify_ty(*x, *y, vars, subst)
        }
        (TyKind::Ptr { space: x, kind: kx }, TyKind::Ptr { space: y, kind: ky }) => {
            kx == ky && unify_ty(*x, *y, vars, subst)
        }
        (
            TyKind::Record {
                def: dx, args: xs, ..
            },
            TyKind::Record {
                def: dy, args: ys, ..
            },
        ) => dx == dy && unify_args(xs, ys, vars, subst),
        (
            TyKind::Closure {
                params: xs,
                ret: rx,
            },
            TyKind::Closure {
                params: ys,
                ret: ry,
            },
        ) => unify_args(xs, ys, vars, subst) && unify_ty(*rx, *ry, vars, subst),
        (
            TyKind::DevArray {
                elem: x,
                dims: dx,
                placement: px,
            },
            TyKind::DevArray {
                elem: y,
                dims: dy,
                placement: py,
            },
        ) => dx == dy && unify_ty(*x, *y, vars, subst) && unify_ty(*px, *py, vars, subst),
        (TyKind::Future(xs), TyKind::Future(ys)) | (TyKind::Tuple(xs), TyKind::Tuple(ys)) => {
            xs.len() == ys.len()
                && xs
                    .iter()
                    .zip(ys.iter())
                    .all(|(&x, &y)| unify_ty(x, y, vars, subst))
        }
        (TyKind::Tensor { elem: x, dims: dx }, TyKind::Tensor { elem: y, dims: dy }) => {
            dx == dy && unify_ty(*x, *y, vars, subst)
        }
        (TyKind::Array { elem: x, dims: dx }, TyKind::Array { elem: y, dims: dy }) => {
            dx == dy && unify_ty(*x, *y, vars, subst)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::ty::TyCtxt;
    use crate::with_tcx;

    #[test]
    fn head_keys_are_total_over_ground_heads_and_none_for_headless() {
        with_tcx(|tcx: &TyCtxt| {
            assert_eq!(
                head_key(tcx.mk_int(IntTy::Signed(32))),
                Some(HeadKey::Int(IntTy::Signed(32)))
            );
            assert_eq!(head_key(tcx.mk_bool()), Some(HeadKey::Bool));
            assert_eq!(head_key(tcx.mk_generic(GenericId(7))), None);
            assert_eq!(head_key(tcx.mk(TyKind::Bottom)), None);
        });
    }

    #[test]
    fn crossing_generic_heads_overlap_two_sided() {
        // Pair<T, i32> vs Pair<u8, U>: one-way matching fails in both
        // directions; the two-sided unifier finds T ↦ u8, U ↦ i32.
        with_tcx(|tcx: &TyCtxt| {
            let t = GenericId(1);
            let u = GenericId(2);
            let pair = DefId(0);
            let i32t = tcx.mk_int(IntTy::Signed(32));
            let u8t = tcx.mk_int(IntTy::Unsigned(8));
            let a = tcx.mk_record(
                pair,
                &[tcx.mk_generic(t), i32t],
                crate::semi::ty::Flexivity::Irrelevant,
            );
            let b = tcx.mk_record(
                pair,
                &[u8t, tcx.mk_generic(u)],
                crate::semi::ty::Flexivity::Irrelevant,
            );
            let vars: FxHashSet<GenericId> = [t, u].into_iter().collect();
            let mut subst = FxHashMap::default();
            assert!(unify_args(&[a], &[b], &vars, &mut subst));
            // Disjoint instantiations stay disjoint.
            let c = tcx.mk_record(pair, &[i32t, i32t], crate::semi::ty::Flexivity::Irrelevant);
            let d = tcx.mk_record(pair, &[u8t, u8t], crate::semi::ty::Flexivity::Irrelevant);
            let mut subst = FxHashMap::default();
            assert!(!unify_args(&[c], &[d], &vars, &mut subst));
        });
    }

    #[test]
    fn occurs_check_rejects_infinite_solutions() {
        with_tcx(|tcx: &TyCtxt| {
            let t = GenericId(1);
            let list = DefId(0);
            let g = tcx.mk_generic(t);
            let wrapped = tcx.mk_record(list, &[g], crate::semi::ty::Flexivity::Irrelevant);
            let vars: FxHashSet<GenericId> = [t].into_iter().collect();
            let mut subst = FxHashMap::default();
            assert!(!unify_args(&[g], &[wrapped], &vars, &mut subst));
        });
    }

    #[test]
    fn alpha_renaming_identifies_only_renamed_heads() {
        with_tcx(|tcx: &TyCtxt| {
            let pair = DefId(0);
            let flex = crate::semi::ty::Flexivity::Irrelevant;
            let [t, u, v, w] = [1, 2, 3, 4].map(GenericId);
            let g = |id| tcx.mk_generic(id);
            let a_vars: FxHashSet<GenericId> = [t, u].into_iter().collect();
            let b_vars: FxHashSet<GenericId> = [v, w].into_iter().collect();

            // Pair<T, U> and Pair<V, W>: the same head under fresh ids.
            let a = tcx.mk_record(pair, &[g(t), g(u)], flex);
            let b = tcx.mk_record(pair, &[g(v), g(w)], flex);
            let renaming = alpha_renaming(&[a], &[b], &a_vars, &b_vars).expect("renamed");
            assert_eq!(renaming.get(&t), Some(&v));
            assert_eq!(renaming.get(&u), Some(&w));

            // Pair<T, T> and Pair<V, W> overlap but are distinct heads.
            let same = tcx.mk_record(pair, &[g(t), g(t)], flex);
            assert!(alpha_renaming(&[same], &[b], &a_vars, &b_vars).is_none());

            // Pair<T, i32> and Pair<V, W> differ by an instantiation.
            let inst = tcx.mk_record(pair, &[g(t), tcx.mk_int(IntTy::Signed(32))], flex);
            assert!(alpha_renaming(&[inst], &[b], &a_vars, &b_vars).is_none());
        });
    }
}
