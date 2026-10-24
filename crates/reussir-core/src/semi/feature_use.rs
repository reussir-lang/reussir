//! The optional features each function needs.
//!
//! A function needs a feature when its signature or body mentions something
//! the feature provides (a gated type such as `Nat`, a kernel, a launch), or
//! when it calls a function that needs it. Libraries are elaborated with
//! every feature, so `core` declares everything on every build. A compiler
//! without a feature then leaves the ground functions that need it unlowered
//! (monomorphization never roots them, see
//! [`crate::full::mono::MonoFeatures`]), and a program that calls one is
//! rejected at the call, like any other use of the feature.

use rustc_hash::FxHashMap;

use crate::feature::{Feature, FeatureSet};
use crate::full::mono::for_each_expr;
use crate::semi::Elaborator;
use crate::semi::hir::{ExprKind, Function};
use crate::semi::ty::{DefId, Ty, TyKind};

/// The features a value of type `ty` needs.
pub fn ty_features(ty: Ty<'_>) -> FeatureSet {
    let mut needed = FeatureSet::EMPTY;
    add_ty_features(ty, &mut needed);
    needed
}

fn add_ty_features(ty: Ty<'_>, needed: &mut FeatureSet) {
    match *ty.kind() {
        TyKind::BigInt(_) => needed.insert(Feature::Rcgmp),
        TyKind::Token => needed.insert(Feature::OpenXla),
        TyKind::Future(tys) => {
            needed.insert(Feature::OpenXla);
            tys.iter().for_each(|&t| add_ty_features(t, needed));
        }
        TyKind::Record { args, .. } | TyKind::Tuple(args) => {
            args.iter().for_each(|&t| add_ty_features(t, needed));
        }
        TyKind::Closure { params, ret } => {
            params.iter().for_each(|&t| add_ty_features(t, needed));
            add_ty_features(ret, needed);
        }
        TyKind::DevArray {
            elem, placement, ..
        } => {
            add_ty_features(elem, needed);
            add_ty_features(placement, needed);
        }
        TyKind::Array { elem, .. }
        | TyKind::Tensor { elem, .. }
        | TyKind::Cell { elem, .. }
        | TyKind::Nullable(elem)
        | TyKind::ConstSeq(elem)
        | TyKind::Arc(elem) => add_ty_features(elem, needed),
        TyKind::Int(_)
        | TyKind::Fp(_)
        | TyKind::Bool
        | TyKind::Str
        | TyKind::Char
        | TyKind::Unit
        | TyKind::DefaultPlacement
        | TyKind::Generic(_)
        | TyKind::Hole(_)
        | TyKind::Bottom => {}
    }
}

/// The features each function needs, closed over calls. `known` gives the
/// needs of interface prototypes, whose definitions are compiled elsewhere;
/// any other function without a body (an intrinsic or foreign declaration)
/// needs what its signature mentions. The result omits functions that need
/// nothing.
pub fn function_requirements<'f, 'tcx: 'f>(
    functions: impl IntoIterator<Item = &'f Function<'tcx>>,
    known: &FxHashMap<DefId, FeatureSet>,
) -> FxHashMap<DefId, FeatureSet> {
    // What each function needs by itself, and the functions it calls.
    let mut direct: Vec<(DefId, FeatureSet, Vec<DefId>)> = Vec::new();
    for f in functions {
        if f.body.is_none() && known.contains_key(&f.def) {
            continue;
        }
        let mut needed = ty_features(f.return_ty);
        f.params
            .iter()
            .for_each(|&(_, _, ty)| add_ty_features(ty, &mut needed));
        if f.is_kernel {
            needed.insert(Feature::OpenXla);
        }
        let mut callees = Vec::new();
        let Some(body) = &f.body else {
            direct.push((f.def, needed, callees));
            continue;
        };
        for_each_expr(body, &mut |e| {
            add_ty_features(e.ty, &mut needed);
            match &e.kind {
                ExprKind::FuncCall { target, .. } => callees.push(*target),
                ExprKind::Launch { .. } | ExprKind::TensorOp { .. } | ExprKind::Async(_) => {
                    needed.insert(Feature::OpenXla)
                }
                _ => {}
            }
        });
        direct.push((f.def, needed, callees));
    }
    let mut needs: FxHashMap<DefId, FeatureSet> = known
        .iter()
        .filter(|(def, _)| !direct.iter().any(|(d, ..)| d == *def))
        .map(|(&def, &needed)| (def, needed))
        .collect();
    loop {
        let mut changed = false;
        for (def, own, callees) in &direct {
            let total = callees
                .iter()
                .filter_map(|c| needs.get(c))
                .fold(*own, |acc, &n| acc.union(n));
            if !total.is_empty() && needs.get(def) != Some(&total) {
                needs.insert(*def, total);
                changed = true;
            }
        }
        if !changed {
            return needs;
        }
    }
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// Compute [`Elaborator::feature_requirements`] over every function the
    /// session knows, and reject the calls in the bodies checked since
    /// `first_body` that reach an extern function needing a feature this
    /// program may not use. Calls to local functions need no check: a local
    /// body reports its own uses.
    pub(super) fn check_feature_requirements(&mut self, first_body: usize) {
        self.feature_requirements = function_requirements(
            self.elaborated.iter().chain(&self.extern_functions),
            &self.feature_requirements,
        );
        let mut calls = Vec::new();
        for f in &self.elaborated[first_body..] {
            let Some(body) = &f.body else {
                continue;
            };
            for_each_expr(body, &mut |e| {
                let ExprKind::FuncCall { target, .. } = &e.kind else {
                    return;
                };
                if !self.extern_defs.contains_key(target) {
                    return;
                }
                let needed = self.feature_requirements.get(target).copied();
                let missing = self.features.missing(needed.unwrap_or(FeatureSet::EMPTY));
                if !missing.is_empty() {
                    calls.push((f.file, *target, e.span, missing));
                }
            });
        }
        for (file, target, span, missing) in calls {
            self.set_current_file(file);
            let what = format!("`{}`", self.defs.path(target).display(self.resolver));
            for feature in missing.iter() {
                self.require_feature(feature, &what, span);
            }
        }
    }
}
