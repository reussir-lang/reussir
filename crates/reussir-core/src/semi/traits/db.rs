//! The trait/impl registry: dense-id storage, the `(trait, head)` candidate
//! index, super-trait reachability, and REPL-checkpoint truncation.
//!
//! Resolution lives in [`super::select::SelectCtxt`], which reads this
//! registry; coherence (the overlap and orphan gates in the elaborator)
//! is what entitles selection to commit to the first matching candidate.

use reussir_syntax::kind::TokenKey;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::semi::const_eval::ConstValue;
use crate::semi::ty::{DefId, Ty, TyCtxt, TyKind, mentions_assoc};

use super::coherence::{HeadKey, head_key};
use super::def::{ImplDef, TraitDef};
use super::select::{ParamEnv, SELECT_DEPTH_LIMIT, SelectCtxt};
use super::subst::{rebuild, replace_generics};
use super::{Evidence, ImplId, Obligation, TraitId, TraitRef};

/// The collected trait program plus resolution.
#[derive(Default)]
pub struct TraitDb<'tcx> {
    traits: Vec<TraitDef<'tcx>>,
    impls: Vec<ImplDef<'tcx>>,
    /// The dense [`TraitId`] of each path-keyed trait def.
    by_def: FxHashMap<DefId, TraitId>,
    /// Impls bucketed by `(trait, self-head)` — the candidate index for
    /// overlap checking (and, in the selection stack, for `select`).
    by_head: FxHashMap<(TraitId, HeadKey), SmallVec<[ImplId; 2]>>,
}

/// Why an obligation could not be discharged.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectError<'tcx> {
    /// No impl makes `τ : Trait` hold. Carries the *deepest* failing goal:
    /// a where-clause failure on the unique applicable impl surfaces the
    /// inner obligation, which is the root cause.
    NoImpl(TraitRef<'tcx>),
    /// Proof search exceeded [`super::select::SELECT_DEPTH_LIMIT`].
    DepthLimit(TraitRef<'tcx>),
}

/// Why [`TraitDb::assoc_const_value`] or [`TraitDb::assoc_ty`] has nothing
/// to give.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssocError<'tcx> {
    /// The trait declares no associated const or type of that name.
    Undeclared,
    /// Selection proves no impl of the trait for the self type.
    Select(SelectError<'tcx>),
    /// The bound holds only through a sub-trait, so no impl of the trait
    /// itself defines the item.
    Indirect,
}

/// What an associated type is once its trait reference is selected.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssocTy<'tcx> {
    /// The type the selected impl binds, or the trait's default for it. It
    /// may mention further associated types.
    Bound(Ty<'tcx>),
    /// The reference holds by a bound in scope, so the type stays as written.
    Rigid,
}

impl<'tcx> TraitDb<'tcx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_trait(&mut self, def: TraitDef<'tcx>) -> TraitId {
        // `trait_def` indexes `traits` by `TraitId`, so ids must be dense and
        // inserted in order.
        debug_assert_eq!(
            def.id.0 as usize,
            self.traits.len(),
            "trait ids must be dense"
        );
        let id = def.id;
        let displaced = self.by_def.insert(def.def, id);
        debug_assert!(displaced.is_none(), "one TraitDb entry per trait def");
        self.traits.push(def);
        id
    }

    /// The dense id of the trait declared under `def`, if any. Every
    /// trait-namespace def has an entry — the elaborator registers them
    /// together.
    pub fn trait_by_def(&self, def: DefId) -> Option<TraitId> {
        self.by_def.get(&def).copied()
    }

    /// Checkpoint counters for [`TraitDb::truncate`].
    pub fn traits_len(&self) -> usize {
        self.traits.len()
    }

    pub fn impls_len(&self) -> usize {
        self.impls.len()
    }

    /// Retract every trait past `traits` and every impl past `impls` — the
    /// REPL's atomic-rollback hook, mirroring `DefTable::truncate` (ids are
    /// dense, so popping the tails restores exactly the checkpointed state).
    pub fn truncate(&mut self, traits: usize, impls: usize) {
        while self.traits.len() > traits {
            let def = self.traits.pop().expect("len checked above");
            let removed = self.by_def.remove(&def.def);
            debug_assert_eq!(
                removed,
                Some(def.id),
                "by_def must point at the popped trait"
            );
        }
        while self.impls.len() > impls {
            let def = self.impls.pop().expect("len checked above");
            let head = head_key(def.self_ty).expect("registered with a head");
            let bucket = self
                .by_head
                .get_mut(&(def.trait_ref.trait_id, head))
                .expect("registered impls have a bucket");
            let popped = bucket.pop();
            debug_assert_eq!(popped, Some(def.id), "buckets append in id order");
            if bucket.is_empty() {
                self.by_head.remove(&(def.trait_ref.trait_id, head));
            }
        }
    }

    pub fn add_impl(&mut self, def: ImplDef<'tcx>) -> ImplId {
        // Keep `ImplId`s dense and in storage order so they can index `impls`.
        debug_assert_eq!(
            def.id.0 as usize,
            self.impls.len(),
            "impl ids must be dense"
        );
        let id = def.id;
        let head = head_key(def.self_ty).expect("blanket impls are rejected before registration");
        self.by_head
            .entry((def.trait_ref.trait_id, head))
            .or_default()
            .push(id);
        self.impls.push(def);
        id
    }

    /// The impls of `trait_id` whose self head is `head`.
    pub fn impls_for(&self, trait_id: TraitId, head: HeadKey) -> &[ImplId] {
        self.by_head
            .get(&(trait_id, head))
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn trait_def(&self, id: TraitId) -> &TraitDef<'tcx> {
        &self.traits[id.0 as usize]
    }

    pub fn impl_def(&self, id: ImplId) -> &ImplDef<'tcx> {
        &self.impls[id.0 as usize]
    }

    /// Mutable access for the elaborator's two-phase populate: trait stubs
    /// register first (so bounds resolve during the record/function scans),
    /// then supertraits and members fill in.
    pub(crate) fn trait_def_mut(&mut self, id: TraitId) -> &mut TraitDef<'tcx> {
        &mut self.traits[id.0 as usize]
    }

    /// Does holding `have` imply holding `want`? True when they are equal or
    /// `want` is in `have`'s super-trait closure.
    pub fn implies(&self, have: TraitId, want: TraitId) -> bool {
        have == want || self.reaches(self.trait_def(have), want)
    }

    /// Is `target` in `def`'s transitive super-trait closure?
    fn reaches(&self, def: &TraitDef<'tcx>, target: TraitId) -> bool {
        def.supertraits
            .iter()
            .any(|s| s.trait_id == target || self.reaches(self.trait_def(s.trait_id), target))
    }

    /// The value associated const `name` of `trait_id` takes for the ground
    /// `self_ty`. Selection runs with no assumptions (everything is ground)
    /// and names the unique impl; the impl's own value wins, and an impl
    /// that omits the const takes the trait's default value.
    pub fn assoc_const_value(
        &self,
        tcx: &TyCtxt<'tcx>,
        trait_id: TraitId,
        self_ty: Ty<'tcx>,
        name: TokenKey,
    ) -> Result<ConstValue<'tcx>, AssocError<'tcx>> {
        let decls = &self.trait_def(trait_id).assoc_consts;
        let index = decls
            .iter()
            .position(|c| c.name == name)
            .ok_or(AssocError::Undeclared)?;
        let goal = Obligation::Trait(TraitRef {
            trait_id,
            args: vec![self_ty],
        });
        match SelectCtxt::new(self, tcx, &[]).select(&goal) {
            Ok(Evidence::Impl { impl_id, .. }) => Ok(self.impl_def(impl_id).assoc_consts[index]
                .as_ref()
                .or(decls[index].default.as_ref())
                .expect("conformance requires a value or a default value")
                .clone()),
            Ok(_) => Err(AssocError::Indirect),
            Err(error) => Err(AssocError::Select(error)),
        }
    }

    /// The associated type `name` of the trait reference `trait_id<args>`
    /// (`args[0]` is the self type), which holds no inference hole. `env`
    /// is the bounds in scope; a reference they prove leaves the type rigid.
    pub fn assoc_ty(
        &self,
        tcx: &TyCtxt<'tcx>,
        env: ParamEnv<'_>,
        trait_id: TraitId,
        args: &[Ty<'tcx>],
        name: TokenKey,
    ) -> Result<AssocTy<'tcx>, AssocError<'tcx>> {
        let tdef = self.trait_def(trait_id);
        let index = tdef
            .assoc_tys
            .iter()
            .position(|a| a.name == name)
            .ok_or(AssocError::Undeclared)?;
        let goal = Obligation::Trait(TraitRef {
            trait_id,
            args: args.to_vec(),
        });
        let evidence = SelectCtxt::new(self, tcx, env)
            .select(&goal)
            .map_err(AssocError::Select)?;
        let (impl_id, impl_args) = match &evidence {
            Evidence::Impl { impl_id, args, .. } => (*impl_id, args),
            _ if evidence.is_assumed() => return Ok(AssocTy::Rigid),
            _ => return Err(AssocError::Indirect),
        };
        let imp = self.impl_def(impl_id);
        let (ty, binder): (_, FxHashMap<_, _>) = match imp.assoc_tys[index] {
            Some(ty) => (
                ty,
                imp.generics
                    .iter()
                    .copied()
                    .zip(impl_args.iter().copied())
                    .collect(),
            ),
            None => (
                tdef.assoc_tys[index]
                    .default
                    .expect("conformance requires a binding or a default"),
                std::iter::once(tdef.self_param)
                    .chain(tdef.params.iter().map(|&(_, g)| g))
                    .zip(args.iter().copied())
                    .collect(),
            ),
        };
        Ok(AssocTy::Bound(replace_generics(tcx, ty, &binder)))
    }

    /// Replace every associated type in the hole-free `ty` that an impl
    /// decides by the type it stands for, to a fixed point. The first one
    /// that cannot be decided is returned with the reason.
    pub fn normalize(
        &self,
        tcx: &TyCtxt<'tcx>,
        env: ParamEnv<'_>,
        ty: Ty<'tcx>,
    ) -> Result<Ty<'tcx>, (Ty<'tcx>, AssocError<'tcx>)> {
        let mut failed = None;
        let ty = self.normalize_at(tcx, env, ty, 0, &mut failed);
        failed.map_or(Ok(ty), Err)
    }

    fn normalize_at(
        &self,
        tcx: &TyCtxt<'tcx>,
        env: ParamEnv<'_>,
        ty: Ty<'tcx>,
        depth: usize,
        failed: &mut Option<(Ty<'tcx>, AssocError<'tcx>)>,
    ) -> Ty<'tcx> {
        if !mentions_assoc(ty) {
            return ty;
        }
        rebuild(tcx, ty, &mut |_| None, &mut |assoc| {
            let TyKind::Assoc {
                trait_def,
                name,
                args,
            } = *assoc.kind()
            else {
                unreachable!("`rebuild` hands over associated types only")
            };
            let projected = match self.trait_by_def(trait_def) {
                // A default may name the type it defines, directly or not.
                Some(trait_id) if depth > SELECT_DEPTH_LIMIT => {
                    Err(AssocError::Select(SelectError::DepthLimit(TraitRef {
                        trait_id,
                        args: args.to_vec(),
                    })))
                }
                Some(trait_id) => self.assoc_ty(tcx, env, trait_id, args, name),
                None => Err(AssocError::Undeclared),
            };
            match projected {
                Ok(AssocTy::Rigid) => assoc,
                Ok(AssocTy::Bound(ty)) => self.normalize_at(tcx, env, ty, depth + 1, failed),
                Err(error) => {
                    failed.get_or_insert((assoc, error));
                    tcx.mk(TyKind::Bottom)
                }
            }
        })
    }

    /// All impls in registration (dense-id) order — the deterministic scan
    /// order for super-trait projection in selection.
    pub fn impls(&self) -> impl Iterator<Item = &ImplDef<'tcx>> {
        self.impls.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::traits::builtins::Builtins;
    use crate::semi::ty::TyCtxt;
    use crate::with_tcx;

    #[test]
    fn truncate_retracts_traits_and_impls() {
        with_tcx(|tcx: &TyCtxt| {
            let mut db = TraitDb::new();
            let mut defs = crate::semi::resolve::DefTable::new();
            let mut interner = lasso::Rodeo::<reussir_syntax::kind::TokenKey>::new();
            let b = Builtins::register(&mut db, &mut defs, &mut interner, tcx);
            let (traits, impls) = (db.traits_len(), db.impls_len());

            // Register a user trait + impl past the builtins, then retract.
            let key = reussir_syntax::Interner::get_or_intern(&mut interner, "Show");
            let def = defs.declare_trait(key).expect("fresh");
            let id = TraitId(traits as u32);
            db.add_trait(TraitDef {
                id,
                def,
                visibility: crate::surface::Visibility::Public,
                sealed: false,
                self_param: crate::semi::ty::GenericId(0),
                params: vec![],
                supertraits: vec![],
                methods: vec![],
                assoc_tys: vec![],
                assoc_consts: vec![],
                span: None,
                file: reussir_syntax::source::FileId::ROOT,
            });
            let unit = tcx.mk_unit();
            db.add_impl(ImplDef {
                id: ImplId(impls as u32),
                generics: vec![],
                trait_ref: TraitRef {
                    trait_id: id,
                    args: vec![unit],
                },
                self_ty: unit,
                where_clauses: vec![],
                methods: vec![],
                assoc_consts: vec![],
                assoc_tys: vec![],
                span: None,
                file: reussir_syntax::source::FileId::ROOT,
            });
            assert_eq!(db.trait_by_def(def), Some(id));

            db.truncate(traits, impls);
            assert_eq!(db.traits_len(), traits);
            assert_eq!(db.impls_len(), impls);
            assert_eq!(db.trait_by_def(def), None);
            // The builtins survive and re-adding at the same dense ids works.
            assert_eq!(db.trait_by_def(db.trait_def(b.num).def), Some(b.num));
            db.add_trait(TraitDef {
                id,
                def,
                visibility: crate::surface::Visibility::Public,
                sealed: false,
                self_param: crate::semi::ty::GenericId(0),
                params: vec![],
                supertraits: vec![],
                methods: vec![],
                assoc_tys: vec![],
                assoc_consts: vec![],
                span: None,
                file: reussir_syntax::source::FileId::ROOT,
            });
            assert_eq!(db.trait_by_def(def), Some(id));
        });
    }

    #[test]
    fn trait_by_def_round_trips_builtins() {
        with_tcx(|tcx: &TyCtxt| {
            let mut db = TraitDb::new();
            let mut defs = crate::semi::resolve::DefTable::new();
            let mut interner = lasso::Rodeo::<reussir_syntax::kind::TokenKey>::new();
            let b = Builtins::register(&mut db, &mut defs, &mut interner, tcx);
            for id in [
                b.num,
                b.integral,
                b.floating_point,
                b.ptr_like,
                b.sync,
                b.addr_space,
            ] {
                assert_eq!(db.trait_by_def(db.trait_def(id).def), Some(id));
            }
        });
    }
}
