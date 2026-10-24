//! Fresh inference instantiation at a summary call boundary.

use super::*;
use crate::semi::infer::InferCtxt;

impl<'tcx> Summary<'tcx> {
    /// Instantiate this template in a caller without interpreting template
    /// HoleIds in the caller's inference table. Generic actuals are substituted
    /// atomically: a hole inside an actual is already caller-owned and must not
    /// pass through the template hole map, even when its numeric ID coincides.
    pub fn instantiate_fresh(
        &self,
        tcx: &TyCtxt<'tcx>,
        infer: &mut InferCtxt<'_, 'tcx>,
        call: &Call<'tcx>,
    ) -> Vec<Relation<'tcx>> {
        let generics = self
            .generics
            .iter()
            .enumerate()
            .map(|(index, &generic)| {
                let actual = call
                    .ty_args
                    .get(index)
                    .copied()
                    .unwrap_or_else(|| infer.new_hole_ty());
                (generic, actual)
            })
            .collect();
        let mut subst = FreshSubstitution {
            tcx,
            infer,
            generics,
            mapped: FxHashMap::default(),
        };
        self.instantiate_call(|ty| subst.ty(ty), &call.args, call.result)
    }
}

/// The map is keyed by template type identity and exists for exactly one
/// invocation. It also memoizes composites, avoiding repeated traversal of
/// shared type DAGs. It never resolves a template hole through `infer`.
struct FreshSubstitution<'a, 'i, 'ctx, 'tcx> {
    tcx: &'a TyCtxt<'tcx>,
    infer: &'i mut InferCtxt<'ctx, 'tcx>,
    generics: FxHashMap<GenericId, Ty<'tcx>>,
    mapped: FxHashMap<Ty<'tcx>, Ty<'tcx>>,
}

impl<'tcx> FreshSubstitution<'_, '_, '_, 'tcx> {
    fn ty(&mut self, template: Ty<'tcx>) -> Ty<'tcx> {
        if let Some(&mapped) = self.mapped.get(&template) {
            return mapped;
        }
        let tcx = self.tcx;
        let mapped = match *template.kind() {
            TyKind::Generic(generic) => self.generics.get(&generic).copied().unwrap_or(template),
            TyKind::Hole(_) => self.infer.new_hole_ty(),
            TyKind::Record { def, args, flex } => {
                let args: Vec<_> = args.iter().map(|&ty| self.ty(ty)).collect();
                tcx.mk_record(def, &args, flex)
            }
            TyKind::Closure { params, ret } => {
                let params: Vec<_> = params.iter().map(|&ty| self.ty(ty)).collect();
                tcx.mk_closure(&params, self.ty(ret))
            }
            TyKind::Nullable(inner) => tcx.mk_nullable(self.ty(inner)),
            TyKind::Arc(inner) => tcx.mk_arc(self.ty(inner)),
            TyKind::ConstSeq(elem) => tcx.mk_const_seq(self.ty(elem)),
            TyKind::Cell { elem, kind } => tcx.mk_cell(self.ty(elem), kind),
            TyKind::Array { elem, dims } => tcx.mk_array(self.ty(elem), dims),
            TyKind::Tensor { elem, dims } => tcx.mk_tensor(self.ty(elem), dims),
            TyKind::DevArray {
                elem,
                dims,
                placement,
            } => tcx.mk_devarray(self.ty(elem), dims, Some(self.ty(placement))),
            TyKind::Tuple(fields) => {
                let fields: Vec<_> = fields.iter().map(|&ty| self.ty(ty)).collect();
                tcx.mk_tuple(&fields)
            }
            TyKind::Future(fields) => {
                let fields: Vec<_> = fields.iter().map(|&ty| self.ty(ty)).collect();
                tcx.mk_future(&fields)
            }
            TyKind::Token
            | TyKind::DefaultPlacement
            | TyKind::Int(_)
            | TyKind::BigInt(_)
            | TyKind::Fp(_)
            | TyKind::Bool
            | TyKind::Str
            | TyKind::Char
            | TyKind::Unit
            | TyKind::Bottom => template,
        };
        self.mapped.insert(template, mapped);
        mapped
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fresh_instantiation_distinguishes_template_holes_from_caller_ids() {
        use crate::semi::ty::{CellKind, Flexivity};
        crate::with_tcx(|tcx| {
            let mut infer = InferCtxt::new(tcx);
            let caller = infer.new_hole_ty();
            // An imported/recovery template may use the very same numeric ID.
            let template_hole = caller;
            let generic = GenericId(17);
            let missing = GenericId(18);
            let g = tcx.mk_generic(generic);
            let result = tcx.mk_tuple(&[
                tcx.mk_future(&[template_hole]),
                tcx.mk_devarray(g, &[0, 3], Some(template_hole)),
                tcx.mk_closure(&[tcx.mk_nullable(template_hole)], tcx.mk_arc(template_hole)),
                tcx.mk_cell(template_hole, CellKind::Plain),
                tcx.mk_array(template_hole, &[4]),
                tcx.mk_tensor(template_hole, &[2]),
                tcx.mk_record(DefId(9), &[template_hole, g], Flexivity::Irrelevant),
                tcx.mk_generic(missing),
            ]);
            let summary = Summary {
                def: DefId(1),
                generics: vec![generic, missing],
                params: vec![g],
                result,
                relations: vec![Relation::Equal {
                    left: template_hole,
                    right: template_hole,
                }],
                has_body: true,
            };
            let call = Call {
                target: summary.def,
                ty_args: vec![caller],
                args: vec![Argument::unknown(caller)],
                result: caller,
            };
            let first = summary.instantiate_fresh(tcx, &mut infer, &call);
            let second = summary.instantiate_fresh(tcx, &mut infer, &call);
            let fresh = match first[1] {
                Relation::Equal { left, right } => {
                    assert_eq!(left, right);
                    left
                }
                _ => panic!("body equality"),
            };
            assert_ne!(fresh, caller);
            assert!(
                matches!(first[0], Relation::Equal {left, right} if left == caller && right == caller)
            );
            assert!(
                matches!(second[1], Relation::Equal {left, ..} if left != fresh && left != caller)
            );
            let mapped_result = match first.last().unwrap() {
                Relation::Equal { left, right } => {
                    assert_eq!(*right, caller);
                    *left
                }
                _ => panic!("call result equality"),
            };
            let TyKind::Tuple(fields) = mapped_result.kind() else {
                panic!("tuple")
            };
            assert_eq!(fields[0], tcx.mk_future(&[fresh]));
            assert_eq!(fields[1], tcx.mk_devarray(caller, &[0, 3], Some(fresh)));
            assert_eq!(
                fields[2],
                tcx.mk_closure(&[tcx.mk_nullable(fresh)], tcx.mk_arc(fresh))
            );
            assert_eq!(fields[3], tcx.mk_cell(fresh, CellKind::Plain));
            assert_eq!(fields[4], tcx.mk_array(fresh, &[4]));
            assert_eq!(fields[5], tcx.mk_tensor(fresh, &[2]));
            assert_eq!(
                fields[6],
                tcx.mk_record(DefId(9), &[fresh, caller], Flexivity::Irrelevant)
            );
            assert_ne!(fields[7], fresh);
            assert_ne!(fields[7], caller);
            infer.unify(fresh, tcx.mk_bool()).unwrap();
            assert_eq!(infer.resolve(caller), caller);
        });
    }
}
