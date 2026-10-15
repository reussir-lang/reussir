//! Ordinary trait dispatch for body-derived shape relations. Selection uses the
//! same coherence-checked trait database as expression checking and mono; no
//! method spelling or operation-specific shape knowledge lives here.

use rustc_hash::FxHashMap;

use crate::semi::ctxt::Elaborator;
use crate::semi::shape_summary::{Argument, Call};
use crate::semi::traits::{Evidence, Obligation, SelectCtxt, SelectError, TraitRef};
use crate::semi::ty::{DefId, Ty};
use crate::surface::Span;

use super::rules::RuleStatus;

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    pub(super) fn resolve_shape_trait_call(
        &mut self,
        trait_def: DefId,
        method: u32,
        ty_args: &[Ty<'tcx>],
        args: &[Argument<'tcx>],
        result: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<RuleStatus, String> {
        let tid = self
            .traits
            .trait_by_def(trait_def)
            .ok_or_else(|| "unknown trait in a shape relation".to_owned())?;
        let tdef = self.traits.trait_def(tid);
        let signature = tdef
            .methods
            .get(method as usize)
            .ok_or_else(|| "invalid trait method index in a shape relation".to_owned())?
            .clone();
        let trait_arity = 1 + tdef.params.len();
        if ty_args.len() != trait_arity + signature.generics.len()
            || args.len() != signature.params.len()
        {
            return Err("invalid trait method argument count in a shape relation".into());
        }
        let ty_args: Vec<_> = ty_args.iter().map(|&ty| self.infer.resolve(ty)).collect();
        let binders = std::iter::once(tdef.self_param)
            .chain(tdef.params.iter().map(|&(_, g)| g))
            .chain(signature.generics.iter().map(|&(_, g)| g));
        let subst: FxHashMap<_, _> = binders.zip(ty_args.iter().copied()).collect();
        for (&formal, actual) in signature.params.iter().zip(args) {
            let formal = crate::semi::traits::subst::replace_generics(self.tcx, formal, &subst);
            self.infer
                .unify(formal, actual.ty)
                .map_err(|_| "incompatible argument for a trait shape relation".to_owned())?;
        }
        let declared_result =
            crate::semi::traits::subst::replace_generics(self.tcx, signature.ret, &subst);
        self.infer
            .unify(declared_result, result)
            .map_err(|_| "incompatible result for a trait shape relation".to_owned())?;
        // Signature equalities may have resolved a receiver hole. Method-local
        // holes do not participate in selecting the receiver's impl.
        let trait_args: Vec<_> = ty_args[..trait_arity]
            .iter()
            .map(|&ty| self.infer.resolve(ty))
            .collect();
        let mut holes = Vec::new();
        for &ty in &trait_args {
            crate::semi::fulfill::collect_holes(ty, &mut holes);
        }
        if !holes.is_empty() {
            return Ok(RuleStatus::Deferred);
        }
        let goal = Obligation::Trait(TraitRef {
            trait_id: tid,
            args: trait_args.clone(),
        });
        match SelectCtxt::new(&self.traits, self.tcx, &self.generics).select(&goal) {
            Ok(Evidence::Impl {
                impl_id,
                args: impl_args,
                ..
            }) => {
                let implementation = self.traits.impl_def(impl_id);
                let Some(&target) = implementation.methods.get(method as usize) else {
                    // Builtin comparison impls deliberately have no function
                    // body. Their declared signatures are the full contract.
                    if self
                        .lang
                        .item_of(trait_def)
                        .is_some_and(|item| item.is_cmp_trait())
                    {
                        return Ok(RuleStatus::Solved);
                    }
                    return Err(format!(
                        "selected impl of `{}` has no method body",
                        self.trait_display(tid)
                    ));
                };
                let ty_args = impl_args
                    .into_iter()
                    .chain(ty_args[trait_arity..].iter().copied())
                    .collect();
                self.ensure_body_checked(target);
                self.expand_shape_call(
                    Call {
                        target,
                        ty_args,
                        args: args.to_vec(),
                        result,
                    },
                    span,
                );
                Ok(RuleStatus::Solved)
            }
            // A rigid generic can satisfy a declared bound without choosing an
            // implementation. Preserve the residual for a later grounded call.
            Ok(Evidence::Param { .. }) => Ok(RuleStatus::Deferred),
            Ok(Evidence::Super { of, .. }) if evidence_is_parameter(&of) => {
                Ok(RuleStatus::Deferred)
            }
            Ok(Evidence::Super { .. }) => Err(format!(
                "calling a method of `{}` requires a direct impl, not only a super-trait proof",
                self.trait_display(tid)
            )),
            Err(SelectError::NoImpl(inner)) => {
                Err(self.no_impl_message(trait_args[0], tid, &inner))
            }
            Err(SelectError::DepthLimit(goal)) => Err(self.depth_limit_message(&goal)),
        }
    }
}

fn evidence_is_parameter(evidence: &Evidence<'_>) -> bool {
    match evidence {
        Evidence::Param { .. } => true,
        Evidence::Super { of, .. } => evidence_is_parameter(of),
        Evidence::Impl { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::ty::{IntTy, TyKind};

    #[test]
    fn trait_shape_calls_select_normal_impls_and_preserve_method_actuals() {
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(
                r#"
                pub trait Identity { fn id<T>(self: Self, value: T) -> T; }
                pub struct Marker {}
                impl Identity for Marker { fn id<T>(self: Self, value: T) -> T { value } }
            "#,
                interner.clone(),
            );
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = crate::surface::program(&parsed.root);
            let mut elab = crate::semi::elaborate(tcx, &program, &interner);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            let implementation = elab.traits.impls().find(|i| !i.methods.is_empty()).unwrap();
            let trait_def = elab.traits.trait_def(implementation.trait_ref.trait_id).def;
            let receiver = implementation.self_ty;
            let method_actual = elab.infer.new_hole_ty();
            let result = elab.infer.new_hole_ty();
            let int = tcx.mk_int(IntTy::Signed(32));
            let args = [Argument::unknown(receiver), Argument::unknown(int)];
            assert!(matches!(
                elab.resolve_shape_trait_call(
                    trait_def,
                    0,
                    &[receiver, method_actual],
                    &args,
                    result,
                    None
                ),
                Ok(RuleStatus::Solved)
            ));
            elab.resolve_shape_constraints(false);
            assert_eq!(elab.infer.resolve(method_actual), int);
            assert_eq!(elab.infer.resolve(result), int);

            let receiver_hole = elab.infer.new_hole_ty();
            let args = [Argument::unknown(receiver_hole), Argument::unknown(int)];
            assert!(matches!(
                elab.resolve_shape_trait_call(
                    trait_def,
                    0,
                    &[receiver_hole, int],
                    &args,
                    int,
                    None
                ),
                Ok(RuleStatus::Deferred)
            ));
            assert!(matches!(
                elab.infer.resolve(receiver_hole).kind(),
                TyKind::Hole(_)
            ));
            assert!(
                elab.resolve_shape_trait_call(
                    trait_def,
                    0,
                    &[int, int],
                    &[Argument::unknown(int), Argument::unknown(int)],
                    int,
                    None
                )
                .is_err()
            );
            assert!(
                elab.resolve_shape_trait_call(trait_def, 1, &[receiver, int], &args, int, None)
                    .is_err()
            );
        });
    }
    #[test]
    fn selected_trait_body_propagates_tensor_shape_constraints() {
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(
                r#"
                pub trait Matrix { #[kernel] fn transpose<A, B>(x: Tensor<A>) -> Tensor<B>; }
                pub struct Marker {}
                impl Matrix for Marker {
                    #[kernel] fn transpose<A, B>(x: Tensor<A>) -> Tensor<B> {
                        core::intrinsic::tensor::transpose(x)
                    }
                }
            "#,
                interner.clone(),
            );
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = crate::surface::program(&parsed.root);
            use reussir_syntax::Interner;
            let mut keys = interner.clone();
            let files = [crate::semi::PackageFile {
                file: reussir_syntax::source::FileId::ROOT,
                module: vec![keys.get_or_intern("dep")],
                program: &program,
            }];
            let producer = crate::semi::elaborate_package(tcx, &files, &interner);
            assert!(!producer.has_errors(), "{:?}", producer.reports);
            let (traits, impls) =
                crate::semi::hir::trait_texts(&producer.traits, &producer.defs, producer.resolver);
            let text = crate::semi::hir::print::Printer::new(&producer.defs, producer.resolver)
                .with_traits(&traits, &impls)
                .program(
                    &producer.elaborated,
                    &producer.strings.entries(),
                    &producer.records,
                    &producer.trampolines,
                );
            let text = format!("interface 1 package \"dep\" producer \"rrc 0.1.0\";\n{text}");
            let parsed =
                crate::semi::hir::build::parse_program(tcx, &text).expect("trait RRI roundtrip");
            let mut elab = Elaborator::new(tcx, &interner);
            elab.declare_extern_package(
                &mut keys,
                &crate::semi::ExternPackage {
                    name: "dep",
                    parsed: &parsed,
                    files: &[],
                },
            );
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            let implementation = elab.traits.impls().find(|i| !i.methods.is_empty()).unwrap();
            let trait_def = elab.traits.trait_def(implementation.trait_ref.trait_id).def;
            let receiver = implementation.self_ty;
            let elem = tcx.mk_fp(crate::semi::ty::FpTy::Ieee(32));
            let source_shape = tcx.mk_array(elem, &[2, 3]);
            let output_shape = elab.infer.new_hole_ty();
            let result = tcx.mk_tensor(output_shape, &[]);
            let args = [Argument::unknown(tcx.mk_tensor(elem, &[2, 3]))];
            assert!(matches!(
                elab.resolve_shape_trait_call(
                    trait_def,
                    0,
                    &[receiver, source_shape, output_shape],
                    &args,
                    result,
                    None
                ),
                Ok(RuleStatus::Solved)
            ));
            elab.resolve_shape_constraints(false);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            assert_eq!(elab.infer.resolve(result), tcx.mk_tensor(elem, &[3, 2]));
        });
    }
    #[test]
    fn source_trait_wrapper_schedules_forward_impl_body() {
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(
                r#"
                pub trait Matrix { #[kernel] fn transpose<A, B>(x: Tensor<A>) -> Tensor<B>; }
                pub struct Marker {}
                #[kernel] fn apply<M: Matrix, A, B>(x: Tensor<A>) -> Tensor<B> { M::transpose(x) }
                #[kernel] pub fn caller(x: Tensor<[f32; 2, 3]>) -> Tensor<[f32; 3, 2]> {
                    let result = apply<Marker, _, _>(x);
                    result
                }
                impl Matrix for Marker {
                    #[kernel] fn transpose<A, B>(x: Tensor<A>) -> Tensor<B> {
                        core::intrinsic::tensor::transpose(x)
                    }
                }
            "#,
                interner.clone(),
            );
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = crate::surface::program(&parsed.root);
            let elab = crate::semi::elaborate(tcx, &program, &interner);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            let (_, errors) = crate::full::mono::monomorphize(&elab.mono_input());
            assert!(errors.is_empty(), "{errors:?}");
        });
    }
    #[test]
    fn source_mutual_trait_and_direct_recursion_keeps_declared_contracts() {
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(
                r#"
                pub trait Cycle { #[kernel] fn again<T>(x: Tensor<T>) -> Tensor<T>; }
                pub struct First {}
                pub struct Second {}
                #[kernel] fn invoke<M: Cycle, T>(x: Tensor<T>) -> Tensor<T> { M::again(x) }
                #[kernel] pub fn caller(x: Tensor<[f32; 2]>) -> Tensor<[f32; 2]> {
                    invoke<First, _>(x)
                }
                impl Cycle for First {
                    #[kernel] fn again<T>(x: Tensor<T>) -> Tensor<T> { invoke<Second, T>(x) }
                }
                impl Cycle for Second {
                    #[kernel] fn again<T>(x: Tensor<T>) -> Tensor<T> { invoke<First, T>(x) }
                }
            "#,
                interner.clone(),
            );
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = crate::surface::program(&parsed.root);
            let elab = crate::semi::elaborate(tcx, &program, &interner);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            let (_, errors) = crate::full::mono::monomorphize(&elab.mono_input());
            assert!(errors.is_empty(), "{errors:?}");
        });
    }
}
