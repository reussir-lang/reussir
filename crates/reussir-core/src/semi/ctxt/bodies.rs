//! On-demand checking of local bodies for reusable shape summaries.
//!
//! All signatures and source bodies are registered before checking starts. A
//! caller may request a callee's typed body regardless of declaration order.
//! Re-entering an active definition stops at its declared contract, preserving
//! recursive calls as summary edges instead of unfolding them indefinitely.
use super::*;

#[derive(Default)]
pub(super) struct BodyScheduler {
    source: FxHashMap<DefId, (surface::Function, Option<Span>)>,
    active: FxHashSet<DefId>,
    checked: FxHashSet<DefId>,
}

impl BodyScheduler {
    pub(super) fn register(
        &mut self,
        def: DefId,
        function: &surface::Function,
        span: Option<Span>,
    ) {
        self.source.insert(def, (function.clone(), span));
    }
    pub(super) fn rollback(&mut self, live: u32) {
        self.source.retain(|def, _| def.0 < live);
        self.active.retain(|def| def.0 < live);
        self.checked.retain(|def| def.0 < live);
    }
}

/// Everything that body checking may replace in the suspended caller. Global
/// definition tables, diagnostics, interners, expression IDs, and completed
/// bodies deliberately remain shared with the nested check.
struct FunctionFrame<'a, 'tcx> {
    infer: InferCtxt<'a, 'tcx>,
    vars: VarEnv<'tcx>,
    generic_names: FxHashMap<TokenKey, GenericId>,
    self_alias: Option<Ty<'tcx>>,
    inside_region: bool,
    inside_kernel: bool,
    regional_generics: Vec<GenericId>,
    fulfill: FulfillCtxt<'tcx>,
    reported_holes: FxHashSet<HoleId>,
    file: FileId,
    module: Vec<TokenKey>,
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// Returns false for an already checked definition or an active recursive
    /// edge. Called by the single body-checking entry point.
    pub(in crate::semi) fn begin_shape_body(&mut self, def: DefId) -> bool {
        !self.body_scheduler.checked.contains(&def) && self.body_scheduler.active.insert(def)
    }
    pub(in crate::semi) fn is_shape_body_active(&self, def: DefId) -> bool {
        self.body_scheduler.active.contains(&def)
    }
    pub(in crate::semi) fn finish_shape_body(&mut self, def: DefId) {
        self.body_scheduler.active.remove(&def);
        self.body_scheduler.checked.insert(def);
    }

    /// Make a local typed body available without disturbing the current
    /// function's inference state. True means a Function entry is available;
    /// that entry may be an opaque prototype. False means an active recursive
    /// call or an unavailable definition, whose contract remains residual.
    pub(in crate::semi) fn ensure_shape_body(&mut self, def: DefId) -> bool {
        if self.body_scheduler.checked.contains(&def)
            || self.elaborated.iter().any(|f| f.def == def)
            || self.extern_functions.iter().any(|f| f.def == def)
        {
            return true;
        }
        if self.body_scheduler.active.contains(&def) {
            return false;
        }
        let Some((function, span)) = self.body_scheduler.source.get(&def).cloned() else {
            return false;
        };
        let frame = self.suspend_function();
        self.check_function(&function, def, span);
        self.restore_function(frame);
        self.body_scheduler.checked.contains(&def)
    }

    fn suspend_function(&mut self) -> FunctionFrame<'a, 'tcx> {
        FunctionFrame {
            infer: std::mem::replace(&mut self.infer, InferCtxt::new(self.tcx)),
            vars: std::mem::take(&mut self.vars),
            generic_names: std::mem::take(&mut self.generic_names),
            self_alias: self.self_alias.take(),
            inside_region: std::mem::replace(&mut self.inside_region, false),
            inside_kernel: std::mem::replace(&mut self.inside_kernel, false),
            regional_generics: std::mem::take(&mut self.regional_generics),
            fulfill: std::mem::take(&mut self.fulfill),
            reported_holes: std::mem::take(&mut self.reported_holes),
            file: self.current_file,
            module: self.defs.module().to_vec(),
        }
    }
    fn restore_function(&mut self, frame: FunctionFrame<'a, 'tcx>) {
        self.infer = frame.infer;
        self.vars = frame.vars;
        self.generic_names = frame.generic_names;
        self.self_alias = frame.self_alias;
        self.inside_region = frame.inside_region;
        self.inside_kernel = frame.inside_kernel;
        self.regional_generics = frame.regional_generics;
        self.fulfill = frame.fulfill;
        self.reported_holes = frame.reported_holes;
        self.current_file = frame.file;
        self.defs.set_module(frame.module);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::infer::shape::Dim;
    use crate::with_tcx;

    #[test]
    fn suspended_function_restores_inference_and_scope() {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let mut elab = Elaborator::new(tcx, &interner);
            let d = elab.infer.new_dim();
            let s = elab.infer.ranked_shape(vec![d]);
            let t = elab.infer.new_tensor_ty(tcx.mk_bool(), s);
            let module = interner.intern("caller");
            let generic = interner.intern("T");
            elab.defs.set_module(vec![module]);
            elab.generic_names.insert(generic, GenericId(19));
            elab.self_alias = Some(t);
            elab.inside_kernel = true;
            elab.inside_region = true;
            elab.regional_generics.push(GenericId(19));
            let frame = elab.suspend_function();
            elab.enter_function(&[]);
            elab.defs.set_module(vec![]);
            let nested = elab.infer.new_hole_ty();
            elab.infer.unify(nested, tcx.mk_unit()).unwrap();
            elab.restore_function(frame);
            assert_eq!(elab.defs.module(), &[module]);
            assert_eq!(elab.generic_names.get(&generic), Some(&GenericId(19)));
            assert_eq!(elab.self_alias, Some(t));
            assert!(elab.inside_kernel && elab.inside_region);
            assert_eq!(elab.regional_generics, vec![GenericId(19)]);
            assert_eq!(elab.infer.resolve_dim(d), d);
            elab.infer.unify_dims(d, Dim::Known(4)).unwrap();
            assert_eq!(elab.infer.resolve(t), tcx.mk_tensor(tcx.mk_bool(), &[4]));
        });
    }

    #[test]
    fn recursion_guard_and_rollback_do_not_poison_reused_definition_ids() {
        let mut scheduler = BodyScheduler::default();
        scheduler.active.insert(DefId(3));
        scheduler.checked.insert(DefId(4));
        scheduler.checked.insert(DefId(1));
        scheduler.rollback(3);
        assert!(scheduler.active.is_empty());
        assert_eq!(scheduler.checked, [DefId(1)].into_iter().collect());
    }
    #[test]
    fn forward_body_check_preserves_caller_variables_and_output_order() {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let source = "fn caller<T: Num>(x: T) -> T { callee(x) + x } fn callee<T: Num>(x: T) -> T { x + x }";
            let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let mut elab = Elaborator::new(tcx, &interner);
            elab.run(&surface::program(&parsed.root));
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            assert_eq!(elab.elaborated.len(), 2);
            assert_eq!(elab.sym(elab.elaborated[0].name), "caller");
            assert_eq!(elab.sym(elab.elaborated[1].name), "callee");
            assert_eq!(elab.body_scheduler.checked.len(), 2);
            assert!(elab.body_scheduler.active.is_empty());
        });
    }

    #[test]
    fn mutually_recursive_bodies_are_checked_once() {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let source = "fn first(n: i32) -> i32 { if n == 0 { 0 } else { second(n - 1) } } fn second(n: i32) -> i32 { first(n) }";
            let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let mut elab = Elaborator::new(tcx, &interner);
            elab.run(&surface::program(&parsed.root));
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            assert_eq!(elab.elaborated.len(), 2);
            assert_eq!(elab.body_scheduler.checked.len(), 2);
            assert!(elab.body_scheduler.active.is_empty());
        });
    }

    #[test]
    fn failed_incremental_batch_retracts_body_sources_and_check_state() {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let mut elab = Elaborator::new(tcx, &interner);
            let parse = |source| {
                let p = reussir_syntax::parse_with_interner(source, interner.clone());
                assert!(p.ok(), "{:?}", p.errors);
                surface::program(&p.root)
            };
            elab.try_extend(&parse("fn original(x: i32) -> i32 { x }"))
                .unwrap();
            let baseline = elab.body_scheduler.source.len();
            assert!(
                elab.try_extend(&parse(
                    "fn caller(x: i32) -> i32 { later(x) } fn later(x: i32) -> i32 { true }"
                ))
                .is_err()
            );
            assert_eq!(elab.body_scheduler.source.len(), baseline);
            assert_eq!(elab.body_scheduler.checked.len(), baseline);
            assert!(elab.body_scheduler.active.is_empty());
            elab.try_extend(&parse(
                "fn caller(x: i32) -> i32 { later(x) } fn later(x: i32) -> i32 { original(x) }",
            ))
            .unwrap();
            assert_eq!(elab.elaborated.len(), 3);
            assert_eq!(elab.sym(elab.elaborated[1].name), "caller");
            assert_eq!(elab.sym(elab.elaborated[2].name), "later");
        });
    }

    #[test]
    fn cross_module_forward_body_restores_name_resolution_scope() {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parse = |source| {
                let p = reussir_syntax::parse_with_interner(source, interner.clone());
                assert!(p.ok(), "{:?}", p.errors);
                surface::program(&p.root)
            };
            let root = parse(
                "fn caller(x: i32) -> i32 { later::callee(x) + own(x) } fn own(x: i32) -> i32 { x }",
            );
            let child = parse(
                "fn callee(x: i32) -> i32 { let flag: bool = own(true); x } fn own(x: bool) -> bool { x }",
            );
            let pkg = interner.intern("pkg");
            let later = interner.intern("later");
            let mut elab = Elaborator::new(tcx, &interner);
            elab.run_package(&[
                PackageFile {
                    file: FileId::ROOT,
                    module: vec![pkg],
                    program: &root,
                },
                PackageFile {
                    file: FileId::ROOT,
                    module: vec![pkg, later],
                    program: &child,
                },
            ]);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            assert_eq!(elab.elaborated.len(), 4);
            assert_eq!(elab.sym(elab.elaborated[0].name), "caller");
            assert_eq!(elab.sym(elab.elaborated[2].name), "callee");
        });
    }
}
