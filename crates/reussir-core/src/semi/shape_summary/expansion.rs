//! Cached call graphs and bounded summary composition.

use super::*;
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

/// Memoize derived HIR templates for one elaboration session. Clear/invalidate
/// entries if a REPL replaces a body or the operation schema changes.
#[derive(Default)]
pub struct SummaryCache<'tcx> {
    summaries: FxHashMap<DefId, Summary<'tcx>>,
}

impl<'tcx> SummaryCache<'tcx> {
    pub fn get_or_derive(
        &mut self,
        tcx: &TyCtxt<'tcx>,
        function: &Function<'tcx>,
    ) -> &Summary<'tcx> {
        if function.body.is_some()
            && self
                .summaries
                .get(&function.def)
                .is_some_and(|s| !s.has_body)
        {
            self.summaries.remove(&function.def);
        }
        self.summaries
            .entry(function.def)
            .or_insert_with(|| Summary::derive(tcx, function))
    }

    pub fn get(&self, def: DefId) -> Option<&Summary<'tcx>> {
        self.summaries.get(&def)
    }

    /// Collect the reachable ordinary call graph without unfolding recursion.
    /// Missing/opaque callees and trait calls remain explicit residual edges.
    pub fn collect<'a>(
        &mut self,
        tcx: &TyCtxt<'tcx>,
        root: DefId,
        lookup: impl Fn(DefId) -> Option<&'a Function<'tcx>>,
    ) -> Vec<DefId>
    where
        'tcx: 'a,
    {
        let mut queue = VecDeque::from([root]);
        let mut seen = FxHashSet::default();
        let mut collected = Vec::new();
        while let Some(def) = queue.pop_front() {
            if !seen.insert(def) {
                continue;
            }
            let Some(function) = lookup(def) else {
                continue;
            };
            let summary = self.get_or_derive(tcx, function);
            for relation in &summary.relations {
                if let Relation::Call { target, .. } = relation {
                    queue.push_back(*target);
                }
            }
            collected.push(def);
        }
        collected
    }

    pub fn invalidate(&mut self, def: DefId) {
        self.summaries.remove(&def);
    }
    pub fn clear(&mut self) {
        self.summaries.clear();
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResidualReason {
    /// The declaration exists, but its HIR has not been elaborated/loaded yet.
    PendingBody,
    /// A foreign/ground prototype intentionally has no available HIR body.
    Opaque,
    /// This edge returns to a currently expanding recursive component.
    Recursive,
}

#[derive(Clone, Debug)]
pub struct ResidualCall<'tcx> {
    pub call: Call<'tcx>,
    pub reason: ResidualReason,
}

/// A call's result is deliberately absent: the same input contract can be
/// reused to constrain an independently inferred result at another use site.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct CallKey<'tcx> {
    target: DefId,
    ty_args: Vec<Ty<'tcx>>,
    args: Vec<Argument<'tcx>>,
}

impl<'tcx> CallKey<'tcx> {
    fn of(call: &Call<'tcx>) -> Self {
        Self {
            target: call.target,
            ty_args: call.ty_args.clone(),
            args: call.args.clone(),
        }
    }
}

fn identify_operands(args: &mut [Argument<'_>]) {
    for argument in args {
        match &mut argument.value {
            None => argument.value = Some(opaque_value(false)),
            Some(Value::Scalar) => argument.value = Some(opaque_value(true)),
            Some(Value::Tuple(fields)) => identify_operands(fields),
            Some(Value::Projection { base, .. }) => {
                identify_operands(std::slice::from_mut(base.as_mut()))
            }
            Some(
                Value::Integer(_) | Value::Bool(_) | Value::Parameter(_) | Value::Opaque { .. },
            ) => {}
        }
    }
}

/// Body-derived facts plus the edges that still require their declared
/// contracts. A recursive edge is not proved merely because traversal stopped.
#[derive(Clone, Debug, Default)]
pub struct Expansion<'tcx> {
    pub relations: Vec<Relation<'tcx>>,
    pub residual_calls: Vec<ResidualCall<'tcx>>,
    pub pending_bodies: Vec<DefId>,
    pub recursive_components: Vec<Vec<DefId>>,
}

impl<'tcx> SummaryCache<'tcx> {
    /// Compose ordinary bodies through typed call edges. Each invocation asks
    /// the caller for a fresh substitution; sibling calls never share one.
    ///
    /// Lookup returns None only while a body is unavailable. A known prototype
    /// must instead return its Function with body=None, distinguishing an opaque
    /// contract from work that should be retried after forward elaboration.
    ///
    /// Trait calls remain relations for the trait solver. Recursive backedges
    /// remain explicit calls for signature checking; this function neither
    /// guesses a recursive shape nor recursively unfolds polymorphic recursion.
    pub fn expand<'a>(
        &mut self,
        tcx: &TyCtxt<'tcx>,
        root: Call<'tcx>,
        lookup: impl Fn(DefId) -> Option<&'a Function<'tcx>>,
        mut instantiate: impl FnMut(&Summary<'tcx>, &Call<'tcx>) -> Vec<Relation<'tcx>>,
    ) -> Expansion<'tcx>
    where
        'tcx: 'a,
    {
        let graph = self.collect(tcx, root.target, &lookup);
        let mut expansion = Expansion {
            recursive_components: self.recursive_components(&graph),
            ..Expansion::default()
        };
        enum Task<'tcx> {
            Enter(Call<'tcx>),
            Exit(DefId),
        }
        let mut tasks = vec![Task::Enter(root)];
        let mut active = FxHashSet::default();
        let mut pending = FxHashSet::default();
        let mut expanded: FxHashMap<CallKey<'tcx>, Call<'tcx>> = FxHashMap::default();
        while let Some(task) = tasks.pop() {
            let mut call = match task {
                Task::Exit(def) => {
                    active.remove(&def);
                    continue;
                }
                Task::Enter(call) => call,
            };
            identify_operands(&mut call.args);
            let key = CallKey::of(&call);
            // Active recursion is retained even if its template is memoized;
            // reuse is not a proof of a recursive contract.
            let reason = if active.contains(&call.target) {
                Some(ResidualReason::Recursive)
            } else if let Some(previous) = expanded.get(&key) {
                expansion.relations.push(Relation::Equal {
                    left: previous.result,
                    right: call.result,
                });
                expansion
                    .relations
                    .extend(
                        previous
                            .args
                            .iter()
                            .zip(&call.args)
                            .map(|(previous, current)| Relation::Equal {
                                left: previous.ty,
                                right: current.ty,
                            }),
                    );
                None
            } else if let Some(function) = lookup(call.target) {
                let summary = self.get_or_derive(tcx, function);
                let relations = instantiate(summary, &call);
                if summary.has_body {
                    expanded.insert(key, call.clone());
                    active.insert(call.target);
                    tasks.push(Task::Exit(call.target));
                    // Stack reversal preserves source call order while every
                    // operation remains an independent monotonic relation.
                    let mut callees = Vec::new();
                    for relation in relations {
                        match relation {
                            Relation::Call {
                                target,
                                ty_args,
                                args,
                                result,
                            } => callees.push(Call {
                                target,
                                ty_args,
                                args,
                                result,
                            }),
                            relation => expansion.relations.push(relation),
                        }
                    }
                    tasks.extend(callees.into_iter().rev().map(Task::Enter));
                    None
                } else {
                    expansion.relations.extend(relations);
                    Some(ResidualReason::Opaque)
                }
            } else {
                if pending.insert(call.target) {
                    expansion.pending_bodies.push(call.target);
                }
                Some(ResidualReason::PendingBody)
            };
            if let Some(reason) = reason {
                expansion.residual_calls.push(ResidualCall { call, reason });
            }
        }
        expansion
    }

    /// Iterative Kosaraju decomposition keeps deep ordinary call chains off
    /// the Rust stack. Only actual recursive components are returned.
    fn recursive_components(&self, nodes: &[DefId]) -> Vec<Vec<DefId>> {
        let node_set: FxHashSet<_> = nodes.iter().copied().collect();
        let mut edges: FxHashMap<DefId, Vec<DefId>> = FxHashMap::default();
        let mut reverse: FxHashMap<DefId, Vec<DefId>> = FxHashMap::default();
        for &def in nodes {
            let outgoing: Vec<_> = self
                .get(def)
                .into_iter()
                .flat_map(|s| &s.relations)
                .filter_map(|r| match r {
                    Relation::Call { target, .. } if node_set.contains(target) => Some(*target),
                    _ => None,
                })
                .collect();
            for &target in &outgoing {
                reverse.entry(target).or_default().push(def);
            }
            edges.insert(def, outgoing);
        }
        let mut seen = FxHashSet::default();
        let mut order = Vec::new();
        for &root in nodes {
            let mut stack = vec![(root, false)];
            while let Some((def, exiting)) = stack.pop() {
                if exiting {
                    order.push(def);
                    continue;
                }
                if !seen.insert(def) {
                    continue;
                }
                stack.push((def, true));
                stack.extend(
                    edges
                        .get(&def)
                        .into_iter()
                        .flatten()
                        .rev()
                        .map(|&d| (d, false)),
                );
            }
        }
        seen.clear();
        let mut recursive = Vec::new();
        for root in order.into_iter().rev() {
            if !seen.insert(root) {
                continue;
            }
            let mut component = vec![root];
            let mut stack = vec![root];
            while let Some(def) = stack.pop() {
                for &prev in reverse.get(&def).into_iter().flatten() {
                    if seen.insert(prev) {
                        component.push(prev);
                        stack.push(prev);
                    }
                }
            }
            if component.len() > 1 || edges.get(&root).is_some_and(|es| es.contains(&root)) {
                recursive.push(component);
            }
        }
        recursive
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::imported;
    use super::*;
    use crate::semi::ty::{FpTy, IntTy};
    use std::sync::Arc;
    fn instantiate_ground<'tcx>(
        tcx: &TyCtxt<'tcx>,
        summary: &Summary<'tcx>,
        call: &Call<'tcx>,
    ) -> Vec<Relation<'tcx>> {
        let subst = summary
            .generics
            .iter()
            .copied()
            .zip(call.ty_args.iter().copied())
            .collect();
        summary.instantiate_call(
            |ty| crate::full::subst::subst_ty(tcx, ty, &subst),
            &call.args,
            call.result,
        )
    }

    #[test]
    fn recursive_graph_remains_finite_and_preserves_nonrecursive_work() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn a<T>(x: Tensor<[T; 2, 2]>) -> Tensor<[T; 2, 2]> {
                    let y = core::intrinsic::tensor::transpose(x);
                    b(y)
                }
                #[kernel] fn b<T>(x: Tensor<[T; 2, 2]>) -> Tensor<[T; 2, 2]> { a(x) }
            "#,
                &interner,
            );
            let functions = &elab.extern_functions;
            let mut cache = SummaryCache::default();
            let graph = cache.collect(tcx, functions[0].def, |d| {
                functions.iter().find(|f| f.def == d)
            });
            assert_eq!(graph.len(), 2);
            assert_eq!(
                graph
                    .iter()
                    .flat_map(|d| &cache.get(*d).unwrap().relations)
                    .filter(|r| matches!(r, Relation::Call { .. }))
                    .count(),
                2
            );
            assert!(
                graph
                    .iter()
                    .flat_map(|d| &cache.get(*d).unwrap().relations)
                    .any(|r| matches!(
                        r,
                        Relation::Tensor {
                            op: TensorFn::Transpose,
                            ..
                        }
                    ))
            );
        });
    }

    #[test]
    fn composition_retries_forward_bodies_and_binds_the_call_result() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn outer<T>(x: T) -> Tensor<[T; 2, 3]> { middle(x) }
                #[kernel] fn middle<T>(x: T) -> Tensor<[T; 2, 3]> {
                    let a: Tensor<[T; 3, 2]> = core::intrinsic::tensor::fill(x);
                    core::intrinsic::tensor::transpose(a)
                }
            "#,
                &interner,
            );
            let functions = &elab.extern_functions;
            let outer = functions
                .iter()
                .find(|f| elab.defs.path(f.def).display(elab.resolver) == "dep::outer")
                .unwrap();
            let middle = functions.iter().find(|f| f.def != outer.def).unwrap();
            let f32_ty = tcx.mk_fp(FpTy::Ieee(32));
            let result = tcx.mk_tensor(f32_ty, &[2, 3]);
            let call = Call {
                target: outer.def,
                ty_args: vec![f32_ty],
                args: vec![Argument::unknown(f32_ty)],
                result,
            };
            let mut cache = SummaryCache::default();
            let pending = cache.expand(
                tcx,
                call.clone(),
                |def| {
                    functions
                        .iter()
                        .find(|f| f.def == def && f.def != middle.def)
                },
                |s, c| instantiate_ground(tcx, s, c),
            );
            assert_eq!(pending.pending_bodies, [middle.def]);
            assert_eq!(pending.residual_calls.len(), 1);
            assert_eq!(
                pending.residual_calls[0].reason,
                ResidualReason::PendingBody
            );
            let complete = cache.expand(
                tcx,
                call,
                |def| functions.iter().find(|f| f.def == def),
                |s, c| instantiate_ground(tcx, s, c),
            );
            assert!(complete.pending_bodies.is_empty());
            assert!(complete.residual_calls.is_empty());
            assert_eq!(
                complete
                    .relations
                    .iter()
                    .filter(|r| matches!(r, Relation::Tensor { .. }))
                    .count(),
                2
            );
            assert!(complete.relations.iter().any(|r|
                matches!(r, Relation::Equal { left, right } if *left == result && *right == result)));
        });
    }

    #[test]
    fn composition_marks_recursive_scc_and_keeps_backedge_contract() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn a<T>(x: Tensor<[T; 2, 2]>) -> Tensor<[T; 2, 2]> {
                    b(core::intrinsic::tensor::transpose(x))
                }
                #[kernel] fn b<T>(x: Tensor<[T; 2, 2]>) -> Tensor<[T; 2, 2]> { a(x) }
            "#,
                &interner,
            );
            let functions = &elab.extern_functions;
            let elem = tcx.mk_fp(FpTy::Ieee(32));
            let ty = tcx.mk_tensor(elem, &[2, 2]);
            let root = functions
                .iter()
                .find(|f| elab.defs.path(f.def).display(elab.resolver) == "dep::a")
                .unwrap();
            let mut calls = 0;
            let expansion = SummaryCache::default().expand(
                tcx,
                Call {
                    target: root.def,
                    ty_args: vec![elem],
                    args: vec![Argument::unknown(ty)],
                    result: ty,
                },
                |def| functions.iter().find(|f| f.def == def),
                |s, c| {
                    calls += 1;
                    instantiate_ground(tcx, s, c)
                },
            );
            assert_eq!(calls, 2, "backedge must not recursively instantiate");
            assert_eq!(expansion.recursive_components.len(), 1);
            assert_eq!(expansion.recursive_components[0].len(), 2);
            assert_eq!(expansion.residual_calls.len(), 1);
            assert_eq!(
                expansion.residual_calls[0].reason,
                ResidualReason::Recursive
            );
            assert_eq!(expansion.residual_calls[0].call.target, root.def);
            assert!(expansion.relations.iter().any(|r| matches!(
                r,
                Relation::Tensor {
                    op: TensorFn::Transpose,
                    ..
                }
            )));
        });
    }

    #[test]
    fn opaque_prototype_is_not_pending_and_cache_refreshes_when_body_arrives() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn fill<T>(x: T) -> Tensor<[T; 2]> {
                    core::intrinsic::tensor::fill(x)
                }
            "#,
                &interner,
            );
            let function = &elab.extern_functions[0];
            let mut prototype = function.clone();
            prototype.body = None;
            let elem = tcx.mk_fp(FpTy::Ieee(32));
            let call = Call {
                target: function.def,
                ty_args: vec![elem],
                args: vec![Argument::unknown(elem)],
                result: tcx.mk_tensor(elem, &[2]),
            };
            let mut cache = SummaryCache::default();
            let opaque = cache.expand(
                tcx,
                call.clone(),
                |_| Some(&prototype),
                |s, c| instantiate_ground(tcx, s, c),
            );
            assert!(opaque.pending_bodies.is_empty());
            assert_eq!(opaque.residual_calls[0].reason, ResidualReason::Opaque);
            let body = cache.expand(
                tcx,
                call,
                |_| Some(function),
                |s, c| instantiate_ground(tcx, s, c),
            );
            assert!(body.residual_calls.is_empty());
            assert!(body.relations.iter().any(|r| matches!(
                r,
                Relation::Tensor {
                    op: TensorFn::Fill,
                    ..
                }
            )));
        });
    }
    #[test]
    fn shared_call_dag_expands_linearly_with_unknown_operand_provenance() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let mut source =
                String::from("#[kernel] fn layer_0(x:Tensor<[f32;2]>)->Tensor<[f32;2]>{x}\n");
            for level in 1..=25 {
                source.push_str(&format!("#[kernel] pub fn layer_{level}(x:Tensor<[f32;2]>)->Tensor<[f32;2]>{{ let y=core::intrinsic::tensor::add(x,x); let first=layer_{}(y); layer_{}(y) }}\n", level-1, level-1));
            }
            let elab = imported(tcx, &source, &interner);
            let functions = &elab.extern_functions;
            let root = functions
                .iter()
                .find(|f| elab.defs.path(f.def).display(elab.resolver) == "dep::layer_25")
                .unwrap();
            let mut cache = SummaryCache::default();
            let mut count = 0;
            let expansion = cache.expand(
                tcx,
                Call {
                    target: root.def,
                    ty_args: vec![],
                    args: vec![Argument::unknown(root.params[0].2)],
                    result: root.return_ty,
                },
                |d| functions.iter().find(|f| f.def == d),
                |summary, call| {
                    count += 1;
                    instantiate_ground(tcx, summary, call)
                },
            );
            assert_eq!(count, 26);
            assert!(
                expansion.relations.len() < 200,
                "{} relations",
                expansion.relations.len()
            );
            assert!(expansion.residual_calls.is_empty());
        });
    }

    #[test]
    fn call_memo_keys_keep_constants_shapes_and_independent_unknowns_distinct() {
        crate::with_tcx(|tcx| {
            let scalar = tcx.mk_int(IntTy::Signed(64));
            let one = Integer::from(1);
            let two = Integer::from(2);
            fn make<'tcx>(scalar: Ty<'tcx>, arg: Argument<'tcx>) -> Call<'tcx> {
                Call {
                    target: DefId(3),
                    ty_args: vec![scalar],
                    args: vec![arg],
                    result: scalar,
                }
            }
            let known = make(
                scalar,
                Argument {
                    ty: scalar,
                    value: Some(Value::Integer(&one)),
                },
            );
            let other = make(
                scalar,
                Argument {
                    ty: scalar,
                    value: Some(Value::Integer(&two)),
                },
            );
            assert_ne!(CallKey::of(&known), CallKey::of(&other));
            let mut unknown = make(scalar, Argument::unknown(scalar));
            let mut independent = unknown.clone();
            identify_operands(&mut unknown.args);
            identify_operands(&mut independent.args);
            assert_ne!(CallKey::of(&unknown), CallKey::of(&independent));
            let mut forwarded = unknown.clone();
            identify_operands(&mut forwarded.args);
            assert_eq!(CallKey::of(&unknown), CallKey::of(&forwarded));
            forwarded.ty_args = vec![tcx.mk_tensor(scalar, &[3])];
            assert_ne!(CallKey::of(&unknown), CallKey::of(&forwarded));
            forwarded = unknown.clone();
            forwarded.args[0].ty = tcx.mk_tensor(scalar, &[2]);
            assert_ne!(CallKey::of(&unknown), CallKey::of(&forwarded));
            let tuple = tcx.mk_tuple(&[scalar, scalar]);
            let mut actual = [Argument::unknown(tuple)];
            identify_operands(&mut actual);
            let projection = |path| Argument {
                ty: scalar,
                value: Some(Value::Projection {
                    base: Box::new(Argument {
                        ty: tuple,
                        value: Some(Value::Parameter(0)),
                    }),
                    path: vec![path],
                }),
            };
            let first = projection(0).instantiate(&mut |ty| ty, &actual);
            let repeated = projection(0).instantiate(&mut |ty| ty, &actual);
            let second = projection(1).instantiate(&mut |ty| ty, &actual);
            assert_eq!(first, repeated);
            assert_ne!(first, second);
            let local = opaque_value(false);
            let summary = Summary {
                def: DefId(4),
                generics: vec![],
                params: vec![],
                result: scalar,
                relations: vec![Relation::Tensor {
                    op: TensorFn::Add,
                    result: scalar,
                    args: vec![
                        Argument {
                            ty: scalar,
                            value: Some(local.clone()),
                        },
                        Argument {
                            ty: scalar,
                            value: Some(local),
                        },
                    ],
                }],
                has_body: true,
            };
            let first = summary.instantiate(|ty| ty, &[]);
            let next = summary.instantiate(|ty| ty, &[]);
            let Relation::Tensor { args: first, .. } = &first[0] else {
                panic!("tensor")
            };
            let Relation::Tensor { args: next, .. } = &next[0] else {
                panic!("tensor")
            };
            assert_eq!(first[0], first[1]);
            assert_ne!(first[0], next[0]);
            let mut float = [Argument {
                ty: tcx.mk_fp(FpTy::Ieee(32)),
                value: Some(Value::Scalar),
            }];
            identify_operands(&mut float);
            assert!(matches!(
                float[0].value,
                Some(Value::Opaque { scalar: true, .. })
            ));
        });
    }
}
