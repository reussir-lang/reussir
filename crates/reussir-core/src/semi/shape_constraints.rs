//! Deferred relations collected by bidirectional expression checking.
//!
//! This module schedules constraints and owns diagnostic spans. Operation
//! semantics belong exclusively to `rules`; the inference store knows only
//! equality, rank, and dimensions. Neither syntax nor a method's spelling is
//! used to select an operation rule.

mod calls;
use super::shape_rules as rules;
#[cfg(test)]
mod tests;

use crate::intrinsic::TensorFn;
use crate::semi::ctxt::Elaborator;
use crate::semi::hir::{Expr, ExprKind, VarId};
use crate::semi::shape_summary::{
    Argument, Call, Relation, ResidualReason, ScalarBound, SummaryCache, Value,
};
use crate::semi::traits::TraitId;
use crate::semi::ty::{DefId, Ty, TyKind};
use crate::surface::Span;
use rules::{Phase, RuleStatus};

#[derive(Clone, Debug)]
pub(super) enum Constraint<'tcx> {
    Relation(Relation<'tcx>),
    RecursiveCall(Call<'tcx>),
    Condition(Ty<'tcx>),
    /// Surface arithmetic/comparison has scalar and Tensor interpretations.
    /// Selecting a family is deferred; a scalar trait obligation must never be
    /// attached to a value that subsequently resolves to a Tensor.
    Operator {
        op: TensorFn,
        scalar_bound: Option<TraitId>,
        result: Ty<'tcx>,
        args: Vec<Argument<'tcx>>,
    },
}

#[derive(Clone, Debug)]
pub(super) struct Pending<'tcx> {
    constraint: Constraint<'tcx>,
    span: Option<Span>,
    ancestry: Vec<DefId>,
}

#[derive(Default)]
pub(super) struct ShapeConstraints<'tcx> {
    pending: Vec<Pending<'tcx>>,
    // These contracts are recovered from generic HIR by shape_summary and
    // reinstantiated at each call; they are not considered discharged here.
    residual: Vec<Pending<'tcx>>,
    summaries: SummaryCache<'tcx>,
    scalar_types: Vec<Ty<'tcx>>,
    active_ancestry: Vec<DefId>,
    values: rustc_hash::FxHashMap<VarId, Argument<'tcx>>,
}

impl<'tcx> ShapeConstraints<'tcx> {
    pub(super) fn register_scalar(&mut self, ty: Ty<'tcx>) {
        self.scalar_types.push(ty);
    }

    pub(super) fn register(&mut self, constraint: Constraint<'tcx>, span: Option<Span>) {
        self.pending.push(Pending {
            constraint,
            span,
            ancestry: self.active_ancestry.clone(),
        });
    }
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// Every source construction enters here, including trait calls and the
    /// older async/slice intrinsics. New Tensor operations cannot accidentally
    /// bypass the common relation layer by using another surface spelling.
    pub(super) fn register_expression_relations(
        &mut self,
        kind: &ExprKind<'tcx>,
        ty: Ty<'tcx>,
        span: Option<Span>,
    ) {
        match kind {
            ExprKind::Let { var, value, .. } => {
                let fact = Argument::from_expr_with(value, &self.fulfill.shapes.values);
                self.fulfill.shapes.values.insert(*var, fact);
            }
            ExprKind::TensorOp { op, args } => self.register_tensor_relation(*op, ty, args, span),
            ExprKind::FuncCall {
                target,
                ty_args,
                args,
                ..
            } => self.register_shape_call(*target, ty_args, args, ty, span),
            ExprKind::TraitCall {
                trait_def,
                method,
                ty_args,
                args,
            } if self.inside_kernel => {
                let args = self.source_arguments(args);
                self.fulfill.shapes.register(
                    Constraint::Relation(Relation::TraitCall {
                        trait_def: *trait_def,
                        method: *method,
                        ty_args: ty_args.clone(),
                        args,
                        result: ty,
                    }),
                    span,
                );
                self.resolve_shape_constraints(false);
            }
            _ => {}
        }
    }

    fn source_arguments(&self, args: &[Expr<'tcx>]) -> Vec<Argument<'tcx>> {
        args.iter()
            .map(|arg| Argument::from_expr_with(arg, &self.fulfill.shapes.values))
            .collect()
    }

    pub(super) fn register_condition_relation(&mut self, ty: Ty<'tcx>, span: Option<Span>) {
        self.fulfill
            .shapes
            .register(Constraint::Condition(ty), span);
        self.resolve_shape_constraints(false);
    }

    pub(super) fn register_shape_call(
        &mut self,
        target: DefId,
        ty_args: &[Ty<'tcx>],
        args: &[Expr<'tcx>],
        result: Ty<'tcx>,
        span: Option<Span>,
    ) {
        if !self.inside_kernel && !self.functions.get(&target).is_some_and(|f| f.is_kernel) {
            return;
        }
        self.expand_shape_call(
            Call {
                target,
                ty_args: ty_args.to_vec(),
                result,
                args: self.source_arguments(args),
            },
            span,
        );
        self.resolve_shape_constraints(false);
    }

    pub(super) fn expand_shape_call(&mut self, call: Call<'tcx>, span: Option<Span>) {
        if self.fulfill.shapes.active_ancestry.contains(&call.target) {
            self.fulfill
                .shapes
                .register(Constraint::RecursiveCall(call), span);
            return;
        }
        self.fulfill.shapes.active_ancestry.push(call.target);
        let mut cache = std::mem::take(&mut self.fulfill.shapes.summaries);
        let infer = &mut self.infer;
        let functions = &self.elaborated;
        let externs = &self.extern_functions;
        let tcx = self.tcx;
        let expansion = cache.expand(
            tcx,
            call,
            |def| functions.iter().chain(externs).find(|f| f.def == def),
            |summary, call| summary.instantiate_fresh(tcx, infer, call),
        );
        self.fulfill.shapes.summaries = cache;
        for relation in expansion.relations {
            if matches!(&relation, Relation::Tensor { op, .. } if !rules::supports(*op)) {
                continue;
            }
            self.fulfill
                .shapes
                .register(Constraint::Relation(relation), span);
        }
        for residual in expansion.residual_calls {
            if residual.reason == ResidualReason::Opaque {
                // Its signature equalities were included by the expansion.
                continue;
            }
            let call = residual.call;
            if residual.reason == ResidualReason::Recursive {
                self.fulfill
                    .shapes
                    .register(Constraint::RecursiveCall(call), span);
            } else {
                self.fulfill.shapes.register(
                    Constraint::Relation(Relation::Call {
                        target: call.target,
                        ty_args: call.ty_args,
                        args: call.args,
                        result: call.result,
                    }),
                    span,
                );
            }
        }
        self.fulfill.shapes.active_ancestry.pop();
    }

    fn has_scalar_evidence(&mut self, ty: Ty<'tcx>) -> bool {
        let ty = self.infer.shallow_resolve(ty);
        matches!(
            ty.kind(),
            TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool | TyKind::Char
        ) || self
            .fulfill
            .shapes
            .scalar_types
            .iter()
            .any(|&known| self.infer.shallow_resolve(known) == ty)
    }

    fn argument_evidence(&mut self, args: &[Argument<'tcx>]) -> Vec<Argument<'tcx>> {
        args.iter()
            .cloned()
            .map(|arg| self.argument_evidence_one(arg))
            .collect()
    }

    fn argument_evidence_one(&mut self, mut arg: Argument<'tcx>) -> Argument<'tcx> {
        // These are transient rule inputs. Keep summary memoization provenance
        // intact while exposing caller trait evidence, including tuple fields.
        if self.has_scalar_evidence(arg.ty) {
            match &mut arg.value {
                None | Some(Value::Projection { .. } | Value::Parameter(_)) => {
                    arg.value = Some(Value::Scalar);
                }
                Some(Value::Opaque { scalar, .. }) => *scalar = true,
                _ => {}
            }
        }
        let fields = match &arg.value {
            Some(Value::Tuple(fields)) => Some(fields.clone()),
            _ => match self.infer.shallow_resolve(arg.ty).kind() {
                TyKind::Tuple(fields) => {
                    Some(fields.iter().copied().map(Argument::unknown).collect())
                }
                _ => None,
            },
        };
        if let Some(fields) = fields {
            arg.value = Some(Value::Tuple(
                fields
                    .into_iter()
                    .map(|field| self.argument_evidence_one(field))
                    .collect(),
            ));
        }
        arg
    }

    pub(super) fn register_tensor_relation(
        &mut self,
        op: TensorFn,
        result: Ty<'tcx>,
        args: &[Expr<'tcx>],
        span: Option<Span>,
    ) {
        if !rules::supports(op) {
            return;
        }
        let args = self.source_arguments(args);
        self.fulfill.shapes.register(
            Constraint::Relation(Relation::Tensor { op, result, args }),
            span,
        );
        self.resolve_shape_constraints(false);
    }

    pub(super) fn register_operator_relation(
        &mut self,
        op: TensorFn,
        scalar_bound: Option<TraitId>,
        result: Ty<'tcx>,
        args: &[Expr<'tcx>],
        span: Option<Span>,
    ) {
        let args = self.source_arguments(args);
        self.fulfill.shapes.register(
            Constraint::Operator {
                op,
                scalar_bound,
                result,
                args,
            },
            span,
        );
        self.resolve_shape_constraints(false);
    }

    /// Run a fair fixed point. Deferred entries stay live across later source
    /// uses; only proof, contradiction, or the final ambiguity boundary removes
    /// them. Allocating an inference variable is not solving a constraint.
    pub(super) fn resolve_shape_constraints(&mut self, finalizing: bool) {
        self.resolve_shape_constraints_in_phase(finalizing, Phase::Propagate);
    }

    pub(super) fn default_runtime_shapes(&mut self) {
        self.resolve_shape_constraints_in_phase(false, Phase::DefaultRuntime);
    }

    fn resolve_shape_constraints_in_phase(&mut self, finalizing: bool, phase: Phase) {
        let mut pending = std::mem::take(&mut self.fulfill.shapes.pending);
        loop {
            let revision = self.infer.progress();
            let mut deferred = Vec::new();
            for entry in pending {
                let ancestry = std::mem::replace(
                    &mut self.fulfill.shapes.active_ancestry,
                    entry.ancestry.clone(),
                );
                let outcome = self.apply_shape_constraint(&entry.constraint, entry.span, phase);
                self.fulfill.shapes.active_ancestry = ancestry;
                match outcome {
                    Ok(RuleStatus::Solved) => {}
                    Ok(RuleStatus::Deferred) => deferred.push(entry),
                    Err(reason) => self.error(
                        entry.span,
                        format!("type mismatch in shape relation: {reason}"),
                    ),
                }
            }
            pending = deferred;
            // Trait registration can enqueue additional work in the future;
            // include it before deciding whether the fixed point is stable.
            let added_relations = !self.fulfill.shapes.pending.is_empty();
            pending.append(&mut self.fulfill.shapes.pending);
            if pending.is_empty() || (!added_relations && self.infer.progress() == revision) {
                break;
            }
        }
        if finalizing {
            for entry in pending {
                let types = self.shape_constraint_types(&entry.constraint);
                let mut holes = Vec::new();
                let mut generic = false;
                for ty in types {
                    let ty = self.infer.resolve(ty);
                    super::fulfill::collect_holes(ty, &mut holes);
                    generic |= has_generic(ty);
                }
                if !holes.is_empty() {
                    if holes.iter().any(|h| !self.reported_holes.contains(h)) {
                        self.error(entry.span, "type annotations needed: cannot determine tensor shape or operation family");
                    }
                    self.reported_holes.extend(holes);
                } else if generic
                    || matches!(&entry.constraint,
                    Constraint::Relation(Relation::Call { target, .. }) if self.is_body_check_active(*target))
                {
                    self.fulfill.shapes.residual.push(entry);
                } else {
                    self.error(entry.span, "cannot establish the deferred shape relation");
                }
            }
        } else {
            self.fulfill.shapes.pending = pending;
        }
    }

    fn apply_shape_constraint(
        &mut self,
        constraint: &Constraint<'tcx>,
        span: Option<Span>,
        phase: Phase,
    ) -> Result<RuleStatus, String> {
        match constraint {
            Constraint::RecursiveCall(call) => self.check_shape_call_contract(call),
            Constraint::Condition(ty) | Constraint::Relation(Relation::Condition { ty }) => {
                if matches!(self.infer.resolve(*ty).kind(), TyKind::Tensor { elem, dims }
                    if dims.is_empty() && matches!(elem.kind(), TyKind::Generic(_)))
                {
                    return Ok(RuleStatus::Deferred);
                }
                if let Some(view) = self.infer.tensor_view(*ty) {
                    let rank_zero = self.infer.ranked_shape(Vec::new());
                    self.infer
                        .unify_shapes(view.shape, rank_zero)
                        .map_err(|e| format!("type mismatch: condition requires rank zero: {e}"))?;
                    self.infer
                        .unify(view.elem, self.tcx.mk_bool())
                        .map_err(|_| {
                            "type mismatch: kernel condition requires a bool element type"
                                .to_owned()
                        })?;
                } else if matches!(self.infer.shallow_resolve(*ty).kind(), TyKind::Hole(_)) {
                    return Ok(RuleStatus::Deferred);
                } else {
                    self.infer.unify(*ty, self.tcx.mk_bool()).map_err(|_| {
                        "type mismatch: condition must have type bool or rank-zero Tensor<bool>"
                            .to_owned()
                    })?;
                }
                Ok(RuleStatus::Solved)
            }
            Constraint::Relation(Relation::Tensor { op, result, args }) => {
                let args = self.argument_evidence(args);
                rules::apply(&mut self.infer, self.tcx, *op, *result, &args, phase)
            }
            Constraint::Relation(Relation::Operator {
                op,
                scalar_bound,
                result,
                args,
            }) => {
                use crate::semi::lang::LangItem;
                let scalar_bound = match scalar_bound {
                    ScalarBound::Numeric => Some(self.lang.num),
                    ScalarBound::Equality | ScalarBound::Ordering => {
                        let item = if *scalar_bound == ScalarBound::Equality {
                            LangItem::PartialEq
                        } else {
                            LangItem::PartialOrd
                        };
                        self.lang
                            .get(item)
                            .and_then(|def| self.traits.trait_by_def(def))
                    }
                };
                self.apply_shape_constraint(
                    &Constraint::Operator {
                        op: *op,
                        scalar_bound,
                        result: *result,
                        args: args.clone(),
                    },
                    span,
                    phase,
                )
            }
            Constraint::Relation(Relation::Equal { left, right }) => {
                self.infer
                    .unify(*left, *right)
                    .map_err(|_| "incompatible types in shape relation".to_owned())?;
                Ok(RuleStatus::Solved)
            }
            Constraint::Operator {
                op,
                scalar_bound,
                result,
                args,
            } => {
                let Some(first) = args.first() else {
                    return Err("missing operator operand".into());
                };
                if self.infer.tensor_view(first.ty).is_some()
                    || self.infer.tensor_view(*result).is_some()
                {
                    return rules::apply(&mut self.infer, self.tcx, *op, *result, args, phase);
                }
                let operand = self.infer.resolve(first.ty);
                let scalar_literal = self.has_scalar_evidence(first.ty)
                    || args.iter().any(|arg| {
                        matches!(
                            arg.value,
                            Some(
                                Value::Integer(_)
                                    | Value::Bool(_)
                                    | Value::Scalar
                                    | Value::Opaque { scalar: true, .. }
                            )
                        )
                    })
                    || (op.is_comparison()
                        && matches!(self.infer.resolve(*result).kind(), TyKind::Bool));
                if matches!(operand.kind(), TyKind::Hole(_)) && !scalar_literal {
                    return Ok(RuleStatus::Deferred);
                }
                let expected = if op.is_comparison() {
                    self.tcx.mk_bool()
                } else {
                    operand
                };
                self.infer
                    .unify(*result, expected)
                    .map_err(|_| "incompatible scalar operator result".to_owned())?;
                if let Some(bound) = scalar_bound {
                    self.register_bound(*bound, operand, span);
                }
                Ok(RuleStatus::Solved)
            }
            // Call graph expansion supplies primitive relations through the
            // summary adapter. A missing body is a residual contract, never an
            // assertion that the relationship has already been established.
            Constraint::Relation(Relation::Call {
                target,
                ty_args,
                args,
                result,
            }) => {
                let call = Call {
                    target: *target,
                    ty_args: ty_args.clone(),
                    args: args.clone(),
                    result: *result,
                };
                if self.is_body_check_active(*target) {
                    self.check_shape_call_contract(&call)
                } else {
                    Ok(RuleStatus::Deferred)
                }
            }
            Constraint::Relation(Relation::TraitCall {
                trait_def,
                method,
                ty_args,
                args,
                result,
            }) => self.resolve_shape_trait_call(*trait_def, *method, ty_args, args, *result, span),
        }
    }

    fn check_shape_call_contract(&mut self, call: &Call<'tcx>) -> Result<RuleStatus, String> {
        // Recursion assumes the declared contract, exactly as ordinary
        // bidirectional function checking. It does not assume the callee body's
        // additional shape equations; those require an independently available
        // summary. This also bounds polymorphically recursive expansion.
        let Some(proto) = self.functions.get(&call.target).cloned() else {
            return Ok(RuleStatus::Deferred);
        };
        if proto.params.len() != call.args.len() || proto.generics.len() != call.ty_args.len() {
            return Err("recursive shape contract has incompatible arguments".into());
        }
        let inst = super::infer::Instantiation::from_pairs(
            proto
                .generics
                .iter()
                .zip(&call.ty_args)
                .map(|((_, g), &t)| (*g, t)),
        );
        for ((_, parameter), argument) in proto.params.iter().zip(&call.args) {
            self.infer
                .unify_instantiated(*parameter, &inst, argument.ty)
                .map_err(|_| "recursive call violates its parameter type contract".to_owned())?;
        }
        self.infer
            .unify_instantiated(proto.return_ty, &inst, call.result)
            .map_err(|_| "recursive call violates its result type contract".to_owned())?;
        Ok(RuleStatus::Solved)
    }

    fn shape_constraint_types(&self, constraint: &Constraint<'tcx>) -> Vec<Ty<'tcx>> {
        match constraint {
            Constraint::RecursiveCall(call) => std::iter::once(call.result)
                .chain(call.args.iter().map(|a| a.ty))
                .chain(call.ty_args.iter().copied())
                .collect(),
            Constraint::Condition(ty) | Constraint::Relation(Relation::Condition { ty }) => {
                vec![*ty]
            }
            Constraint::Relation(Relation::Equal { left, right }) => vec![*left, *right],
            Constraint::Operator { result, args, .. }
            | Constraint::Relation(
                Relation::Operator { result, args, .. } | Relation::Tensor { result, args, .. },
            ) => std::iter::once(*result)
                .chain(args.iter().map(|a| a.ty))
                .collect(),
            Constraint::Relation(
                Relation::Call {
                    result,
                    args,
                    ty_args,
                    ..
                }
                | Relation::TraitCall {
                    result,
                    args,
                    ty_args,
                    ..
                },
            ) => std::iter::once(*result)
                .chain(args.iter().map(|a| a.ty))
                .chain(ty_args.iter().copied())
                .collect(),
        }
    }
}

fn has_generic(ty: Ty<'_>) -> bool {
    match ty.kind() {
        TyKind::Generic(_) => true,
        TyKind::Record { args, .. } | TyKind::Future(args) | TyKind::Tuple(args) => {
            args.iter().any(|&t| has_generic(t))
        }
        TyKind::Closure { params, ret } => {
            params.iter().any(|&t| has_generic(t)) || has_generic(*ret)
        }
        TyKind::Nullable(t)
        | TyKind::Arc(t)
        | TyKind::Cell { elem: t, .. }
        | TyKind::Array { elem: t, .. }
        | TyKind::Tensor { elem: t, .. } => has_generic(*t),
        TyKind::DevArray {
            elem, placement, ..
        } => has_generic(*elem) || has_generic(*placement),
        _ => false,
    }
}
