//! Reusable shape relations recovered from ordinary typed HIR bodies.
//!
//! RRI already carries generic bodies. Summaries are session-local views of that
//! HIR, not a serialized interface or a second form of method dispatch. A caller
//! supplies its normal fresh generic substitution when instantiating a summary.
//! Calls remain explicit graph edges, so recursion never causes eager unfolding.

use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

use crate::intrinsic::TensorFn;
use crate::literal::Integer;
use crate::semi::hir::{ArithOp, CmpOp, Expr, ExprKind, Function, VarId};
use crate::semi::infer::InferCtxt;
use crate::semi::ty::{DefId, GenericId, Ty, TyCtxt, TyKind};

/// Compile-time value information relevant to shape operations. Parameter
/// references are relative to the summary's own formal argument list.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value<'tcx> {
    Integer(&'tcx Integer),
    Bool(bool),
    /// A scalar literal whose numeric payload is irrelevant to shape inference.
    Scalar,
    /// Nonconstant operand provenance used only while composing summaries.
    /// The scalar flag retains a literal's family proof without inventing a
    /// numeric value. These identities never enter HIR or an interface.
    Opaque {
        id: u64,
        scalar: bool,
    },
    Tuple(Vec<Argument<'tcx>>),
    Parameter(usize),
    Projection {
        base: Box<Argument<'tcx>>,
        path: Vec<u32>,
    },
}

/// A typed operand, optionally retaining a constant or forwarded parameter.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Argument<'tcx> {
    pub ty: Ty<'tcx>,
    pub value: Option<Value<'tcx>>,
}

impl<'tcx> Argument<'tcx> {
    pub fn unknown(ty: Ty<'tcx>) -> Self {
        Self { ty, value: None }
    }

    pub fn from_expr(expr: &Expr<'tcx>) -> Self {
        Self::from_expr_with(expr, &FxHashMap::default())
    }

    pub(super) fn from_expr_with(expr: &Expr<'tcx>, vars: &FxHashMap<VarId, Self>) -> Self {
        let value = match &expr.kind {
            ExprKind::ConstInt(n) => Some(Value::Integer(n)),
            ExprKind::ConstBool(b) => Some(Value::Bool(*b)),
            ExprKind::ConstFloat(_) => Some(Value::Scalar),
            ExprKind::Var(v) => vars.get(v).and_then(|a| a.value.clone()),
            ExprKind::Tuple(fields) => Some(Value::Tuple(
                fields
                    .iter()
                    .map(|e| Self::from_expr_with(e, vars))
                    .collect(),
            )),
            ExprKind::Seq(es) => {
                let mut scoped = vars.clone();
                let mut result = None;
                for e in es {
                    if let ExprKind::Let { var, value, .. } = &e.kind {
                        scoped.insert(*var, Self::from_expr_with(value, &scoped));
                    }
                    result = Self::from_expr_with(e, &scoped).value;
                }
                result
            }
            ExprKind::Proj(base, path) => {
                let mut arg = Self::from_expr_with(base, vars);
                for &index in path {
                    arg = match arg.value {
                        Some(Value::Tuple(fields)) => match fields.get(index as usize) {
                            Some(field) => field.clone(),
                            None => return Self::unknown(expr.ty),
                        },
                        _ => {
                            return Self {
                                ty: expr.ty,
                                value: Some(Value::Projection {
                                    base: Box::new(Self::from_expr_with(base, vars)),
                                    path: path.clone(),
                                }),
                            };
                        }
                    };
                }
                arg.value
            }
            _ => None,
        };
        Self { ty: expr.ty, value }
    }

    /// Substitute only template types. Values supplied by the caller already
    /// live in its inference context and must not be freshened a second time.
    fn instantiate(&self, map_ty: &mut impl FnMut(Ty<'tcx>) -> Ty<'tcx>, actual: &[Self]) -> Self {
        let value = match &self.value {
            Some(Value::Parameter(i)) => actual.get(*i).and_then(|a| a.value.clone()),
            Some(Value::Tuple(fields)) => Some(Value::Tuple(
                fields
                    .iter()
                    .map(|a| a.instantiate(map_ty, actual))
                    .collect(),
            )),
            Some(Value::Projection { base, path }) => {
                let mut arg = base.instantiate(map_ty, actual);
                for &index in path {
                    arg = match arg.value {
                        Some(Value::Tuple(fields)) => match fields.get(index as usize) {
                            Some(field) => field.clone(),
                            None => return Self::unknown(map_ty(self.ty)),
                        },
                        _ => {
                            return Self {
                                ty: map_ty(self.ty),
                                value: Some(Value::Projection {
                                    base: Box::new(base.instantiate(map_ty, actual)),
                                    path: path.clone(),
                                }),
                            };
                        }
                    };
                }
                arg.value
            }
            value => value.clone(),
        };
        Self {
            ty: map_ty(self.ty),
            value,
        }
    }
}

/// Scalar obligations use semantic categories, not session-local trait IDs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScalarBound {
    Numeric,
    Equality,
    Ordering,
}

/// Relations refer to template types until instantiated. A call edge is an
/// obligation to consult the callee's summary, not permission to inspect names
/// or choose a trait implementation speculatively.
#[derive(Clone, Debug)]
pub enum Relation<'tcx> {
    /// A branch/loop predicate must be scalar Bool or rank-zero Tensor<Bool>.
    Condition {
        ty: Ty<'tcx>,
    },
    /// The operator family remains undecided until operands resolve. In
    /// particular a comparison can return Bool or a Tensor of Bool.
    Operator {
        op: TensorFn,
        scalar_bound: ScalarBound,
        result: Ty<'tcx>,
        args: Vec<Argument<'tcx>>,
    },
    Equal {
        left: Ty<'tcx>,
        right: Ty<'tcx>,
    },
    Tensor {
        op: TensorFn,
        result: Ty<'tcx>,
        args: Vec<Argument<'tcx>>,
    },
    Call {
        target: DefId,
        ty_args: Vec<Ty<'tcx>>,
        args: Vec<Argument<'tcx>>,
        result: Ty<'tcx>,
    },
    TraitCall {
        trait_def: DefId,
        method: u32,
        ty_args: Vec<Ty<'tcx>>,
        args: Vec<Argument<'tcx>>,
        result: Ty<'tcx>,
    },
}

impl<'tcx> Relation<'tcx> {
    pub fn instantiate(
        &self,
        map_ty: &mut impl FnMut(Ty<'tcx>) -> Ty<'tcx>,
        actual: &[Argument<'tcx>],
    ) -> Self {
        match self {
            Self::Condition { ty } => Self::Condition { ty: map_ty(*ty) },
            Self::Operator {
                op,
                scalar_bound,
                result,
                args,
            } => Self::Operator {
                op: *op,
                scalar_bound: *scalar_bound,
                result: map_ty(*result),
                args: args.iter().map(|a| a.instantiate(map_ty, actual)).collect(),
            },
            Self::Equal { left, right } => Self::Equal {
                left: map_ty(*left),
                right: map_ty(*right),
            },
            Self::Tensor { op, result, args } => Self::Tensor {
                op: *op,
                result: map_ty(*result),
                args: args.iter().map(|a| a.instantiate(map_ty, actual)).collect(),
            },
            Self::Call {
                target,
                ty_args,
                args,
                result,
            } => Self::Call {
                target: *target,
                ty_args: ty_args.iter().map(|&t| map_ty(t)).collect(),
                args: args.iter().map(|a| a.instantiate(map_ty, actual)).collect(),
                result: map_ty(*result),
            },
            Self::TraitCall {
                trait_def,
                method,
                ty_args,
                args,
                result,
            } => Self::TraitCall {
                trait_def: *trait_def,
                method: *method,
                ty_args: ty_args.iter().map(|&t| map_ty(t)).collect(),
                args: args.iter().map(|a| a.instantiate(map_ty, actual)).collect(),
                result: map_ty(*result),
            },
        }
    }
}

/// A function's constraints in its own binder space. This object contains no
/// mutable inference assignments and can be reused for arbitrarily many calls.
#[derive(Clone, Debug)]
pub struct Summary<'tcx> {
    pub def: DefId,
    pub generics: Vec<GenericId>,
    pub params: Vec<Ty<'tcx>>,
    pub result: Ty<'tcx>,
    pub relations: Vec<Relation<'tcx>>,
    /// A prototype supplies its signature, but no body-derived constraints.
    pub has_body: bool,
}

impl<'tcx> Summary<'tcx> {
    pub fn derive(tcx: &TyCtxt<'tcx>, function: &Function<'tcx>) -> Self {
        let mut summary = Self {
            def: function.def,
            generics: function.generics.iter().map(|&(_, g)| g).collect(),
            params: function.params.iter().map(|&(_, _, t)| t).collect(),
            result: function.return_ty,
            relations: Vec::new(),
            has_body: function.body.is_some(),
        };
        let Some(body) = &function.body else {
            return summary;
        };
        let mut vars: FxHashMap<VarId, Argument<'tcx>> = function
            .params
            .iter()
            .enumerate()
            .map(|(i, &(_, var, ty))| {
                (
                    var,
                    Argument {
                        ty,
                        value: Some(Value::Parameter(i)),
                    },
                )
            })
            .collect();
        summary.equal(function.return_ty, body.ty);
        // The existing HIR visitor covers nested closures, matches, branches,
        // tuples and loop regions. Each operation contributes its local rules;
        // function calls remain composable edges regardless of body size.
        crate::full::mono::for_each_expr(body, &mut |e| {
            let args_of = |args: &[Expr<'tcx>]| {
                args.iter()
                    .map(|a| Argument::from_expr_with(a, &vars))
                    .collect()
            };
            match &e.kind {
                ExprKind::TensorOp { op, args } => summary.relations.push(Relation::Tensor {
                    op: *op,
                    result: e.ty,
                    args: args_of(args),
                }),
                ExprKind::FuncCall {
                    target,
                    ty_args,
                    args,
                    ..
                } => summary.relations.push(Relation::Call {
                    target: *target,
                    ty_args: ty_args.clone(),
                    args: args_of(args),
                    result: e.ty,
                }),
                ExprKind::TraitCall {
                    trait_def,
                    method,
                    ty_args,
                    args,
                } => summary.relations.push(Relation::TraitCall {
                    trait_def: *trait_def,
                    method: *method,
                    ty_args: ty_args.clone(),
                    args: args_of(args),
                    result: e.ty,
                }),
                ExprKind::Arith(left, op, right) => {
                    summary.equal(left.ty, right.ty);
                    let tensor_op = match op {
                        ArithOp::Add => Some(TensorFn::Add),
                        ArithOp::Sub => Some(TensorFn::Sub),
                        ArithOp::Mul => Some(TensorFn::Mul),
                        ArithOp::Div => Some(TensorFn::Div),
                        ArithOp::Mod => Some(TensorFn::Rem),
                        // Integer-only bit operations already carry their
                        // scalar contract and do not have a Tensor overload.
                        _ => None,
                    };
                    if let Some(op) = tensor_op {
                        summary.relations.push(Relation::Operator {
                            op,
                            scalar_bound: ScalarBound::Numeric,
                            result: e.ty,
                            args: vec![
                                Argument::from_expr_with(left, &vars),
                                Argument::from_expr_with(right, &vars),
                            ],
                        });
                    } else {
                        summary.equal(e.ty, left.ty);
                    }
                }
                ExprKind::Cmp(left, op, right) => {
                    summary.equal(left.ty, right.ty);
                    let (op, scalar_bound) = match op {
                        CmpOp::Eq => (TensorFn::Eq, ScalarBound::Equality),
                        CmpOp::Ne => (TensorFn::Ne, ScalarBound::Equality),
                        CmpOp::Lt => (TensorFn::Lt, ScalarBound::Ordering),
                        CmpOp::Le => (TensorFn::Le, ScalarBound::Ordering),
                        CmpOp::Gt => (TensorFn::Gt, ScalarBound::Ordering),
                        CmpOp::Ge => (TensorFn::Ge, ScalarBound::Ordering),
                    };
                    summary.relations.push(Relation::Operator {
                        op,
                        scalar_bound,
                        result: e.ty,
                        args: vec![
                            Argument::from_expr_with(left, &vars),
                            Argument::from_expr_with(right, &vars),
                        ],
                    });
                }
                ExprKind::Negate(value) => summary.relations.push(Relation::Operator {
                    op: TensorFn::Neg,
                    scalar_bound: ScalarBound::Numeric,
                    result: e.ty,
                    args: vec![Argument::from_expr_with(value, &vars)],
                }),
                ExprKind::Not(value) => {
                    summary.equal(e.ty, value.ty);
                    summary.equal(e.ty, tcx.mk_bool());
                }
                ExprKind::If(condition, then, otherwise) => {
                    summary
                        .relations
                        .push(Relation::Condition { ty: condition.ty });
                    summary.equal(e.ty, then.ty);
                    summary.equal(e.ty, otherwise.ty);
                }
                ExprKind::While {
                    initial,
                    condition,
                    body,
                    ..
                } => {
                    summary
                        .relations
                        .push(Relation::Condition { ty: condition.ty });
                    summary.equal(e.ty, initial.ty);
                    summary.equal(e.ty, body.ty);
                }
                ExprKind::Tuple(fields) => {
                    summary.equal(
                        e.ty,
                        tcx.mk_tuple(&fields.iter().map(|e| e.ty).collect::<Vec<_>>()),
                    );
                }
                ExprKind::Seq(es) => {
                    if let Some(last) = es.last() {
                        summary.equal(e.ty, last.ty);
                    }
                }
                ExprKind::Let { var, value, .. } => {
                    let mut argument = Argument::from_expr_with(value, &vars);
                    if argument.value.is_none() {
                        // Repeated uses of one computed local denote the same
                        // operand, even when its value is not statically known.
                        argument.value = Some(opaque_value(false));
                    }
                    vars.insert(*var, argument);
                }
                // Other nodes are still traversed. Their ground signatures
                // already describe their type relationships; opaque calls and
                // casts do not justify inventing a shape equality.
                _ => {}
            }
        });
        summary
    }

    fn equal(&mut self, left: Ty<'tcx>, right: Ty<'tcx>) {
        if left != right {
            self.relations.push(Relation::Equal { left, right });
        }
    }

    /// The mapping must be a single fresh substitution for this invocation,
    /// including fresh body-local holes if any. The caller owns unification.
    pub fn instantiate(
        &self,
        mut map_ty: impl FnMut(Ty<'tcx>) -> Ty<'tcx>,
        actual: &[Argument<'tcx>],
    ) -> Vec<Relation<'tcx>> {
        let mut relations: Vec<_> = self
            .params
            .iter()
            .zip(actual)
            .map(|(&formal, actual)| Relation::Equal {
                left: map_ty(formal),
                right: actual.ty,
            })
            .collect();
        let mut local_values = FxHashMap::default();
        relations.extend(self.relations.iter().map(|template| {
            let mut relation = template.clone();
            match &mut relation {
                Relation::Operator { args, .. }
                | Relation::Tensor { args, .. }
                | Relation::Call { args, .. }
                | Relation::TraitCall { args, .. } => {
                    freshen_operand_values(args, &mut local_values);
                }
                Relation::Equal { .. } | Relation::Condition { .. } => {}
            }
            relation.instantiate(&mut map_ty, actual)
        }));
        relations
    }
}

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

/// A typed call edge in the caller's inference context.
#[derive(Clone, Debug)]
pub struct Call<'tcx> {
    pub target: DefId,
    pub ty_args: Vec<Ty<'tcx>>,
    pub args: Vec<Argument<'tcx>>,
    pub result: Ty<'tcx>,
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

/// A process-unique token avoids collisions when residual trait calls carry
/// operand facts into a later expansion. Memo tables themselves remain local
/// to one expansion, so no inference assignments leak between invocations.
fn opaque_value(scalar: bool) -> Value<'static> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT_OPERAND: AtomicU64 = AtomicU64::new(0);
    Value::Opaque {
        id: NEXT_OPERAND.fetch_add(1, Ordering::Relaxed),
        scalar,
    }
}

/// Freshen only opaque facts contained in the template. Caller values arrive
/// later through Parameter substitution and are deliberately not visited here.
fn freshen_operand_values<'tcx>(
    args: &mut [Argument<'tcx>],
    mapped: &mut FxHashMap<u64, Value<'tcx>>,
) {
    for argument in args {
        match &mut argument.value {
            Some(Value::Opaque { id, scalar }) => {
                argument.value = Some(
                    mapped
                        .entry(*id)
                        .or_insert_with(|| opaque_value(*scalar))
                        .clone(),
                );
            }
            Some(Value::Tuple(fields)) => freshen_operand_values(fields, mapped),
            Some(Value::Projection { base, .. }) => {
                freshen_operand_values(std::slice::from_mut(base.as_mut()), mapped)
            }
            _ => {}
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

    /// Bind the callee's declared result as well as its formal arguments.
    /// The mapping belongs to this one invocation, never to the cached template.
    pub fn instantiate_call(
        &self,
        mut map_ty: impl FnMut(Ty<'tcx>) -> Ty<'tcx>,
        args: &[Argument<'tcx>],
        result: Ty<'tcx>,
    ) -> Vec<Relation<'tcx>> {
        let declared_result = map_ty(self.result);
        let mut relations = self.instantiate(map_ty, args);
        relations.push(Relation::Equal {
            left: declared_result,
            right: result,
        });
        relations
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
    use std::sync::Arc;

    use reussir_syntax::source::FileId;
    use reussir_syntax::{Interner, SessionInterner};

    use super::*;
    use crate::semi::hir::{build::parse_program, print::Printer};
    use crate::semi::infer::InferCtxt;
    use crate::semi::ty::{FpTy, IntTy};
    use crate::semi::{Elaborator, ExternPackage, PackageFile};

    fn imported<'a, 'tcx>(
        tcx: &'a TyCtxt<'tcx>,
        source: &str,
        interner: &'a SessionInterner,
    ) -> Elaborator<'a, 'tcx> {
        let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
        assert!(parsed.ok(), "{:?}", parsed.errors);
        let program = crate::surface::program(&parsed.root);
        let mut keys = interner.clone();
        let files = [PackageFile {
            file: FileId::ROOT,
            module: vec![keys.get_or_intern("dep")],
            program: &program,
        }];
        let producer = crate::semi::elaborate_package(tcx, &files, interner);
        assert!(!producer.has_errors(), "{:?}", producer.reports);
        let text = Printer::new(&producer.defs, producer.resolver).program(
            &producer.elaborated,
            &producer.strings.entries(),
            &producer.records,
            &producer.trampolines,
        );
        let text = format!("interface 1 package \"dep\" producer \"rrc 0.1.0\";\n{text}");
        let parsed = parse_program(tcx, &text).expect("RRI body roundtrip");
        let mut consumer = Elaborator::new(tcx, interner);
        consumer.declare_extern_package(
            &mut keys,
            &ExternPackage {
                name: "dep",
                parsed: &parsed,
                files: &[],
            },
        );
        assert!(!consumer.has_errors(), "{:?}", consumer.reports);
        consumer
    }

    #[test]
    fn imported_ordinary_wrapper_graph_preserves_all_tensor_relations() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] fn matrix<T>(x: T) -> Tensor<[T; 2, 3]> {
                    let filled: Tensor<[T; 3, 2]> = core::intrinsic::tensor::fill(x);
                    core::intrinsic::tensor::transpose(filled)
                }
                #[kernel] fn middle<T>(x: T) -> Tensor<[T; 2, 3]> { matrix(x) }
                #[kernel] pub fn outer<T>(x: T) -> Tensor<[T; 2, 3]> { middle(x) }
                #[kernel] pub fn product<T>(a: Tensor<[T; 2, 3]>, b: Tensor<[T; 3, 4]>,
                                           p: Tensor<[bool; 2, 4]>) -> Tensor<[T; 2, 4]> {
                    let d = core::intrinsic::tensor::dot(a, b);
                    core::intrinsic::tensor::select(p, d, d)
                }
            "#,
                &interner,
            );
            let functions = &elab.extern_functions;
            let find = |name: &str| {
                functions
                    .iter()
                    .find(|f| elab.defs.path(f.def).display(elab.resolver) == name)
                    .unwrap()
            };
            let mut cache = SummaryCache::default();
            let graph = cache.collect(tcx, find("dep::outer").def, |d| {
                functions.iter().find(|f| f.def == d)
            });
            assert_eq!(graph.len(), 3);
            let matrix = cache.get(find("dep::matrix").def).unwrap();
            let ops: Vec<_> = matrix
                .relations
                .iter()
                .filter_map(|r| match r {
                    Relation::Tensor { op, .. } => Some(*op),
                    _ => None,
                })
                .collect();
            assert_eq!(ops, [TensorFn::Fill, TensorFn::Transpose]);
            let product = Summary::derive(tcx, find("dep::product"));
            let ops: Vec<_> = product
                .relations
                .iter()
                .filter_map(|r| match r {
                    Relation::Tensor { op, .. } => Some(*op),
                    _ => None,
                })
                .collect();
            assert_eq!(ops, [TensorFn::Dot, TensorFn::Select]);
        });
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
    fn two_instantiations_do_not_share_type_holes_or_parameter_values() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn grid<T>(axis: (i64,)) -> Tensor<[T; ?, 3]> {
                    let a = axis.0;
                    core::intrinsic::tensor::dynamic_iota((a, 3), 0)
                }
            "#,
                &interner,
            );
            let summary = Summary::derive(tcx, &elab.extern_functions[0]);
            let mut infer = InferCtxt::new(tcx);
            let first = infer.instantiate(&summary.generics);
            let second = infer.instantiate(&summary.generics);
            let elem = tcx.mk(TyKind::Generic(summary.generics[0]));
            let first_elem = infer.instantiate_ty(elem, &first);
            let second_elem = infer.instantiate_ty(elem, &second);
            assert_ne!(first_elem, second_elem);
            infer.unify(first_elem, tcx.mk_fp(FpTy::Ieee(32))).unwrap();
            infer.unify(second_elem, tcx.mk_fp(FpTy::Ieee(64))).unwrap();

            let i64_ty = tcx.mk_int(IntTy::Signed(64));
            let axis = |n| Argument {
                ty: tcx.mk_tuple(&[i64_ty]),
                value: Some(Value::Tuple(vec![Argument {
                    ty: i64_ty,
                    value: Some(Value::Integer(tcx.alloc(Integer::from(n)))),
                }])),
            };
            for (inst, n) in [(&first, 0i64), (&second, 1i64)] {
                let relations = summary.instantiate(|t| infer.instantiate_ty(t, inst), &[axis(n)]);
                let argument = relations
                    .iter()
                    .find_map(|r| match r {
                        Relation::Tensor {
                            op: TensorFn::DynamicIota,
                            args,
                            ..
                        } => args.first(),
                        _ => None,
                    })
                    .unwrap();
                match &argument.value {
                    Some(Value::Tuple(fields)) => match fields[0].value {
                        Some(Value::Integer(value)) => assert_eq!(value, &Integer::from(n)),
                        _ => panic!("tuple parameter value did not reach dynamic_iota"),
                    },
                    _ => panic!("dynamic_iota shape tuple was lost"),
                }
            }
            assert_eq!(infer.resolve(first_elem), tcx.mk_fp(FpTy::Ieee(32)));
            assert_eq!(infer.resolve(second_elem), tcx.mk_fp(FpTy::Ieee(64)));
        });
    }

    #[test]
    fn nested_control_flow_is_traversed_without_executing_it() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn state<T>(x: Tensor<[T; 2, 2]>) -> (Tensor<[T; 2, 2]>, ()) {
                    let y = while [s = x] (false) {
                        if true { core::intrinsic::tensor::transpose(s) } else { s }
                    };
                    (y, ())
                }
            "#,
                &interner,
            );
            let summary = Summary::derive(tcx, &elab.extern_functions[0]);
            assert_eq!(
                summary
                    .relations
                    .iter()
                    .filter(|r| matches!(r, Relation::Condition { .. }))
                    .count(),
                2
            );
            let mapped = summary.instantiate(
                |ty| {
                    if ty == tcx.mk_bool() {
                        tcx.mk_tensor(ty, &[])
                    } else {
                        ty
                    }
                },
                &[],
            );
            assert!(
                mapped
                    .iter()
                    .filter_map(|r| match r {
                        Relation::Condition { ty } => Some(*ty),
                        _ => None,
                    })
                    .all(|ty| ty == tcx.mk_tensor(tcx.mk_bool(), &[]))
            );
            assert!(summary.relations.iter().any(|r| matches!(
                r,
                Relation::Tensor {
                    op: TensorFn::Transpose,
                    ..
                }
            )));
        });
    }

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
    fn nested_block_and_tuple_projection_preserve_scalar_literal_facts() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn filled() -> Tensor<[f32; 2]> {
                    core::intrinsic::tensor::fill({
                        let pair = (1.0, ());
                        let value = pair.0;
                        value
                    })
                }
            "#,
                &interner,
            );
            let summary = Summary::derive(tcx, &elab.extern_functions[0]);
            let args = summary
                .relations
                .iter()
                .find_map(|r| match r {
                    Relation::Tensor {
                        op: TensorFn::Fill,
                        args,
                        ..
                    } => Some(args),
                    _ => None,
                })
                .expect("fill relation");
            assert!(matches!(args[0].value, Some(Value::Scalar)));
        });
    }

    #[test]
    fn comparison_summary_keeps_operator_family_and_freshens_its_result() {
        crate::with_tcx(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let elab = imported(
                tcx,
                r#"
                #[kernel] pub fn compare(x: i64, y: i64) -> bool { x < y }
            "#,
                &interner,
            );
            let summary = Summary::derive(tcx, &elab.extern_functions[0]);
            assert!(summary.relations.iter().any(|r| matches!(
                r,
                Relation::Operator {
                    op: TensorFn::Lt,
                    scalar_bound: ScalarBound::Ordering,
                    ..
                }
            )));
            let mut infer = InferCtxt::new(tcx);
            let result = infer.new_hole_ty();
            let relations =
                summary.instantiate(|ty| if ty == tcx.mk_bool() { result } else { ty }, &[]);
            assert!(relations.iter().any(|r| matches!(r,
                Relation::Operator { result: r, .. } if *r == result)));
            assert!(!relations.iter().any(|r| matches!(r,
                Relation::Equal { left, right } if (*left == result && *right == tcx.mk_bool()) || (*right == result && *left == tcx.mk_bool()))));
        });
    }
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
