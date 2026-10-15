//! Reusable shape relations recovered from ordinary typed HIR bodies.
//!
//! RRI already carries generic bodies. Summaries are session-local views of that
//! HIR, not a serialized interface or a second form of method dispatch. A caller
//! supplies its normal fresh generic substitution when instantiating a summary.
//! Calls remain explicit graph edges, so recursion never causes eager unfolding.

mod expansion;
mod fresh;
pub use expansion::{Expansion, ResidualCall, ResidualReason, SummaryCache};

use rustc_hash::FxHashMap;

use crate::intrinsic::TensorFn;
use crate::literal::Integer;
use crate::semi::hir::{ArithOp, CmpOp, Expr, ExprKind, Function, VarId};
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

/// A typed call edge in the caller's inference context.
#[derive(Clone, Debug)]
pub struct Call<'tcx> {
    pub target: DefId,
    pub ty_args: Vec<Ty<'tcx>>,
    pub args: Vec<Argument<'tcx>>,
    pub result: Ty<'tcx>,
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

impl<'tcx> Summary<'tcx> {
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

#[cfg(test)]
mod tests;
