//! Integer decision trees become StableHLO case regions. Case indices are
//! synthesized from full-width comparisons, so wide integer keys never alias.
use super::kernel::{KernelEnv, KernelLowerer};
use super::{Result, err};
use reussir_backend::melior::ir::{Block, Region, RegionLike, Value};
use reussir_core::full::mir::{Binding, DecisionTree, Expr, SwitchCases};
use reussir_core::literal::Integer;
use reussir_core::semi::hir::CmpOp;
use reussir_core::semi::ty::{IntTy, Ty, TyKind};
use reussir_core::surface::Span;

impl<'c, 'p, 'tcx> KernelLowerer<'c, 'p, 'tcx> {
    pub(super) fn match_expr<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        scrutinee: &Expr<'tcx>,
        tree: DecisionTree<'tcx>,
        ty: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        if !matches!(scrutinee.ty.kind(), TyKind::Int(_)) {
            return Err(self.error("kernel match scrutinee must be an integer", span));
        }
        let values = self.expr(block, env, scrutinee)?;
        if values.len() != 1 {
            return err("kernel integer match requires one SSA scrutinee");
        }
        self.match_tree(block, env, values[0], scrutinee.ty, tree, ty, span)
    }

    fn match_bindings<'b>(
        &self,
        env: &mut KernelEnv<'c, 'b>,
        value: Value<'c, 'b>,
        bindings: &[Binding<'tcx>],
    ) -> Result<()> {
        for (var, path) in bindings {
            if !path.is_empty() {
                return err("kernel integer pattern binding cannot project its scrutinee");
            }
            env.vars.insert(*var, vec![value]);
        }
        Ok(())
    }

    fn match_tree<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        value: Value<'c, 'b>,
        source_ty: Ty<'tcx>,
        tree: DecisionTree<'tcx>,
        result_ty: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        match tree {
            DecisionTree::Uncovered | DecisionTree::Unreachable => Err(self.error(
                "kernel integer match contains an uncovered or unreachable decision leaf",
                span,
            )),
            DecisionTree::Leaf { body, bindings } => env.subscope(|nested| {
                self.match_bindings(nested, value, bindings)?;
                self.expr(block, nested, body)
            }),
            DecisionTree::Guard {
                bindings,
                guard,
                success,
                failure,
            } => {
                let types = self.control_result_types(result_ty)?;
                if !matches!(guard.ty.kind(), TyKind::Bool) {
                    return err("kernel match guard must be bool");
                }
                let predicates = env.subscope(|nested| {
                    self.match_bindings(nested, value, bindings)?;
                    self.expr(block, nested, guard)
                })?;
                if predicates.len() != 1 {
                    return err("kernel match guard must produce one SSA value");
                }
                let mut regions = Vec::new();
                for branch in [success, failure] {
                    let body = Block::new(&[]);
                    env.subscope(|nested| {
                        self.match_bindings(nested, value, bindings)?;
                        let results = self.match_tree(
                            &body, nested, value, source_ty, *branch, result_ty, span,
                        )?;
                        self.check_values(&results, &types, span)?;
                        self.ret(&body, &results, span)
                    })?;
                    let region = Region::new();
                    region.append_block(body);
                    regions.push(region);
                }
                self.operation(
                    block,
                    "stablehlo.if",
                    &predicates,
                    &types,
                    &[],
                    regions,
                    span,
                )
            }
            DecisionTree::Switch {
                scrutinee,
                cases: SwitchCases::Int { cases, default },
            } => {
                if !scrutinee.is_empty() {
                    return err("kernel integer switch cannot project its scrutinee");
                }
                let TyKind::Int(int_ty) = *source_ty.kind() else {
                    return err("kernel match scrutinee must be an integer");
                };
                let count = i32::try_from(cases.len()).map_err(|_| {
                    self.error("kernel integer match has too many case branches", span)
                })?;
                let types = self.control_result_types(result_ty)?;
                let mut index =
                    self.integer_constant(block, IntTy::Signed(32), &Integer::from(count), span)?;
                // Compare in the source type, then select an i32 branch ordinal.
                // Casting the scrutinee itself to i32 would alias e.g. 0 and 2^32.
                for (ordinal, (key, _)) in cases.iter().enumerate().rev() {
                    if !reussir_core::literal::int_fits(key, int_ty) {
                        return Err(self.error(
                            "integer pattern is out of range for kernel match scrutinee",
                            span,
                        ));
                    }
                    let key = self.integer_constant(block, int_ty, key, span)?;
                    let equal = self.compare(block, value, key, source_ty, CmpOp::Eq, span)?;
                    let ordinal = self.integer_constant(
                        block,
                        IntTy::Signed(32),
                        &Integer::from(ordinal as i32),
                        span,
                    )?;
                    index = self.select(block, equal, ordinal, index, span)?;
                }
                let mut regions = Vec::with_capacity(cases.len() + 1);
                for branch in cases
                    .iter()
                    .map(|(_, tree)| tree)
                    .chain(std::iter::once(default))
                {
                    let body = Block::new(&[]);
                    env.subscope(|nested| {
                        let results = self.match_tree(
                            &body, nested, value, source_ty, *branch, result_ty, span,
                        )?;
                        self.check_values(&results, &types, span)?;
                        self.ret(&body, &results, span)
                    })?;
                    let region = Region::new();
                    region.append_block(body);
                    regions.push(region);
                }
                self.operation(
                    block,
                    "stablehlo.case",
                    &[index],
                    &types,
                    &[],
                    regions,
                    span,
                )
            }
            DecisionTree::Switch { .. } => {
                Err(self.error("kernel match only supports integer decision trees", span))
            }
        }
    }
}
