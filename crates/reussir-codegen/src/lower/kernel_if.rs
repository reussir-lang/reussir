//! StableHLO conditionals capture outer SSA values and yield flattened products.
use super::kernel::{KernelEnv, KernelLowerer};
use super::{Result, err};
use reussir_backend::melior::ir::{Block, Region, RegionLike, Value};
use reussir_core::full::mir::Expr;
use reussir_core::semi::ty::{Ty, TyKind};
use reussir_core::surface::Span;

impl<'c, 'p, 'tcx> KernelLowerer<'c, 'p, 'tcx> {
    pub(super) fn if_expr<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        condition: &Expr<'tcx>,
        then: &Expr<'tcx>,
        otherwise: &Expr<'tcx>,
        ty: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        if !matches!(condition.ty.kind(), TyKind::Bool)
            && !matches!(condition.ty.kind(), TyKind::Tensor { elem, dims } if dims.is_empty() && matches!(elem.kind(), TyKind::Bool))
        {
            return err("kernel if predicate must be bool or rank-zero Tensor<bool>");
        }
        let predicate = self.expr(block, env, condition)?;
        if predicate.len() != 1 {
            return err("kernel if predicate must produce one SSA value");
        }
        let types = self.control_result_types(ty)?;
        let mut regions = Vec::new();
        for branch in [then, otherwise] {
            let body = Block::new(&[]);
            env.subscope(|nested| {
                let values = self.expr(&body, nested, branch)?;
                self.check_values(&values, &types, branch.span)?;
                self.ret(&body, &values, branch.span)
            })?;
            let region = Region::new();
            region.append_block(body);
            regions.push(region);
        }
        self.operation(
            block,
            "stablehlo.if",
            &predicate,
            &types,
            &[],
            regions,
            span,
        )
    }
}
