//! Outline a launch's reachable kernel closure and retain exact buffer metadata.

use super::{CodegenUnit, LoweringError, Result, err, kernel::KernelLowerer, ty::TypeCtx};
use reussir_backend::dialect::ty::{ifrt_array, ifrt_devices, ifrt_hlo_sharding};
use reussir_backend::melior::ir::attribute::{
    ArrayAttribute, DictionaryAttribute, StringAttribute, TypeAttribute,
};
use reussir_backend::melior::ir::operation::OperationBuilder;
use reussir_backend::melior::ir::r#type::FunctionType;
use reussir_backend::melior::ir::{
    Attribute, Block, BlockLike, Identifier, Operation, Region, RegionLike, Type,
};
use reussir_backend::melior::{Context, dialect::func};
use reussir_core::full::mir::{self, ExprKind};
use reussir_core::semi::ty::{Ty, TyKind};
use reussir_core::surface::Visibility;
use rustc_hash::FxHashSet;

pub(super) fn name(expr: &mir::Expr<'_>) -> String {
    format!("_reussir_launch_{}", expr.id.0)
}

pub(super) fn contains_launch(expr: &mir::Expr<'_>) -> bool {
    let mut found = false;
    mir::for_each_expr(expr, &mut |e| {
        found |= matches!(e.kind, ExprKind::Launch { .. })
    });
    found
}

pub(super) fn buffer_types<'c, 'tcx>(
    tys: &TypeCtx<'c, '_, 'tcx>,
    ty: Ty<'tcx>,
    out: &mut Vec<Type<'c>>,
) -> Result<()> {
    match ty.kind() {
        TyKind::DevArray { .. } => out.push(
            ifrt_array(tys.devarray_inner_of(ty)?)
                .ok_or_else(|| LoweringError::feature_disabled("openxla", "kernel launch"))?,
        ),
        TyKind::Tuple(fields) => {
            for &field in *fields {
                buffer_types(tys, field, out)?;
            }
        }
        TyKind::Unit => {}
        _ => return err("launch operands must be DevArray buffers, tuples of buffers, or unit"),
    }
    Ok(())
}

pub(super) fn outline<'c, 'p, 'tcx>(
    context: &'c Context,
    kernel: &KernelLowerer<'c, 'p, 'tcx>,
    tys: &TypeCtx<'c, 'p, 'tcx>,
    expr: &mir::Expr<'tcx>,
) -> Result<Operation<'c>> {
    let ExprKind::Launch { callee, args } = expr.kind else {
        return err("expected a launch expression");
    };
    let function = kernel
        .program
        .functions
        .iter()
        .find(|f| f.symbol == callee)
        .ok_or_else(|| LoweringError("launch target has no function definition".into()))?;
    if !function.is_kernel || function.body.is_none() {
        return err("launch requires a kernel definition, including its imported body");
    }
    if args.len() != function.params.len() {
        return err("launch argument count does not match the kernel");
    }
    let TyKind::Tensor { elem, dims } = function.return_ty.kind() else {
        return err("launch requires exactly one Tensor result");
    };
    let TyKind::DevArray {
        elem: actual_elem,
        dims: actual_dims,
        ..
    } = expr.ty.kind()
    else {
        return err("launch result must be a DevArray");
    };
    if elem != actual_elem || dims != actual_dims {
        return err("launch result dtype and shape must match the kernel Tensor result");
    }
    let mut input_buffers = Vec::new();
    for (arg, param) in args.iter().zip(&function.params) {
        validate_argument(arg.ty, param.ty)?;
        buffer_types(tys, arg.ty, &mut input_buffers)?;
    }
    let output = ifrt_array(tys.devarray_inner_of(expr.ty)?)
        .ok_or_else(|| LoweringError::feature_disabled("openxla", "kernel launch"))?;
    if reussir_backend::dialect::ty::ifrt_requires_global_extents(output) {
        return err(
            "partitioned dynamic launch results require an explicit logical result-shape contract; use a static result shape or an unpartitioned placement",
        );
    }
    let devices = ifrt_devices(output)
        .ok_or_else(|| LoweringError("invalid launch output placement".into()))?;
    if input_buffers
        .iter()
        .any(|&t| ifrt_devices(t) != Some(devices))
    {
        return err("launch input and output placements must use the same ordered devices");
    }
    let mut seen = FxHashSet::default();
    let mut active = FxHashSet::default();
    let mut closure = Vec::new();
    reachable(kernel.program, callee, &mut seen, &mut active, &mut closure)?;
    let loc = kernel.loc(expr.span);
    let region = Region::new();
    let block = Block::new(&[]);
    for function in closure {
        let mut helper = function.clone();
        helper.visibility = Visibility::Private;
        block.append_operation(kernel.function(&helper, CodegenUnit { index: 0, count: 1 })?);
    }
    let inputs = function
        .params
        .iter()
        .map(|p| kernel.types(p.ty))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    let results = kernel.types(function.return_ty)?;
    let entry_region = Region::new();
    let entry = Block::new(&inputs.iter().map(|&t| (t, loc)).collect::<Vec<_>>());
    let operands = (0..inputs.len())
        .map(|i| {
            entry
                .argument(i)
                .map(Into::into)
                .map_err(|e| LoweringError(e.to_string().into()))
        })
        .collect::<Result<Vec<_>>>()?;
    let call = entry.append_operation(func::call(
        context,
        reussir_backend::melior::ir::attribute::FlatSymbolRefAttribute::new(
            context,
            kernel.program.symbol(callee),
        ),
        &operands,
        &results,
        loc,
    ));
    let values = (0..results.len())
        .map(|i| {
            call.result(i)
                .map(Into::into)
                .map_err(|e| LoweringError(e.to_string().into()))
        })
        .collect::<Result<Vec<_>>>()?;
    entry.append_operation(func::r#return(&values, loc));
    entry_region.append_block(entry);
    let arg_attrs = sharding_attrs(context, &input_buffers);
    let res_attrs = sharding_attrs(context, &[output]);
    block.append_operation(func::func(
        context,
        StringAttribute::new(context, "main"),
        TypeAttribute::new(FunctionType::new(context, &inputs, &results).into()),
        entry_region,
        &[
            (Identifier::new(context, "arg_attrs"), arg_attrs),
            (Identifier::new(context, "res_attrs"), res_attrs),
            (
                Identifier::new(context, "reussir.kernel_function"),
                Attribute::unit(context),
            ),
        ],
        loc,
    ));
    region.append_block(block);
    OperationBuilder::new("builtin.module", loc)
        .add_attributes(&[
            (
                Identifier::new(context, "sym_name"),
                StringAttribute::new(context, &name(expr)).into(),
            ),
            (
                Identifier::new(context, "sym_visibility"),
                StringAttribute::new(context, "private").into(),
            ),
        ])
        .add_regions([region])
        .build()
        .map_err(|e| LoweringError(e.to_string().into()))
}

fn sharding_attrs<'c>(context: &'c Context, types: &[Type<'c>]) -> Attribute<'c> {
    let attrs = types
        .iter()
        .map(|&t| {
            let attrs = ifrt_hlo_sharding(t)
                .map(|s| vec![(Identifier::new(context, "mhlo.sharding"), s)])
                .unwrap_or_default();
            DictionaryAttribute::new(context, &attrs).into()
        })
        .collect::<Vec<_>>();
    ArrayAttribute::new(context, &attrs).into()
}

fn validate_argument(actual: Ty<'_>, expected: Ty<'_>) -> Result<()> {
    match (actual.kind(), expected.kind()) {
        (TyKind::DevArray { elem, dims, .. }, TyKind::Tensor { elem: e, dims: d })
            if elem == e && dims == d =>
        {
            Ok(())
        }
        (TyKind::DevArray { elem, dims, .. }, TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool)
            if *elem == expected && dims.is_empty() =>
        {
            Ok(())
        }
        (TyKind::Unit, TyKind::Unit) => Ok(()),
        (TyKind::Tuple(actual), TyKind::Tuple(expected)) if actual.len() == expected.len() => {
            for (&a, &e) in actual.iter().zip(*expected) {
                validate_argument(a, e)?;
            }
            Ok(())
        }
        _ => err(
            "launch buffer dtype, rank, shape, or product structure does not match the kernel parameter",
        ),
    }
}

fn reachable<'p, 'tcx>(
    program: &'p mir::Program<'tcx>,
    symbol: mir::Symbol,
    seen: &mut FxHashSet<mir::Symbol>,
    active: &mut FxHashSet<mir::Symbol>,
    out: &mut Vec<&'p mir::Function<'tcx>>,
) -> Result<()> {
    if active.contains(&symbol) {
        return err("recursive kernel calls cannot be launched");
    }
    if !seen.insert(symbol) {
        return Ok(());
    }
    let function = program
        .functions
        .iter()
        .find(|f| f.symbol == symbol)
        .ok_or_else(|| LoweringError("reachable kernel helper is missing".into()))?;
    if !function.is_kernel {
        return err("launched kernels cannot call host functions");
    }
    let body = function
        .body
        .ok_or_else(|| LoweringError("reachable kernel helper has no imported body".into()))?;
    active.insert(symbol);
    let mut calls = Vec::new();
    mir::for_each_expr(body, &mut |e| {
        if let ExprKind::Call { callee, .. } = e.kind {
            calls.push(callee);
        }
    });
    for call in calls {
        reachable(program, call, seen, active, out)?;
    }
    active.remove(&symbol);
    out.push(function);
    Ok(())
}
