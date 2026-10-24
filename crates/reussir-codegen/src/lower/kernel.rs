//! Kernel-only lowering. Source products are flattened into SSA lists; no
//! ownership operations, host aggregates, or scalar host instructions enter
//! this path. Control-flow extensions share this environment and type mapping.
use std::cell::Cell;
use std::ops::Range;

use super::ty::TypeCtx;
use super::{CodegenUnit, LoweringError, Result, err};
use crate::source::{FileId, SourceCache};
use indexmap::IndexMap;
use reussir_backend::dialect::ty::{dynamic_dimension_size, stablehlo_future, stablehlo_token};
use reussir_backend::melior::Context;
use reussir_backend::melior::dialect::func;
use reussir_backend::melior::ir::attribute::{
    Attribute, DenseElementsAttribute, DenseI64ArrayAttribute, StringAttribute, TypeAttribute,
};
use reussir_backend::melior::ir::operation::OperationBuilder;
use reussir_backend::melior::ir::r#type::{FunctionType, IntegerType, RankedTensorType};
use reussir_backend::melior::ir::{
    Block, BlockLike, Identifier, Location, Operation, Region, RegionLike, Type, TypeLike, Value,
    ValueLike,
};
use reussir_core::full::mir::{
    self, Binding, ClosureExpr, DecisionTree, Expr, ExprKind, SwitchCases,
};
use reussir_core::intrinsic::TensorFn;
use reussir_core::literal::{self, Integer};
use reussir_core::semi::hir::{ArithOp, CmpOp, VarId};
use reussir_core::semi::ty::{DYNAMIC_EXTENT, IntTy, Ty, TyCtxt, TyKind};
use reussir_core::surface::{Span, Visibility};

pub(super) struct KernelEnv<'c, 'b> {
    pub(super) vars: IndexMap<VarId, Vec<Value<'c, 'b>>>,
    pub(super) is_kernel: bool,
}
impl<'c, 'b> KernelEnv<'c, 'b> {
    pub(super) fn new() -> Self {
        Self {
            vars: IndexMap::new(),
            is_kernel: true,
        }
    }
    /// Clone the outer bindings at a shorter lifetime. Branch-local definitions
    /// cannot escape, and shadowing existing bindings cannot mutate the caller.
    pub(super) fn subscope<'s, R>(&self, f: impl FnOnce(&mut KernelEnv<'c, 's>) -> R) -> R
    where
        'b: 's,
    {
        let mut nested = KernelEnv {
            vars: self.vars.clone(),
            is_kernel: self.is_kernel,
        };
        f(&mut nested)
    }
}

pub(super) struct KernelLowerer<'c, 'p, 'tcx> {
    pub(super) context: &'c Context,
    pub(super) tcx: &'p TyCtxt<'tcx>,
    pub(super) program: &'p mir::Program<'tcx>,
    tys: TypeCtx<'c, 'p, 'tcx>,
    source: Option<&'p SourceCache>,
    file: Cell<FileId>,
}
impl<'c, 'p, 'tcx> KernelLowerer<'c, 'p, 'tcx> {
    pub(super) fn new(
        context: &'c Context,
        tcx: &'p TyCtxt<'tcx>,
        program: &'p mir::Program<'tcx>,
        source: Option<&'p SourceCache>,
    ) -> Self {
        Self {
            context,
            tcx,
            program,
            tys: TypeCtx::new(context, program),
            source,
            file: Cell::new(FileId::from_index(0)),
        }
    }
    pub(super) fn loc(&self, span: Option<Span>) -> Location<'c> {
        if let (Some(source), Some(span)) = (self.source, span) {
            let file = self.file.get();
            if source.is_available(file) {
                let (sl, sc) = source.line_col(file, span.start as usize);
                let (el, ec) = source.line_col(file, span.end as usize);
                return Location::file_line_col_range(
                    self.context,
                    source.name(file),
                    sl,
                    sc,
                    el,
                    ec,
                );
            }
        }
        Location::unknown(self.context)
    }
    pub(super) fn error(
        &self,
        message: impl Into<std::borrow::Cow<'static, str>>,
        span: Option<Span>,
    ) -> LoweringError {
        LoweringError::at_source(message, self.file.get(), span)
    }
    fn element(&self, ty: Ty<'tcx>) -> Result<Type<'c>> {
        if reussir_core::semi::ty_eval::tensor_element_rejection(ty).is_some() {
            return err("kernel tensor element must be a supported numeric scalar or bool");
        }
        if let TyKind::Int(IntTy::Unsigned(width)) = ty.kind() {
            return Type::parse(self.context, &format!("ui{width}"))
                .ok_or_else(|| self.error("invalid unsigned kernel element type", None));
        }
        match ty.kind() {
            TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool => self.tys.mlir_ty(ty),
            _ => err("kernel tensor elements must be numeric scalars or bool"),
        }
    }
    pub(super) fn types(&self, ty: Ty<'tcx>) -> Result<Vec<Type<'c>>> {
        match *ty.kind() {
            TyKind::Unit => Ok(Vec::new()),
            TyKind::Tuple(fields) => {
                let mut types = Vec::new();
                for field in fields {
                    types.extend(self.types(*field)?);
                }
                Ok(types)
            }
            TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool => Ok(vec![
                RankedTensorType::new(&[], self.element(ty)?, None).into(),
            ]),
            TyKind::Tensor { elem, dims } => {
                if dims
                    .iter()
                    .any(|d| *d != DYNAMIC_EXTENT && *d > i64::MAX as u64)
                {
                    return err("tensor extent exceeds the signed 64-bit MLIR dimension limit");
                }
                let dims: Vec<_> = dims
                    .iter()
                    .map(|d| {
                        if *d == DYNAMIC_EXTENT {
                            dynamic_dimension_size()
                        } else {
                            *d
                        }
                    })
                    .collect();
                Ok(vec![
                    RankedTensorType::new(&dims, self.element(elem)?, None).into(),
                ])
            }
            TyKind::Token => stablehlo_token(self.context)
                .map(|t| vec![t])
                .ok_or_else(|| {
                    self.error(super::openxla_disabled_message("kernel lowering"), None)
                }),
            TyKind::Future(payloads) => {
                let mut types = Vec::new();
                for payload in payloads {
                    if !matches!(payload.kind(), TyKind::Tensor { .. }) {
                        return err("Future payloads must be Tensor types");
                    }
                    types.extend(self.types(*payload)?);
                }
                stablehlo_future(self.context, &types)
                    .map(|t| vec![t])
                    .ok_or_else(|| {
                        self.error("invalid kernel Future type or OpenXLA disabled", None)
                    })
            }
            _ => err("host or unresolved type cannot enter kernel SSA lowering"),
        }
    }
    /// StableHLO control regions yield tensors and tokens, never Future values.
    pub(super) fn control_result_types(&self, ty: Ty<'tcx>) -> Result<Vec<Type<'c>>> {
        match ty.kind() {
            TyKind::Future(_) => err("StableHLO control-flow results cannot contain Future"),
            TyKind::Tuple(fields) => {
                let mut result = Vec::new();
                for field in *fields {
                    result.extend(self.control_result_types(*field)?);
                }
                Ok(result)
            }
            _ => self.types(ty),
        }
    }
    pub(super) fn project(
        &self,
        mut ty: Ty<'tcx>,
        path: &[u32],
    ) -> Result<(Range<usize>, Ty<'tcx>)> {
        let mut start = 0;
        for index in path {
            let TyKind::Tuple(fields) = ty.kind() else {
                return err("kernel projections require a tuple");
            };
            let index = *index as usize;
            let field = fields.get(index).ok_or_else(|| {
                self.error("kernel tuple projection index is out of bounds", None)
            })?;
            for preceding in &fields[..index] {
                start += self.types(*preceding)?.len();
            }
            ty = *field;
        }
        Ok((start..start + self.types(ty)?.len(), ty))
    }
    pub(super) fn operation<'b>(
        &self,
        block: &'b Block<'c>,
        name: &str,
        operands: &[Value<'c, 'b>],
        results: &[Type<'c>],
        attrs: &[(Identifier<'c>, Attribute<'c>)],
        regions: Vec<Region<'c>>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        let mut builder = OperationBuilder::new(name, self.loc(span))
            .add_operands(operands)
            .add_results(results)
            .add_attributes(attrs);
        for region in regions {
            builder = builder.add_regions([region]);
        }
        let op = builder
            .build()
            .map_err(|e| self.error(format!("cannot build {name}: {e}"), span))?;
        let op = block.append_operation(op);
        (0..results.len())
            .map(|i| {
                op.result(i)
                    .map(Into::into)
                    .map_err(|e| self.error(e.to_string(), span))
            })
            .collect()
    }
    pub(super) fn ret<'b>(
        &self,
        block: &'b Block<'c>,
        values: &[Value<'c, 'b>],
        span: Option<Span>,
    ) -> Result<()> {
        self.operation(block, "stablehlo.return", values, &[], &[], vec![], span)
            .map(|_| ())
    }
    pub(super) fn function(
        &self,
        function: &mir::Function<'tcx>,
        unit: CodegenUnit,
    ) -> Result<Operation<'c>> {
        self.file.set(function.file);
        if stablehlo_token(self.context).is_none() {
            return Err(self.error(
                super::openxla_disabled_message("kernel lowering"),
                function.body.and_then(|b| b.span),
            ));
        }
        if function.is_regional {
            return err("a kernel cannot have a regional calling convention");
        }
        let inputs = function
            .params
            .iter()
            .map(|p| self.types(p.ty))
            .collect::<Result<Vec<_>>>()?;
        let inputs_flat: Vec<_> = inputs.iter().flatten().copied().collect();
        let results = self.types(function.return_ty)?;
        let symbol = self.program.symbol(function.symbol);
        // Generic instances are implementation helpers, even when their
        // source declaration is public for cross-package use. This is the
        // same `_RI` classification used by native generic-instance linkage.
        let public_entry = function.visibility != Visibility::Private && !symbol.starts_with("_RI");
        if public_entry
            && inputs_flat
                .iter()
                .chain(&results)
                .any(|t| !t.is_ranked_tensor())
        {
            return Err(self.error(
                "public kernel entry parameters and results must flatten to tensors",
                function.body.and_then(|b| b.span),
            ));
        }
        let region = Region::new();
        let loc = self.loc(function.body.and_then(|b| b.span));
        if let Some(body) = function.body.filter(|_| unit.is_home(symbol)) {
            let args: Vec<_> = inputs_flat.iter().map(|t| (*t, loc)).collect();
            let block = Block::new(&args);
            let mut env = KernelEnv::new();
            let mut offset = 0;
            for (param, types) in function.params.iter().zip(&inputs) {
                let values = (offset..offset + types.len())
                    .map(|i| {
                        block
                            .argument(i)
                            .map(Into::into)
                            .map_err(|e| self.error(e.to_string(), body.span))
                    })
                    .collect::<Result<Vec<_>>>()?;
                offset += types.len();
                env.vars.insert(param.var, values);
            }
            let values = self.expr(&block, &mut env, body)?;
            self.check_values(&values, &results, body.span)?;
            block.append_operation(func::r#return(&values, loc));
            region.append_block(block);
        }
        let mut attrs = vec![(
            Identifier::new(self.context, "reussir.kernel_function"),
            Attribute::unit(self.context),
        )];
        if !public_entry || function.body.is_none() || !unit.is_home(symbol) {
            attrs.push((
                Identifier::new(self.context, "sym_visibility"),
                StringAttribute::new(self.context, "private").into(),
            ));
        }
        Ok(func::func(
            self.context,
            StringAttribute::new(self.context, symbol),
            TypeAttribute::new(FunctionType::new(self.context, &inputs_flat, &results).into()),
            region,
            &attrs,
            loc,
        ))
    }
    pub(super) fn check_values(
        &self,
        values: &[Value<'c, '_>],
        types: &[Type<'c>],
        span: Option<Span>,
    ) -> Result<()> {
        if values.len() != types.len() || values.iter().zip(types).any(|(v, t)| v.r#type() != *t) {
            return Err(self.error(
                "kernel SSA values do not match their flattened source type",
                span,
            ));
        }
        Ok(())
    }
    pub(super) fn expr<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        e: &Expr<'tcx>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        if !env.is_kernel {
            return Err(self.error("kernel lowering requires a kernel environment", e.span));
        }
        let result = match e.kind {
            ExprKind::Var(var) => env
                .vars
                .get(&var)
                .cloned()
                .ok_or_else(|| self.error(format!("unbound kernel variable {var:?}"), e.span))?,
            ExprKind::Tuple(fields) => {
                let mut values = Vec::new();
                for field in fields {
                    values.extend(self.expr(block, env, field)?);
                }
                values
            }
            ExprKind::Proj(value, path) => {
                let values = self.expr(block, env, value)?;
                let (range, _) = self.project(value.ty, path)?;
                values
                    .get(range)
                    .ok_or_else(|| self.error("malformed kernel tuple projection", e.span))?
                    .to_vec()
            }
            ExprKind::Let { var, value, .. } => {
                let values = self.expr(block, env, value)?;
                env.vars.insert(var, values);
                Vec::new()
            }
            ExprKind::Seq(exprs) => {
                let mut values = Vec::new();
                for expr in exprs {
                    values = self.expr(block, env, expr)?;
                }
                values
            }
            ExprKind::ConstInt(value) => {
                let TyKind::Int(ty) = *e.ty.kind() else {
                    return err("kernel integer literal has non-integer type");
                };
                vec![self.integer_constant(block, ty, value, e.span)?]
            }
            ExprKind::ConstBool(value) => {
                self.constant(block, e.ty, if value { "true" } else { "false" }, e.span)?
            }
            ExprKind::ConstFloat(value) => {
                let TyKind::Fp(fp) = *e.ty.kind() else {
                    return err("kernel float literal has non-float type");
                };
                let (exponent, mantissa) = literal::ieee_params(fp)
                    .ok_or_else(|| self.error("unsupported kernel float format", e.span))?;
                let bits = value
                    .to_ieee_bits(exponent, mantissa)
                    .map_err(|_| self.error("kernel float literal overflow", e.span))?;
                let digits = ((1 + exponent + mantissa) as usize).div_ceil(4);
                self.constant(block, e.ty, &format!("0x{bits:0digits$X}"), e.span)?
            }
            ExprKind::Call {
                callee,
                args,
                regional,
            } => {
                let function = self
                    .program
                    .functions
                    .iter()
                    .find(|f| f.symbol == callee)
                    .ok_or_else(|| self.error("unknown kernel callee", e.span))?;
                if !function.is_kernel || regional {
                    return Err(self.error(
                        "cannot call a host or regional function from a kernel",
                        e.span,
                    ));
                }
                if args.len() != function.params.len() {
                    return Err(self.error(
                        "kernel call argument count does not match its signature",
                        e.span,
                    ));
                }
                let mut values = Vec::new();
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    let argument = self.expr(block, env, arg)?;
                    values.extend(argument.iter().copied());
                    arguments.push(argument);
                }
                let inputs = function
                    .params
                    .iter()
                    .map(|p| self.types(p.ty))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>();
                self.check_values(&values, &inputs, e.span)?;
                let results = self.types(function.return_ty)?;
                if let Some(body) = super::intrinsic_wrapper_body(function) {
                    // Evaluate actual arguments once in source order, then bind
                    // the resulting SSA values in an isolated callee frame.
                    // This also preserves tuple/unit flattening across calls.
                    let mut frame = KernelEnv::new();
                    for (parameter, argument) in function.params.iter().zip(arguments) {
                        self.check_values(&argument, &self.types(parameter.ty)?, e.span)?;
                        frame.vars.insert(parameter.var, argument);
                    }
                    let saved_file = self.file.replace(function.file);
                    let result = self.expr(block, &mut frame, body);
                    self.file.set(saved_file);
                    let values = result?;
                    self.check_values(&values, &results, e.span)?;
                    self.check_values(&values, &self.types(e.ty)?, e.span)?;
                    return Ok(values);
                }
                let op = block.append_operation(func::call(
                    self.context,
                    reussir_backend::melior::ir::attribute::FlatSymbolRefAttribute::new(
                        self.context,
                        self.program.symbol(callee),
                    ),
                    &values,
                    &results,
                    self.loc(e.span),
                ));
                (0..results.len())
                    .map(|i| {
                        op.result(i)
                            .map(Into::into)
                            .map_err(|x| self.error(x.to_string(), e.span))
                    })
                    .collect::<Result<Vec<_>>>()?
            }
            ExprKind::Arith(left, op, right) => {
                let lhs = self.expr(block, env, left)?;
                let rhs = self.expr(block, env, right)?;
                let name = match op {
                    ArithOp::Add => "add",
                    ArithOp::Sub => "subtract",
                    ArithOp::Mul => "multiply",
                    ArithOp::Div => "divide",
                    ArithOp::Mod => "remainder",
                    ArithOp::And | ArithOp::BitAnd => "and",
                    ArithOp::Or | ArithOp::BitOr => "or",
                    ArithOp::BitXor => "xor",
                    ArithOp::Shl => "shift_left",
                    ArithOp::Shr => {
                        if matches!(left.ty.kind(), TyKind::Int(IntTy::Unsigned(_))) {
                            "shift_right_logical"
                        } else {
                            "shift_right_arithmetic"
                        }
                    }
                };
                let mut operands = lhs;
                operands.extend(rhs);
                self.operation(
                    block,
                    &format!("stablehlo.{name}"),
                    &operands,
                    &self.types(e.ty)?,
                    &[],
                    vec![],
                    e.span,
                )?
            }
            ExprKind::Cmp(left, op, right) => {
                let lhs = self.expr(block, env, left)?;
                let rhs = self.expr(block, env, right)?;
                if lhs.len() != 1 || rhs.len() != 1 {
                    return err("kernel comparison requires scalar operands");
                }
                vec![self.compare(block, lhs[0], rhs[0], left.ty, op, e.span)?]
            }
            ExprKind::Negate(value) | ExprKind::Not(value) => {
                let operands = self.expr(block, env, value)?;
                let name = if matches!(e.kind, ExprKind::Negate(_)) {
                    "stablehlo.negate"
                } else {
                    "stablehlo.not"
                };
                self.operation(
                    block,
                    name,
                    &operands,
                    &self.types(e.ty)?,
                    &[],
                    vec![],
                    e.span,
                )?
            }
            ExprKind::While {
                state,
                initial,
                condition,
                body,
            } => self.while_expr(block, env, state, initial, condition, body, e.span)?,
            ExprKind::Match(scrutinee, tree) => {
                self.match_expr(block, env, scrutinee, tree, e.ty, e.span)?
            }
            ExprKind::Async(capture) => self.async_region(block, env, &capture, e.ty, e.span)?,
            ExprKind::TensorOp { op, args } => {
                if op.is_basic() {
                    self.basic_tensor_op(block, env, op, args, e.ty, e.span)?
                } else {
                    self.async_tensor_op(block, env, op, args, e.ty, e.span)?
                }
            }
            ExprKind::If(condition, then, otherwise) => {
                self.if_expr(block, env, condition, then, otherwise, e.ty, e.span)?
            }
            _ => {
                return Err(self.error("operation is not supported in kernel SSA lowering", e.span));
            }
        };
        self.check_values(&result, &self.types(e.ty)?, e.span)?;
        Ok(result)
    }
    pub(super) fn while_expr<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        state: VarId,
        initial: &Expr<'tcx>,
        condition: &Expr<'tcx>,
        body: &Expr<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        if !matches!(condition.ty.kind(), TyKind::Bool)
            && !matches!(condition.ty.kind(), TyKind::Tensor { elem, dims }
                if dims.is_empty() && matches!(elem.kind(), TyKind::Bool))
        {
            return err("kernel while predicate must be bool or rank-zero Tensor<bool>");
        }
        let types = self.control_result_types(initial.ty)?;
        let initial = self.expr(block, env, initial)?;
        self.check_values(&initial, &types, span)?;
        let arguments: Vec<_> = types.iter().map(|ty| (*ty, self.loc(span))).collect();
        let mut regions = Vec::new();
        for (expression, predicate) in [(condition, true), (body, false)] {
            let region_block = Block::new(&arguments);
            env.subscope(|nested| {
                nested.vars.insert(
                    state,
                    (0..types.len())
                        .map(|i| {
                            region_block
                                .argument(i)
                                .expect("constructed state argument")
                                .into()
                        })
                        .collect(),
                );
                let values = self.expr(&region_block, nested, expression)?;
                let expected = if predicate {
                    self.types(self.tcx.mk_bool())?
                } else {
                    types.clone()
                };
                self.check_values(&values, &expected, expression.span)?;
                self.ret(&region_block, &values, expression.span)
            })?;
            let region = Region::new();
            region.append_block(region_block);
            regions.push(region);
        }
        self.operation(
            block,
            "stablehlo.while",
            &initial,
            &types,
            &[],
            regions,
            span,
        )
    }
    fn constant<'b>(
        &self,
        block: &'b Block<'c>,
        ty: Ty<'tcx>,
        text: &str,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        let types = self.types(ty)?;
        if types.len() != 1 {
            return err("kernel constant requires a scalar type");
        }
        let attr_text = format!("dense<{text}> : {}", types[0]);
        let value = Attribute::parse(self.context, &attr_text)
            .ok_or_else(|| self.error(format!("invalid kernel constant: {attr_text}"), span))?;
        self.operation(
            block,
            "stablehlo.constant",
            &[],
            &types,
            &[(Identifier::new(self.context, "value"), value)],
            vec![],
            span,
        )
    }
    pub(super) fn integer_constant<'b>(
        &self,
        block: &'b Block<'c>,
        ty: IntTy,
        value: &Integer,
        span: Option<Span>,
    ) -> Result<Value<'c, 'b>> {
        Ok(self.constant(
            block,
            self.tcx.mk(TyKind::Int(ty)),
            &value.to_string(),
            span,
        )?[0])
    }
    pub(super) fn compare<'b>(
        &self,
        block: &'b Block<'c>,
        lhs: Value<'c, 'b>,
        rhs: Value<'c, 'b>,
        ty: Ty<'tcx>,
        op: CmpOp,
        span: Option<Span>,
    ) -> Result<Value<'c, 'b>> {
        let direction = match op {
            CmpOp::Lt => "LT",
            CmpOp::Gt => "GT",
            CmpOp::Le => "LE",
            CmpOp::Ge => "GE",
            CmpOp::Eq => "EQ",
            CmpOp::Ne => "NE",
        };
        let comparison = match ty.kind() {
            TyKind::Fp(_) => "FLOAT",
            TyKind::Int(IntTy::Unsigned(_)) => "UNSIGNED",
            _ => "SIGNED",
        };
        let attrs = [
            (
                "comparison_direction",
                format!("#stablehlo<comparison_direction {direction}>"),
            ),
            (
                "compare_type",
                format!("#stablehlo<comparison_type {comparison}>"),
            ),
        ]
        .map(|(name, text)| {
            Attribute::parse(self.context, &text)
                .map(|attr| (Identifier::new(self.context, name), attr))
                .ok_or_else(|| self.error("invalid StableHLO comparison attribute", span))
        })
        .into_iter()
        .collect::<Result<Vec<_>>>()?;
        let result =
            RankedTensorType::new(&[], IntegerType::new(self.context, 1).into(), None).into();
        Ok(self.operation(
            block,
            "stablehlo.compare",
            &[lhs, rhs],
            &[result],
            &attrs,
            vec![],
            span,
        )?[0])
    }
    pub(super) fn select<'b>(
        &self,
        block: &'b Block<'c>,
        predicate: Value<'c, 'b>,
        lhs: Value<'c, 'b>,
        rhs: Value<'c, 'b>,
        span: Option<Span>,
    ) -> Result<Value<'c, 'b>> {
        Ok(self.operation(
            block,
            "stablehlo.select",
            &[predicate, lhs, rhs],
            &[lhs.r#type()],
            &[],
            vec![],
            span,
        )?[0])
    }
}

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

// Integer decision trees use full-width comparisons and dense branch ordinals.
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

impl<'c, 'p, 'tcx> KernelLowerer<'c, 'p, 'tcx> {
    pub(super) fn async_region<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        capture: &ClosureExpr<'tcx>,
        result: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        if !capture.params.is_empty() {
            return Err(self.error("async regions cannot declare closure parameters", span));
        }
        reussir_core::async_check::mir(capture.captures, capture.body, result)
            .map_err(|e| self.error(e, span))?;
        let mut operands = Vec::new();
        let mut inputs = Vec::new();
        let mut lengths = Vec::new();
        for &(var, ty) in capture.captures {
            let values = env
                .vars
                .get(&var)
                .ok_or_else(|| self.error("unbound async capture", span))?;
            let types = self.types(ty)?;
            self.check_values(values, &types, span)?;
            if types
                .iter()
                .any(|&ty| RankedTensorType::try_from(ty).is_err())
            {
                return Err(self.error("StableHLO async captures must be tensor SSA values", span));
            }
            lengths.push((var, types.len()));
            operands.extend(values.iter().copied());
            inputs.extend(types.into_iter().map(|t| (t, self.loc(span))));
        }
        let body = Block::new(&inputs);
        let mut inner = KernelEnv::new();
        let mut offset = 0;
        for (var, len) in lengths {
            let values = (offset..offset + len)
                .map(|i| {
                    body.argument(i)
                        .map(Into::into)
                        .map_err(|e| self.error(format!("missing async block argument: {e}"), span))
                })
                .collect::<Result<Vec<_>>>()?;
            inner.vars.insert(var, values);
            offset += len;
        }
        let values = self.expr(&body, &mut inner, capture.body)?;
        self.check_values(&values, &self.types(capture.body.ty)?, span)?;
        self.ret(&body, &values, span)?;
        let region = Region::new();
        region.append_block(body);
        self.operation(
            block,
            "stablehlo.async_start",
            &operands,
            &self.types(result)?,
            &[],
            vec![region],
            span,
        )
    }

    fn basic_tensor_op<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        op: TensorFn,
        args: &[Expr<'tcx>],
        result: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        use TensorFn::*;
        reussir_core::tensor_check::mir(op, result, args).map_err(|e| self.error(e, span))?;
        let mut attrs = Vec::new();
        let mut values = Vec::new();
        let results = self.types(result)?;
        let int_attr = |name: &str, value: i64| -> Result<_> {
            Ok((
                Identifier::new(self.context, name),
                Attribute::parse(self.context, &format!("{value} : i64"))
                    .ok_or_else(|| self.error("invalid integer attribute", span))?,
            ))
        };
        let name = match op {
            Fill | DynamicFill => {
                values.extend(self.expr(block, env, &args[0])?);
                attrs.push((
                    Identifier::new(self.context, "broadcast_dimensions"),
                    DenseI64ArrayAttribute::new(self.context, &[]).into(),
                ));
                if op == DynamicFill {
                    values.push(self.tensor_shape(block, env, &args[1], span)?);
                    "stablehlo.dynamic_broadcast_in_dim"
                } else {
                    "stablehlo.broadcast_in_dim"
                }
            }
            Iota | DynamicIota => {
                let axis = reussir_core::tensor_check::mir_axis(op, result, args)
                    .map_err(|e| self.error(e, span))?;
                attrs.push(int_attr("iota_dimension", axis)?);
                if op == DynamicIota {
                    values.push(self.tensor_shape(block, env, &args[0], span)?);
                    "stablehlo.dynamic_iota"
                } else {
                    "stablehlo.iota"
                }
            }
            _ => {
                for arg in args {
                    values.extend(self.expr(block, env, arg)?);
                }
                match op {
                    Add => "stablehlo.add",
                    Sub => "stablehlo.subtract",
                    Mul => "stablehlo.multiply",
                    Div => "stablehlo.divide",
                    Rem => "stablehlo.remainder",
                    Neg => "stablehlo.negate",
                    Abs => "stablehlo.abs",
                    Min => "stablehlo.minimum",
                    Max => "stablehlo.maximum",
                    Select => "stablehlo.select",
                    Dot => {
                        let TyKind::Tensor { dims, .. } = args[0].ty.kind() else {
                            return err("dot requires Tensor operands");
                        };
                        let text = format!(
                            "#stablehlo.dot<lhs_batching_dimensions = [], rhs_batching_dimensions = [], lhs_contracting_dimensions = [{}], rhs_contracting_dimensions = [0]>",
                            dims.len() - 1
                        );
                        attrs.push((
                            Identifier::new(self.context, "dot_dimension_numbers"),
                            Attribute::parse(self.context, &text)
                                .ok_or_else(|| self.error("invalid dot dimension numbers", span))?,
                        ));
                        "stablehlo.dot_general"
                    }
                    Transpose => {
                        attrs.push((
                            Identifier::new(self.context, "permutation"),
                            DenseI64ArrayAttribute::new(self.context, &[1, 0]).into(),
                        ));
                        "stablehlo.transpose"
                    }
                    Eq | Ne | Lt | Le | Gt | Ge => {
                        let direction = match op {
                            Eq => "EQ",
                            Ne => "NE",
                            Lt => "LT",
                            Le => "LE",
                            Gt => "GT",
                            Ge => "GE",
                            _ => unreachable!(),
                        };
                        let TyKind::Tensor { elem, .. } = args[0].ty.kind() else {
                            return err("comparison requires Tensor operands");
                        };
                        let comparison = match elem.kind() {
                            TyKind::Fp(_) => "FLOAT",
                            TyKind::Int(IntTy::Unsigned(_)) | TyKind::Bool => "UNSIGNED",
                            _ => "SIGNED",
                        };
                        for (name, value) in [
                            ("comparison_direction", direction),
                            ("compare_type", comparison),
                        ] {
                            let kind = if name == "compare_type" {
                                "comparison_type"
                            } else {
                                name
                            };
                            let attr = Attribute::parse(
                                self.context,
                                &format!("#stablehlo<{kind} {value}>"),
                            )
                            .ok_or_else(|| {
                                self.error("invalid tensor comparison attribute", span)
                            })?;
                            attrs.push((Identifier::new(self.context, name), attr));
                        }
                        "stablehlo.compare"
                    }
                    _ => return err("not a basic tensor operation"),
                }
            }
        };
        self.operation(block, name, &values, &results, &attrs, vec![], span)
    }

    /// Assemble the source shape product into StableHLO's rank-one i64 value.
    /// Source dimensions remain independent rank-zero values until this boundary.
    fn tensor_shape<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        shape: &Expr<'tcx>,
        span: Option<Span>,
    ) -> Result<Value<'c, 'b>> {
        let values = self.expr(block, env, shape)?;
        let element: Type<'c> = IntegerType::new(self.context, 64).into();
        let result: Type<'c> = RankedTensorType::new(&[values.len() as u64], element, None).into();
        if values.is_empty() {
            let value = DenseElementsAttribute::i64_values(result, &[]);
            return Ok(self.operation(
                block,
                "stablehlo.constant",
                &[],
                &[result],
                &[(Identifier::new(self.context, "value"), value.into())],
                vec![],
                span,
            )?[0]);
        }
        let vector: Type<'c> = RankedTensorType::new(&[1], element, None).into();
        let mut pieces = Vec::new();
        for value in values {
            pieces.push(
                self.operation(
                    block,
                    "stablehlo.reshape",
                    &[value],
                    &[vector],
                    &[],
                    vec![],
                    span,
                )?[0],
            );
        }
        if pieces.len() == 1 {
            return Ok(pieces[0]);
        }
        let dimension = Attribute::parse(self.context, "0 : i64")
            .ok_or_else(|| self.error("invalid concatenate dimension", span))?;
        Ok(self.operation(
            block,
            "stablehlo.concatenate",
            &pieces,
            &[result],
            &[(Identifier::new(self.context, "dimension"), dimension)],
            vec![],
            span,
        )?[0])
    }

    pub(super) fn async_tensor_op<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut KernelEnv<'c, 'b>,
        op: TensorFn,
        args: &[Expr<'tcx>],
        result: Ty<'tcx>,
        span: Option<Span>,
    ) -> Result<Vec<Value<'c, 'b>>> {
        reussir_core::async_check::mir_tensor_op(op, result, args)
            .map_err(|e| self.error(e, span))?;
        let mut values = Vec::new();
        for a in args.iter().take(
            if matches!(op, TensorFn::Slice | TensorFn::CollectivePermute) {
                1
            } else {
                args.len()
            },
        ) {
            values.extend(self.expr(block, env, a)?);
        }
        let results = self.types(result)?;
        match op {
            TensorFn::CollectivePermute => {
                let pairs = reussir_core::async_check::mir_permutation_pairs(args)
                    .map_err(|e| self.error(e, span))?;
                let rows = i64::try_from(pairs.len() / 2)
                    .map_err(|_| self.error("too many collective_permute pairs", span))?;
                let ty = RankedTensorType::new(
                    &[rows as u64, 2],
                    IntegerType::new(self.context, 64).into(),
                    None,
                );
                let attr = DenseElementsAttribute::i64_values(ty.into(), &pairs);
                self.operation(
                    block,
                    "stablehlo.collective_permute",
                    &values,
                    &results,
                    &[(
                        Identifier::new(self.context, "source_target_pairs"),
                        attr.into(),
                    )],
                    vec![],
                    span,
                )
            }
            TensorFn::Slice | TensorFn::DynamicSlice => {
                let TyKind::Tensor { dims, .. } = result.kind() else {
                    return Err(self.error("dynamic_slice result must be a Tensor", span));
                };
                let sizes: Vec<_> = dims.iter().map(|&d| d as i64).collect();
                if op == TensorFn::Slice || dims.is_empty() {
                    // With no indices dynamic_slice is exactly a rank-zero slice.
                    // Canonicalize it to avoid the pinned upstream custom-printer bug.
                    let starts = if op == TensorFn::Slice {
                        reussir_core::async_check::mir_slice_offsets(result, args)
                            .map_err(|e| self.error(e, span))?
                    } else {
                        Vec::new()
                    };
                    let limits: Vec<_> = starts
                        .iter()
                        .zip(sizes.iter())
                        .map(|(a, b)| a + b)
                        .collect();
                    let strides = vec![1; starts.len()];
                    let attrs = [
                        ("start_indices", starts),
                        ("limit_indices", limits),
                        ("strides", strides),
                    ]
                    .map(|(name, values)| {
                        (
                            Identifier::new(self.context, name),
                            DenseI64ArrayAttribute::new(self.context, &values).into(),
                        )
                    });
                    return self.operation(
                        block,
                        "stablehlo.slice",
                        &values,
                        &results,
                        &attrs,
                        vec![],
                        span,
                    );
                }
                let attr = DenseI64ArrayAttribute::new(self.context, &sizes);
                self.operation(
                    block,
                    "stablehlo.dynamic_slice",
                    &values,
                    &results,
                    &[(Identifier::new(self.context, "slice_sizes"), attr.into())],
                    vec![],
                    span,
                )
            }
            TensorFn::AsyncDone => self.operation(
                block,
                "stablehlo.async_done",
                &values,
                &results,
                &[],
                vec![],
                span,
            ),
            _ => Err(self.error("host tensor intrinsic cannot lower in a kernel", span)),
        }
    }
}
