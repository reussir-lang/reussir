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
use reussir_backend::melior::ir::attribute::{Attribute, StringAttribute, TypeAttribute};
use reussir_backend::melior::ir::operation::OperationBuilder;
use reussir_backend::melior::ir::r#type::{FunctionType, IntegerType, RankedTensorType};
use reussir_backend::melior::ir::{
    Block, BlockLike, Identifier, Location, Operation, Region, RegionLike, Type, TypeLike, Value,
    ValueLike,
};
use reussir_core::full::mir::{self, Expr, ExprKind};
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
                .ok_or_else(|| self.error("kernel lowering requires REUSSIR_ENABLE_OPENXLA", None)),
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
                "kernel lowering requires REUSSIR_ENABLE_OPENXLA",
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
        if function.visibility != Visibility::Private
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
        let symbol = self.program.symbol(function.symbol);
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
            Identifier::new(self.context, "reussir.kernel"),
            Attribute::unit(self.context),
        )];
        if function.visibility == Visibility::Private
            || function.body.is_none()
            || !unit.is_home(symbol)
        {
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
                let mut values = Vec::new();
                for arg in args {
                    values.extend(self.expr(block, env, arg)?);
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
