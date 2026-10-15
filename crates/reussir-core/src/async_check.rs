//! Shared constraints for the pinned StableHLO async and slice operations.
use crate::intrinsic::TensorFn;
use crate::semi::ty::{DYNAMIC_EXTENT, Ty, TyKind};

pub fn tensor_op(op: TensorFn, result: Ty<'_>, args: &[Ty<'_>]) -> Result<(), String> {
    let error = |s: &str| Err(s.replace("dynamic_slice", op.as_str()));
    match op {
        TensorFn::AsyncDone => {
            if let [arg] = args
                && let TyKind::Future(ps) = arg.kind()
                && ps.len() == 1
                && ps[0] == result
                && matches!(
                    result.kind(),
                    TyKind::Tensor { .. } | TyKind::Generic(_) | TyKind::Hole(_)
                )
            {
                return Ok(());
            }
            error(
                "async_done requires exactly one Future<Tensor> operand and its matching Tensor result",
            )
        }
        TensorFn::CollectivePermute => {
            if let [input, _] = args
                && *input == result
                && matches!(
                    input.kind(),
                    TyKind::Tensor { .. } | TyKind::Generic(_) | TyKind::Hole(_)
                )
            {
                Ok(())
            } else {
                error(
                    "collective_permute requires a Tensor operand and the same Tensor result type",
                )
            }
        }
        TensorFn::DynamicSlice | TensorFn::Slice => {
            let [input, starts] = args else {
                return error("dynamic_slice requires input and start-index tuple");
            };
            if matches!(input.kind(), TyKind::Generic(_) | TyKind::Hole(_))
                || matches!(result.kind(), TyKind::Generic(_) | TyKind::Hole(_))
            {
                return Ok(());
            }
            let (TyKind::Tensor { elem: ie, dims: id }, TyKind::Tensor { elem: re, dims: rd }) =
                (input.kind(), result.kind())
            else {
                return error("dynamic_slice input and result must be Tensor types");
            };
            if ie != re {
                return error("dynamic_slice input and result element types must match");
            }
            if id.len() != rd.len() {
                return error("dynamic_slice input and result ranks must match");
            }
            if rd.iter().any(|&d| d == DYNAMIC_EXTENT) {
                return error("dynamic_slice result must have a static shape");
            }
            if id
                .iter()
                .zip(rd.iter())
                .any(|(&i, &r)| i != DYNAMIC_EXTENT && r > i)
            {
                return error("dynamic_slice size exceeds the input extent");
            }
            let fields = match starts.kind() {
                TyKind::Tuple(fs) => *fs,
                TyKind::Unit => &[],
                TyKind::Generic(_) | TyKind::Hole(_) => return Ok(()),
                _ => return error("dynamic_slice start indices must form a tuple"),
            };
            if fields.len() != id.len() {
                return error("dynamic_slice requires one start index per input dimension");
            }
            let mut integer = None;
            for t in fields {
                let e = match t.kind() {
                    TyKind::Int(_) => *t,
                    TyKind::Tensor { elem, dims } if dims.is_empty() => *elem,
                    TyKind::Generic(_) | TyKind::Hole(_) => continue,
                    _ => {
                        return error(
                            "dynamic_slice indices must be integers or rank-zero integer Tensors",
                        );
                    }
                };
                if !matches!(
                    e.kind(),
                    TyKind::Int(_) | TyKind::Generic(_) | TyKind::Hole(_)
                ) {
                    return error("dynamic_slice indices must have integer element types");
                }
                if integer.is_some_and(|prev| prev != e) {
                    return error("dynamic_slice indices must share one integer type");
                }
                integer = Some(e);
            }
            Ok(())
        }
        _ => crate::tensor_check::operation(op, result, args),
    }
}

fn nonnegative_offset(value: &crate::literal::Integer) -> Result<i64, String> {
    i64::try_from(value)
        .ok()
        .filter(|&v| v >= 0)
        .ok_or_else(|| {
            "slice start offset is outside the nonnegative signed 64-bit range".to_owned()
        })
}

#[derive(Clone, PartialEq, Eq)]
enum Origin {
    Capture,
    Result,
    Tuple(Vec<Origin>),
    Unit,
}

fn capture_type(ty: Ty<'_>) -> bool {
    match ty.kind() {
        TyKind::Tensor { .. }
        | TyKind::Int(_)
        | TyKind::Fp(_)
        | TyKind::Bool
        | TyKind::Generic(_)
        | TyKind::Hole(_) => true,
        TyKind::Tuple(fs) => fs.iter().all(|&f| capture_type(f)),
        TyKind::Unit => true,
        _ => false,
    }
}
fn origin(ty: Ty<'_>) -> Origin {
    match ty.kind() {
        TyKind::Tuple(fs) => Origin::Tuple(fs.iter().map(|&f| origin(f)).collect()),
        TyKind::Unit => Origin::Unit,
        _ => Origin::Capture,
    }
}

// HIR and MIR use the same structural forms for async bodies. Validate both
// textual inputs and the monomorphized graph with one rule implementation.
macro_rules! validate {
    ($name:ident,$opcheck:ident,$offsets:ident,$pairs:ident,$expr:path,$kind:path) => {
        pub fn $opcheck<'tcx>(
            op: TensorFn,
            result: Ty<'_>,
            args: &[$expr],
        ) -> Result<(), String> {
            let types: Vec<_> = args.iter().map(|a| a.ty).collect();
            tensor_op(op, result, &types)?;
            crate::tensor_check::$name(op, result, args)?;
            if op == TensorFn::Slice { $offsets(result, args)?; }
            if op == TensorFn::CollectivePermute { $pairs(args)?; }
            Ok(())
        }
        pub fn $offsets<'tcx>(
            result: Ty<'_>,
            args: &[$expr],
        ) -> Result<Vec<i64>, String> {
            use $kind as K;
            let [input, starts] = args else {
                return Err("slice requires input and a literal start-index tuple".into());
            };
            let fields: &[$expr] = match &starts.kind {
                K::Tuple(fields) => fields.as_ref(),
                K::Seq(fields) if fields.is_empty() => &[],
                _ => {
                    return Err(
                        "slice start offsets must be a tuple of nonnegative integer literals".into(),
                    );
                }
            };
            if !(matches!(starts.ty.kind(), TyKind::Tuple(types)
                if types.len() == fields.len() && types.iter().zip(fields.iter()).all(|(ty, field)| *ty == field.ty))
                || fields.is_empty() && matches!(starts.ty.kind(), TyKind::Unit)) {
                return Err("slice start tuple annotation does not match its fields".into());
            }
            let mut offsets = Vec::new();
            for field in fields {
                if !matches!(field.ty.kind(), TyKind::Int(crate::semi::ty::IntTy::Signed(64))) {
                    return Err("slice offsets must have signed 64-bit integer scalar type annotations".into());
                }
                let K::ConstInt(value) = &field.kind else {
                    return Err(
                        "slice start offsets must be a tuple of nonnegative integer literals".into(),
                    );
                };
                let offset = nonnegative_offset(value)?;
                offsets.push(offset);
            }
            if let TyKind::Tensor { dims, .. } = result.kind() {
                if dims.len() != offsets.len() {
                    return Err("slice requires one start offset per input dimension".into());
                }
                for (axis, (&size, &start)) in dims.iter().zip(offsets.iter()).enumerate() {
                    let limit = (start as u64)
                        .checked_add(size)
                        .filter(|&v| v <= i64::MAX as u64)
                        .ok_or("slice limit exceeds signed 64-bit shape range")?;
                    if let TyKind::Tensor {
                        dims: input_dims, ..
                    } = input.ty.kind()
                    {
                        if let Some(&bound) = input_dims.get(axis)
                            && bound != DYNAMIC_EXTENT
                            && limit > bound
                        {
                            return Err("slice limit exceeds the input extent".into());
                        }
                    }
                }
            }
            Ok(offsets)
        }
        pub fn $pairs<'tcx>(args: &[$expr]) -> Result<Vec<i64>, String> {
            use $kind as K;
            use rustc_hash::FxHashSet;
            let [_, metadata] = args else {
                return Err("collective_permute requires input and literal source/target pairs".into());
            };
            let rows: &[$expr] = match &metadata.kind {
                K::Tuple(rows) => rows.as_ref(),
                K::Seq(rows) if rows.is_empty() => &[],
                _ => return Err("collective_permute pairs must be a tuple of literal (source, target) pairs".into()),
            };
            if !(matches!(metadata.ty.kind(), TyKind::Tuple(types)
                if types.len() == rows.len() && types.iter().zip(rows.iter()).all(|(ty,row)| *ty == row.ty))
                || rows.is_empty() && matches!(metadata.ty.kind(), TyKind::Unit))
            {
                return Err("collective_permute pair tuple annotation does not match its fields".into());
            }
            let mut sources = FxHashSet::default();
            let mut targets = FxHashSet::default();
            let mut pairs = Vec::new();
            for row in rows {
                let K::Tuple(fields) = &row.kind else {
                    return Err("collective_permute requires literal (source, target) pairs".into());
                };
                if fields.len() != 2 || !matches!(row.ty.kind(), TyKind::Tuple(types)
                    if types.len() == 2 && types.iter().zip(fields.iter()).all(|(ty,field)| *ty == field.ty))
                {
                    return Err("collective_permute requires exactly two integers per pair".into());
                }
                let mut pair = [0i64; 2];
                for (index, field) in fields.iter().enumerate() {
                    if !matches!(field.ty.kind(), TyKind::Int(crate::semi::ty::IntTy::Signed(64))) {
                        return Err("collective_permute replica ids must have signed 64-bit integer scalar type annotations".into());
                    }
                    let K::ConstInt(value) = &field.kind else {
                        return Err("collective_permute replica ids must be nonnegative integer literals".into());
                    };
                    pair[index] = nonnegative_offset(value)
                        .map_err(|_| "collective_permute replica id is outside the nonnegative signed 64-bit range")?;
                }
                if !sources.insert(pair[0]) || !targets.insert(pair[1]) {
                    return Err("collective_permute requires unique source ids and unique target ids".into());
                }
                pairs.extend(pair);
            }
            Ok(pairs)
        }
        pub fn $name<'tcx>(
            captures: &[(crate::semi::hir::VarId, Ty<'tcx>)],
            body: &$expr,
            result: Ty<'tcx>,
        ) -> Result<(), String> {
            use $kind as K;
            use crate::semi::hir::VarId;
            use rustc_hash::{FxHashMap, FxHashSet};
            if !matches!(result.kind(),TyKind::Future(ps) if ps.len()==1 && ps[0]==body.ty)
                || !matches!(
                    body.ty.kind(),
                    TyKind::Tensor { .. } | TyKind::Generic(_) | TyKind::Hole(_)
                )
            {
                return Err("async result must be a single-payload Future matching its Tensor body".into());
            }
            let mut env: FxHashMap<VarId, (Ty<'tcx>, Origin)> = FxHashMap::default();
            for &(v, t) in captures {
                if !capture_type(t) {
                    return Err(
                        "async captures must lower to tensor operands (no Future, Token or host handles)"
                            .into(),
                    );
                }
                if env.insert(v, (t, origin(t))).is_some() {
                    return Err("duplicate async capture".into());
                }
            }
            let initial: FxHashSet<_> = env.keys().copied().collect();
            let mut used = FxHashSet::default();
            let mut count = 0;
            fn walk<'tcx>(
                e: &$expr,
                env: &mut FxHashMap<VarId, (Ty<'tcx>, Origin)>,
                used: &mut FxHashSet<VarId>,
                count: &mut usize,
            ) -> Result<Origin, String> {
                match &e.kind {
                    K::Var(var) => {
                        let Some((ty, origin)) = env.get(var) else {
                            return Err("async body references an undeclared capture or binding".into());
                        };
                        if *ty != e.ty {
                            return Err("async capture or binding type does not match its use".into());
                        }
                        used.insert(*var);
                        Ok(origin.clone())
                    }
                    K::Tuple(fields) => {
                        if !matches!(e.ty.kind(), TyKind::Tuple(types)
                            if types.len() == fields.len()
                            && types.iter().zip(fields.iter()).all(|(ty, field)| *ty == field.ty))
                        {
                            return Err("async tuple annotation does not match its fields".into());
                        }
                        let mut origins = Vec::new();
                        for field in fields.iter() {
                            origins.push(walk(field, env, used, count)?);
                        }
                        Ok(Origin::Tuple(origins))
                    }
                    K::Seq(exprs) => {
                        if exprs.last().is_some_and(|last| last.ty != e.ty)
                            || (exprs.is_empty() && !matches!(e.ty.kind(), TyKind::Unit))
                        {
                            return Err("async sequence annotation does not match its result".into());
                        }
                        let mut origin = Origin::Unit;
                        for expr in exprs.iter() {
                            origin = walk(expr, env, used, count)?;
                        }
                        Ok(origin)
                    }
                    K::Let { var, value, .. } => {
                        if !matches!(e.ty.kind(), TyKind::Unit) {
                            return Err("async binding expression must have unit type".into());
                        }
                        let origin = walk(value, env, used, count)?;
                        if env.insert(*var, (value.ty, origin)).is_some() {
                            return Err("async local binding reuses a variable identity".into());
                        }
                        Ok(Origin::Unit)
                    }
                    K::Proj(value, path) => {
                        let mut origin = walk(value, env, used, count)?;
                        let mut ty = value.ty;
                        for &index in path.iter() {
                            let (Origin::Tuple(origins), TyKind::Tuple(types)) = (origin, ty.kind()) else {
                                return Err("async projection must select a tuple field".into());
                            };
                            origin = origins.get(index as usize).cloned()
                                .ok_or("async tuple projection is out of bounds")?;
                            ty = *types.get(index as usize)
                                .ok_or("async tuple projection is out of bounds")?;
                        }
                        if ty != e.ty {
                            return Err("async projection annotation does not match its field".into());
                        }
                        Ok(origin)
                    }
                    K::TensorOp { op: op @ (TensorFn::DynamicSlice | TensorFn::Slice | TensorFn::CollectivePermute), args } => {
                        $opcheck(*op, e.ty, args)?;
                        // Static offsets become attributes, so they emit no SSA operations.
                        let operands = if matches!(op, TensorFn::Slice | TensorFn::CollectivePermute) { 1 } else { args.len() };
                        for arg in args.iter().take(operands) {
                            walk(arg, env, used, count)?;
                        }
                        *count += 1;
                        Ok(Origin::Result)
                    }
                    _ => Err("async body must contain one supported collective or slice operation; bind computations and constants outside the async block".into()),
                }
            }
            let returned = walk(body, &mut env, &mut used, &mut count)?;
            if count != 1 || returned != Origin::Result {
                return Err("async body must return the result of exactly one supported collective or slice operation".into());
            }
            if initial.iter().any(|v| !used.contains(v)) {
                return Err("async capture list contains an unused variable".into());
            }
            Ok(())
        }
    };
}
validate!(
    hir,
    hir_tensor_op,
    hir_slice_offsets,
    hir_permutation_pairs,
    crate::semi::hir::Expr<'tcx>,
    crate::semi::hir::ExprKind
);
validate!(
    mir,
    mir_tensor_op,
    mir_slice_offsets,
    mir_permutation_pairs,
    crate::full::mir::Expr<'tcx>,
    crate::full::mir::ExprKind
);

#[cfg(test)]
mod tests {
    use crate::{in_arena, semi, surface};

    #[test]
    fn malformed_serialized_async_regions_are_rejected() {
        in_arena(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(
                r#"
                #[kernel] fn begin(x:Tensor<[f32;8]>, i:i64)->Future<Tensor<[f32;2]>> {
                    async { let pair=(core::intrinsic::tensor::dynamic_slice<Tensor<[f32;2]>>(x,(i,)),()); pair.0 }
                }
                #[kernel] fn stat(x:Tensor<[f32;8]>)->Future<Tensor<[f32;2]>> {
                    async { core::intrinsic::tensor::slice<Tensor<[f32;2]>>(x,(2,)) }
                }
                #[kernel] pub fn run(x:Tensor<[f32;8]>,i:i64)->Tensor<[f32;2]> {
                    let unused=core::intrinsic::tensor::async_done(stat(x));
                    core::intrinsic::tensor::async_done(begin(x,i))
                }
            "#,
                interner.clone(),
            );
            assert!(parsed.ok());
            let source = surface::program(&parsed.root);
            let elab = semi::elaborate(tcx, &source, &interner);
            assert!(!elab.has_errors(), "{:?}", elab.reports);
            let hir = semi::hir::print::Printer::new(&elab.defs, elab.resolver).program(
                &elab.elaborated,
                &elab.strings.entries(),
                &elab.records,
                &elab.trampolines,
            );
            let (program, reports) = crate::full::mono::monomorphize(&elab.mono_input());
            assert!(reports.is_empty(), "{reports:?}");
            let mir =
                crate::full::mir::print::Printer::new(&elab.defs, elab.resolver).program(&program);
            for (text, is_hir) in [(hir, true), (mir, false)] {
                let parse_error = |text: &str| {
                    if is_hir {
                        semi::hir::build::parse_program(tcx, text)
                            .err()
                            .map(|e| format!("{e:?}"))
                    } else {
                        crate::full::mir::build::parse_program(tcx, text)
                            .err()
                            .map(|e| format!("{e:?}"))
                    }
                };
                assert!(parse_error(&text).is_none(), "{text}");
                let start = text
                    .match_indices("async[")
                    .map(|(index, _)| index + 6)
                    .find(|&start| {
                        let end = start + text[start..].find("] {").unwrap();
                        text[start..end].contains(": i64")
                    })
                    .expect("dynamic slice capture list");
                let end = start + text[start..].find("] {").unwrap();
                let captures = &text[start..end];
                for (capture_text, expected) in [
                    (String::new(), "undeclared capture"),
                    (format!("{captures}, {captures}"), "duplicate async capture"),
                    (format!("{captures}, v999: Tensor<f32>"), "unused variable"),
                    (captures.replacen("v0:", "v998:", 1), "undeclared capture"),
                    (captures.replace("i64", "bool"), "type does not match"),
                ] {
                    let malformed = format!("{}{}{}", &text[..start], capture_text, &text[end..]);
                    let error = parse_error(&malformed).unwrap_or_else(|| {
                        panic!("malformed async captures accepted: {expected}\n{malformed}")
                    });
                    assert!(
                        error.contains(expected),
                        "expected {expected}: {error}\n{malformed}"
                    );
                }
                let malformed = text.replace("tensor#dynamic_slice", "tensor#async_done");
                assert!(
                    parse_error(&malformed)
                        .unwrap()
                        .contains("async_done requires")
                );
                let malformed =
                    text.replace("(2 : i64,) : (i64,)", "(2 : Tensor<i64>,) : (Tensor<i64>,)");
                assert_ne!(text, malformed, "missing static offset literal");
                let error = parse_error(&malformed).expect("forged static offset type accepted");
                assert!(
                    error.contains("slice start tuple annotation")
                        || error.contains("integer scalar type"),
                    "{error}"
                );
                // The first projection is the async body's returned tensor.
                let projection = text
                    .find(", 0) : Tensor")
                    .unwrap_or_else(|| panic!("tuple result projection: {text}"))
                    + 7;
                let end_ty = projection + text[projection..].find('>').unwrap() + 1;
                let malformed = format!("{}bool{}", &text[..projection], &text[end_ty..]);
                let error = parse_error(&malformed).expect("forged projection accepted");
                assert!(
                    error.contains("projection annotation")
                        || error.contains("single-payload Future"),
                    "{error}"
                );
            }
        });
    }
}
