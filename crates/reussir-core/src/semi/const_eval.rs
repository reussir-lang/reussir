//! Compile-time constants: the value domain and its evaluator.
//!
//! [`Elaborator::eval_const`] evaluates a surface expression against the type
//! the constant must have, producing a [`ConstValue`] the compiler can inspect
//! (an array extent reads its `u64`, for example). A constant is a scalar
//! literal — an integer, float, `bool`, or `char`, where unary minus folds into
//! a numeric literal — a sequence literal `[e0, e1, ...]` of type
//! `ConstSeq<T>`, or a constructor of a `[value]` record whose fields are
//! constants (`S { f: e }`, `E::V { e }`, `E::V`). Numbers stay exact
//! ([`crate::literal`]) and are checked against their type here, so a value is
//! always representable in it.
//!
//! [`const_value_kind`] turns a value back into a HIR expression for the
//! program to use at runtime: scalars become literals, a sequence becomes
//! [`ExprKind::ConstSeq`], which codegen emits as an immutable global, and a
//! record value becomes the constructor call that builds it.

use reussir_syntax::kind::TokenKey;
use rustc_hash::FxHashMap;

use crate::literal::{self, FloatLit, Integer};
use crate::semi::hir::{Expr, ExprId, ExprKind};
use crate::semi::ty::{DefId, Ty, TyCtxt, TyKind};
use crate::surface::{self, Const, Span, UnaryOp};

use super::ctxt::{DefaultCap, Elaborator, Record, RecordFields};

/// A scalar constant, representable in the type it was evaluated against.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstScalar {
    Int(Integer),
    Float(FloatLit),
    Bool(bool),
    /// A Unicode scalar value, as its code point.
    Char(u32),
}

/// A value computed at compile time.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstValue<'tcx> {
    Scalar(ConstScalar),
    /// A `ConstSeq<elem>` value.
    Seq {
        elem: Ty<'tcx>,
        elems: Vec<ConstScalar>,
    },
    /// A value of the `[value]` record `def`: a struct when `variant` is
    /// `None`, else that variant of an enum (its index, as in
    /// [`ExprKind::VariantCall`]). `fields` are in declaration order.
    Record {
        def: DefId,
        variant: Option<usize>,
        fields: Vec<ConstValue<'tcx>>,
    },
}

/// What [`Elaborator::check_const_ty`] lists as the constant types, for
/// diagnostics.
pub(super) const CONST_TYS: &str = "an integer, float, `bool`, or `char` scalar, a `ConstSeq` \
     of one, or a non-generic `[value]` record whose fields have constant types";

/// The part of a type that keeps it from being a constant type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonConstField<'tcx> {
    /// The field path from the checked type, such as `sharding.Tiled.0` (a
    /// variant contributes its name and the payload index); empty when the
    /// checked type itself is the offending part.
    pub path: Vec<String>,
    pub ty: Ty<'tcx>,
}

/// Whether a scalar constant can have type `ty`: an integer, a float with an
/// IEEE encoding, `bool`, or `char`.
pub fn is_const_scalar_ty(ty: Ty<'_>) -> bool {
    match *ty.kind() {
        TyKind::Int(_) | TyKind::Bool | TyKind::Char => true,
        TyKind::Fp(fp) => literal::ieee_params(fp).is_some(),
        _ => false,
    }
}

/// The fields a constructor of `record` supplies, in declaration order: the
/// struct's own, or those of variant `variant` of an enum, which are
/// positional.
fn ctor_fields<'tcx>(
    record: &Record<'tcx>,
    variant: Option<usize>,
) -> Vec<(Option<TokenKey>, Ty<'tcx>)> {
    match (&record.fields, variant) {
        (Some(RecordFields::Named(fs)), None) => fs.iter().map(|f| (Some(f.0), f.1)).collect(),
        (Some(RecordFields::Unnamed(fs)), None) => fs.iter().map(|f| (None, f.0)).collect(),
        (Some(RecordFields::Variants(vs)), Some(v)) => {
            vs[v].fields.iter().map(|&ty| (None, ty)).collect()
        }
        _ => unreachable!("a struct value names no variant, and an enum value names one"),
    }
}

/// The HIR expression kind denoting `value`: scalars become literals, a
/// sequence becomes [`ExprKind::ConstSeq`], and a record value becomes the
/// [`ExprKind::CompoundCall`] or [`ExprKind::VariantCall`] that builds it
/// from its fields' expressions, each numbered by `fresh_id`.
pub fn const_value_kind<'tcx>(
    tcx: &TyCtxt<'tcx>,
    records: &FxHashMap<DefId, Record<'tcx>>,
    value: &ConstValue<'tcx>,
    span: Option<Span>,
    fresh_id: &mut impl FnMut() -> ExprId,
) -> ExprKind<'tcx> {
    match value {
        ConstValue::Scalar(ConstScalar::Int(n)) => ExprKind::ConstInt(tcx.alloc(n.clone())),
        ConstValue::Scalar(ConstScalar::Float(f)) => ExprKind::ConstFloat(tcx.alloc(f.clone())),
        ConstValue::Scalar(ConstScalar::Bool(b)) => ExprKind::ConstBool(*b),
        ConstValue::Scalar(ConstScalar::Char(c)) => ExprKind::ConstChar(*c),
        ConstValue::Seq { elems, .. } => ExprKind::ConstSeq(tcx.alloc(elems.clone()).as_slice()),
        ConstValue::Record {
            def,
            variant,
            fields,
        } => {
            let args = ctor_fields(&records[def], *variant)
                .into_iter()
                .zip(fields)
                .map(|((_, ty), field)| Expr {
                    kind: const_value_kind(tcx, records, field, span, fresh_id),
                    ty,
                    span,
                    id: fresh_id(),
                })
                .collect();
            // Constant records are not generic.
            let ty_args = Vec::new();
            match *variant {
                None => ExprKind::CompoundCall {
                    target: *def,
                    ty_args,
                    args,
                },
                Some(variant) => ExprKind::VariantCall {
                    target: *def,
                    ty_args,
                    variant,
                    args,
                },
            }
        }
    }
}

/// The scalar a literal expression denotes, folding unary minus into numeric
/// literals. `Err` describes what the expression is instead.
fn literal_scalar(expr: &surface::Expr) -> Result<ConstScalar, &'static str> {
    match expr.kind() {
        surface::ExprKind::ConstExpr(Const::ConstInt(n)) => Ok(ConstScalar::Int(n)),
        surface::ExprKind::ConstExpr(Const::ConstFloat(f)) => Ok(ConstScalar::Float(f)),
        surface::ExprKind::ConstExpr(Const::ConstBool(b)) => Ok(ConstScalar::Bool(b)),
        surface::ExprKind::ConstExpr(Const::ConstChar(c)) => Ok(ConstScalar::Char(c)),
        surface::ExprKind::ConstExpr(Const::ConstString(_)) => Err("a string literal"),
        surface::ExprKind::SeqLit(_) => Err("a sequence literal"),
        surface::ExprKind::CtorCallExpr(_) => Err("a record constructor"),
        surface::ExprKind::UnaryOpExpr(UnaryOp::Negate, inner) => match literal_scalar(&inner)? {
            ConstScalar::Int(n) => Ok(ConstScalar::Int(-n)),
            ConstScalar::Float(f) => Ok(ConstScalar::Float(f.neg())),
            ConstScalar::Bool(_) | ConstScalar::Char(_) => Err("a negated non-numeric literal"),
        },
        _ => Err("an expression that cannot be evaluated at compile time"),
    }
}

fn scalar_description(value: &ConstScalar) -> &'static str {
    match value {
        ConstScalar::Int(_) => "an integer literal",
        ConstScalar::Float(_) => "a float literal",
        ConstScalar::Bool(_) => "a bool literal",
        ConstScalar::Char(_) => "a char literal",
    }
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// Whether a constant can have type `ty`: a scalar accepted by
    /// [`is_const_scalar_ty`], `ConstSeq<T>` of one, or a non-generic
    /// `[value]` struct or enum all of whose fields (of every variant) have
    /// constant types. `Err` names the first part that does not.
    pub fn check_const_ty(&self, ty: Ty<'tcx>) -> Result<(), NonConstField<'tcx>> {
        self.check_const_ty_in(ty, &mut Vec::new())
    }

    /// [`Self::check_const_ty`] inside the records `enclosing`. A record
    /// enclosing itself is an infinite-size `[value]` cycle, reported where
    /// the records are declared; here it only stops the walk.
    fn check_const_ty_in(
        &self,
        ty: Ty<'tcx>,
        enclosing: &mut Vec<DefId>,
    ) -> Result<(), NonConstField<'tcx>> {
        let here = Err(NonConstField {
            path: Vec::new(),
            ty,
        });
        let def = match *ty.kind() {
            TyKind::ConstSeq(elem) if is_const_scalar_ty(elem) => return Ok(()),
            TyKind::Record { def, args, .. } if args.is_empty() && !enclosing.contains(&def) => def,
            _ if is_const_scalar_ty(ty) => return Ok(()),
            _ => return here,
        };
        let record = &self.records[&def];
        if record.default_cap != DefaultCap::Value {
            return here;
        }
        let fields: Vec<(String, Ty<'tcx>)> = match &record.fields {
            Some(RecordFields::Named(fs)) => {
                fs.iter().map(|f| (self.sym(f.0).to_owned(), f.1)).collect()
            }
            Some(RecordFields::Unnamed(fs)) => fs
                .iter()
                .enumerate()
                .map(|(k, f)| (k.to_string(), f.0))
                .collect(),
            Some(RecordFields::Variants(vs)) => vs
                .iter()
                .flat_map(|v| {
                    let name = self.sym(v.name);
                    v.fields
                        .iter()
                        .enumerate()
                        .map(move |(k, &f)| (format!("{name}.{k}"), f))
                })
                .collect(),
            Some(RecordFields::Opaque) | None => return here,
        };
        enclosing.push(def);
        let checked = fields.into_iter().try_for_each(|(step, field_ty)| {
            self.check_const_ty_in(field_ty, enclosing)
                .map_err(|mut part| {
                    part.path.insert(0, step);
                    part
                })
        });
        enclosing.pop();
        checked
    }

    /// `ty`, a type [`Self::check_const_ty`] rejects with `part`, shown with
    /// the offending field for a diagnostic.
    pub(super) fn non_const_ty_display(&self, ty: Ty<'tcx>, part: &NonConstField<'tcx>) -> String {
        let shown = self.ty_display(ty);
        if part.path.is_empty() {
            return format!("`{shown}`");
        }
        format!(
            "`{shown}`, whose field `{}` has type `{}`",
            part.path.join("."),
            self.ty_display(part.ty)
        )
    }

    /// Evaluate `expr` as a constant of type `ty`, which must be a constant
    /// type ([`Self::check_const_ty`]). Reports a diagnostic and returns
    /// `None` when `expr` is not a constant of that type.
    pub fn eval_const(&mut self, expr: &surface::Expr, ty: Ty<'tcx>) -> Option<ConstValue<'tcx>> {
        let ty = self.infer.resolve(ty);
        if matches!(ty.kind(), TyKind::Bottom) {
            return None;
        }
        if let Err(part) = self.check_const_ty(ty) {
            let found = self.non_const_ty_display(ty, &part);
            self.error(
                Some(expr.span()),
                format!("a constant must have a constant type ({CONST_TYS}), found {found}"),
            );
            return None;
        }
        match *ty.kind() {
            TyKind::ConstSeq(elem) => {
                let surface::ExprKind::SeqLit(items) = expr.kind() else {
                    self.expected_const_error(expr, ty);
                    return None;
                };
                // Evaluate every element, so each invalid one is reported.
                let elems: Vec<_> = items
                    .iter()
                    .map(|item| self.eval_scalar(item, elem))
                    .collect();
                let elems = elems.into_iter().collect::<Option<_>>()?;
                Some(ConstValue::Seq { elem, elems })
            }
            TyKind::Record { def, .. } => self.eval_record(expr, ty, def),
            _ => self.eval_scalar(expr, ty).map(ConstValue::Scalar),
        }
    }

    /// Report that `expr` is not a constant of type `ty`, describing what it
    /// is instead.
    fn expected_const_error(&mut self, expr: &surface::Expr, ty: Ty<'tcx>) {
        let found = literal_scalar(expr).map_or_else(|what| what, |v| scalar_description(&v));
        self.error(
            Some(expr.span()),
            format!(
                "expected a constant of type `{}`, found {found}",
                self.ty_display(ty)
            ),
        );
    }

    /// Evaluate a constructor of `ty`, a constant type of record `def`:
    /// `S { f: e, .. }` or `S { e, .. }` for a struct, `E::V { e, .. }` or
    /// `E::V` for an enum variant. The path resolves and the arguments match
    /// the fields as for a constructor call in a body, and each field
    /// evaluates against its declared type.
    fn eval_record(
        &mut self,
        expr: &surface::Expr,
        ty: Ty<'tcx>,
        def: DefId,
    ) -> Option<ConstValue<'tcx>> {
        let span = Some(expr.span());
        let (path, args) = match expr.kind() {
            surface::ExprKind::CtorCallExpr(ctor) => (ctor.name, ctor.args.into_vec()),
            // A qualified path without arguments: a unit variant.
            surface::ExprKind::Var(path) if !path.segments.is_empty() => (path, Vec::new()),
            _ => {
                self.expected_const_error(expr, ty);
                return None;
            }
        };
        let path = self.expand_path(&path).unwrap_or(path);
        let (ctor_def, variant) = self.resolve_ctor_path(&path, span)?;
        if ctor_def != def {
            let found = self.defs.path(ctor_def).display(self.resolver);
            self.error(
                span,
                format!(
                    "type mismatch: expected `{}`, found `{found}`",
                    self.ty_display(ty)
                ),
            );
            return None;
        }
        // Only an enum qualifies a variant, so a struct's path names none.
        let is_enum = matches!(self.records[&def].fields, Some(RecordFields::Variants(_)));
        let variant = match (is_enum, variant) {
            (true, Some(name)) => {
                let enum_name = *path.segments.last().expect("a variant path is qualified");
                Some(self.variant_index(def, enum_name, name, span)?)
            }
            (true, None) => {
                self.error(
                    span,
                    format!("`{}` is an enum, not a struct", self.sym(path.basename)),
                );
                return None;
            }
            (false, _) => {
                self.report_private_ctor_fields(def, span);
                None
            }
        };
        let fields = ctor_fields(&self.records[&def], variant);
        let names: Vec<Option<TokenKey>> = fields.iter().map(|&(name, _)| name).collect();
        let (by_field, exact) = self.match_ctor_args(path.basename, &args, &names, span);
        // Evaluate every supplied field, so each invalid one is reported.
        let values: Vec<_> = by_field
            .iter()
            .zip(&fields)
            .map(|(&arg, &(_, field_ty))| arg.and_then(|i| self.eval_const(&args[i].1, field_ty)))
            .collect();
        let values = values.into_iter().collect::<Option<_>>()?;
        exact.then_some(ConstValue::Record {
            def,
            variant,
            fields: values,
        })
    }

    fn eval_scalar(&mut self, expr: &surface::Expr, ty: Ty<'tcx>) -> Option<ConstScalar> {
        let span = Some(expr.span());
        let value = match literal_scalar(expr) {
            Ok(value) => value,
            Err(found) => {
                let shown = self.ty_display(ty);
                self.error(
                    span,
                    format!("expected a constant of type `{shown}`, found {found}"),
                );
                return None;
            }
        };
        let in_range = match (&value, *ty.kind()) {
            (ConstScalar::Int(n), TyKind::Int(int_ty)) => literal::int_fits(n, int_ty),
            (ConstScalar::Float(f), TyKind::Fp(fp)) => literal::ieee_params(fp)
                .is_some_and(|(exp_bits, mant_bits)| f.to_ieee_bits(exp_bits, mant_bits).is_ok()),
            (ConstScalar::Bool(_), TyKind::Bool) | (ConstScalar::Char(_), TyKind::Char) => true,
            _ => {
                let shown = self.ty_display(ty);
                let found = scalar_description(&value);
                self.error(
                    span,
                    format!("expected a constant of type `{shown}`, found {found}"),
                );
                return None;
            }
        };
        if in_range {
            return Some(value);
        }
        let shown = self.ty_display(ty);
        let message = match &value {
            ConstScalar::Float(f) => {
                format!(
                    "float literal `{f}` is out of range for `{shown}` (it would round to infinity)"
                )
            }
            ConstScalar::Int(n) => format!("integer literal `{n}` is out of range for `{shown}`"),
            ConstScalar::Bool(_) | ConstScalar::Char(_) => unreachable!("always representable"),
        };
        self.error(span, message);
        None
    }

    /// The HIR expression of type `ty` that denotes `value`, which
    /// [`Self::eval_const`] produced against `ty`.
    pub fn const_value_expr(
        &mut self,
        value: &ConstValue<'tcx>,
        ty: Ty<'tcx>,
        span: Option<Span>,
    ) -> Expr<'tcx> {
        // `Self::fresh_expr_id`, split from the borrow of the records.
        let counter = &mut self.expr_counter;
        let mut fresh_id = || {
            let id = ExprId(*counter);
            *counter += 1;
            id
        };
        let kind = const_value_kind(self.tcx, &self.records, value, span, &mut fresh_id);
        self.mk_expr(kind, ty, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::ty::{FpTy, IntTy, TyCtxt};

    type Interner = reussir_syntax::SessionInterner;

    /// The tail expression of `fn f(x: u64) { <source> }`.
    fn tail_expr(interner: &Interner, source: &str) -> surface::Expr {
        let text = format!("fn f(x: u64) {{ {source} }}");
        let parsed = reussir_syntax::parse_with_interner(&text, interner.clone());
        assert!(parsed.ok(), "{:?}", parsed.errors);
        let program = surface::program(&parsed.root);
        let surface::StmtKind::Function(function) = program[0].kind() else {
            panic!("expected a function");
        };
        let surface::ExprKind::ExprSeq(items) = function.body.expect("body").kind() else {
            panic!("expected a block body");
        };
        items.last().expect("tail expression").clone()
    }

    fn messages(elab: &Elaborator<'_, '_>) -> Vec<String> {
        elab.reports.iter().map(|r| r.message.clone()).collect()
    }

    /// Evaluate the tail expression of `fn f(x: u64) { <source> }` against
    /// `ty`, returning the value and the reported messages.
    fn eval<'tcx>(
        tcx: &TyCtxt<'tcx>,
        source: &str,
        ty: Ty<'tcx>,
    ) -> (Option<ConstValue<'tcx>>, Vec<String>) {
        let interner = Interner::new(reussir_syntax::new_threaded_interner());
        let mut elab = Elaborator::new(tcx, &interner);
        let value = elab.eval_const(&tail_expr(&interner, source), ty);
        (value, messages(&elab))
    }

    /// Elaborate the records `decls` and hand `f` the elaborator, the type
    /// of a record by name, and a parser of tail expressions over the same
    /// names ([`tail_expr`]).
    fn with_records(
        decls: &str,
        f: impl for<'a, 'tcx> FnOnce(
            &mut Elaborator<'a, 'tcx>,
            &dyn Fn(&Elaborator<'a, 'tcx>, &str) -> Ty<'tcx>,
            &dyn Fn(&str) -> surface::Expr,
        ),
    ) {
        crate::with_tcx(|tcx| {
            let interner = Interner::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(decls, interner.clone());
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = surface::program(&parsed.root);
            let mut elab = crate::semi::elaborate(tcx, &program, &interner);
            assert!(!elab.has_errors(), "{:#?}", elab.reports);
            let record_ty = |elab: &Elaborator<'_, '_>, name: &str| {
                let def = elab
                    .records
                    .keys()
                    .find(|&&d| elab.sym(elab.defs.path(d).name()) == name);
                let def = *def.expect("declared record");
                tcx.mk_record(def, &[], crate::semi::ty::Flexivity::Irrelevant)
            };
            f(&mut elab, &record_ty, &|source| {
                tail_expr(&interner, source)
            });
        });
    }

    fn int(n: i64) -> ConstScalar {
        ConstScalar::Int(Integer::from(n))
    }

    #[test]
    fn sequence_literals_evaluate_to_range_checked_elements() {
        crate::with_tcx(|tcx| {
            let i8_ty = tcx.mk_int(IntTy::Signed(8));
            let (value, errors) = eval(tcx, "[-128, 127, -(1)]", tcx.mk_const_seq(i8_ty));
            assert!(errors.is_empty(), "{errors:?}");
            assert_eq!(
                value,
                Some(ConstValue::Seq {
                    elem: i8_ty,
                    elems: vec![int(-128), int(127), int(-1)],
                })
            );

            let f32_ty = tcx.mk_fp(FpTy::Ieee(32));
            let (value, _) = eval(tcx, "[-1.5]", tcx.mk_const_seq(f32_ty));
            let Some(ConstValue::Seq { elems, .. }) = value else {
                panic!("expected a sequence");
            };
            assert_eq!(
                elems,
                [ConstScalar::Float(literal::parse_float("1.5").neg())]
            );

            let (value, _) = eval(tcx, "['a', 'b']", tcx.mk_const_seq(tcx.mk_char()));
            let Some(ConstValue::Seq { elems, .. }) = value else {
                panic!("expected a sequence");
            };
            assert_eq!(elems, [ConstScalar::Char(97), ConstScalar::Char(98)]);
        });
    }

    #[test]
    fn every_invalid_element_is_reported() {
        crate::with_tcx(|tcx| {
            let u8_ty = tcx.mk_int(IntTy::Unsigned(8));
            let (value, errors) = eval(tcx, "[256, 1, -1, x]", tcx.mk_const_seq(u8_ty));
            assert_eq!(value, None);
            assert_eq!(
                errors,
                [
                    "integer literal `256` is out of range for `u8`",
                    "integer literal `-1` is out of range for `u8`",
                    "expected a constant of type `u8`, found an expression that cannot be evaluated at compile time",
                ]
            );
        });
    }

    #[test]
    fn scalar_constants_and_rejected_types() {
        crate::with_tcx(|tcx| {
            let u64_ty = tcx.mk_int(IntTy::Unsigned(64));
            let (value, errors) = eval(tcx, "18446744073709551615", u64_ty);
            assert!(errors.is_empty(), "{errors:?}");
            assert_eq!(
                value,
                Some(ConstValue::Scalar(ConstScalar::Int(Integer::from(
                    u64::MAX
                ))))
            );

            let (value, errors) = eval(tcx, "[1]", u64_ty);
            assert_eq!(value, None);
            assert_eq!(
                errors,
                ["expected a constant of type `u64`, found a sequence literal"]
            );

            let (value, errors) = eval(tcx, "\"s\"", tcx.mk_str());
            assert_eq!(value, None);
            assert_eq!(
                errors,
                [
                    "a constant must have a constant type (an integer, float, `bool`, or `char` scalar, a `ConstSeq` of one, or a non-generic `[value]` record whose fields have constant types), found `str`"
                ]
            );
        });
    }

    #[test]
    fn a_sequence_value_becomes_a_const_seq_expression() {
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let mut elab = Elaborator::new(tcx, &interner);
            let bool_ty = tcx.mk_bool();
            let ty = tcx.mk_const_seq(bool_ty);
            let elems = vec![ConstScalar::Bool(true), ConstScalar::Bool(false)];
            let value = ConstValue::Seq {
                elem: bool_ty,
                elems: elems.clone(),
            };
            let expr = elab.const_value_expr(&value, ty, None);
            assert_eq!(expr.ty, ty);
            let ExprKind::ConstSeq(stored) = expr.kind else {
                panic!("expected a ConstSeq node, got {:?}", expr.kind);
            };
            assert_eq!(stored, elems.as_slice());
        });
    }

    /// `[value]` records nesting each other: a struct inside an enum
    /// variant inside a struct, plus a tuple struct and a unit variant.
    const NESTED: &str = r#"
        enum [value] Memory { Device, Host }
        struct [value] Tiling(ConstSeq<u64>, u64)
        enum [value] Sharding { Replicated, Tiled(Tiling) }
        struct [value] Placement { devices: ConstSeq<i32>, sharding: Sharding, memory: Memory }
    "#;

    #[test]
    fn nested_records_evaluate_in_declaration_order() {
        with_records(NESTED, |elab, record_ty, expr| {
            let placement = record_ty(elab, "Placement");
            let source = r#"Placement {
                memory: Memory::Host,
                sharding: Sharding::Tiled { Tiling { [2, 1], 0 } },
                devices: [3, -1],
            }"#;
            let value = elab.eval_const(&expr(source), placement);
            assert!(elab.reports.is_empty(), "{:?}", messages(elab));
            let def = |name| match *record_ty(elab, name).kind() {
                TyKind::Record { def, .. } => def,
                _ => unreachable!(),
            };
            let seq = |elem, elems: &[i64]| ConstValue::Seq {
                elem,
                elems: elems.iter().map(|&n| int(n)).collect(),
            };
            let i32_ty = elab.tcx.mk_int(IntTy::Signed(32));
            let u64_ty = elab.tcx.mk_int(IntTy::Unsigned(64));
            let tiling = ConstValue::Record {
                def: def("Tiling"),
                variant: None,
                fields: vec![seq(u64_ty, &[2, 1]), ConstValue::Scalar(int(0))],
            };
            assert_eq!(
                value,
                Some(ConstValue::Record {
                    def: def("Placement"),
                    variant: None,
                    fields: vec![
                        seq(i32_ty, &[3, -1]),
                        ConstValue::Record {
                            def: def("Sharding"),
                            variant: Some(1),
                            fields: vec![tiling],
                        },
                        ConstValue::Record {
                            def: def("Memory"),
                            variant: Some(1),
                            fields: vec![],
                        },
                    ],
                })
            );
        });
    }

    #[test]
    fn every_invalid_nested_field_is_reported() {
        with_records(NESTED, |elab, record_ty, expr| {
            let placement = record_ty(elab, "Placement");
            let source = r#"Placement {
                devices: [x],
                sharding: Sharding::Tiled { Tiling { [1], 2, 3 } },
                memory: Sharding::Replicated,
            }"#;
            assert_eq!(elab.eval_const(&expr(source), placement), None);
            assert_eq!(
                messages(elab),
                [
                    "expected a constant of type `i32`, found an expression that cannot be evaluated at compile time",
                    "`Tiling` expects 2 field(s), got 3",
                    "type mismatch: expected `Memory`, found `Sharding`",
                ]
            );
        });
    }

    #[test]
    fn const_types_recurse_through_value_records() {
        let decls = format!(
            r#"{NESTED}
            struct Shared {{ x: i64 }}
            enum [value] Labeled {{ Plain, Named(str) }}
            struct [value] Holder {{ inner: Placement, label: Labeled, shared: Shared }}
            struct [value] Pair<T> {{ a: T, b: T }}
            struct [value] Wrapped {{ pair: Pair<u8> }}
            "#
        );
        with_records(&decls, |elab, record_ty, _| {
            let str_ty = elab.tcx.mk_str();
            assert_eq!(elab.check_const_ty(record_ty(elab, "Placement")), Ok(()));
            let holder = elab.check_const_ty(record_ty(elab, "Holder"));
            assert_eq!(
                holder.map_err(|part| part.path),
                Err(vec!["label".to_owned(), "Named".to_owned() + ".0"])
            );
            let wrapped = elab.check_const_ty(record_ty(elab, "Wrapped")).unwrap_err();
            assert_eq!(wrapped.path, ["pair"]);
            assert!(matches!(wrapped.ty.kind(), TyKind::Record { args, .. } if !args.is_empty()));
            let shared = record_ty(elab, "Shared");
            assert_eq!(
                elab.check_const_ty(shared),
                Err(NonConstField {
                    path: vec![],
                    ty: shared
                })
            );
            assert!(elab.check_const_ty(elab.tcx.mk_const_seq(str_ty)).is_err());
        });
    }

    #[test]
    fn a_record_value_becomes_its_constructor_call() {
        with_records(NESTED, |elab, record_ty, expr| {
            let ty = record_ty(elab, "Sharding");
            let source = "Sharding::Tiled { Tiling { [7], 1 } }";
            let value = elab.eval_const(&expr(source), ty).expect("a constant");
            let built = elab.const_value_expr(&value, ty, None);
            assert_eq!(built.ty, ty);
            let ExprKind::VariantCall {
                variant: 1, args, ..
            } = built.kind
            else {
                panic!("expected a variant construction, got {:?}", built.kind);
            };
            let [tiling] = args.as_slice() else {
                panic!("expected one payload field");
            };
            assert_eq!(tiling.ty, record_ty(elab, "Tiling"));
            let ExprKind::CompoundCall { args, .. } = &tiling.kind else {
                panic!("expected a struct construction, got {:?}", tiling.kind);
            };
            let [dims, axis] = args.as_slice() else {
                panic!("expected two fields");
            };
            assert!(matches!(dims.kind, ExprKind::ConstSeq(elems) if elems == [int(7)]));
            let ExprKind::ConstInt(n) = axis.kind else {
                panic!("expected an integer literal, got {:?}", axis.kind);
            };
            assert_eq!(ConstScalar::Int(n.clone()), int(1));
            assert_eq!(axis.ty, elab.tcx.mk_int(IntTy::Unsigned(64)));
        });
    }
}
