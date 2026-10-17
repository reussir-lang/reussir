//! Compile-time constants: the value domain and its evaluator.
//!
//! [`Elaborator::eval_const`] evaluates a surface expression against the type
//! the constant must have, producing a [`ConstValue`] the compiler can inspect
//! (an array extent reads its `u64`, for example). A constant is a scalar
//! literal — an integer, float, `bool`, or `char`, where unary minus folds into
//! a numeric literal — or a sequence literal `[e0, e1, ...]` of type
//! `ConstSeq<T>`. Numbers stay exact ([`crate::literal`]) and are checked
//! against their type here, so a value is always representable in it.
//!
//! [`Elaborator::const_value_expr`] turns a value back into a HIR expression
//! for the program to use at runtime: scalars become literals, and a sequence
//! becomes [`ExprKind::ConstSeq`], which codegen emits as an immutable global.

use crate::literal::{self, FloatLit, Integer};
use crate::semi::hir::{Expr, ExprKind};
use crate::semi::ty::{Ty, TyKind};
use crate::surface::{self, Const, Span, UnaryOp};

use super::ctxt::Elaborator;

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
    /// Evaluate `expr` as a constant of type `ty`: a scalar type accepted by
    /// [`is_const_scalar_ty`], or `ConstSeq<T>` of one. Reports a diagnostic
    /// and returns `None` when `expr` is not a constant of that type.
    pub fn eval_const(&mut self, expr: &surface::Expr, ty: Ty<'tcx>) -> Option<ConstValue<'tcx>> {
        let ty = self.infer.resolve(ty);
        match *ty.kind() {
            TyKind::ConstSeq(elem) if is_const_scalar_ty(elem) => {
                let surface::ExprKind::SeqLit(items) = expr.kind() else {
                    let found =
                        literal_scalar(expr).map_or_else(|what| what, |v| scalar_description(&v));
                    self.error(
                        Some(expr.span()),
                        format!(
                            "expected a constant of type `{}`, found {found}",
                            self.ty_display(ty)
                        ),
                    );
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
            _ if is_const_scalar_ty(ty) => self.eval_scalar(expr, ty).map(ConstValue::Scalar),
            TyKind::Bottom => None,
            _ => {
                self.error(
                    Some(expr.span()),
                    format!(
                        "a constant must have a concrete integer, float, `bool`, `char`, or `ConstSeq` type, found `{}`",
                        self.ty_display(ty)
                    ),
                );
                None
            }
        }
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
        let kind = match value {
            ConstValue::Scalar(ConstScalar::Int(n)) => {
                ExprKind::ConstInt(self.tcx.alloc(n.clone()))
            }
            ConstValue::Scalar(ConstScalar::Float(f)) => {
                ExprKind::ConstFloat(self.tcx.alloc(f.clone()))
            }
            ConstValue::Scalar(ConstScalar::Bool(b)) => ExprKind::ConstBool(*b),
            ConstValue::Scalar(ConstScalar::Char(c)) => ExprKind::ConstChar(*c),
            ConstValue::Seq { elems, .. } => {
                ExprKind::ConstSeq(self.tcx.alloc(elems.clone()).as_slice())
            }
        };
        self.mk_expr(kind, ty, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::ty::{FpTy, IntTy, TyCtxt};

    /// Evaluate the tail expression of `fn f(x: u64) { <source> }` against
    /// `ty`, returning the value and the reported messages.
    fn eval<'tcx>(
        tcx: &TyCtxt<'tcx>,
        source: &str,
        ty: Ty<'tcx>,
    ) -> (Option<ConstValue<'tcx>>, Vec<String>) {
        let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
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
        let mut elab = Elaborator::new(tcx, &interner);
        let value = elab.eval_const(items.last().expect("tail expression"), ty);
        (
            value,
            elab.reports.iter().map(|r| r.message.clone()).collect(),
        )
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
                    "a constant must have a concrete integer, float, `bool`, `char`, or `ConstSeq` type, found `str`"
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
}
