//! Lowering of the built-in integers `Nat` and `Int` onto the rcgmp dialect.
//!
//! A value of either type is a bare `!rcgmp.mpz` handle: a small integer is an
//! immediate, a large one a reference-counted heap object. The ownership
//! analysis treats both types as reference-counted, so its dups and drops
//! become `rcgmp.retain` and `rcgmp.release`. The core enums' constructors map
//! onto arithmetic: `Nat::Zero` is `0`, `Nat::Succ(n)` is `n + 1`,
//! `Int::OfNat(n)` is `n` itself, and `Int::NegSucc(n)` is `-(n + 1)`, the
//! bitwise complement of `n`.

use reussir_backend::melior::dialect::arith::{self, CmpiPredicate};
use reussir_backend::melior::ir::attribute::{Attribute, IntegerAttribute};
use reussir_backend::melior::ir::{Block, Value, ValueLike};
use reussir_backend::rcgmp::{self, Binary, Predicate, Shift, Unary};
use reussir_core::full::mir::{Expr, ExprKind};
use reussir_core::literal::{self, Integer};
use reussir_core::semi::hir::{ArithOp, CmpOp};
use reussir_core::semi::ty::{BigIntTy, Ty, TyKind};

use super::{Env, Lowerer};
use crate::lower::ty::num_class;
use crate::lower::{LoweringError, Result, err};

impl<'c, 'p, 'tcx> Lowerer<'c, 'p, 'tcx> {
    /// `rcgmp.constant` holding `n`, whose attribute the dialect wants at the
    /// narrowest signless type holding the value in two's complement.
    pub(super) fn bigint_constant<'b>(
        &self,
        block: &'b Block<'c>,
        n: &Integer,
        ty: Ty<'tcx>,
    ) -> Result<Value<'c, 'b>> {
        let text = format!("{n} : i{}", literal::signed_width(n));
        let value = Attribute::parse(self.context, &text).ok_or_else(|| {
            LoweringError(format!("could not build integer constant `{text}`").into())
        })?;
        let ty = self.tys.mlir_ty(ty)?;
        Ok(self.append(block, rcgmp::constant(self.context, value, ty, self.loc())))
    }

    /// A constructor of `Nat` or `Int`, numbered in declaration order (the
    /// frontend checks the enums' shape). The payload is consumed.
    pub(super) fn bigint_ctor<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        e: &Expr<'tcx>,
        variant: usize,
        args: &[Expr<'tcx>],
    ) -> Result<Value<'c, 'b>> {
        let TyKind::BigInt(int) = *e.ty.kind() else {
            return err("built-in integer constructor of a non-integer type");
        };
        let loc = self.loc();
        let payload = match args {
            [] => None,
            [arg] => Some(
                self.expr(block, env, arg)?
                    .ok_or_else(|| LoweringError("integer constructor payload is unit".into()))?,
            ),
            _ => return err("built-in integer constructor with several payloads"),
        };
        match (int, variant, payload) {
            (BigIntTy::Nat, 0, None) => self.bigint_constant(block, &Integer::from(0), e.ty),
            (BigIntTy::Nat, 1, Some(n)) => {
                let one = self.bigint_constant(block, &Integer::from(1), e.ty)?;
                Ok(self.append(block, rcgmp::binary(Binary::Add, n, one, loc)))
            }
            (BigIntTy::Int, 0, Some(n)) => Ok(n),
            (BigIntTy::Int, 1, Some(n)) => Ok(self.append(block, rcgmp::unary(Unary::Not, n, loc))),
            _ => err("malformed built-in integer constructor"),
        }
    }

    /// Compare two integers. The comparison borrows its operands: the
    /// ownership analysis releases each one afterwards when this was its last
    /// use, through the temporaries recorded here.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn bigint_cmp<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        l: &Expr<'tcx>,
        lv: Value<'c, 'b>,
        op: CmpOp,
        r: &Expr<'tcx>,
        rv: Value<'c, 'b>,
    ) -> Value<'c, 'b> {
        for (operand, value) in [(l, lv), (r, rv)] {
            if !matches!(operand.kind, ExprKind::Var(_) | ExprKind::Proj(..)) {
                self.remember_temp(env, operand, value);
            }
        }
        let predicate = match op {
            CmpOp::Eq => Predicate::Eq,
            CmpOp::Ne => Predicate::Ne,
            CmpOp::Lt => Predicate::Slt,
            CmpOp::Le => Predicate::Sle,
            CmpOp::Gt => Predicate::Sgt,
            CmpOp::Ge => Predicate::Sge,
        };
        self.append(
            block,
            rcgmp::cmpi(self.context, predicate, lv, rv, self.loc()),
        )
    }

    /// Integer arithmetic, consuming both operands. Subtraction on `Nat` is
    /// truncated at zero, so the type stays closed under it. Division and
    /// remainder truncate, as they do for fixed-width integers, and a zero
    /// divisor terminates the program. Bitwise operations act on the infinite
    /// two's complement, so `>>` rounds toward negative infinity.
    pub(super) fn bigint_arith<'b>(
        &self,
        block: &'b Block<'c>,
        int: BigIntTy,
        op: ArithOp,
        lv: Value<'c, 'b>,
        rv: Value<'c, 'b>,
    ) -> Result<Value<'c, 'b>> {
        let binary = match (op, int) {
            (ArithOp::Add, _) => Binary::Add,
            (ArithOp::Sub, BigIntTy::Nat) => Binary::Monus,
            (ArithOp::Sub, BigIntTy::Int) => Binary::Sub,
            (ArithOp::Mul, _) => Binary::Mul,
            (ArithOp::Div, _) => Binary::Div,
            (ArithOp::Mod, _) => Binary::Rem,
            (ArithOp::BitAnd, _) => Binary::And,
            (ArithOp::BitOr, _) => Binary::Or,
            (ArithOp::BitXor, _) => Binary::Xor,
            (ArithOp::Shl | ArithOp::Shr, _) => {
                let shift = if matches!(op, ArithOp::Shl) {
                    Shift::Left
                } else {
                    Shift::Right
                };
                let amount = self.bigint_shift_amount(block, rv);
                return Ok(self.append(block, rcgmp::shift(shift, lv, amount, self.loc())));
            }
            (ArithOp::And | ArithOp::Or, _) => {
                return err("logical operator on a built-in integer");
            }
        };
        Ok(self.append(block, rcgmp::binary(binary, lv, rv, self.loc())))
    }

    /// The bit count a shift takes, from the `i64` shift amount: the rcgmp
    /// shifts read it as unsigned, so a negative amount terminates the
    /// program instead.
    fn bigint_shift_amount<'b>(
        &self,
        block: &'b Block<'c>,
        amount: Value<'c, 'b>,
    ) -> Value<'c, 'b> {
        let loc = self.loc();
        let zero = self.append(
            block,
            arith::constant(
                self.context,
                IntegerAttribute::new(amount.r#type(), 0).into(),
                loc,
            ),
        );
        let nonnegative = self.append(
            block,
            arith::cmpi(self.context, CmpiPredicate::Sge, amount, zero, loc),
        );
        self.runtime_assert(block, nonnegative, "shift amount is negative");
        amount
    }

    /// `as` between a built-in integer and another integer type. A fixed-width
    /// source becomes a new integer: sign-extended into `Int` when signed, and
    /// zero-extended otherwise (the checker admits only unsigned sources and
    /// `char` into `Nat`). A fixed-width target takes the low bits of the
    /// integer's two's complement, as a narrowing `as` does, and borrows the
    /// operand. `Nat` into `Int` is the same integer.
    pub(super) fn bigint_cast<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        x: &Expr<'tcx>,
        xv: Value<'c, 'b>,
        to: Ty<'tcx>,
    ) -> Result<Value<'c, 'b>> {
        let loc = self.loc();
        let to_ty = self.tys.mlir_ty(to)?;
        match (*x.ty.kind(), *to.kind()) {
            (TyKind::BigInt(_), TyKind::BigInt(_)) => Ok(xv),
            (TyKind::BigInt(_), _) => {
                if !matches!(x.kind, ExprKind::Var(_) | ExprKind::Proj(..)) {
                    self.remember_temp(env, x, xv);
                }
                Ok(self.append(block, rcgmp::trunci(xv, to_ty, loc)))
            }
            (_, TyKind::BigInt(int)) => {
                let signed = int == BigIntTy::Int && num_class(x.ty)?.signed;
                Ok(self.append(block, rcgmp::extend(xv, signed, to_ty, loc)))
            }
            _ => err("integer cast without a built-in integer"),
        }
    }
}
