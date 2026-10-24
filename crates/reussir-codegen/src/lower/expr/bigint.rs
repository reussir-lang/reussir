//! Lowering of the built-in integers `Nat` and `Int` onto the rcgmp dialect.
//!
//! A value of either type is a bare `!rcgmp.mpz` handle: a small integer is an
//! immediate, a large one a reference-counted heap object. The ownership
//! analysis treats both types as reference-counted, so its dups and drops
//! become `rcgmp.retain` and `rcgmp.release`. The core enums' constructors map
//! onto arithmetic: `Nat::Zero` is `0`, `Nat::Succ(n)` is `n + 1`,
//! `Int::OfNat(n)` is `n` itself, and `Int::NegSucc(n)` is `-(n + 1)`, the
//! bitwise complement of `n`. A match on either type is one `rcgmp.match`
//! over the arms of [`bigint_match::plan`].

use reussir_backend::builders;
use reussir_backend::melior::dialect::arith::{self, CmpiPredicate};
use reussir_backend::melior::ir::attribute::{Attribute, IntegerAttribute};
use reussir_backend::melior::ir::{Block, BlockLike, Region, RegionLike, Type, Value, ValueLike};
use reussir_backend::rcgmp::{self, ArmKind, Binary, Predicate, Shift, Unary};
use reussir_core::full::bigint_match::{self, ArmTest};
use reussir_core::full::mir::{self, Expr, ExprKind, SwitchCases};
use reussir_core::intrinsic::BigIntFn;
use reussir_core::literal::{self, Integer};
use reussir_core::semi::hir::{ArithOp, CmpOp, VarId};
use reussir_core::semi::ty::{BigIntTy, Ty, TyKind};
use smallvec::SmallVec;

use super::{Anchor, Cursor, Env, Holder, Lowerer};
use crate::lower::ty::{is_unit, num_class};
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
        let value = self.narrowest(n)?;
        let ty = self.tys.mlir_ty(ty)?;
        Ok(self.append(block, rcgmp::constant(self.context, value, ty, self.loc())))
    }

    /// `n` as an integer attribute of the narrowest signless type holding it in
    /// two's complement, the form the dialect's constants and match arms take.
    fn narrowest(&self, n: &Integer) -> Result<Attribute<'c>> {
        let text = format!("{n} : i{}", literal::signed_width(n));
        Attribute::parse(self.context, &text)
            .ok_or_else(|| LoweringError(format!("could not build integer `{text}`").into()))
    }

    /// Lower a switch on the `Nat`/`Int` value at `path` to one `rcgmp.match`
    /// over the arms of [`bigint_match::plan`]. The match consumes its operand:
    /// a reference this match holds for `path` moves in, unless a range arm
    /// still needs the value; any other value is retained for the match, its
    /// owner keeping it as for a borrowed scrutinee.
    ///
    /// In each arm, the occurrences it fixes resolve to constants: the merged
    /// constructor fields, and in a `case` arm of a match that took its operand
    /// the switched value itself. An arm argument (a distance, or the switched
    /// value in a `default` arm of a match that took it) is a [`Holder`].
    #[allow(clippy::too_many_arguments)]
    pub(super) fn bigint_switch<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        root_idx: usize,
        path: &[u32],
        cursor: Cursor<'c, 'b, 'tcx>,
        int: BigIntTy,
        cases: SwitchCases<'tcx>,
        result_ty: Ty<'tcx>,
    ) -> Result<Option<Value<'c, 'b>>> {
        let loc = self.loc();
        let plan = bigint_match::plan(int, path, &cases);
        let scrut_ty = cursor.ty();
        let value = self.load_cursor(block, cursor)?;
        let held = env
            .holders
            .iter()
            .rposition(|h| h.root_idx == root_idx && !h.settled && h.path[..] == *path);
        let moved = match held {
            Some(i) if !plan.keeps_scrutinee => {
                env.holders[i].settled = true;
                true
            }
            _ => {
                block.append_operation(rcgmp::retain(value, loc));
                false
            }
        };

        let mpz = self.tys.mlir_ty(scrut_ty)?;
        let nat = self.tcx.mk_bigint(BigIntTy::Nat);
        let unit = is_unit(result_ty);
        let result_types = if unit {
            Vec::new()
        } else {
            vec![self.tys.mlir_ty(result_ty)?]
        };
        let mut arms = Vec::with_capacity(plan.arms.len());
        let mut regions = Vec::with_capacity(plan.arms.len());
        for arm in &plan.arms {
            let (kind, key) = match &arm.test {
                ArmTest::Case(k) => (ArmKind::Case, Some(k)),
                ArmTest::AtLeast(c) => (ArmKind::AtLeast, Some(c)),
                ArmTest::AtMost(c) => (ArmKind::AtMost, Some(c)),
                ArmTest::Default => (ArmKind::Default, None),
            };
            arms.push((kind, key.map(|k| self.narrowest(k)).transpose()?));
            let argument = arm
                .argument
                .as_ref()
                .filter(|_| moved || kind != ArmKind::Default);
            let arm_block = if argument.is_some() {
                Block::new(&[(mpz, loc)])
            } else {
                Block::new(&[])
            };
            env.subscope(|scope| {
                let own = key
                    .filter(|_| moved && kind == ArmKind::Case)
                    .map(|k| (path.to_vec(), k.clone(), scrut_ty));
                let fixed = arm
                    .constants
                    .iter()
                    .map(|(at, k)| (at.clone(), k.clone(), nat));
                let used = fixed
                    .chain(own)
                    .filter(|(at, ..)| bigint_match::uses_path(arm.tree, at));
                for (at, k, ty) in used {
                    let val = self.bigint_constant(&arm_block, &k, ty)?;
                    scope
                        .anchors
                        .push((SmallVec::from_vec(at), Anchor::Root { val, ty }));
                }
                if let Some(at) = argument {
                    let val: Value<'c, '_> = arm_block
                        .argument(0)
                        .map_err(|e| LoweringError(format!("missing match argument: {e}").into()))?
                        .into();
                    let ty = if at[..] == *path { scrut_ty } else { nat };
                    let at = SmallVec::from_slice(at);
                    scope.anchors.push((at.clone(), Anchor::Root { val, ty }));
                    scope.holders.push(Holder {
                        root_idx,
                        path: at,
                        val,
                        settled: false,
                    });
                }
                let value = self.lower_tree(&arm_block, scope, root_idx, arm.tree, result_ty)?;
                if !unit && value.is_none() {
                    return err("match arm produced no value");
                }
                arm_block.append_operation(rcgmp::yield_(value.as_slice(), loc));
                Ok(())
            })?;
            let region = Region::new();
            region.append_block(arm_block);
            regions.push(region);
        }
        let arms = rcgmp::match_arms(self.context, &arms);
        let op = block.append_operation(rcgmp::match_(
            self.context,
            value,
            arms,
            &result_types,
            regions,
            loc,
        ));
        Ok((!unit).then(|| op.result(0).unwrap().into()))
    }

    /// At a leaf, settle the references its match's arms still hold: one bound
    /// to a variable the body uses becomes that variable's value (a move, as
    /// the ownership analysis treats it); any other is released here.
    pub(super) fn settle_holders<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        root_idx: usize,
        body: &Expr<'tcx>,
        bindings: &[(VarId, &[u32])],
    ) {
        let loc = self.loc();
        for holder in env.holders.iter_mut() {
            if holder.root_idx != root_idx || holder.settled {
                continue;
            }
            holder.settled = true;
            let taken = bindings
                .iter()
                .any(|&(var, at)| at == &holder.path[..] && mir::uses_var(body, var));
            if !taken {
                block.append_operation(rcgmp::release(holder.val, loc));
            }
        }
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

    /// A `core::intrinsic::bigint` call. The borrowing operations leave their
    /// integer operand with the caller, which releases it afterwards when
    /// this was its last use, through the temporary recorded here; the others
    /// consume their integer operands.
    pub(super) fn lower_bigint_intrinsic<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        e: &Expr<'tcx>,
        func: BigIntFn,
        args: &[Expr<'tcx>],
    ) -> Result<Value<'c, 'b>> {
        let mut operands = Vec::with_capacity(args.len());
        for arg in args {
            operands.push(
                self.expr(block, env, arg)?
                    .ok_or_else(|| LoweringError("bigint intrinsic operand is unit".into()))?,
            );
        }
        if func.borrows_operand() && !matches!(args[0].kind, ExprKind::Var(_) | ExprKind::Proj(..))
        {
            self.remember_temp(env, &args[0], operands[0]);
        }
        let loc = self.loc();
        let binary = |op| Ok(self.append(block, rcgmp::binary(op, operands[0], operands[1], loc)));
        match func {
            BigIntFn::Gcdi => binary(Binary::Gcd),
            BigIntFn::Edivsi => binary(Binary::EuclidDiv),
            BigIntFn::Emodsi => binary(Binary::EuclidRem),
            BigIntFn::Floordivsi => binary(Binary::FloorDiv),
            BigIntFn::Floormodsi => binary(Binary::FloorRem),
            BigIntFn::DivsiExact => Ok(self.append(
                block,
                rcgmp::divsi_exact(self.context, operands[0], operands[1], loc),
            )),
            BigIntFn::Ipowi => Ok(self.append(block, rcgmp::ipowi(operands[0], operands[1], loc))),
            BigIntFn::Absi => Ok(self.append(block, rcgmp::unary(Unary::Abs, operands[0], loc))),
            BigIntFn::Log2i => {
                let ty = self.tys.mlir_ty(e.ty)?;
                Ok(self.append(block, rcgmp::log2i(operands[0], ty, loc)))
            }
            BigIntFn::Testbiti => Ok(self.append(
                block,
                rcgmp::testbiti(self.context, operands[0], operands[1], loc),
            )),
            BigIntFn::Sitofp => {
                let ty = self.tys.mlir_ty(e.ty)?;
                Ok(self.append(block, rcgmp::sitofp(operands[0], ty, loc)))
            }
            BigIntFn::Fptosi => {
                let ty = self.tys.mlir_ty(e.ty)?;
                Ok(self.append(block, rcgmp::fptosi(operands[0], ty, loc)))
            }
            BigIntFn::LimbCount => {
                let count = self.append(block, rcgmp::limb_count(self.context, operands[0], loc));
                let ty = self.tys.mlir_ty(e.ty)?;
                Ok(self.append(block, arith::index_castui(count, ty, loc)))
            }
            // A limb is as wide as a pointer, which the rcgmp runtime (built
            // only for the 64-bit hosts) and the `u64` result both assume;
            // the dialect's verifier rejects any other target.
            BigIntFn::Limb => {
                let index = self.append(
                    block,
                    arith::index_castui(operands[1], Type::index(self.context), loc),
                );
                let ty = self.tys.mlir_ty(e.ty)?;
                Ok(self.append(block, rcgmp::limb(operands[0], index, ty, loc)))
            }
            BigIntFn::TrunciOverflow => {
                let TyKind::Tuple([fixed, _]) = e.ty.kind() else {
                    return err("`trunci_overflow` result is not a pair");
                };
                let unsigned = !num_class(*fixed)?.signed;
                let op = block.append_operation(rcgmp::trunci_overflow(
                    self.context,
                    operands[0],
                    unsigned,
                    self.tys.mlir_ty(*fixed)?,
                    loc,
                ));
                let (value, overflow) = (
                    op.result(0).expect("truncated value").into(),
                    op.result(1).expect("overflow flag").into(),
                );
                Ok(self.append(
                    block,
                    builders::record_compound(&[value, overflow], self.tys.mlir_ty(e.ty)?, loc),
                ))
            }
        }
    }
}
