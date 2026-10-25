//! Lowering of the built-in integers `Nat` and `Int` onto the rcgmp dialect.
//!
//! A value of either type is a bare `!rcgmp.mpz` handle: a small integer is an
//! immediate, a large one a reference-counted heap object. The ownership
//! analysis treats both types as reference-counted, so its dups and drops
//! become `rcgmp.retain` and `rcgmp.release`. The core enums' constructors map
//! onto arithmetic: `Nat::Zero` is `0`, `Nat::Succ(n)` is `n + 1`,
//! `Int::OfNat(n)` is `n` itself, and `Int::NegSucc(n)` is `-(n + 1)`, the
//! bitwise complement of `n`.
//!
//! A switch on either type is one `rcgmp.match` whose arms are the switch's
//! keys, ranges and default (see the pattern compiler's "Built-in integers").
//! The match consumes the integer and gives a range arm its distance from the
//! bound, and a `default` arm the integer, as an owned region argument; every
//! occurrence derived from the integer (it, and the fields of `Succ`, `OfNat`
//! and `NegSucc`) is a constant in a `case` arm and that argument plus a
//! constant otherwise.
//!
//! Their `Arc` colorings lower the same way onto `!rcgmp.mpz<atomic>`, whose
//! operations count atomically; a match on one derives `Arc<Nat>` values.

use reussir_backend::builders;
use reussir_backend::melior::dialect::arith::{self, CmpiPredicate};
use reussir_backend::melior::ir::attribute::{Attribute, IntegerAttribute};
use reussir_backend::melior::ir::{Block, BlockLike, Region, RegionLike, Type, Value, ValueLike};
use reussir_backend::rcgmp::{self, ArmKind, Binary, Predicate, Shift, Unary};
use reussir_core::full::mir::{self, DecisionTree, Expr, ExprKind};
use reussir_core::full::ownership::subtrees;
use reussir_core::intrinsic::BigIntFn;
use reussir_core::literal::{self, Integer};
use reussir_core::semi::hir::{ArithOp, CmpOp, IntRange, VarId};
use reussir_core::semi::ty::{BigIntTy, Ty, TyKind};

use smallvec::SmallVec;

use super::{Env, Lowerer};
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
        let Some((int, _)) = e.ty.bigint() else {
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
        match (x.ty.bigint(), to.bigint()) {
            (Some(_), Some(_)) => Ok(xv),
            (Some(_), None) => {
                if !matches!(x.kind, ExprKind::Var(_) | ExprKind::Proj(..)) {
                    self.remember_temp(env, x, xv);
                }
                Ok(self.append(block, rcgmp::trunci(xv, to_ty, loc)))
            }
            (None, Some((int, _))) => {
                let signed = int == BigIntTy::Int && num_class(x.ty)?.signed;
                Ok(self.append(block, rcgmp::extend(xv, signed, to_ty, loc)))
            }
            (None, None) => err("integer cast without a built-in integer"),
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

    /// Lower a switch on the `Nat`/`Int` value at `path` to one `rcgmp.match`.
    /// The match consumes its operand: the scrutinee itself when the ownership
    /// analysis gave it to this root switch, else a retained reference, its
    /// owner keeping the value as for a borrowed scrutinee.
    pub(super) fn bigint_switch<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        (root_idx, path): (usize, &[u32]),
        (value, int, atomic): (Value<'c, 'b>, BigIntTy, bool),
        (cases, ranges, default): IntCases<'tcx>,
        result_ty: Ty<'tcx>,
    ) -> Result<Option<Value<'c, 'b>>> {
        let loc = self.loc();
        match env.moved_roots.iter().position(|&r| r == root_idx) {
            Some(i) if path.is_empty() => {
                env.moved_roots.remove(i);
            }
            _ => {
                block.append_operation(rcgmp::retain(value, loc));
            }
        }

        let mut arms: Vec<(ArmTest, &'tcx DecisionTree<'tcx>)> = cases
            .iter()
            .map(|(k, t)| (ArmTest::Case((*k).clone()), t))
            .collect();
        arms.extend(ranges.iter().map(|(r, t)| match r {
            IntRange::AtLeast(c) => (ArmTest::AtLeast((*c).clone()), t),
            IntRange::AtMost(c) => (ArmTest::AtMost((*c).clone()), t),
        }));
        if !matches!(default, DecisionTree::Unreachable | DecisionTree::Uncovered) {
            arms.push((ArmTest::Default, default));
        }

        let mpz = value.r#type();
        let unit = is_unit(result_ty);
        let result_types = if unit {
            Vec::new()
        } else {
            vec![self.tys.mlir_ty(result_ty)?]
        };
        let mut attrs = Vec::with_capacity(arms.len());
        let mut regions = Vec::with_capacity(arms.len());
        for (test, tree) in arms {
            let (kind, key) = match &test {
                ArmTest::Case(k) => (ArmKind::Case, Some(k)),
                ArmTest::AtLeast(c) => (ArmKind::AtLeast, Some(c)),
                ArmTest::AtMost(c) => (ArmKind::AtMost, Some(c)),
                ArmTest::Default => (ArmKind::Default, None),
            };
            attrs.push((kind, key.map(|k| self.narrowest(k)).transpose()?));
            // Only an arm whose sub-tree uses a derived occurrence takes the
            // argument; otherwise the match releases the integer itself.
            let argument = kind != ArmKind::Case && uses_derived(tree, path);
            let arm_block = if argument {
                Block::new(&[(mpz, loc)])
            } else {
                Block::new(&[])
            };
            env.subscope(|scope| {
                let arg = if argument {
                    let val: Value<'c, '_> = arm_block
                        .argument(0)
                        .map_err(|e| LoweringError(format!("missing match argument: {e}").into()))?
                        .into();
                    Some(val)
                } else {
                    None
                };
                scope.int_arms.push(IntArm {
                    root_idx,
                    path: SmallVec::from_slice(path),
                    int,
                    atomic,
                    test,
                    arg,
                    views: Vec::new(),
                });
                let value = self.lower_tree(&arm_block, scope, root_idx, tree, result_ty)?;
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
        let attrs = rcgmp::match_arms(self.context, &attrs);
        let op = block.append_operation(rcgmp::match_(
            self.context,
            value,
            attrs,
            &result_types,
            regions,
            loc,
        ));
        Ok((!unit).then(|| op.result(0).unwrap().into()))
    }

    /// `n` as an integer attribute of the narrowest signless type holding it in
    /// two's complement, the form the dialect's constants and match arms take.
    fn narrowest(&self, n: &Integer) -> Result<Attribute<'c>> {
        let text = format!("{n} : i{}", literal::signed_width(n));
        Attribute::parse(self.context, &text)
            .ok_or_else(|| LoweringError(format!("could not build integer `{text}`").into()))
    }

    /// The innermost arm of the match at `root_idx` that produced the value at
    /// `path`, with how that value follows from the arm.
    pub(super) fn int_arm_value(
        &self,
        env: &Env<'c, '_, 'tcx>,
        root_idx: usize,
        path: &[u32],
    ) -> Option<(usize, Derived)> {
        let i = env.int_arms.iter().rposition(|arm| {
            arm.root_idx == root_idx
                && path.starts_with(&arm.path)
                && path[arm.path.len()..].iter().all(|&f| f == 0)
        })?;
        let arm = &env.int_arms[i];
        Some((i, derived(arm, path.len() - arm.path.len())))
    }

    /// Materialize a value an arm produced (see [`ArmUse`]).
    pub(super) fn bind_int_arm_value<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        (i, derived): (usize, Derived),
        mode: ArmUse,
    ) -> Result<(Value<'c, 'b>, Ty<'tcx>)> {
        let loc = self.loc();
        let arm = &env.int_arms[i];
        let ty = match derived {
            Derived::Whole(_) | Derived::Constant { whole: true, .. } => {
                self.tcx.mk_bigint(arm.int)
            }
            _ => self.tcx.mk_bigint(BigIntTy::Nat),
        };
        // Values derived from an atomically counted integer are counted alike.
        let ty = if arm.atomic { self.tcx.mk_arc(ty) } else { ty };
        let offset = match &derived {
            Derived::Constant { value, .. } => {
                return Ok((self.bigint_constant(block, value, ty)?, ty));
            }
            Derived::Whole(offset) | Derived::Field(offset) => offset.clone(),
        };
        let arg = arm
            .arg
            .ok_or_else(|| LoweringError("match arm value has no argument".into()))?;
        let identity = matches!(&offset, Offset::Plus(c) if *c == 0);
        if mode == ArmUse::Share {
            block.append_operation(rcgmp::retain(arg, loc));
        }
        if identity {
            if mode == ArmUse::Take {
                env.int_arms[i].arg = None;
            }
            return Ok((arg, ty));
        }
        if mode == ArmUse::Read {
            block.append_operation(rcgmp::retain(arg, loc));
        }
        let computed = match &offset {
            Offset::Plus(c) => {
                let c = self.bigint_constant(block, c, ty)?;
                self.append(block, rcgmp::binary(Binary::Add, arg, c, loc))
            }
            Offset::Minus(c) => {
                let c = self.bigint_constant(block, c, ty)?;
                self.append(block, rcgmp::binary(Binary::Sub, c, arg, loc))
            }
        };
        match mode {
            ArmUse::Take => env.int_arms[i].arg = None,
            ArmUse::Read => env.int_arms[i].views.push(computed),
            ArmUse::Share => {}
        }
        Ok((computed, ty))
    }

    /// At a leaf, release what the arms of the match at `root_idx` still own:
    /// arguments no binding took, and values computed for guards.
    pub(super) fn settle_int_arms<'b>(
        &self,
        block: &'b Block<'c>,
        env: &mut Env<'c, 'b, 'tcx>,
        root_idx: usize,
    ) {
        let loc = self.loc();
        for arm in env.int_arms.iter_mut().filter(|a| a.root_idx == root_idx) {
            for view in arm.views.drain(..) {
                block.append_operation(rcgmp::release(view, loc));
            }
            if let Some(arg) = arm.arg.take() {
                block.append_operation(rcgmp::release(arg, loc));
            }
        }
    }
}

/// The cases of a switch on an integer: keys, ordered ranges, and the default.
pub(super) type IntCases<'tcx> = (
    &'tcx [(&'tcx Integer, DecisionTree<'tcx>)],
    &'tcx [(IntRange<'tcx>, DecisionTree<'tcx>)],
    &'tcx DecisionTree<'tcx>,
);

/// How a binding uses a value an arm of a `Nat`/`Int` match produced.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ArmUse {
    /// A leaf binding owns it: the arm's argument moves into it (or into the
    /// arithmetic computing it).
    Take,
    /// A leaf binding owns it while a later binding of the leaf still takes
    /// the argument: it gets a retained reference.
    Share,
    /// A guard reads it: the arm keeps its argument, and a computed value is
    /// kept with the arm and released at each leaf.
    Read,
}

/// The integers an arm of a `Nat`/`Int` match selects.
#[derive(Clone, Debug)]
pub(super) enum ArmTest {
    Case(Integer),
    AtLeast(Integer),
    AtMost(Integer),
    Default,
}

/// One arm of a `Nat`/`Int` match the walk is inside.
#[derive(Clone)]
pub(super) struct IntArm<'c, 'b> {
    root_idx: usize,
    path: SmallVec<[u32; 4]>,
    int: BigIntTy,
    /// Whether the integer is an `Arc` coloring, counted atomically.
    atomic: bool,
    test: ArmTest,
    /// The region argument while the arm owns it: the distance from a range's
    /// bound, or the integer in a `default` arm.
    arg: Option<Value<'c, 'b>>,
    /// Values computed for guard bindings, released at each leaf.
    views: Vec<Value<'c, 'b>>,
}

/// A value derived from the integer an arm switched on.
#[derive(Clone, Debug)]
pub(super) enum Derived {
    /// A constant, the integer itself (`whole`) or a constructor field.
    Constant { value: Integer, whole: bool },
    /// The integer itself.
    Whole(Offset),
    /// The field of a nested constructor.
    Field(Offset),
}

/// How a derived value follows from the arm's argument `d`.
#[derive(Clone, Debug)]
pub(super) enum Offset {
    /// `d + c`, with `c >= 0`.
    Plus(Integer),
    /// `c - d`.
    Minus(Integer),
}

/// The occurrence `depth` constructor fields below the integer `x` an arm
/// switched on: `x - depth` on a `Nat`; on an `Int`, `x` itself at depth 0,
/// else `x - (depth - 1)` for `OfNat` and `-depth - x` for `NegSucc`.
fn derived(arm: &IntArm<'_, '_>, depth: usize) -> Derived {
    let depth = Integer::from(depth as u64);
    let negative = match &arm.test {
        ArmTest::Case(k) => *k < 0,
        ArmTest::AtMost(_) => true,
        ArmTest::AtLeast(_) | ArmTest::Default => false,
    };
    // The value as `x - c`, or as `-c - x` on the negative side.
    let c = match arm.int {
        BigIntTy::Nat => depth.clone(),
        BigIntTy::Int if depth == 0 => Integer::from(0),
        BigIntTy::Int if negative => depth.clone(),
        BigIntTy::Int => depth.clone() - Integer::from(1),
    };
    let flipped = arm.int == BigIntTy::Int && negative && depth != 0;
    let wrap = |o| {
        if depth == 0 {
            Derived::Whole(o)
        } else {
            Derived::Field(o)
        }
    };
    match &arm.test {
        ArmTest::Case(k) => Derived::Constant {
            value: if flipped { -c - k } else { k - c },
            whole: depth == 0,
        },
        // `d = x - l`.
        ArmTest::AtLeast(l) => wrap(Offset::Plus(l - c)),
        ArmTest::Default => wrap(Offset::Plus(-c)),
        // `d = m - x`: `-c - x` is `d - c - m`, and `x` itself is `m - d`.
        ArmTest::AtMost(m) if flipped => wrap(Offset::Plus(-c - m)),
        ArmTest::AtMost(m) => wrap(Offset::Minus(m.clone())),
    }
}

/// Whether a binding below `tree` that its scope uses names the integer at
/// `path` or a constructor field derived from it.
fn uses_derived(tree: &DecisionTree<'_>, path: &[u32]) -> bool {
    let derived = |at: &[u32]| at.starts_with(path) && at[path.len()..].iter().all(|&f| f == 0);
    match *tree {
        DecisionTree::Leaf { body, bindings } => bindings
            .iter()
            .any(|&(var, at): &(VarId, &[u32])| derived(at) && mir::uses_var(body, var)),
        DecisionTree::Guard {
            bindings,
            guard,
            success,
            failure,
        } => {
            bindings.iter().any(|&(var, at): &(VarId, &[u32])| {
                derived(at)
                    && (mir::uses_var(guard, var)
                        || mir::tree_uses_var(success, var)
                        || mir::tree_uses_var(failure, var))
            }) || uses_derived(success, path)
                || uses_derived(failure, path)
        }
        DecisionTree::Switch { cases, .. } => {
            subtrees(&cases).iter().any(|t| uses_derived(t, path))
        }
        DecisionTree::Uncovered | DecisionTree::Unreachable => false,
    }
}
