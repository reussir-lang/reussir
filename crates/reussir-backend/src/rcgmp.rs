//! Builders for the `rcgmp` dialect: reference-counted arbitrary-precision
//! integers, which back Reussir's `Nat` and `Int`.
//!
//! The dialect is optional (the `rcgmp` [feature](crate::features)), and its
//! TableGen sources exist only in builds that fetch it, so melior's
//! [`dialect!`](melior::dialect) macro cannot generate these builders the way it
//! does for the Reussir dialect. Each builder names its operation instead, and
//! [`mpz_type`] reports whether the dialect is present at all.
//!
//! Ownership follows the dialect: consuming operations take one owned
//! reference per operand, while [`cmpi`], [`trunci`], [`trunci_overflow`],
//! [`log2i`] and [`testbiti`] borrow their integer operands and leave their
//! references with the caller.

use melior::Context;
use melior::ir::attribute::{Attribute, IntegerAttribute};
use melior::ir::operation::OperationBuilder;
use melior::ir::r#type::IntegerType;
use melior::ir::{Identifier, Location, Operation, Type, Value, ValueLike};

/// `!rcgmp.mpz`, the nonatomic integer handle, or `None` when this build lacks
/// the dialect.
pub fn mpz_type(context: &Context) -> Option<Type<'_>> {
    Type::parse(context, "!rcgmp.mpz")
}

/// `rcgmp.constant <value> : <ty>`. `value` is an integer attribute of the
/// narrowest signless type holding the constant in two's complement, as the
/// dialect requires.
pub fn constant<'c>(
    context: &'c Context,
    value: Attribute<'c>,
    ty: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("rcgmp.constant", location)
        .add_attributes(&[(Identifier::new(context, "value"), value)])
        .add_results(&[ty])
        .build()
        .expect("valid rcgmp.constant")
}

/// `rcgmp.retain <value>`: add one owned reference to a borrowed integer.
pub fn retain<'c>(value: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("rcgmp.retain", location)
        .add_operands(&[value])
        .build()
        .expect("valid rcgmp.retain")
}

/// `rcgmp.release <value>`: consume one owned reference.
pub fn release<'c>(value: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("rcgmp.release", location)
        .add_operands(&[value])
        .build()
        .expect("valid rcgmp.release")
}

/// A binary operation consuming both operands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Binary {
    Add,
    Sub,
    /// Subtraction clamped at zero.
    Monus,
    Mul,
    /// Truncated quotient; a zero divisor terminates the program.
    Div,
    /// Truncated remainder; a zero divisor terminates the program.
    Rem,
    /// Euclidean quotient and remainder, and the quotient rounded toward
    /// negative infinity with its remainder; a zero divisor terminates the
    /// program.
    EuclidDiv,
    EuclidRem,
    FloorDiv,
    FloorRem,
    /// The greatest common divisor of the magnitudes.
    Gcd,
    /// Bitwise operations, on the infinite two's complement.
    And,
    Or,
    Xor,
}

impl Binary {
    fn name(self) -> &'static str {
        match self {
            Binary::Add => "rcgmp.addi",
            Binary::Sub => "rcgmp.subi",
            Binary::Monus => "rcgmp.monusi",
            Binary::Mul => "rcgmp.muli",
            Binary::Div => "rcgmp.divsi",
            Binary::Rem => "rcgmp.remsi",
            Binary::EuclidDiv => "rcgmp.edivsi",
            Binary::EuclidRem => "rcgmp.emodsi",
            Binary::FloorDiv => "rcgmp.floordivsi",
            Binary::FloorRem => "rcgmp.floormodsi",
            Binary::Gcd => "rcgmp.gcdi",
            Binary::And => "rcgmp.andi",
            Binary::Or => "rcgmp.ori",
            Binary::Xor => "rcgmp.xori",
        }
    }
}

/// `<op> <lhs>, <rhs>`: consume both operands and produce one owned result.
pub fn binary<'c>(
    op: Binary,
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new(op.name(), location)
        .add_operands(&[lhs, rhs])
        .add_results(&[lhs.r#type()])
        .build()
        .unwrap_or_else(|_| panic!("valid {}", op.name()))
}

/// A unary operation consuming its operand.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unary {
    /// Bitwise complement, `-x - 1`.
    Not,
    Neg,
    Abs,
}

/// `<op> <operand>`: consume the operand and produce one owned result.
pub fn unary<'c>(op: Unary, operand: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    let name = match op {
        Unary::Not => "rcgmp.noti",
        Unary::Neg => "rcgmp.negi",
        Unary::Abs => "rcgmp.absi",
    };
    OperationBuilder::new(name, location)
        .add_operands(&[operand])
        .add_results(&[operand.r#type()])
        .build()
        .unwrap_or_else(|_| panic!("valid {name}"))
}

/// `rcgmp.divsi <lhs>, <rhs> exact`: the quotient of an exact division,
/// consuming both operands. `lhs` must be a multiple of `rhs`; a zero divisor
/// terminates the program.
pub fn divsi_exact<'c>(
    context: &'c Context,
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("rcgmp.divsi", location)
        .add_attributes(&[(Identifier::new(context, "exact"), Attribute::unit(context))])
        .add_operands(&[lhs, rhs])
        .add_results(&[lhs.r#type()])
        .build()
        .expect("valid rcgmp.divsi")
}

/// `rcgmp.ipowi <base>, <exponent>`: consume `base` and raise it to the
/// unsigned fixed-width `exponent`.
pub fn ipowi<'c>(
    base: Value<'c, '_>,
    exponent: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("rcgmp.ipowi", location)
        .add_operands(&[base, exponent])
        .add_results(&[base.r#type()])
        .build()
        .expect("valid rcgmp.ipowi")
}

/// `rcgmp.log2i <value> : … to <ty>`: the base-2 logarithm of a borrowed
/// integer's magnitude, rounded down (zero for zero).
pub fn log2i<'c>(value: Value<'c, '_>, ty: Type<'c>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("rcgmp.log2i", location)
        .add_operands(&[value])
        .add_results(&[ty])
        .build()
        .expect("valid rcgmp.log2i")
}

/// `rcgmp.testbiti <value>, <index>`: bit `index` of a borrowed integer's
/// infinite two's complement.
pub fn testbiti<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    index: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("rcgmp.testbiti", location)
        .add_operands(&[value, index])
        .add_results(&[IntegerType::new(context, 1).into()])
        .build()
        .expect("valid rcgmp.testbiti")
}

/// A shift by a fixed-width, unsigned number of bits.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shift {
    Left,
    /// Arithmetic: rounds toward negative infinity.
    Right,
}

/// `<op> <value>, <amount>`: consume `value` and shift it by `amount` bits.
pub fn shift<'c>(
    op: Shift,
    value: Value<'c, '_>,
    amount: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    let name = match op {
        Shift::Left => "rcgmp.shli",
        Shift::Right => "rcgmp.shrsi",
    };
    OperationBuilder::new(name, location)
        .add_operands(&[value, amount])
        .add_results(&[value.r#type()])
        .build()
        .unwrap_or_else(|_| panic!("valid {name}"))
}

/// The predicates of [`cmpi`], the signed predicates of `arith.cmpi`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Predicate {
    Eq = 0,
    Ne = 1,
    Slt = 2,
    Sle = 3,
    Sgt = 4,
    Sge = 5,
}

/// `rcgmp.cmpi <predicate>, <lhs>, <rhs>`: compare two borrowed integers.
pub fn cmpi<'c>(
    context: &'c Context,
    predicate: Predicate,
    lhs: Value<'c, '_>,
    rhs: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    let i64_type = IntegerType::new(context, 64).into();
    OperationBuilder::new("rcgmp.cmpi", location)
        .add_attributes(&[(
            Identifier::new(context, "predicate"),
            IntegerAttribute::new(i64_type, predicate as i64).into(),
        )])
        .add_operands(&[lhs, rhs])
        .add_results(&[IntegerType::new(context, 1).into()])
        .build()
        .expect("valid rcgmp.cmpi")
}

/// `rcgmp.extsi` (`signed`) or `rcgmp.extui`: a new owned integer holding a
/// fixed-width integer read as signed or unsigned.
pub fn extend<'c>(
    value: Value<'c, '_>,
    signed: bool,
    ty: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let name = if signed { "rcgmp.extsi" } else { "rcgmp.extui" };
    OperationBuilder::new(name, location)
        .add_operands(&[value])
        .add_results(&[ty])
        .build()
        .unwrap_or_else(|_| panic!("valid {name}"))
}

/// `rcgmp.trunci <value> : … to <ty>`: the low bits of a borrowed integer's
/// infinite two's complement.
pub fn trunci<'c>(value: Value<'c, '_>, ty: Type<'c>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("rcgmp.trunci", location)
        .add_operands(&[value])
        .add_results(&[ty])
        .build()
        .expect("valid rcgmp.trunci")
}

/// `rcgmp.trunci_overflow <value> [unsigned] : … to <ty>`: the low bits of a
/// borrowed integer, and whether it does not fit `ty` as a signed integer, or
/// as an unsigned one with `unsigned`.
pub fn trunci_overflow<'c>(
    context: &'c Context,
    value: Value<'c, '_>,
    unsigned: bool,
    ty: Type<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    let mut builder = OperationBuilder::new("rcgmp.trunci_overflow", location);
    if unsigned {
        builder = builder.add_attributes(&[(
            Identifier::new(context, "isUnsigned"),
            Attribute::unit(context),
        )]);
    }
    builder
        .add_operands(&[value])
        .add_results(&[ty, IntegerType::new(context, 1).into()])
        .build()
        .expect("valid rcgmp.trunci_overflow")
}
