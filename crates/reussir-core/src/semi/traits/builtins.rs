//! The built-in ("lang item") traits and impls.
//!
//! These replace the formerly hard-coded primitive class hierarchy. Membership
//! now lives in ordinary [`TraitDef`]/[`ImplDef`] data resolved by the same
//! engine as any user trait — there is no special-cased DAG, and the table
//! below is a *declaration*: [`reussir_trait_dsl::builtin_traits!`] expands it
//! into the `Builtins` struct and its `register` function, assigning dense
//! `TraitId`/`ImplId`s in declaration order. The hierarchy is:
//!
//! ```text
//! Num            PtrLike      Sync      AddrSpace (impl: Generic)
//! ├── Integral   (impl: i8..=u64)
//! └── FloatingPoint (impl: f16..f8)
//! ```
//!
//! Each operator but the shifts has its own class, whose method the
//! operator is sugar for: `Num` is a subclass of `Add Sub Mul Div Rem`,
//! `Integral` also of `BitAnd BitOr BitXor`, and `bool` is the one member of
//! `Not`. `Neg` is apart from `Num`, which has members without negatives.
//!
//! [`TraitDef`]: super::TraitDef
//! [`ImplDef`]: super::ImplDef

use reussir_trait_dsl::builtin_traits;

builtin_traits! {
    /// The operands of `+`.
    #[sealed]
    trait Add {
        fn add(self: Self, other: Self) -> Self;
    }
    /// The operands of binary `-`.
    #[sealed]
    trait Sub {
        fn sub(self: Self, other: Self) -> Self;
    }
    /// The operands of `*`.
    #[sealed]
    trait Mul {
        fn mul(self: Self, other: Self) -> Self;
    }
    /// The operands of `/`.
    #[sealed]
    trait Div {
        fn div(self: Self, other: Self) -> Self;
    }
    /// The operands of `%`.
    #[sealed]
    trait Rem {
        fn rem(self: Self, other: Self) -> Self;
    }
    /// The operands of unary `-`.
    #[sealed]
    trait Neg {
        fn neg(self: Self) -> Self;
    }
    /// The operands of `&`.
    #[sealed]
    trait BitAnd {
        fn bitand(self: Self, other: Self) -> Self;
    }
    /// The operands of `|`.
    #[sealed]
    trait BitOr {
        fn bitor(self: Self, other: Self) -> Self;
    }
    /// The operands of `^`.
    #[sealed]
    trait BitXor {
        fn bitxor(self: Self, other: Self) -> Self;
    }
    /// The operands of `!`.
    #[sealed]
    trait Not {
        fn not(self: Self) -> Self;
    }
    /// The root numeric class: arithmetic and numeric literals.
    #[sealed]
    trait Num: Add + Sub + Mul + Div + Rem;
    /// Whole-number types; `Num` through the super-trait edge. Shifts ask
    /// for this class directly.
    #[sealed]
    trait Integral: Num + BitAnd + BitOr + BitXor;
    /// Floating-point types; `Num` through the super-trait edge.
    #[sealed]
    trait FloatingPoint: Num;
    /// Pointer-shaped values (nullable-niche eligible).
    #[sealed]
    trait PtrLike;
    /// Marker (auto) trait: a value may be reached from any thread,
    /// concurrently. It subsumes both of Rust's `Send` and `Sync` —
    /// duplicable rc handles make transfer-without-sharing unprovable, so
    /// the two predicates collapse (see *agentic-note*: design-thread-safety).
    /// Declared for name resolution and bound syntax only: ground `Sync`
    /// goals are answered *structurally* by [`crate::semi::traits::sync`],
    /// never by impl search, so it has (and can have) no impls.
    #[sealed]
    #[structural]
    trait Sync;
    /// The address spaces an opaque pointer may point into.
    #[sealed]
    trait AddrSpace;

    impl Integral for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128;
    impl FloatingPoint for f16 | f32 | f64 | bf16 | f8;
    impl AddrSpace for Generic;

    // A method call needs an impl of the method's own trait: the edges from
    // `Num` and `Integral` satisfy bounds only.
    impl Add for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl Sub for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl Mul for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl Div for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl Rem for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl Neg for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128
        | f16 | f32 | f64 | bf16 | f8;
    impl BitAnd for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128;
    impl BitOr for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128;
    impl BitXor for i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64 | i128 | u128;
    impl Not for bool;
}

#[cfg(test)]
mod tests {
    use super::Builtins;
    use crate::semi::traits::{TraitDb, TraitId};
    use crate::with_tcx;

    /// Dense declaration-ordered trait ids, the scalar impls, all sealed,
    /// `Sync` impl-less.
    #[test]
    fn generated_table_matches_the_handwritten_contract() {
        with_tcx(|tcx| {
            let mut db = TraitDb::new();
            let mut defs = crate::semi::resolve::DefTable::new();
            let mut interner = lasso::Rodeo::<reussir_syntax::kind::TokenKey>::new();
            let b = Builtins::register(&mut db, &mut defs, &mut interner, tcx);
            assert_eq!(
                [
                    b.num,
                    b.integral,
                    b.floating_point,
                    b.ptr_like,
                    b.sync,
                    b.addr_space
                ],
                [
                    TraitId(10),
                    TraitId(11),
                    TraitId(12),
                    TraitId(13),
                    TraitId(14),
                    TraitId(15)
                ]
            );
            assert_eq!(Builtins::IDS.addr_space, b.addr_space);
            assert_eq!(db.traits_len(), 16);
            assert_eq!(
                db.impls_len(),
                137,
                "10 integral + 5 floating impls + 1 address space, 6 arithmetic \
                 traits over the 15 numbers, 3 bitwise over the 10 integers, and \
                 `bool : Not`"
            );
            for i in 0..16u32 {
                assert!(db.trait_def(TraitId(i)).sealed);
            }
            assert!(
                db.impls().all(|imp| imp.trait_ref.trait_id != b.sync),
                "Sync is structural; it must have no impls"
            );
            let supers = |t: TraitId| -> Vec<TraitId> {
                db.trait_def(t)
                    .supertraits
                    .iter()
                    .map(|s| s.trait_id)
                    .collect()
            };
            assert_eq!(supers(b.num), [b.add, b.sub, b.mul, b.div, b.rem]);
            assert_eq!(supers(b.integral), [b.num, b.bit_and, b.bit_or, b.bit_xor]);
            assert_eq!(supers(b.floating_point), [b.num]);
            assert!(supers(b.add).is_empty() && supers(b.sync).is_empty());
            // Each operator trait declares the method its operator is sugar
            // for; the binary ones take the other operand.
            let params = |t: TraitId| -> Vec<usize> {
                let methods = &db.trait_def(t).methods;
                methods.iter().map(|m| m.params.len()).collect()
            };
            assert_eq!(params(b.add), [2]);
            assert_eq!(params(b.neg), [1]);
            assert!(params(b.num).is_empty());
        });
    }
}
