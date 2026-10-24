//! `Nat` and `Int` end to end through the JIT, under both of its code
//! generators: values outgrow the immediate range into heap integers, so the
//! compiled code calls the rcgmp runtime the JIT links in.

use std::sync::Arc;

use reussir_backend::features;
use reussir_backend::pipeline::{LoweringOptions, OptLevel, run_lowering_pipeline};
use reussir_codegen::lower::{LinkagePolicy, lower_program};
use reussir_core::full::mono::monomorphize;
use reussir_core::semi::ctxt::PackageFile;
use reussir_core::semi::elaborate_package;
use reussir_core::{in_arena, surface};
use reussir_jit::OrcJit;
use reussir_syntax::Interner;
use reussir_syntax::source::FileId;

#[path = "../src/testing.rs"]
mod testing;

/// Compile `source` as a module of a package carrying `core`'s numeric and
/// comparison modules, which declare `Nat` and `Int`, and JIT it under every
/// available code generator.
fn jit_run_each(source: &str, run: impl Fn(&OrcJit)) {
    if !features::enabled("rcgmp") {
        return;
    }
    let mut levels = vec![OptLevel::Default];
    if features::enabled("tpde") {
        levels.push(OptLevel::Tpde);
    }
    let files: [(&[&str], &str); 4] = [
        (&[], "pub mod cmp;\npub mod num;\npub mod t;"),
        (&["cmp"], include_str!("../../../library/core/src/cmp.rr")),
        (&["num"], include_str!("../../../library/core/src/num.rr")),
        (&["t"], source),
    ];
    for level in levels {
        let context = testing::context();
        let mut module = in_arena(|tcx| {
            let interner = Arc::new(reussir_syntax::new_threaded_interner());
            let mut keys = interner.clone();
            let package = Interner::get_or_intern(&mut keys, "core");
            let parses: Vec<_> = files
                .iter()
                .map(|(_, src)| reussir_syntax::parse_with_interner(src, interner.clone()))
                .collect();
            for parse in &parses {
                assert!(parse.ok(), "parse errors: {:#?}", parse.errors);
            }
            let programs: Vec<_> = parses.iter().map(|p| surface::program(&p.root)).collect();
            let package_files: Vec<_> = files
                .iter()
                .zip(&programs)
                .enumerate()
                .map(|(index, ((path, _), program))| PackageFile {
                    file: FileId::from_index(index as u32),
                    module: std::iter::once(package)
                        .chain(path.iter().map(|s| Interner::get_or_intern(&mut keys, s)))
                        .collect(),
                    program,
                })
                .collect();
            let elab = elaborate_package(tcx, &package_files, &interner);
            assert!(!elab.has_errors(), "{:#?}", elab.reports);
            let (full, reports) = monomorphize(&elab.mono_input());
            assert!(reports.is_empty(), "{reports:#?}");
            lower_program(&context, tcx, &full, None, None, LinkagePolicy::Jit, &[])
                .expect("lowering succeeds")
        });
        let options = LoweringOptions {
            opt: level,
            ..LoweringOptions::default()
        };
        run_lowering_pipeline(&context, &mut module, &options).expect("pipeline lowers to LLVM");
        let jit = OrcJit::with_runtime().expect("create JIT");
        jit.add_module(&module, level).expect("add lowered module");
        run(&jit);
    }
}

fn entry(jit: &OrcJit, name: &str) -> extern "C" fn(i64) -> i64 {
    let address = jit
        .lookup(name)
        .unwrap_or_else(|e| panic!("lookup {name}: {e}"));
    unsafe { std::mem::transmute(address as usize) }
}

#[test]
fn constructors_and_comparisons_cross_the_immediate_range() {
    // 2^61 - 1 is the largest immediate on a 64-bit target.
    let source = r#"
        fn succ(n: Nat) -> Nat { Nat::Succ{n} }
        fn neg_succ(n: Nat) -> Int { Int::NegSucc{n} }
        fn max(a: Nat, b: Nat) -> Nat { if a < b { b } else { a } }
        fn holds(case: i64) -> bool {
            match case {
                0 => succ(2305843009213693951) == 2305843009213693952,
                1 => neg_succ(2305843009213693952) == -2305843009213693953,
                2 => {
                    let big = max(3, succ(18446744073709551615));
                    big == 18446744073709551616 && big > 3 && big != Nat::Zero
                },
                _ => Int::OfNat{succ(0)} > neg_succ(0)
            }
        }
        pub fn check(case: i64) -> i64 { if holds(case) { 1 } else { 0 } }
        extern "C" trampoline "check" = check;
    "#;
    jit_run_each(source, |jit| {
        let check = entry(jit, "check");
        for case in 0..4 {
            assert_eq!(check(case), 1, "case {case}");
        }
    });
}

#[test]
fn matches_select_by_value_on_both_sides_of_the_immediate_range() {
    // One `rcgmp.match` per switch: `case` arms for literals and `Zero`,
    // `at_least` for `Succ` chains and `OfNat`, `at_most` for `NegSucc`.
    let source = r#"
        fn succ(n: Nat) -> Nat { Nat::Succ{n} }
        fn depth(n: Nat) -> i64 {
            match n {
                0 => 0,
                Nat::Succ(Nat::Succ(m)) => if m == 18446744073709551615 { 3 } else { 2 },
                _ => 1
            }
        }
        fn sign(i: Int) -> i64 {
            match i {
                Int::OfNat(Nat::Zero) => 0,
                Int::OfNat(_) => 1,
                Int::NegSucc(k) => if k > 2305843009213693951 { -2 } else { -1 }
            }
        }
        fn holds(case: i64) -> bool {
            match case {
                0 => depth(0) == 0 && depth(1) == 1 && depth(5) == 2,
                1 => depth(succ(succ(18446744073709551615))) == 3,
                2 => sign(0) == 0 && sign(7) == 1 && sign(-7) == -1,
                _ => sign(Int::NegSucc{succ(2305843009213693951)}) == -2
            }
        }
        pub fn check(case: i64) -> i64 { if holds(case) { 1 } else { 0 } }
        extern "C" trampoline "check" = check;
    "#;
    jit_run_each(source, |jit| {
        let check = entry(jit, "check");
        for case in 0..4 {
            assert_eq!(check(case), 1, "case {case}");
        }
    });
}
