//! Tuple composites retain positional fields and structural identity in DWARF.
use reussir_backend::melior::ir::operation::{OperationLike, OperationPrintingFlags};
use reussir_backend::pipeline::{LoweringOptions, run_lowering_pipeline};
use reussir_codegen::lower::{LinkagePolicy, lower_program};
use reussir_core::full::mono::monomorphize;
use reussir_core::{in_arena, semi::elaborate, surface};

#[path = "../src/testing.rs"]
mod testing;

fn lower_debug(source: &str) -> (String, String) {
    let context = testing::context();
    let mut module = in_arena(|tcx| {
        let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
        let parse = reussir_syntax::parse_with_interner(source, interner.clone());
        assert!(parse.ok(), "{:#?}", parse.errors);
        let elab = elaborate(tcx, &surface::program(&parse.root), &interner);
        assert!(!elab.has_errors(), "{:#?}", elab.reports);
        let (full, reports) = monomorphize(&elab.mono_input());
        assert!(reports.is_empty(), "{reports:#?}");
        let mut map = reussir_codegen::source::SourceCache::new();
        map.add_file("tuple_debug.rr", source);
        lower_program(
            &context,
            tcx,
            &full,
            Some(&map),
            Some(parse.resolver()),
            LinkagePolicy::Jit,
            &[],
        )
        .expect("tuple debug lowering succeeds")
    });
    let frontend = module
        .as_operation()
        .to_string_with_flags(OperationPrintingFlags::new().enable_debug_info(true, false))
        .expect("print tuple debug attributes");
    run_lowering_pipeline(
        &context,
        &mut module,
        &LoweringOptions {
            opt: reussir_backend::pipeline::OptLevel::None,
            ..LoweringOptions::default()
        },
    )
    .expect("tuple debug metadata lowers to LLVM");
    let llvm = module.as_operation().to_string();
    (frontend, llvm)
}

#[test]
fn nested_tuples_and_unit_slots_are_distinct_debug_composites() {
    let (frontend, llvm) = lower_debug(
        r#"
        fn read(t: ((), (i64,), bool)) -> i64 { t.1.0 }
        pub fn run(n: i64) -> i64 { read(((), (n,), true)) }
        extern "C" trampoline "tuple_debug_ffi" = run;
    "#,
    );
    for name in ["((), (i64,), bool)", "(i64,)", "()", "f0", "f1", "f2"] {
        assert!(
            frontend.contains(&format!("name : \"{name}\"")),
            "missing {name}: {frontend}"
        );
    }
    assert!(llvm.contains("DW_TAG_class_type"), "{llvm}");
    assert!(
        !llvm.contains("isRecSelf = true"),
        "nested tuples are not recursive: {llvm}"
    );
}

#[test]
fn tuple_inside_a_recursive_managed_record_preserves_debug_recursion() {
    let (frontend, llvm) = lower_debug(
        r#"
        enum List { Nil, Cons((i64, List)) }
        fn read(t: (List, i64)) -> i64 { t.1 }
        pub fn run(n: i64) -> i64 { read((List::Cons{(n, List::Nil)}, n)) }
        extern "C" trampoline "tuple_recursive_debug_ffi" = run;
    "#,
    );
    assert!(frontend.contains("dbg_boxedtype"), "{frontend}");
    assert!(
        llvm.contains("isRecSelf = true"),
        "managed recursion must remain walkable: {llvm}"
    );
}

#[test]
fn erased_unit_parameters_do_not_shift_debug_argument_indices() {
    let (frontend, llvm) = lower_debug(
        r#"
        fn read(u: (), n: i64, v: ()) -> i64 { n }
        pub fn run(n: i64) -> i64 { read((), n, ()) }
        extern "C" trampoline "tuple_unit_debug_ffi" = run;
    "#,
    );
    assert!(frontend.contains("dbg_func_args"), "{frontend}");
    assert!(llvm.contains("arg = 1"), "{llvm}");
    assert!(
        !llvm.contains("arg = 2"),
        "unit must not shift the physical argument: {llvm}"
    );
}
