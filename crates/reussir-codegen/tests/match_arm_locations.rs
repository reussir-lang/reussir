//! The ops in each region of a `match` dispatch have the location of the arm
//! of that region, not the location of the full `match`.
use reussir_backend::melior::ir::operation::{OperationLike, OperationRef, WalkOrder, WalkResult};
use reussir_backend::melior::ir::{BlockLike, RegionLike};
use reussir_codegen::lower::{LinkagePolicy, lower_program};
use reussir_core::full::mono::monomorphize;
use reussir_core::{in_arena, semi::elaborate, surface};

#[path = "../src/testing.rs"]
mod testing;

const SOURCE: &str = "\
pub enum Shape { Dot, Line(i64) }
pub fn size(s: Shape) -> i64 {
    match s {
        Shape::Dot => 0,
        Shape::Line(n) => n + 1
    }
}
pub struct Cell { value: i64 }
pub fn read(c: Nullable<Cell>) -> i64 {
    match c {
        Nullable::NonNull(r) => r.value,
        Nullable::Null => -1
    }
}
pub fn digit(n: i64) -> i64 {
    match n {
        0 => 10,
        1 => 20,
        _ => 30
    }
}
";

/// The start line of a printed `FileLineColRange` location.
fn start_line(location: &str) -> Option<u32> {
    let (_, rest) = location.split_once("\":")?;
    rest.split(':').next()?.parse().ok()
}

/// The start lines of the located ops in each region of the one op `name`.
fn region_lines(module: OperationRef, name: &str) -> Vec<Vec<u32>> {
    let mut found = Vec::new();
    module.walk(WalkOrder::PreOrder, |op| {
        if op.name().as_string_ref().as_str() == Ok(name) {
            found.push(
                op.regions()
                    .map(|region| {
                        let mut lines = Vec::new();
                        let mut next = region.first_block().and_then(|b| b.first_operation());
                        while let Some(inner) = next {
                            inner.walk(WalkOrder::PreOrder, |nested| {
                                lines.extend(start_line(&nested.location().to_string()));
                                WalkResult::Advance
                            });
                            next = inner.next_in_block();
                        }
                        lines
                    })
                    .collect::<Vec<_>>(),
            );
        }
        WalkResult::Advance
    });
    assert_eq!(found.len(), 1, "expected one `{name}`");
    found.pop().unwrap()
}

#[test]
fn dispatch_regions_have_the_lines_of_their_arms() {
    let context = testing::context();
    let module = in_arena(|tcx| {
        let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
        let parse = reussir_syntax::parse_with_interner(SOURCE, interner.clone());
        assert!(parse.ok(), "{:#?}", parse.errors);
        let elab = elaborate(tcx, &surface::program(&parse.root), &interner);
        assert!(!elab.has_errors(), "{:#?}", elab.reports);
        let (full, reports) = monomorphize(&elab.mono_input());
        assert!(reports.is_empty(), "{reports:#?}");
        let mut map = reussir_codegen::source::SourceCache::new();
        map.add_file("arms.rr", SOURCE);
        lower_program(
            &context,
            tcx,
            &full,
            Some(&map),
            None,
            LinkagePolicy::Jit,
            &[],
        )
        .expect("match lowering succeeds")
    });
    // `scf.index_switch` puts its default region first.
    for (name, arm_lines) in [
        ("reussir.record.dispatch", &[4, 5][..]),
        ("reussir.nullable.dispatch", &[11, 12]),
        ("scf.index_switch", &[19, 17, 18]),
    ] {
        let regions = region_lines(module.as_operation(), name);
        assert_eq!(regions.len(), arm_lines.len(), "{name}");
        for (lines, arm_line) in regions.iter().zip(arm_lines) {
            assert!(
                !lines.is_empty() && lines.iter().all(|line| line == arm_line),
                "{name}: a region of the arm on line {arm_line} has ops on lines {lines:?}"
            );
        }
    }
}
