//! The arms of an `rcgmp.match` for a decision-tree switch on `Nat` or `Int`.
//!
//! Decision trees keep the enum view of the built-in integers: a switch on a
//! `Nat` dispatches `Zero` and `Succ(n)`, one on an `Int` dispatches `OfNat(n)`
//! and `NegSucc(n)`, and literal patterns switch on integer keys. An
//! `rcgmp.match` selects by value instead, with `case k`, `at_least c`,
//! `at_most c` and `default` arms; an `at_least c` or `at_most c` region may
//! take the distance of the value from `c` and a `default` region the value
//! itself, each as an owned reference. The match consumes its operand.
//!
//! [`plan`] translates one switch into such arms. `Zero` is `case 0`,
//! `Succ(n)` is `at_least 1` with distance `n`, `OfNat(n)` is `at_least 0` and
//! `NegSucc(n)` is `at_most -1`, both with distance `n`, and the default of a
//! literal switch is `default`. A switch on the field of `Succ`, `OfNat` or
//! `NegSucc` is merged into the enclosing arms, so `Succ(Succ(n))` is one
//! `at_least 2` arm with distance `n`, and a literal `k` under `Succ` is
//! `case k + 1`. The merge is skipped when a range arm of the inner switch
//! still needs the field itself, which the merged match would have consumed:
//! the field is then the enclosing arm's distance, and the inner switch is a
//! match of its own.
//!
//! Code generation emits the arms; the ownership analysis uses the same plan
//! to decide when the match takes the scrutinee's reference.

use crate::full::mir::{DecisionTree, SwitchCases, tree_uses_var, uses_var};
use crate::full::ownership::subtrees;
use crate::literal::Integer;
use crate::semi::ty::BigIntTy;

/// The values an arm selects, relative to the switched value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArmTest {
    Case(Integer),
    AtLeast(Integer),
    AtMost(Integer),
    Default,
}

/// One arm of the match.
#[derive(Clone, Debug)]
pub struct Arm<'tcx> {
    pub test: ArmTest,
    /// The decision sub-tree the arm runs.
    pub tree: &'tcx DecisionTree<'tcx>,
    /// The occurrences below the switched value whose value a `case` arm
    /// fixes: the fields of the merged constructor levels.
    pub constants: Vec<(Vec<u32>, Integer)>,
    /// The occurrence the region argument holds, when the sub-tree uses it:
    /// the switched value for `default`, the distance for a range arm.
    pub argument: Option<Vec<u32>>,
}

/// The arms of one switch.
#[derive(Clone, Debug)]
pub struct Plan<'tcx> {
    /// In first-match order, together covering the type's values: an arm may
    /// select values an earlier one also selects (the default of a literal
    /// switch under `Succ` is `at_least 1` after the literals' `case` arms),
    /// and `default` comes last.
    pub arms: Vec<Arm<'tcx>>,
    /// Whether a range arm uses the switched value itself, which the match
    /// cannot then consume.
    pub keeps_scrutinee: bool,
}

/// The arms for a switch of `cases` on the `int` value at `path`.
pub fn plan<'tcx>(int: BigIntTy, path: &[u32], cases: &SwitchCases<'tcx>) -> Plan<'tcx> {
    let mut arms = Vec::new();
    match (*cases, int) {
        (SwitchCases::Int { cases, default }, _) => {
            for (key, tree) in cases {
                arms.push(case((*key).clone(), tree));
            }
            arms.push(with_argument(ArmTest::Default, default, path));
        }
        (SwitchCases::Ctor([zero, succ]), BigIntTy::Nat) => {
            arms.push(case(Integer::from(0), zero));
            field(path, succ, &mut arms, |test| match test {
                ArmTest::Case(k) => ArmTest::Case(k + Integer::from(1)),
                ArmTest::AtLeast(c) => ArmTest::AtLeast(c + Integer::from(1)),
                _ => ArmTest::AtLeast(Integer::from(1)),
            });
        }
        (SwitchCases::Ctor([of_nat, neg_succ]), BigIntTy::Int) => {
            field(path, of_nat, &mut arms, |test| match test {
                ArmTest::Case(k) => ArmTest::Case(k),
                ArmTest::AtLeast(c) => ArmTest::AtLeast(c),
                _ => ArmTest::AtLeast(Integer::from(0)),
            });
            // `NegSucc(n)` is `-1 - n`: `n >= c` is the value at most `-1 - c`,
            // and both distances are `n - c`.
            field(path, neg_succ, &mut arms, |test| match test {
                ArmTest::Case(k) => ArmTest::Case(Integer::from(-1) - k),
                ArmTest::AtLeast(c) => ArmTest::AtMost(Integer::from(-1) - c),
                _ => ArmTest::AtMost(Integer::from(-1)),
            });
        }
        _ => unreachable!("a built-in integer switches on keys or on its two constructors"),
    }
    let keeps_scrutinee = arms.iter().any(|arm| {
        matches!(arm.test, ArmTest::AtLeast(_) | ArmTest::AtMost(_)) && uses_path(arm.tree, path)
    });
    Plan {
        arms,
        keeps_scrutinee,
    }
}

/// Add the arms of the constructor field `path ++ [0]`, a `Nat` whose sub-tree
/// is `tree`, with `map` taking each test on the field to the test on the
/// switched value.
fn field<'tcx>(
    path: &[u32],
    tree: &'tcx DecisionTree<'tcx>,
    arms: &mut Vec<Arm<'tcx>>,
    map: impl Fn(ArmTest) -> ArmTest,
) {
    let field: Vec<u32> = path.iter().copied().chain([0]).collect();
    if let DecisionTree::Switch { scrutinee, cases } = *tree
        && scrutinee == field.as_slice()
    {
        let inner = plan(BigIntTy::Nat, &field, &cases);
        if !inner.keeps_scrutinee {
            for arm in inner.arms {
                let mut constants = arm.constants;
                if let ArmTest::Case(k) = &arm.test {
                    constants.push((field.clone(), k.clone()));
                }
                arms.push(Arm {
                    test: map(arm.test),
                    tree: arm.tree,
                    constants,
                    argument: arm.argument,
                });
            }
            return;
        }
    }
    arms.push(with_argument(map(ArmTest::Default), tree, &field));
}

fn case<'tcx>(key: Integer, tree: &'tcx DecisionTree<'tcx>) -> Arm<'tcx> {
    Arm {
        test: ArmTest::Case(key),
        tree,
        constants: Vec::new(),
        argument: None,
    }
}

fn with_argument<'tcx>(test: ArmTest, tree: &'tcx DecisionTree<'tcx>, path: &[u32]) -> Arm<'tcx> {
    Arm {
        test,
        tree,
        constants: Vec::new(),
        argument: uses_path(tree, path).then(|| path.to_vec()),
    }
}

/// Whether `tree` uses the value at `path`: a pattern binding of it that the
/// binding's scope uses, or a switch on it.
pub fn uses_path(tree: &DecisionTree<'_>, path: &[u32]) -> bool {
    match *tree {
        DecisionTree::Leaf { body, bindings } => bindings
            .iter()
            .any(|&(var, at)| at == path && uses_var(body, var)),
        DecisionTree::Guard {
            bindings,
            guard,
            success,
            failure,
        } => {
            bindings.iter().any(|&(var, at)| {
                at == path
                    && (uses_var(guard, var)
                        || tree_uses_var(success, var)
                        || tree_uses_var(failure, var))
            }) || uses_path(success, path)
                || uses_path(failure, path)
        }
        DecisionTree::Switch { scrutinee, cases } => {
            scrutinee == path || subtrees(&cases).iter().any(|t| uses_path(t, path))
        }
        DecisionTree::Uncovered | DecisionTree::Unreachable => false,
    }
}
