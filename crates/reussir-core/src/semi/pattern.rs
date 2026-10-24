//! Pattern-match elaboration into decision trees.
//!
//! This compiles a `match` into a [`DecisionTree`] using the clause-matrix
//! algorithm of Maranget, *"Compiling Pattern Matching to Good Decision Trees"*
//! (ML'08). We are free to choose the algorithm here: the decision tree is the
//! frontend's own IR (it drives later code generation), so it need not mirror
//! anything in the backend. A real matrix compiler buys us nested patterns,
//! guards, and exhaustiveness/redundancy checking essentially for free.
//!
//! ## The shape of the algorithm
//!
//! We work over a *clause matrix*. Each **column** is an *occurrence* — a path
//! ([`PatVarRef`]) into the scrutinee value (the root is the empty path, a
//! constructor field `i` extends its parent's path with `i`) together with the
//! type of the value found there. Each **row** is one match arm: a vector of
//! patterns (one per column), the bindings accumulated so far (variable →
//! occurrence), an optional guard, the (already type-checked) body, and the
//! original arm index (for redundancy reporting).
//!
//! `Elaborator::compile_match` then repeats:
//!
//! 1. **Empty matrix** ⇒ nothing matches here: emit [`DecisionTree::Uncovered`]
//!    and flag the match as non-exhaustive.
//! 2. **First row has no constructor tests left** ⇒ it matches unconditionally
//!    (modulo its guard). Emit a [`DecisionTree::Leaf`]; if it has a guard, emit
//!    a [`DecisionTree::Guard`] whose failure branch retries the *remaining*
//!    rows on the same value.
//! 3. **Otherwise** pick a column that still has a constructor and emit a
//!    [`DecisionTree::Switch`] on its occurrence. For each head constructor `c`
//!    we recurse on the *specialized* matrix `S(c, P)` (which expands `c`'s
//!    fields into fresh columns); for open families (ints, strings) we also
//!    recurse on the *default* matrix `D(P)` (the rows that ignore the column).
//!
//! Column choice only affects the *size* of the tree, never correctness; we use
//! the simple "leftmost column that has a constructor" heuristic and leave
//! Maranget's necessity-based heuristics as a future refinement.
//!
//! ## Built-in integers
//!
//! The constructors of `Nat { Zero, Succ(Nat) }` and `Int { OfNat(Nat),
//! NegSucc(Nat) }` are views of one integer, so a `Nat` or `Int` column is not
//! switched constructor by constructor. Each of its patterns denotes an
//! interval: `Zero` and a literal `k` the single value, `Succ^k(p)` the values
//! of `p` shifted by `k` (a binder or wildcard: all from `k` up), `OfNat(p)`
//! the values of `p`, and `NegSucc(p)` their images under `n ↦ -1 - n`. A
//! binder inside binds the value at that depth, `x - k` for `Succ^k(n)` on a
//! `Nat`; its occurrence extends the column's with one `0` per constructor.
//!
//! The column's domain is split at every interval endpoint into segments that
//! no pattern splits, as rustc's pattern analysis splits integer ranges, and
//! each segment specializes the matrix to the rows covering it. Adjacent
//! segments with the same rows merge, each distinct row set is compiled once,
//! and the segments become the arms of one [`SwitchCases::Int`]: single values
//! as keys, nonnegative runs as `at_least` ranges from the highest down,
//! negative runs as `at_most` ranges from the lowest up, so that the arms
//! before a range have taken every value beyond it. Runs of one row set that
//! binds no field and recurs across segments stay for the `default` arm when
//! no range arm would take them first.
//!
//! ## Bindings
//!
//! A variable pattern matches anything but records `(var, occurrence)`. When a
//! column is consumed (specialized or dropped), any binder sitting in it
//! contributes its `(var, occurrence)` to the row; when a row finally becomes a
//! leaf, the remaining binders in its columns are added too. The leaf's
//! `bindings` therefore tell code generation where in the scrutinee to read each
//! bound variable.

use crate::literal::Integer;
use crate::semi::infer::Instantiation;
use crate::semi::ty::{BigIntTy, Ty, TyKind};
use crate::surface::{self, Const, PatternKind, Span};
use crate::utils::string::StringToken;

use super::ctxt::{Elaborator, RecordFields};
use super::hir::{DecisionTree, Expr, ExprKind, IntRange, PatVarRef, SwitchCases, VarId};

/// A head constructor: what a (refutable) pattern tests for at an occurrence.
#[derive(Clone, PartialEq, Eq, Debug)]
enum Ctor<'tcx> {
    Product,
    /// An enum variant, by index into the enum's variant list.
    Variant(usize),
    /// An integer key, arbitrary-precision (the scrutinee type bounds its
    /// range, checked like any literal at monomorphization).
    Int(&'tcx Integer),
    Char(u32),
    Bool(bool),
    Str(StringToken),
    /// The non-null case of a `Nullable<T>` scrutinee, with one field: the
    /// unwrapped `T`.
    NonNull,
    /// The null case of a `Nullable<T>` scrutinee (no fields).
    Null,
}

impl Ctor<'_> {
    fn family(&self) -> Family {
        match self {
            Ctor::Variant(_) => Family::Variant,
            Ctor::Product => Family::Product,
            Ctor::Int(_) => Family::Int,
            Ctor::Char(_) => Family::Char,
            Ctor::Bool(_) => Family::Bool,
            Ctor::Str(_) => Family::Str,
            Ctor::NonNull | Ctor::Null => Family::Nullable,
        }
    }
}

/// The "signature" a column's constructors belong to. A column is well-typed, so
/// every constructor in it shares one family; the family decides how the
/// [`SwitchCases`] are shaped and whether a default branch is needed.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Family {
    Product,
    /// A closed family with one case per enum variant — exhaustive on its own.
    Variant,
    /// A closed two-case family (`true` / `false`).
    Bool,
    /// A closed two-case family (`NonNull` / `Null`).
    Nullable,
    /// Open families: there are always more values than literals, so they need a
    /// default branch.
    Int,
    Char,
    Str,
}

/// A type-checked pattern. Variable bindings carry the [`VarId`] allocated for
/// them during checking; their occurrence is filled in while compiling.
#[derive(Clone, Debug)]
enum Pat<'tcx> {
    /// Matches anything, binds nothing (`_`).
    Wild,
    /// Matches anything, binds the value at this occurrence to `VarId`.
    Bind(VarId),
    /// Matches `ctor` and recurses into its fields.
    Ctor {
        ctor: Ctor<'tcx>,
        fields: Vec<Pat<'tcx>>,
    },
}

impl Pat<'_> {
    fn is_ctor(&self) -> bool {
        matches!(self, Pat::Ctor { .. })
    }
}

/// One column of the clause matrix: where the value lives and what its type is.
#[derive(Clone)]
struct Column<'tcx> {
    occ: PatVarRef,
    ty: Ty<'tcx>,
}

/// One row of the clause matrix: an arm in its current, partially-specialized
/// form.
#[derive(Clone)]
struct Row<'tcx> {
    /// One pattern per column, in column order.
    pats: Vec<Pat<'tcx>>,
    /// Bindings discovered in columns already consumed by specialization.
    bindings: Vec<(VarId, PatVarRef)>,
    guard: Option<Expr<'tcx>>,
    body: Expr<'tcx>,
    /// Index of the originating match arm, for redundancy reporting.
    arm: usize,
}

impl<'tcx> Row<'tcx> {
    /// Drop column `j` from the row, recording the binder (if any) that lived
    /// there. Used by scalar specialization and the default matrix, where the
    /// column carries no sub-fields.
    fn without_col(&self, j: usize, bind: Option<(VarId, PatVarRef)>) -> Row<'tcx> {
        let mut pats = self.pats.clone();
        pats.remove(j);
        let mut bindings = self.bindings.clone();
        bindings.extend(bind);
        Row {
            pats,
            bindings,
            guard: self.guard.clone(),
            body: self.body.clone(),
            arm: self.arm,
        }
    }
}

/// The clause matrix.
struct Matrix<'tcx> {
    cols: Vec<Column<'tcx>>,
    rows: Vec<Row<'tcx>>,
}

/// A set of consecutive integers; a `None` bound is infinite.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Interval {
    lo: Option<Integer>,
    hi: Option<Integer>,
}

impl Interval {
    fn from(lo: Option<Integer>, hi: Option<Integer>) -> Option<Interval> {
        match (&lo, &hi) {
            (Some(lo), Some(hi)) if lo > hi => None,
            _ => Some(Interval { lo, hi }),
        }
    }

    fn is_singleton(&self) -> bool {
        matches!((&self.lo, &self.hi), (Some(lo), Some(hi)) if lo == hi)
    }

    /// Whether `segment`, which no endpoint of `self` splits, lies in `self`.
    fn covers(&self, segment: &Interval) -> bool {
        let at = segment
            .lo
            .as_ref()
            .or(segment.hi.as_ref())
            .expect("a segment of a split domain has a finite bound");
        self.lo.as_ref().is_none_or(|lo| lo <= at) && self.hi.as_ref().is_none_or(|hi| at <= hi)
    }

    fn shift(self, by: i64) -> Interval {
        let by = Integer::from(by);
        Interval {
            lo: self.lo.map(|lo| lo + &by),
            hi: self.hi.map(|hi| hi + &by),
        }
    }
}

/// A pattern on a `Nat` or `Int` column as the integers it matches, and the
/// variable it binds with the depth of its occurrence below the column: 0 for
/// the column's own value, `d` for the field of the `d`-th nested constructor.
#[derive(Clone, Debug)]
struct IntervalPat {
    interval: Interval,
    binder: Option<(VarId, usize)>,
}

impl IntervalPat {
    fn field_binder(&self) -> bool {
        self.binder.is_some_and(|(_, depth)| depth > 0)
    }
}

/// `pat` on a column of `int`, or `None` when it matches no integer (a
/// negative literal on a `Nat`, already diagnosed). The lang enums fix the
/// variant order: `Nat { Zero, Succ(Nat) }` and `Int { OfNat(Nat), NegSucc(Nat) }`.
fn interval_pat(pat: &Pat<'_>, int: BigIntTy) -> Option<IntervalPat> {
    let whole = |binder| match int {
        BigIntTy::Nat => IntervalPat {
            interval: Interval {
                lo: Some(Integer::from(0)),
                hi: None,
            },
            binder,
        },
        BigIntTy::Int => IntervalPat {
            interval: Interval { lo: None, hi: None },
            binder,
        },
    };
    let field = |fields: &[Pat<'_>]| {
        let field = interval_pat(fields.first().unwrap_or(&Pat::Wild), BigIntTy::Nat)?;
        Some((
            field.interval,
            field.binder.map(|(v, depth)| (v, depth + 1)),
        ))
    };
    match pat {
        Pat::Wild => Some(whole(None)),
        Pat::Bind(v) => Some(whole(Some((*v, 0)))),
        Pat::Ctor {
            ctor: Ctor::Int(k), ..
        } => {
            let k = (*k).clone();
            Interval::from(Some(k.clone()), Some(k))
                .filter(|i| int == BigIntTy::Int || i.lo.as_ref().is_some_and(|lo| *lo >= 0))
                .map(|interval| IntervalPat {
                    interval,
                    binder: None,
                })
        }
        Pat::Ctor {
            ctor: Ctor::Variant(v),
            fields,
        } => match (int, v) {
            (BigIntTy::Nat, 0) => Some(IntervalPat {
                interval: Interval {
                    lo: Some(Integer::from(0)),
                    hi: Some(Integer::from(0)),
                },
                binder: None,
            }),
            // `Succ(n)` is `n + 1`; `OfNat(n)` is `n`.
            (BigIntTy::Nat, _) | (BigIntTy::Int, 0) => {
                let (interval, binder) = field(fields)?;
                let interval = if int == BigIntTy::Nat {
                    interval.shift(1)
                } else {
                    interval
                };
                Some(IntervalPat { interval, binder })
            }
            // `NegSucc(n)` is `-1 - n`.
            (BigIntTy::Int, _) => {
                let (interval, binder) = field(fields)?;
                let neg = |b: Integer| Integer::from(-1) - b;
                Some(IntervalPat {
                    interval: Interval {
                        lo: interval.hi.map(neg),
                        hi: interval.lo.map(neg),
                    },
                    binder,
                })
            }
        },
        // Any other constructor was diagnosed at the pattern.
        Pat::Ctor { .. } => Some(whole(None)),
    }
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// (MATCH) `Γ ⊢ match scrut { arms } ⇒ (Match(hs, tree) : α)`: synthesize `scrut ⇒ S`
    /// and pick a fresh result hole `α`; for each arm bind its pattern against `⟦S⟧` in a
    /// marked scope, check the optional guard `⇐ Bool`, and check the body `⇐ α` (so all
    /// arms unify to one type). The arms are compiled to a [`DecisionTree`] by the
    /// clause-matrix algorithm, with exhaustiveness and reachability checked here.
    pub(super) fn infer_match(
        &mut self,
        scrut: &surface::Expr,
        arms: &[(surface::Pattern, surface::Expr)],
        span: Option<Span>,
    ) -> Expr<'tcx> {
        let scrut = self.infer_expr(scrut);
        let scrut_ty = self.infer.shallow_resolve(scrut.ty);
        // Generic matches are checked again after specialization. A wildcard
        // alone imposes no Integral bound, so accepting it here must not let a
        // later Bool/Tuple/Tensor instantiation bypass the kernel restriction.
        if self.inside_kernel
            && !matches!(
                scrut_ty.kind(),
                TyKind::Int(_) | TyKind::Generic(_) | TyKind::Hole(_)
            )
        {
            self.error(scrut.span, "kernel match scrutinee must be an integer");
            return self.poison(span);
        }
        let result = self.infer.new_hole_ty();

        // Type-check every arm, turning each surface pattern into a `Pat` (with
        // binders allocated and in scope for the guard and body).
        let mut rows = Vec::with_capacity(arms.len());
        let mut arm_spans = Vec::with_capacity(arms.len());
        for (i, (pat, body)) in arms.iter().enumerate() {
            let mark = self.vars.mark();
            fn names(kind: &PatternKind, out: &mut Vec<reussir_syntax::kind::TokenKey>) {
                match kind {
                    PatternKind::Bind(name) => out.push(*name),
                    PatternKind::Tuple(fields) => fields.iter().for_each(|p| names(p, out)),
                    PatternKind::Ctor(ctor) => ctor.args.iter().for_each(|a| names(&a.kind, out)),
                    _ => {}
                }
            }
            let mut bound_names = Vec::new();
            names(&pat.kind(), &mut bound_names);
            let mut seen = rustc_hash::FxHashSet::default();
            for name in bound_names {
                if !seen.insert(name) {
                    self.error(
                        Some(pat.span()),
                        format!("duplicate binding `{}` in match pattern", self.sym(name)),
                    );
                }
            }
            let p = self.check_pat(&pat.kind(), Some(pat.span()), scrut_ty);
            let guard = pat.guard().map(|g| {
                let bool_ty = self.tcx.mk_bool();
                self.check_expr(&g, bool_ty)
            });
            let checked_body = self.check_expr(body, result);
            self.vars.restore(mark);

            arm_spans.push(Some(body.span()));
            rows.push(Row {
                pats: vec![p],
                bindings: Vec::new(),
                guard,
                body: checked_body,
                arm: i,
            });
        }

        // Compile the single-column matrix rooted at the scrutinee itself.
        let matrix = Matrix {
            cols: vec![Column {
                occ: PatVarRef::default(),
                ty: scrut_ty,
            }],
            rows,
        };
        let mut reached = vec![false; arms.len()];
        let mut exhaustive = true;
        let tree = self.compile_match(matrix, &mut reached, &mut exhaustive);

        // Report arms that can never match, and a non-exhaustive match.
        for (i, hit) in reached.iter().enumerate() {
            if !hit {
                self.warn(arm_spans[i], "unreachable match arm");
            }
        }
        if !exhaustive {
            self.error(span, "non-exhaustive patterns in `match`");
        }

        self.mk_match(Box::new(scrut), tree, result, span)
    }

    fn mk_match(
        &mut self,
        scrut: Box<Expr<'tcx>>,
        tree: DecisionTree<'tcx>,
        ty: Ty<'tcx>,
        span: Option<Span>,
    ) -> Expr<'tcx> {
        let id = self.fresh_expr_id();
        Expr {
            kind: ExprKind::Match(scrut, tree),
            ty,
            span,
            id,
        }
    }

    // ----- pattern type-checking -----

    /// Type-check a surface pattern against `ty`, allocating binders into the
    /// current scope, and return the typed [`Pat`]. `span` is the pattern's
    /// source span, used to blame any error; nested sub-patterns (a constructor's
    /// arguments) carry no span of their own, so they reuse the enclosing
    /// pattern's — coarse, but still traced to source.
    fn check_pat(&mut self, kind: &PatternKind, span: Option<Span>, ty: Ty<'tcx>) -> Pat<'tcx> {
        let ty = self.infer.shallow_resolve(ty);
        match kind {
            PatternKind::Wildcard => Pat::Wild,
            PatternKind::Tuple(patterns) => {
                let fields = match ty.kind() {
                    TyKind::Unit if patterns.is_empty() => &[][..],
                    TyKind::Tuple(fields) if fields.len() == patterns.len() => *fields,
                    TyKind::Hole(_) => {
                        let fields: Vec<_> =
                            patterns.iter().map(|_| self.infer.new_hole_ty()).collect();
                        let want = self.tcx.mk_tuple(&fields);
                        let _ = self.infer.unify(ty, want);
                        self.tcx.alloc_slice(&fields)
                    }
                    _ => {
                        self.error(
                            span,
                            format!(
                                "tuple pattern with {} fields does not match `{}`",
                                patterns.len(),
                                self.ty_display(ty)
                            ),
                        );
                        return Pat::Wild;
                    }
                };
                let fields = patterns
                    .iter()
                    .zip(fields)
                    .map(|(p, &t)| self.check_pat(p, span, t))
                    .collect();
                Pat::Ctor {
                    ctor: Ctor::Product,
                    fields,
                }
            }
            PatternKind::Bind(name) => {
                let var = self.vars.fresh(*name, ty, None);
                Pat::Bind(var)
            }
            PatternKind::Const(c) => self.check_const_pat(c, span, ty),
            PatternKind::Ctor(ctor) => self.check_ctor_pat(ctor, span, ty),
        }
    }

    fn check_const_pat(&mut self, c: &Const, span: Option<Span>, ty: Ty<'tcx>) -> Pat<'tcx> {
        let ctor = match c {
            Const::ConstInt(i) => {
                // The literal must be *some* integer, but its width/signedness
                // is whatever the scrutinee column already is — do not pin it to
                // `i32`, or matching an `i64`/`u8`/… scrutinee against an integer
                // arm would force `i64 = i32` and mis-type the match. Register
                // the `Integral` bound on the column type, exactly as an integer
                // literal *expression* does; the eventual int-switch lowering
                // reads the column's real type to pick the signed/unsigned cast.
                self.register_bound(self.lang.integral, ty, span);
                if *i < 0
                    && matches!(
                        self.infer.shallow_resolve(ty).bigint(),
                        Some((BigIntTy::Nat, _))
                    )
                {
                    self.error(
                        span,
                        format!("integer pattern `{i}` is out of range for `Nat`"),
                    );
                }
                Ctor::Int(self.tcx.alloc(i.clone()))
            }
            Const::ConstBool(b) => {
                let want = self.tcx.mk_bool();
                self.expect(ty, want, span);
                Ctor::Bool(*b)
            }
            Const::ConstChar(c) => {
                let want = self.tcx.mk_char();
                self.expect(ty, want, span);
                Ctor::Char(*c)
            }
            Const::ConstString(s) => {
                let want = self.tcx.mk_str();
                self.expect(ty, want, span);
                Ctor::Str(self.strings.allocate(s))
            }
            Const::ConstFloat(_) => {
                self.error(span, "floating-point patterns are not supported");
                return Pat::Wild;
            }
        };
        Pat::Ctor {
            ctor,
            fields: Vec::new(),
        }
    }

    fn check_ctor_pat(
        &mut self,
        ctor: &surface::CtorPat,
        span: Option<Span>,
        ty: Ty<'tcx>,
    ) -> Pat<'tcx> {
        // An `Arc<Enum<…>>` scrutinee dispatches on the inner enum's
        // constructors; the pattern spelling is the plain one. The coloring
        // is the atomic context of everything inside, though: bare shared
        // fields bind at their promoted (`Arc`) types (§3.3).
        let arced = matches!(self.infer.shallow_resolve(ty).kind(), TyKind::Arc(_));
        let ty = self.peel_arc(ty);
        // Expand `import` bindings so an imported qualifier (`L::Cons` with
        // `import pkg::List as L`, or an imported `Nullable`) matches the
        // same as its full spelling.
        let expanded_pat;
        let ctor = match self.expand_path(&ctor.path) {
            Some(path) => {
                expanded_pat = surface::CtorPat {
                    path,
                    args: ctor.args.clone(),
                    has_ellipsis: ctor.has_ellipsis,
                    is_named: ctor.is_named,
                };
                &expanded_pat
            }
            None => ctor,
        };
        // The built-in nullable constructors, by the same qualifier convention
        // as [`Elaborator::infer_ctor`] on the expression side: the canonical
        // `core::intrinsic::nullable::Nullable::…`, or the prelude's bare
        // `Nullable::…` when no user record shadows the name.
        if ctor.path.segments.last().map(|k| self.sym(*k)) == Some("Nullable")
            && self.ctor_qualifier_prelude_applies(&ctor.path, "nullable")
        {
            return self.check_nullable_pat(ctor, span, ty);
        }
        let Some((def, args)) = self.ctor_enum(ty) else {
            let shown = self.infer.resolve(ty);
            self.error(
                span,
                format!(
                    "cannot match constructor against `{}`",
                    self.ty_display(shown)
                ),
            );
            return Pat::Wild;
        };
        // The enum is named by the path qualifier (`Enum::Variant`, possibly
        // module-qualified: `m::Enum::Variant`), resolved to its def; bare
        // `Variant` falls back to the scrutinee's own record def.
        let want = ctor.path.basename;
        let enum_def = match ctor.path.segments.last() {
            Some(&seg) => match self.resolve_ctor_qualifier(&ctor.path) {
                Some(d) => {
                    self.record_use(seg);
                    d
                }
                None => {
                    if let Some(msg) = self.extern_private_ctor_msg(&ctor.path) {
                        self.error(span, msg);
                        return Pat::Wild;
                    }
                    let hint = self.record_suggestion(seg);
                    self.error(span, format!("unknown enum `{}`{hint}", self.sym(seg)));
                    return Pat::Wild;
                }
            },
            None => def,
        };
        let Some(record) = self.records.get(&enum_def).cloned() else {
            self.error(span, "unknown enum".to_string());
            return Pat::Wild;
        };
        // Struct patterns do not exist (only enum-variant and Nullable
        // constructors match); if they ever land, private-field bindings must
        // gate on `may_access_private_fields` like projection does.
        let Some(RecordFields::Variants(variants)) = &record.fields else {
            self.error(span, "not an enum".to_string());
            return Pat::Wild;
        };
        let Some(vidx) = variants.iter().position(|v| v.name == want) else {
            self.error(span, format!("no variant `{}`", self.sym(want)));
            return Pat::Wild;
        };

        let payload = variants[vidx].fields.clone();
        if ctor.args.len() > payload.len()
            || (!ctor.has_ellipsis && ctor.args.len() != payload.len())
        {
            self.error(
                span,
                format!(
                    "variant `{}` has {} field(s), but the pattern binds {}",
                    self.sym(want),
                    payload.len(),
                    ctor.args.len()
                ),
            );
        }
        let inst = Instantiation::from_pairs(
            record
                .ty_params
                .iter()
                .map(|(_, g)| *g)
                .zip(args.iter().copied()),
        );
        // Recurse into each declared field. Missing trailing fields (e.g. behind
        // a `..` ellipsis or an arity error) are treated as wildcards.
        let group = if arced {
            self.arc_recursive_group(enum_def)
        } else {
            rustc_hash::FxHashSet::default()
        };
        let fields = payload
            .iter()
            .enumerate()
            .map(|(i, decl)| {
                let field_ty = self.infer.instantiate_ty(*decl, &inst);
                let field_ty = if arced {
                    self.arc_promote_member_ty(&group, field_ty)
                } else {
                    field_ty
                };
                match ctor.args.get(i) {
                    Some(arg) => self.check_pat(&arg.kind, span, field_ty),
                    None => Pat::Wild,
                }
            })
            .collect();
        Pat::Ctor {
            ctor: Ctor::Variant(vidx),
            fields,
        }
    }

    /// `Nullable::NonNull(p)` / `Nullable::Null` patterns against a
    /// `Nullable<T>` scrutinee. `NonNull` binds one sub-pattern at the element
    /// type; `Null` binds nothing.
    fn check_nullable_pat(
        &mut self,
        ctor: &surface::CtorPat,
        span: Option<Span>,
        ty: Ty<'tcx>,
    ) -> Pat<'tcx> {
        // The scrutinee must be (or become) a nullable; a still-unsolved hole
        // is solved to `Nullable<α>` so the sub-pattern can constrain `α`.
        let inner = match ty.kind() {
            TyKind::Nullable(inner) => self.infer.shallow_resolve(*inner),
            TyKind::Hole(_) => {
                let elem = self.infer.new_hole_ty();
                let want = self.tcx.mk_nullable(elem);
                let _ = self.infer.unify(ty, want);
                elem
            }
            _ => {
                let shown = self.infer.resolve(ty);
                self.error(
                    span,
                    format!(
                        "cannot match a nullable pattern against `{}`",
                        self.ty_display(shown)
                    ),
                );
                return Pat::Wild;
            }
        };
        match self.sym(ctor.path.basename) {
            "NonNull" => {
                if ctor.args.len() != 1 {
                    self.error(
                        span,
                        format!(
                            "`Nullable::NonNull` binds exactly 1 value, but the \
                             pattern binds {}",
                            ctor.args.len()
                        ),
                    );
                }
                let field = match ctor.args.first() {
                    Some(arg) => self.check_pat(&arg.kind, span, inner),
                    None => Pat::Wild,
                };
                Pat::Ctor {
                    ctor: Ctor::NonNull,
                    fields: vec![field],
                }
            }
            "Null" => {
                if !ctor.args.is_empty() {
                    self.error(span, "`Nullable::Null` binds no values");
                }
                Pat::Ctor {
                    ctor: Ctor::Null,
                    fields: Vec::new(),
                }
            }
            other => {
                self.error(span, format!("unknown nullable constructor `{other}`"));
                Pat::Wild
            }
        }
    }

    // ----- the matrix compiler -----

    fn compile_match(
        &mut self,
        m: Matrix<'tcx>,
        reached: &mut [bool],
        exhaustive: &mut bool,
    ) -> DecisionTree<'tcx> {
        // (1) No rows can match here: a gap in coverage.
        let Some(first) = m.rows.first() else {
            *exhaustive = false;
            return DecisionTree::Uncovered;
        };

        // (2) The first row tests no constructors: it matches (up to its guard).
        if !first.pats.iter().any(Pat::is_ctor) {
            reached[first.arm] = true;
            // Binders accumulated earlier, plus any still sitting in columns.
            let mut bindings = first.bindings.clone();
            for (col, pat) in m.cols.iter().zip(&first.pats) {
                if let Pat::Bind(v) = pat {
                    bindings.push((*v, col.occ.clone()));
                }
            }
            let body = Box::new(first.body.clone());
            return match &first.guard {
                None => DecisionTree::Leaf { body, bindings },
                Some(guard) => {
                    let guard = Box::new(guard.clone());
                    // If the guard fails, retry the remaining arms on the same
                    // value (their patterns are unchanged: we never specialized).
                    let rest = Matrix {
                        cols: m.cols.clone(),
                        rows: m.rows[1..].to_vec(),
                    };
                    let failure = Box::new(self.compile_match(rest, reached, exhaustive));
                    DecisionTree::Guard {
                        bindings,
                        guard,
                        success: Box::new(DecisionTree::Leaf {
                            body,
                            bindings: Vec::new(),
                        }),
                        failure,
                    }
                }
            };
        }

        // (3) Switch on the leftmost column that still has a constructor.
        let j = m
            .cols
            .iter()
            .enumerate()
            .position(|(c, _)| m.rows.iter().any(|r| r.pats[c].is_ctor()))
            .expect("a constructor column exists when some row tests one");
        let occ = m.cols[j].occ.clone();
        let col_ty = self.infer.shallow_resolve(m.cols[j].ty);
        if let Some((int, _)) = col_ty.bigint() {
            return self.compile_bigint_column(&m, j, int, reached, exhaustive);
        }
        let family = m
            .rows
            .iter()
            .find_map(|r| match &r.pats[j] {
                Pat::Ctor { ctor, .. } => Some(ctor.family()),
                _ => None,
            })
            .expect("the chosen column has a constructor");

        let cases = match family {
            Family::Product => {
                let fields = match col_ty.kind() {
                    TyKind::Tuple(fields) => *fields,
                    _ => &[],
                };
                let sub = self.specialize_constructor(&m, j, Ctor::Product, &occ, fields);
                return self.compile_match(sub, reached, exhaustive);
            }
            // Closed: one subtree per variant; an empty variant subtree is an
            // uncovered case (caught by the recursive call returning `Uncovered`).
            Family::Variant => {
                let n = self.variant_count(col_ty);
                let mut subtrees = Vec::with_capacity(n);
                for v in 0..n {
                    let child_tys = self.variant_field_tys(col_ty, v);
                    let sub =
                        self.specialize_constructor(&m, j, Ctor::Variant(v), &occ, &child_tys);
                    subtrees.push(self.compile_match(sub, reached, exhaustive));
                }
                SwitchCases::Ctor(subtrees)
            }
            // Closed two-case family.
            Family::Bool => {
                let t = self.specialize_scalar(&m, j, &Ctor::Bool(true), &occ);
                let if_true = Box::new(self.compile_match(t, reached, exhaustive));
                let f = self.specialize_scalar(&m, j, &Ctor::Bool(false), &occ);
                let if_false = Box::new(self.compile_match(f, reached, exhaustive));
                SwitchCases::Bool { if_true, if_false }
            }
            // Closed two-case family. The non-null case exposes one field —
            // the unwrapped element — at the *same* occurrence as the switch
            // itself: the textual-IR arms carry no field index and the
            // codegen dispatch rebinds the scrutinee's path to the unwrapped
            // pointer (unwrapping is a pointer no-op), matching the reference.
            Family::Nullable => {
                let inner = match self.infer.shallow_resolve(col_ty).kind() {
                    TyKind::Nullable(inner) => self.infer.shallow_resolve(*inner),
                    // A non-nullable column type means the pattern arm already
                    // errored in `check_nullable_pat`; recover with Bottom.
                    _ => self.tcx.mk(TyKind::Bottom),
                };
                let nn = self.specialize_nonnull(&m, j, &occ, inner);
                let non_null = Box::new(self.compile_match(nn, reached, exhaustive));
                let nl = self.specialize_scalar(&m, j, &Ctor::Null, &occ);
                let null = Box::new(self.compile_match(nl, reached, exhaustive));
                SwitchCases::Nullable { non_null, null }
            }
            // Open: one case per literal, plus a default for everything else.
            Family::Int => {
                let mut cases = Vec::new();
                for k in self.column_ints(&m, j) {
                    let sub = self.specialize_scalar(&m, j, &Ctor::Int(k), &occ);
                    cases.push((k, self.compile_match(sub, reached, exhaustive)));
                }
                let def = self.default_matrix(&m, j, &occ);
                let default = Box::new(self.compile_match(def, reached, exhaustive));
                SwitchCases::Int {
                    cases,
                    ranges: Vec::new(),
                    default,
                }
            }
            Family::Char => {
                let mut cases = Vec::new();
                for c in self.column_chars(&m, j) {
                    let sub = self.specialize_scalar(&m, j, &Ctor::Char(c), &occ);
                    cases.push((c, self.compile_match(sub, reached, exhaustive)));
                }
                let def = self.default_matrix(&m, j, &occ);
                let default = Box::new(self.compile_match(def, reached, exhaustive));
                SwitchCases::Char { cases, default }
            }
            Family::Str => {
                let mut cases = Vec::new();
                for s in self.column_strs(&m, j) {
                    let sub = self.specialize_scalar(&m, j, &Ctor::Str(s), &occ);
                    cases.push((s, self.compile_match(sub, reached, exhaustive)));
                }
                let def = self.default_matrix(&m, j, &occ);
                let default = Box::new(self.compile_match(def, reached, exhaustive));
                SwitchCases::String { cases, default }
            }
        };
        DecisionTree::Switch {
            scrutinee: occ,
            cases,
        }
    }

    /// Switch on a `Nat` or `Int` column (see the module's "Built-in
    /// integers" section): split the integers at every endpoint of the rows'
    /// intervals, compile the rows covering each segment once, and select the
    /// segments with `case` keys and range arms in first-match order.
    fn compile_bigint_column(
        &mut self,
        m: &Matrix<'tcx>,
        j: usize,
        int: BigIntTy,
        reached: &mut [bool],
        exhaustive: &mut bool,
    ) -> DecisionTree<'tcx> {
        let occ = m.cols[j].occ.clone();
        let pats: Vec<Option<IntervalPat>> = m
            .rows
            .iter()
            .map(|row| interval_pat(&row.pats[j], int))
            .collect();
        // A row matching no integer was diagnosed at its literal.
        for (row, pat) in m.rows.iter().zip(&pats) {
            if pat.is_none() {
                reached[row.arm] = true;
            }
        }

        // Cut the domain at every interval endpoint (`Int` also at zero, where
        // the sign of the constructor fields changes), and give each segment
        // the rows covering it, in order.
        let mut cuts = vec![Integer::from(0)];
        for pat in pats.iter().flatten() {
            cuts.extend(pat.interval.lo.clone());
            cuts.extend(pat.interval.hi.as_ref().map(|hi| hi + Integer::from(1)));
        }
        if int == BigIntTy::Nat {
            cuts.retain(|c| *c >= 0);
        }
        cuts.sort();
        cuts.dedup();
        let mut segments: Vec<(Interval, Vec<usize>)> = Vec::new();
        let mut bounds: Vec<Option<Integer>> = match int {
            BigIntTy::Nat => Vec::new(),
            BigIntTy::Int => vec![None],
        };
        bounds.extend(cuts.iter().cloned().map(Some));
        for lo in &bounds {
            let hi = cuts
                .iter()
                .find(|c| lo.as_ref().is_none_or(|lo| *c > lo))
                .map(|c| c - Integer::from(1));
            let segment = Interval { lo: lo.clone(), hi };
            let rows = (0..m.rows.len())
                .filter(|&r| {
                    pats[r]
                        .as_ref()
                        .is_some_and(|p| p.interval.covers(&segment))
                })
                .collect::<Vec<_>>();
            // A segment never straddles zero: range arms are built from the
            // runs on one side of it.
            match segments.last_mut() {
                Some((last, last_rows))
                    if *last_rows == rows && segment.lo.as_ref().is_none_or(|lo| *lo != 0) =>
                {
                    last.hi = segment.hi
                }
                _ => segments.push((segment, rows)),
            }
        }

        // Compile each distinct row set once.
        let mut classes: Vec<(Vec<usize>, DecisionTree<'tcx>, bool)> = Vec::new();
        let mut class_of = Vec::with_capacity(segments.len());
        for (_, rows) in &segments {
            let c = match classes.iter().position(|(r, ..)| r == rows) {
                Some(c) => c,
                None => {
                    let cols = cols_without(&m.cols, j);
                    let sub = rows
                        .iter()
                        .map(|&r| {
                            let bind = pats[r].as_ref().and_then(|p| p.binder).map(|(v, depth)| {
                                let mut path = occ.0.clone();
                                path.extend(std::iter::repeat_n(0, depth));
                                (v, PatVarRef(path))
                            });
                            m.rows[r].without_col(j, bind)
                        })
                        .collect();
                    let tree = self.compile_match(Matrix { cols, rows: sub }, reached, exhaustive);
                    let fields = rows
                        .iter()
                        .any(|&r| pats[r].as_ref().is_some_and(|p| p.field_binder()));
                    classes.push((rows.clone(), tree, fields));
                    classes.len() - 1
                }
            };
            class_of.push(c);
        }

        // The default arm's class: one binding no constructor field that saves
        // arms over spelling its segments out, covering the most segments.
        let default = (0..classes.len())
            .filter(|&c| !classes[c].2)
            .filter(|&c| {
                let mine: Vec<_> = (0..segments.len()).filter(|&s| class_of[s] == c).collect();
                mine.len() >= 2 || mine.iter().any(|&s| !segments[s].0.is_singleton())
            })
            .max_by_key(|&c| {
                let count = class_of.iter().filter(|&&k| k == c).count();
                (count, classes[c].0.first().copied())
            });
        let tree = |c: usize| classes[c].1.clone();

        // Single values first, as `case` keys, unless their row set also has a
        // wider segment on the same side of zero, whose range arm can take
        // them too.
        let negative = |i: &Interval| i.hi.as_ref().is_some_and(|hi| *hi < 0);
        let ranged = |s: usize| {
            (0..segments.len()).any(|t| {
                class_of[t] == class_of[s]
                    && !segments[t].0.is_singleton()
                    && negative(&segments[t].0) == negative(&segments[s].0)
            })
        };
        let mut cases = Vec::new();
        let mut taken = vec![false; segments.len()];
        for (s, (segment, _)) in segments.iter().enumerate() {
            if segment.is_singleton() && Some(class_of[s]) != default && !ranged(s) {
                let key = segment.lo.clone().expect("a singleton is bounded");
                cases.push((&*self.tcx.alloc(key), tree(class_of[s])));
                taken[s] = true;
            }
        }

        // Then the nonnegative runs, highest first, each `at_least` its lower
        // bound, and the negative runs, lowest first, each `at_most` its upper
        // bound: the arms before a run have taken every value above (below)
        // it. Runs of the default class nearest zero are left to `default`.
        let runs = |side: &dyn Fn(&Interval) -> bool| {
            let mut runs: Vec<(Interval, usize)> = Vec::new();
            for (s, (segment, _)) in segments.iter().enumerate() {
                if taken[s] || !side(segment) {
                    continue;
                }
                match runs.last_mut() {
                    Some((run, c)) if *c == class_of[s] => run.hi = segment.hi.clone(),
                    _ => runs.push((segment.clone(), class_of[s])),
                }
            }
            runs
        };
        // A range arm's region argument is the distance from its bound; a row
        // binding `Succ^k(n)` reads it unchanged at bound `k` (`OfNat`: `k - 1`,
        // `NegSucc`: `at_most -k`). Extending a run's bound over values the
        // `case` keys already took keeps the arm's selection and saves the
        // arithmetic.
        let natural = |c: usize, upper: bool| {
            classes[c].0.iter().find_map(|&r| {
                let pat = pats[r].as_ref()?;
                let (_, depth) = pat.binder.filter(|&(_, d)| d > 0)?;
                let depth = Integer::from(depth as u64);
                let on_upper = pat.interval.lo.as_ref().is_some_and(|lo| *lo >= 0);
                match (int, upper && on_upper, !upper && !on_upper) {
                    (BigIntTy::Nat, true, _) => Some(depth),
                    (BigIntTy::Int, true, _) => Some(depth - Integer::from(1)),
                    (BigIntTy::Int, _, true) => Some(-depth),
                    _ => None,
                }
            })
        };
        let all_taken = |lo: &Integer, hi: &Integer| {
            segments.iter().zip(&taken).all(|((seg, _), &t)| {
                t || seg.lo.as_ref().is_some_and(|l| l > hi)
                    || seg.hi.as_ref().is_some_and(|h| h < lo)
            })
        };
        let mut ranges = Vec::new();
        let mut leftover = false;
        let upper = runs(&|i: &Interval| i.lo.as_ref().is_some_and(|lo| *lo >= 0));
        for i in (0..upper.len()).rev() {
            if default.is_some() && upper[..=i].iter().all(|(_, c)| Some(*c) == default) {
                leftover = true;
                break;
            }
            let (run, c) = &upper[i];
            let mut lo = run.lo.clone().expect("a nonnegative run is bounded below");
            if let Some(n) = natural(*c, true)
                && n >= 0
                && n < lo
                && all_taken(&n, &(lo.clone() - Integer::from(1)))
            {
                lo = n;
            }
            if int == BigIntTy::Nat && lo == 0 {
                // `at_least 0` selects every `Nat`.
                return self.int_switch(occ, cases, ranges, tree(*c));
            }
            ranges.push((IntRange::AtLeast(self.tcx.alloc(lo)), tree(*c)));
        }
        let mut lower = runs(&|i: &Interval| i.hi.as_ref().is_some_and(|hi| *hi < 0));
        lower.reverse();
        for i in (0..lower.len()).rev() {
            if default.is_some() && lower[..=i].iter().all(|(_, c)| Some(*c) == default) {
                leftover = true;
                break;
            }
            let (run, c) = &lower[i];
            let mut hi = run.hi.clone().expect("a negative run is bounded above");
            if let Some(m) = natural(*c, false)
                && m < 0
                && m > hi
                && all_taken(&(hi.clone() + Integer::from(1)), &m)
            {
                hi = m;
            }
            ranges.push((IntRange::AtMost(self.tcx.alloc(hi)), tree(*c)));
        }
        let fallback = match default {
            Some(c) if leftover => tree(c),
            _ => DecisionTree::Unreachable,
        };
        self.int_switch(occ, cases, ranges, fallback)
    }

    fn int_switch(
        &self,
        scrutinee: PatVarRef,
        cases: Vec<(&'tcx Integer, DecisionTree<'tcx>)>,
        ranges: Vec<(IntRange<'tcx>, DecisionTree<'tcx>)>,
        default: DecisionTree<'tcx>,
    ) -> DecisionTree<'tcx> {
        DecisionTree::Switch {
            scrutinee,
            cases: SwitchCases::Int {
                cases,
                ranges,
                default: Box::new(default),
            },
        }
    }

    /// `S(Variant(v), P)`: keep the rows that can match variant `v`, expanding
    /// the matched constructor's fields into fresh columns at `occ ++ [i]`.
    fn specialize_constructor(
        &self,
        m: &Matrix<'tcx>,
        j: usize,
        ctor: Ctor<'tcx>,
        occ: &PatVarRef,
        child_tys: &[Ty<'tcx>],
    ) -> Matrix<'tcx> {
        let k = child_tys.len();
        let mut cols = m.cols[..j].to_vec();
        for (i, &ty) in child_tys.iter().enumerate() {
            let mut path = occ.0.clone();
            path.push(i as u32);
            cols.push(Column {
                occ: PatVarRef(path),
                ty,
            });
        }
        cols.extend_from_slice(&m.cols[j + 1..]);

        let mut rows = Vec::new();
        for row in &m.rows {
            // What the field columns become, and whether this row's column-`j`
            // pattern bound the whole value.
            let (sub, bind): (Vec<Pat<'tcx>>, Option<(VarId, PatVarRef)>) = match &row.pats[j] {
                Pat::Ctor {
                    ctor: actual,
                    fields,
                } if *actual == ctor => (fields.clone(), None),
                // A different variant cannot match this case.
                Pat::Ctor { .. } => continue,
                Pat::Wild => (vec![Pat::Wild; k], None),
                Pat::Bind(var) => (vec![Pat::Wild; k], Some((*var, occ.clone()))),
            };
            let mut pats = row.pats[..j].to_vec();
            pats.extend(sub);
            pats.extend_from_slice(&row.pats[j + 1..]);
            let mut bindings = row.bindings.clone();
            bindings.extend(bind);
            rows.push(Row {
                pats,
                bindings,
                guard: row.guard.clone(),
                body: row.body.clone(),
                arm: row.arm,
            });
        }
        Matrix { cols, rows }
    }

    /// `S(NonNull, P)`: keep the rows that can match the non-null case,
    /// expanding the single unwrapped-element field into a fresh column at the
    /// *same* occurrence `occ` (see the `Family::Nullable` arm — unwrapping a
    /// non-null is a pointer no-op, so the payload occupies the scrutinee's
    /// own path, unlike a variant's `occ ++ [i]` fields).
    fn specialize_nonnull(
        &self,
        m: &Matrix<'tcx>,
        j: usize,
        occ: &PatVarRef,
        inner: Ty<'tcx>,
    ) -> Matrix<'tcx> {
        let mut cols = m.cols[..j].to_vec();
        cols.push(Column {
            occ: occ.clone(),
            ty: inner,
        });
        cols.extend_from_slice(&m.cols[j + 1..]);

        let mut rows = Vec::new();
        for row in &m.rows {
            let (sub, bind): (Pat<'tcx>, Option<(VarId, PatVarRef)>) = match &row.pats[j] {
                Pat::Ctor {
                    ctor: Ctor::NonNull,
                    fields,
                } => (fields[0].clone(), None),
                // A `Null` pattern cannot match this case.
                Pat::Ctor { .. } => continue,
                Pat::Wild => (Pat::Wild, None),
                // Binding the whole nullable: within the non-null case the
                // nullable and its unwrapped element are the same pointer, so
                // the binder's occurrence stays `occ` like everything else.
                Pat::Bind(var) => (Pat::Wild, Some((*var, occ.clone()))),
            };
            let mut pats = row.pats[..j].to_vec();
            pats.push(sub);
            pats.extend_from_slice(&row.pats[j + 1..]);
            let mut bindings = row.bindings.clone();
            bindings.extend(bind);
            rows.push(Row {
                pats,
                bindings,
                guard: row.guard.clone(),
                body: row.body.clone(),
                arm: row.arm,
            });
        }
        Matrix { cols, rows }
    }

    /// `S(c, P)` for a field-less constructor (int/bool/string): keep the rows
    /// matching `c` plus the wildcard rows, dropping the tested column.
    fn specialize_scalar(
        &self,
        m: &Matrix<'tcx>,
        j: usize,
        c: &Ctor<'tcx>,
        occ: &PatVarRef,
    ) -> Matrix<'tcx> {
        let cols = cols_without(&m.cols, j);
        let mut rows = Vec::new();
        for row in &m.rows {
            match &row.pats[j] {
                Pat::Ctor { ctor, .. } if ctor == c => rows.push(row.without_col(j, None)),
                Pat::Ctor { .. } => {}
                Pat::Wild => rows.push(row.without_col(j, None)),
                Pat::Bind(var) => rows.push(row.without_col(j, Some((*var, occ.clone())))),
            }
        }
        Matrix { cols, rows }
    }

    /// `D(P)`: the rows that ignore column `j` (wildcards/binders), dropping it.
    /// This is the body of the default branch for an open family.
    fn default_matrix(&self, m: &Matrix<'tcx>, j: usize, occ: &PatVarRef) -> Matrix<'tcx> {
        let cols = cols_without(&m.cols, j);
        let mut rows = Vec::new();
        for row in &m.rows {
            match &row.pats[j] {
                Pat::Ctor { .. } => {}
                Pat::Wild => rows.push(row.without_col(j, None)),
                Pat::Bind(var) => rows.push(row.without_col(j, Some((*var, occ.clone())))),
            }
        }
        Matrix { cols, rows }
    }

    /// The distinct integer literals tested in column `j`, in first-seen order.
    fn column_ints(&self, m: &Matrix<'tcx>, j: usize) -> Vec<&'tcx Integer> {
        let mut seen = Vec::new();
        for row in &m.rows {
            if let Pat::Ctor {
                ctor: Ctor::Int(k), ..
            } = &row.pats[j]
                && !seen.contains(k)
            {
                seen.push(*k);
            }
        }
        seen
    }

    /// The distinct character literals tested in column `j`, in first-seen order.
    fn column_chars(&self, m: &Matrix<'tcx>, j: usize) -> Vec<u32> {
        let mut seen = Vec::new();
        for row in &m.rows {
            if let Pat::Ctor {
                ctor: Ctor::Char(c),
                ..
            } = &row.pats[j]
                && !seen.contains(c)
            {
                seen.push(*c);
            }
        }
        seen
    }

    /// The distinct string literals tested in column `j`, in first-seen order.
    fn column_strs(&self, m: &Matrix<'tcx>, j: usize) -> Vec<StringToken> {
        let mut seen = Vec::new();
        for row in &m.rows {
            if let Pat::Ctor {
                ctor: Ctor::Str(s), ..
            } = &row.pats[j]
                && !seen.contains(s)
            {
                seen.push(*s);
            }
        }
        seen
    }

    /// The field types of variant `v` of the enum at `ty`, instantiated with the
    /// enum's type arguments.
    fn variant_field_tys(&mut self, ty: Ty<'tcx>, v: usize) -> Vec<Ty<'tcx>> {
        let ty = self.peel_arc(ty);
        let Some((def, args)) = self.ctor_enum(ty) else {
            return Vec::new();
        };
        let Some(record) = self.records.get(&def).cloned() else {
            return Vec::new();
        };
        let Some(RecordFields::Variants(variants)) = &record.fields else {
            return Vec::new();
        };
        let Some(var) = variants.get(v) else {
            return Vec::new();
        };
        let payload = var.fields.clone();
        let inst = Instantiation::from_pairs(
            record
                .ty_params
                .iter()
                .map(|(_, g)| *g)
                .zip(args.iter().copied()),
        );
        payload
            .iter()
            .map(|t| self.infer.instantiate_ty(*t, &inst))
            .collect()
    }

    fn variant_count(&mut self, ty: Ty<'tcx>) -> usize {
        let ty = self.peel_arc(ty);
        if let Some((def, _)) = self.ctor_enum(ty)
            && let Some(record) = self.records.get(&def)
            && let Some(RecordFields::Variants(vs)) = &record.fields
        {
            return vs.len();
        }
        0
    }

    // ----- zonking -----

    pub(super) fn zonk_tree(&mut self, tree: DecisionTree<'tcx>) -> DecisionTree<'tcx> {
        match tree {
            DecisionTree::Uncovered => DecisionTree::Uncovered,
            DecisionTree::Unreachable => DecisionTree::Unreachable,
            DecisionTree::Leaf { body, bindings } => DecisionTree::Leaf {
                body: Box::new(self.zonk_expr(*body)),
                bindings,
            },
            DecisionTree::Guard {
                bindings,
                guard,
                success,
                failure,
            } => DecisionTree::Guard {
                bindings,
                guard: Box::new(self.zonk_expr(*guard)),
                success: Box::new(self.zonk_tree(*success)),
                failure: Box::new(self.zonk_tree(*failure)),
            },
            DecisionTree::Switch { scrutinee, cases } => DecisionTree::Switch {
                scrutinee,
                cases: self.zonk_cases(cases),
            },
        }
    }

    fn zonk_cases(&mut self, cases: SwitchCases<'tcx>) -> SwitchCases<'tcx> {
        match cases {
            SwitchCases::Int {
                cases,
                ranges,
                default,
            } => SwitchCases::Int {
                cases: cases
                    .into_iter()
                    .map(|(k, t)| (k, self.zonk_tree(t)))
                    .collect(),
                ranges: ranges
                    .into_iter()
                    .map(|(r, t)| (r, self.zonk_tree(t)))
                    .collect(),
                default: Box::new(self.zonk_tree(*default)),
            },
            SwitchCases::Bool { if_true, if_false } => SwitchCases::Bool {
                if_true: Box::new(self.zonk_tree(*if_true)),
                if_false: Box::new(self.zonk_tree(*if_false)),
            },
            SwitchCases::Char { cases, default } => SwitchCases::Char {
                cases: cases
                    .into_iter()
                    .map(|(k, t)| (k, self.zonk_tree(t)))
                    .collect(),
                default: Box::new(self.zonk_tree(*default)),
            },
            SwitchCases::Ctor(ts) => {
                SwitchCases::Ctor(ts.into_iter().map(|t| self.zonk_tree(t)).collect())
            }
            SwitchCases::String { cases, default } => SwitchCases::String {
                cases: cases
                    .into_iter()
                    .map(|(k, t)| (k, self.zonk_tree(t)))
                    .collect(),
                default: Box::new(self.zonk_tree(*default)),
            },
            SwitchCases::Nullable { non_null, null } => SwitchCases::Nullable {
                non_null: Box::new(self.zonk_tree(*non_null)),
                null: Box::new(self.zonk_tree(*null)),
            },
        }
    }
}

/// `cols` with index `j` removed.
fn cols_without<'tcx>(cols: &[Column<'tcx>], j: usize) -> Vec<Column<'tcx>> {
    let mut out = cols[..j].to_vec();
    out.extend_from_slice(&cols[j + 1..]);
    out
}

#[cfg(test)]
mod tests {
    use crate::semi::elaborate;
    use crate::surface;
    use crate::with_tcx;

    /// Elaborate a source string and return its diagnostics.
    fn reports_of(source: &str) -> Vec<crate::semi::Report> {
        with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parse = reussir_syntax::parse_with_interner(source, interner.clone());
            assert!(parse.ok(), "parse errors: {:#?}", parse.errors);
            let prog = surface::program(&parse.root);
            let elab = elaborate(tcx, &prog, &interner);
            elab.reports.clone()
        })
    }

    fn has_message(source: &str, needle: &str) -> bool {
        reports_of(source)
            .iter()
            .any(|r| r.message.contains(needle))
    }

    const COLOR: &str = "enum Color { Red, Green }\nenum Opt { None, Some(Color) }\n";

    #[test]
    fn kernel_integer_matches_preserve_guards_and_whole_value_bindings() {
        let source = r#"
            #[kernel] fn choose(x: u64, a: u64, b: u64, accept: bool) -> u64 {
                match x { 18446744073709551615 if accept => a, whole => whole }
            }
        "#;
        assert!(reports_of(source).is_empty(), "{:#?}", reports_of(source));
    }

    #[test]
    fn kernel_matches_reject_non_integer_scrutinees_even_with_a_wildcard() {
        for ty in ["bool", "f32", "()", "(i32,)", "Tensor<i32>"] {
            let source = format!(
                "#[kernel] fn bad(x: {ty}, value: i32) -> i32 {{ match x {{ _ => value }} }}"
            );
            assert!(
                has_message(&source, "kernel match scrutinee must be an integer"),
                "{ty}: {:#?}",
                reports_of(&source)
            );
        }
    }

    #[test]
    fn host_tuple_and_boolean_matches_remain_supported() {
        let source =
            "fn host(x: (bool, i32)) -> i32 { match x { (true, n) => n, (false, n) => n } }";
        assert!(reports_of(source).is_empty(), "{:#?}", reports_of(source));
    }

    #[test]
    fn nested_refutable_patterns_compile() {
        // `Opt::Some(Color::Red)` nests a refutable pattern inside a variant —
        // rejected by the old single-column compiler, exhaustive here.
        let src = format!(
            "{COLOR}fn f(x: Opt) -> i32 {{ match x {{\n\
             Opt::None => 0,\n\
             Opt::Some(Color::Red) => 1,\n\
             Opt::Some(Color::Green) => 2\n\
             }} }}"
        );
        assert!(reports_of(&src).is_empty(), "{:#?}", reports_of(&src));
    }

    #[test]
    fn missing_nested_case_is_non_exhaustive() {
        // `Opt::Some(Color::Green)` and `Opt::None` are uncovered.
        let src = format!(
            "{COLOR}fn f(x: Opt) -> i32 {{ match x {{\n\
             Opt::Some(Color::Red) => 1\n\
             }} }}"
        );
        assert!(
            has_message(&src, "non-exhaustive"),
            "{:#?}",
            reports_of(&src)
        );
    }

    #[test]
    fn full_variant_coverage_is_exhaustive() {
        let src = format!(
            "{COLOR}fn f(c: Color) -> i32 {{ match c {{\n\
             Color::Red => 1,\n\
             Color::Green => 2\n\
             }} }}"
        );
        assert!(reports_of(&src).is_empty(), "{:#?}", reports_of(&src));
    }

    #[test]
    fn arm_after_wildcard_is_unreachable() {
        let src = format!(
            "{COLOR}fn f(c: Color) -> i32 {{ match c {{\n\
             x => 1,\n\
             Color::Red => 2\n\
             }} }}"
        );
        assert!(has_message(&src, "unreachable"), "{:#?}", reports_of(&src));
    }

    #[test]
    fn guards_are_supported() {
        // A guarded arm plus a wildcard fallback: exhaustive, no warnings.
        let src = "fn f(b: bool) -> i32 { match b {\n\
                   x if x => 1,\n\
                   _ => 2\n\
                   } }";
        assert!(reports_of(src).is_empty(), "{:#?}", reports_of(src));
    }

    #[test]
    fn a_lone_guarded_arm_is_non_exhaustive() {
        // The guard may fail, so the match is not exhaustive.
        let src = "fn f(b: bool) -> i32 { match b { x if x => 1 } }";
        assert!(has_message(src, "non-exhaustive"), "{:#?}", reports_of(src));
    }

    #[test]
    fn integer_match_needs_a_default() {
        let src = "fn f(n: i32) -> i32 { match n { 0 => 10, 1 => 11 } }";
        assert!(has_message(src, "non-exhaustive"), "{:#?}", reports_of(src));
    }

    #[test]
    fn integer_match_with_wildcard_is_exhaustive() {
        let src = "fn f(n: i32) -> i32 { match n { 0 => 10, _ => 11 } }";
        assert!(reports_of(src).is_empty(), "{:#?}", reports_of(src));
    }

    #[test]
    fn non_i32_integer_match_typechecks() {
        // An `i64` scrutinee matched against integer-literal arms must not be
        // forced to `i32`. Before the fix, `check_const_pat` unified the column
        // type with `i32`, so this reported a spurious type mismatch.
        let src = "fn f(n: i64) -> i64 { match n { 0 => 10, _ => 11 } }";
        assert!(reports_of(src).is_empty(), "{:#?}", reports_of(src));
    }
}
