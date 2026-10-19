//! Evaluating surface types into interned [`Ty`]s, including the capability
//! ("flexivity") coloring of regional records.
//!
//! A `[regional]` record is a type that can be *created locally* inside a region.
//! Flexivity is the per-use color such a value carries; the four [`Flexivity`]
//! states mean:
//!
//! * **Flex** — a live, freshly created regional object. Its `[field]` links can
//!   be mutated while inside the region, and it can be passed to a `regional`
//!   function, but it *cannot be materialized*: it cannot escape the region —
//!   neither returned/frozen-out without first freezing, nor **captured by a
//!   closure** (a capture would let it outlive the region).
//! * **Rigid** — the frozen form: immutable, and *materializable* (it may escape
//!   the region). Freezing on region exit turns Flex into Rigid.
//! * **Regional** — not a value's birth state, but the "inherit my container's
//!   flexivity" color of a `[field]` mutable link: read through a Flex container
//!   it is mutable, through a Rigid one it is frozen. Because nested links
//!   *inherit* rather than store their own flexivity, freezing only the *head*
//!   re-interprets them all at once — so [`Elaborator::freeze_region`] is O(1),
//!   with no recursion.
//! * **Irrelevant** — a non-regional (value/shared) record; primitives carry no
//!   flexivity.
//!
//! Lifecycle: create ⟶ Flex ──(freeze on escape)──▶ Rigid (materializable).
//!
//! Representation note: a regional record's *type annotation* is evaluated by
//! `eval_type` to an unpinned [`Flexivity::Regional`], which
//! [`Elaborator::eval_type_flex`] then pins to Flex (`[flex]`) or Rigid. A
//! freshly *constructed* regional record is different: it is born Flex directly
//! (the checker's `record_ty`) — the live, region-local form, so it is
//! immediately assignable and freezes to Rigid on region exit
//! ([`Elaborator::freeze_region`] maps Flex/Regional → Rigid). Because `unify`
//! ignores flexivity, that Flex value still flows into a binding/return annotated
//! otherwise (e.g. `[rigid]`), where the annotation's coloring is what is read. A
//! `[field]` mutable link is wrapped in `Nullable` (nullable, and
//! reassignable in-region), so *assembling* (assigning) and *projecting*
//! (reading) those fields ride the `Nullable` machinery — and so
//! [`Elaborator::refine_flex`] looks through a `Nullable` head when pinning a
//! binding. Generics are left uncolored — their modality is resolved at
//! monomorphization.

use crate::semi::const_eval::{ConstScalar, ConstValue};
use crate::semi::infer::{Instantiation, shape::Dim};
use crate::semi::resolve::DefKind;
use crate::semi::traits::sync::{SyncEnv, SyncVerdict, wf_arc};
use crate::semi::ty::{DefId, Flexivity, FpTy, IntTy, Ty, TyKind};
use crate::surface::{self, FpType, IntegralType, TypeKind};

use super::ctxt::{DefaultCap, Elaborator, RecordFields};

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// Evaluate an exported or reusable declaration type. Dimension holes are
    /// local inference variables and cannot be retained across function contexts.
    pub fn eval_decl_type(&mut self, ty: &surface::Type) -> Ty<'tcx> {
        let mut holes = Vec::new();
        inferred_dimension_spans(ty, &mut holes);
        if holes.is_empty() {
            self.eval_type(ty)
        } else {
            for span in holes {
                self.error(Some(span), "inferred dimension `_` is only allowed in local type annotations and type arguments; use an explicit extent or `?` in a declaration");
            }
            // Do not allocate local inference IDs in a declaration, even during
            // error recovery: entering the next function resets their context.
            self.tcx.mk_unit()
        }
    }

    /// Declaration counterpart of [`Self::eval_type_flex`].
    pub fn eval_decl_type_flex(&mut self, ty: &surface::Type, is_flex: bool) -> Ty<'tcx> {
        let t = self.eval_decl_type(ty);
        self.refine_flex(t, is_flex)
    }

    /// Evaluate a surface type, coloring concrete regional records as
    /// `Regional` (later refined by [`Elaborator::eval_type_flex`]).
    pub fn eval_type(&mut self, ty: &surface::Type) -> Ty<'tcx> {
        let kind = ty.kind();
        match &kind {
            TypeKind::TypeIntegral(it) => self.tcx.mk_int(match it {
                IntegralType::Signed(w) => IntTy::Signed(*w),
                IntegralType::Unsigned(w) => IntTy::Unsigned(*w),
            }),
            TypeKind::TypeFp(fp) => self.tcx.mk_fp(match fp {
                FpType::Ieee(w) => FpTy::Ieee(*w),
                FpType::BFloat16 => FpTy::BFloat16,
                FpType::Float8 => FpTy::Float8,
            }),
            TypeKind::TypeBool => self.tcx.mk_bool(),
            TypeKind::TypeStr => self.tcx.mk_str(),
            TypeKind::TypeChar => self.tcx.mk_char(),
            TypeKind::TypeUnit => self.tcx.mk_unit(),
            TypeKind::TypeTuple(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|t| self.eval_type_flex(t, false))
                    .collect();
                self.tcx.mk_tuple(&fields)
            }
            TypeKind::TypeArrow(args, ret) => {
                let args: Vec<Ty> = args.iter().map(|a| self.eval_type(a)).collect();
                let ret = self.eval_type(ret);
                self.tcx.mk_closure(&args, ret)
            }
            TypeKind::TypeExpr(path, args) => self.eval_type_expr(path, args, ty.span()),
            TypeKind::TypeArray(elem, extents) => self.eval_array_type(elem, extents, ty.span()),
        }
    }

    fn eval_type_expr(
        &mut self,
        path: &surface::Path,
        args: &[surface::Type],
        span: surface::Span,
    ) -> Ty<'tcx> {
        let key = path.basename;

        // A bare generic parameter in scope.
        if path.segments.is_empty()
            && let Some(&generic) = self.generic_names.get(&key)
        {
            if !args.is_empty() {
                self.error(
                    Some(span),
                    format!("generic `{}` cannot take type arguments", self.sym(key)),
                );
            }
            return self.tcx.mk_generic(generic);
        }

        // `Self` inside an impl member: the applied target type. Contextual —
        // with no alias in scope, `Self` stays an ordinary identifier (the
        // language has no reserved words).
        if path.segments.is_empty()
            && let Some(alias) = self.self_alias
            && self.sym(key) == "Self"
        {
            if !args.is_empty() {
                self.error(Some(span), "`Self` takes no type arguments");
            }
            return alias;
        }

        // Expand `import` bindings before the builtin-name checks so an
        // imported builtin type former spells the same as the original.
        // Generics were checked first — a binding never shadows one.
        let expanded = self.expand_path(path);
        let path = expanded.as_ref().unwrap_or(path);
        let key = path.basename;

        // The prelude builtin type formers live in `core::intrinsic::cell`
        // (the cell kinds — the synchronization kinds are just special
        // cells) and `core::intrinsic::nullable` (`Nullable`); their bare
        // names are default-imported as a prelude, so a bare spelling
        // resolves here unless a user record of the same name shadows it —
        // the same precedence a real import would have. The canonical
        // qualified spelling always names the builtin (`core` is reserved).
        let bare_unshadowed = path.segments.is_empty() && self.defs.resolve_record(key).is_none();

        // The cell type constructors: `Cell<T>` is the plain get/set cell;
        // `RefCell<T>` the exclusive flavor with guarded read-modify-write;
        // `Atomic<T>`, `Mutex<T>`, `FlatLock<T>`, and `RwLock<T>` are the
        // synchronization kinds (thread-safety design §4), each with its own
        // element bound checked by [`Elaborator::report_cell_wf`].
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "cell"))
            && let Some(kind) = crate::semi::ty::CellKind::parse(self.sym(key))
        {
            let name = kind.surface_name();
            return match args {
                [inner] => {
                    let inner = self.eval_type(inner);
                    if self.records_complete {
                        // Same staging as `Arc` wf: the lock kinds' `Sync`
                        // element bound needs record members, so annotation
                        // sites evaluated before fields populate are
                        // re-checked by the post-populate sweep.
                        self.report_cell_wf(inner, kind, Some(span));
                    }
                    self.tcx.mk_cell(inner, kind)
                }
                _ => {
                    self.error(
                        Some(span),
                        format!("`{name}` takes exactly one type argument"),
                    );
                    self.tcx.mk(TyKind::Bottom)
                }
            };
        }

        // The built-in nullable type, `core::intrinsic::nullable::Nullable`.
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "nullable"))
            && self.sym(key) == "Nullable"
        {
            return match args {
                [inner] => {
                    let inner = self.eval_type(inner);
                    // A nullable only accepts a non-null pointer-like inner
                    // (rc/record/closure/…): it encodes the null case in the
                    // pointer's own representation, so a scalar or a nested
                    // nullable has no place to store the discriminant. Reject a
                    // *concretely* non-pointer inner now; generics/holes are left
                    // for the later groundness check, and records are the
                    // intended case.
                    if is_concretely_non_pointer(inner) {
                        self.error(
                            Some(span),
                            format!(
                                "`Nullable` inner type `{}` is not a pointer-like type",
                                self.ty_display(inner)
                            ),
                        );
                    }
                    self.tcx.mk_nullable(inner)
                }
                _ => {
                    self.error(Some(span), "`Nullable` takes exactly one type argument");
                    self.tcx.mk(TyKind::Bottom)
                }
            };
        }

        // The built-in atomic-rc coloring, `core::intrinsic::arc::Arc`.
        // `Arc<X>` is the shared rc box `X` — a `[shared]` record, an array,
        // or a closure — whose refcount is adjusted atomically, so the value
        // may cross threads; everything but the rc discipline is `X`'s own.
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "arc"))
            && self.sym(key) == "Arc"
        {
            return match args {
                [inner] => {
                    let inner = self.eval_type(inner);
                    // Only a shared rc box can be arc-colored: the box layout
                    // is the inner type's own, with atomic counting. A
                    // generic/hole/bottom inner is deferred to the
                    // instantiation check (it lands with `Arc` construction;
                    // until then a generic inner can only be grounded from an
                    // already-checked `Arc` annotation).
                    if let Some(kind) = self.arc_inner_rejection(inner) {
                        self.error(
                            Some(span),
                            format!(
                                "`Arc` inner type `{}` is not a `[shared]` record, array, or closure ({kind})",
                                self.ty_display(inner)
                            ),
                        );
                    } else if self.records_complete {
                        // Body-position annotations run after record fields
                        // populate, so the member (`Sync`) half of the wf
                        // check answers here. Signature and member positions
                        // evaluate earlier and blocked — the post-populate
                        // sweep re-checks them.
                        self.report_arc_wf(inner, Some(span));
                    }
                    self.tcx.mk_arc(inner)
                }
                _ => {
                    self.error(Some(span), "`Arc` takes exactly one type argument");
                    self.tcx.mk(TyKind::Bottom)
                }
            };
        }

        // The built-in constant sequence, `core::intrinsic::const_seq::ConstSeq`.
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "const_seq"))
            && self.sym(key) == "ConstSeq"
        {
            return match args {
                [elem] => {
                    let elem = self.eval_type(elem);
                    // A record element's fields decide: checked once they
                    // populate (`report_const_seq_wf`), as for cells.
                    if self.records_complete {
                        self.report_const_seq_wf(elem, Some(span));
                    }
                    self.tcx.mk_const_seq(elem)
                }
                _ => {
                    self.error(Some(span), "`ConstSeq` takes exactly one type argument");
                    self.tcx.mk(TyKind::Bottom)
                }
            };
        }

        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "tensor"))
            && self.sym(key) == "Token"
        {
            if !args.is_empty() {
                self.error(Some(span), "`Token` takes no type arguments");
            }
            return self.tcx.mk(TyKind::Token);
        }
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "tensor"))
            && self.sym(key) == "Future"
        {
            if args.is_empty() {
                self.error(
                    Some(span),
                    "`Future` takes one or more tensor type arguments",
                );
            }
            let payloads: Vec<_> = args.iter().map(|arg| self.eval_type(arg)).collect();
            for &payload in &payloads {
                if future_payload_rejection(payload).is_some() {
                    self.error(Some(span), "`Future` payload must be a Tensor type");
                }
            }
            return self.tcx.mk_future(&payloads);
        }

        // Tensor's shape syntax resembles an array but does not construct a
        // host array: shapes larger than the host allocation limit are valid. A scalar argument denotes a rank-zero tensor.
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "tensor"))
            && self.sym(key) == "Tensor"
        {
            return match args {
                [inner] => {
                    let (elem, dims) = match inner.kind() {
                        TypeKind::TypeArray(elem, extents) => {
                            let elem = self.eval_type(&elem);
                            let dims = extents
                                .iter()
                                .map(|extent| match extent {
                                    surface::ArrayExtent::Dynamic(_) => {
                                        Dim::Known(crate::semi::ty::DYNAMIC_EXTENT)
                                    }
                                    surface::ArrayExtent::Infer(_) => self.infer.new_dim(),
                                    surface::ArrayExtent::Expr(expr) => {
                                        let dim = self.eval_extent(expr);
                                        if dim > i64::MAX as u64 {
                                            self.error(
                                                Some(expr.span()),
                                                "tensor extent exceeds the signed 64-bit MLIR dimension limit",
                                            );
                                            Dim::Known(1)
                                        } else {
                                            Dim::Known(dim)
                                        }
                                    }
                                })
                                .collect::<Vec<_>>();
                            (elem, dims)
                        }
                        _ => (self.eval_type(inner), Vec::new()),
                    };
                    self.report_tensor_element(elem, Some(span));
                    if let Some(dims) = known_extents(&dims) {
                        self.tcx.mk_tensor(elem, &dims)
                    } else {
                        let shape = self.infer.ranked_shape(dims);
                        self.infer.new_tensor_ty(elem, shape)
                    }
                }
                _ => {
                    self.error(Some(span), "`Tensor` takes exactly one type argument");
                    self.tcx.mk(TyKind::Bottom)
                }
            };
        }

        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "pjrt"))
            && self.sym(key) == "DefaultPlacement"
        {
            if !args.is_empty() {
                self.error(Some(span), "`DefaultPlacement` takes no type arguments");
            }
            return self.tcx.mk(TyKind::DefaultPlacement);
        }
        if (bare_unshadowed || self.is_core_intrinsic_prefix(&path.segments, "pjrt"))
            && self.sym(key) == "DevArray"
        {
            if args.is_empty() || args.len() > 2 {
                self.error(
                    Some(span),
                    "`DevArray` takes an element/shape type and an optional placement type",
                );
                return self.tcx.mk(TyKind::Bottom);
            }
            let (elem, dims) = match args[0].kind() {
                TypeKind::TypeArray(elem, extents) => {
                    let elem = self.eval_type(&elem);
                    let mut dims = Vec::new();
                    for extent in extents {
                        dims.push(match extent {
                            surface::ArrayExtent::Dynamic(_) => {
                                Dim::Known(crate::semi::ty::DYNAMIC_EXTENT)
                            }
                            surface::ArrayExtent::Infer(_) => self.infer.new_dim(),
                            surface::ArrayExtent::Expr(e) => {
                                let n = self.eval_extent(&e);
                                if n > i64::MAX as u64 {
                                    self.error(
                                        Some(e.span()),
                                        "device array extent exceeds i64::MAX",
                                    );
                                }
                                Dim::Known(n.min(i64::MAX as u64))
                            }
                        });
                    }
                    (elem, dims)
                }
                _ => (self.eval_type(&args[0]), Vec::new()),
            };
            if !device_element_supported(elem) {
                self.error(
                    Some(span),
                    "`DevArray` elements must be integer, bool, f16, bf16, f32, or f64 scalars",
                );
            }
            let placement = args.get(1).map(|p| self.eval_type(p));
            if let Some(p) = placement {
                let valid = match *p.kind() {
                    TyKind::DefaultPlacement
                    | TyKind::Generic(_)
                    | TyKind::Hole(_)
                    | TyKind::Bottom => true,
                    TyKind::Record { def, args, .. } => {
                        args.is_empty()
                            && self
                                .records
                                .get(&def)
                                .is_some_and(|r| r.placement.is_some())
                    }
                    _ => false,
                };
                if !valid {
                    self.error(
                        Some(span),
                        "`DevArray` placement must be a `#[placement]` marker type",
                    );
                }
            }
            return if let Some(dims) = known_extents(&dims) {
                self.tcx.mk_devarray(elem, &dims, placement)
            } else {
                let shape = self.infer.ranked_shape(dims);
                let placement = placement.unwrap_or_else(|| self.tcx.mk(TyKind::DefaultPlacement));
                self.infer.new_devarray_ty(elem, shape, placement)
            };
        }

        // Nothing else under the reserved `core` package names a type.
        if path.segments.first().map(|&k| self.sym(k)) == Some("core") {
            let shown = self.path_display(path);
            self.error(
                Some(span),
                format!("the built-in `core` package has no type `{shown}`"),
            );
            return self.tcx.mk(TyKind::Bottom);
        }

        // A user record, resolved to its def — bare in the current module, or
        // module-qualified (`utils::math::Widget`, `root::…`, `super::…`).
        self.eval_record_type_expr(path, args, span, key)
    }

    /// [`arc_inner_rejection`] against this elaborator's record table.
    pub(crate) fn arc_inner_rejection(&self, t: Ty<'tcx>) -> Option<&'static str> {
        arc_inner_rejection(|def| self.records.get(&def).map(|r| r.default_cap), t)
    }

    /// [`array_element_rejection`] against this elaborator's record table.
    pub(crate) fn array_element_rejection(&self, t: Ty<'tcx>) -> Option<&'static str> {
        array_element_rejection(|def| self.records.get(&def).map(|r| r.default_cap), t)
    }

    pub(crate) fn report_tensor_element(&mut self, elem: Ty<'tcx>, span: Option<surface::Span>) {
        if tensor_element_rejection(elem).is_some() {
            self.error(
                span,
                format!(
                    "tensor element type `{}` must be a supported integer, floating-point, or bool scalar",
                    self.ty_display(elem)
                ),
            );
        }
    }

    /// Evaluate an array type (`[f64; 512]`, `[f64; 5, 16, 8]`,
    /// `[f64; ?, 4]`) into a [`TyKind::Array`]. A `?` extent marks the
    /// dimension dynamic ([`DYNAMIC_EXTENT`]): its runtime value travels as
    /// an operand of the constructing intrinsic, not in the type.
    pub(crate) fn eval_array_type(
        &mut self,
        elem: &surface::Type,
        extents: &[surface::ArrayExtent],
        span: surface::Span,
    ) -> Ty<'tcx> {
        if extents.is_empty() {
            return self.tcx.mk(TyKind::Bottom);
        }
        let elem = self.eval_type(elem);
        // Elements are plain scalars or shared rc boxes; anything whose
        // payload would need member-wise treatment through a view stays
        // rejected (a rejection names why).
        if let Some(why) = self.array_element_rejection(elem) {
            self.error(
                Some(span),
                format!(
                    "array element type `{}` is not supported yet ({why}); \
                     elements must be plain scalars or `[shared]` records",
                    self.ty_display(elem)
                ),
            );
        }
        let mut dims = Vec::with_capacity(extents.len());
        // The element-count cap applies to the static factor of the shape;
        // dynamic dimensions are bounded at construction, not here.
        let mut product: u128 = 1;
        for e in extents {
            match e {
                surface::ArrayExtent::Dynamic(_) => {
                    dims.push(Dim::Known(crate::semi::ty::DYNAMIC_EXTENT));
                }
                surface::ArrayExtent::Infer(_) => dims.push(self.infer.new_dim()),
                surface::ArrayExtent::Expr(e) => {
                    let extent = self.eval_extent(e);
                    if extent > i64::MAX as u64 {
                        self.error(Some(e.span()), "array extent exceeds i64::MAX");
                    }
                    product = product.saturating_mul(u128::from(extent));
                    dims.push(Dim::Known(extent));
                }
            }
        }
        if product > (1u128 << 32) {
            self.error(Some(span), "array is too large (more than 2^32 elements)");
        }
        if let Some(dims) = known_extents(&dims) {
            self.tcx.mk_array(elem, &dims)
        } else {
            let shape = self.infer.ranked_shape(dims);
            self.infer.new_array_ty(elem, shape)
        }
    }

    /// Evaluate one array or tensor extent expression as a `u64` constant
    /// ([`Self::eval_const`]). An invalid extent is reported and recovers as
    /// extent 1 (a *dynamic* dimension is spelled `?`, not an expression).
    fn eval_extent(&mut self, e: &surface::Expr) -> u64 {
        let u64_ty = self.tcx.mk_int(IntTy::Unsigned(64));
        match self.eval_const(e, u64_ty) {
            Some(ConstValue::Scalar(ConstScalar::Int(n))) => {
                u64::try_from(&n).expect("a `u64` constant fits `u64`")
            }
            _ => 1,
        }
    }

    fn eval_record_type_expr(
        &mut self,
        path: &surface::Path,
        args: &[surface::Type],
        span: surface::Span,
        key: reussir_syntax::kind::TokenKey,
    ) -> Ty<'tcx> {
        let Some(def) = self.resolve_record_ref(path) else {
            // A private record of a loaded extern package reports access,
            // not absence.
            if let Some(msg) = self.extern_private_msg(path, DefKind::Record) {
                self.error(Some(span), msg);
                return self.tcx.mk(TyKind::Bottom);
            }
            let hint = if path.segments.is_empty() {
                self.record_suggestion(key)
            } else {
                String::new()
            };
            let shown = self.path_display(path);
            self.error(Some(span), format!("unknown type `{shown}`{hint}"));
            return self.tcx.mk(TyKind::Bottom);
        };
        self.record_use(key);
        let default_cap = self.records[&def].default_cap;
        let args: Vec<Ty> = args.iter().map(|a| self.eval_type(a)).collect();
        let flex = match default_cap {
            DefaultCap::Value | DefaultCap::Shared => Flexivity::Irrelevant,
            DefaultCap::Regional => Flexivity::Regional,
        };
        self.tcx.mk_record(def, &args, flex)
    }

    /// Evaluate a type that carries a `[flex]` flag (a parameter, return type,
    /// or `let` binding). A regional record refines to `Flex` or `Rigid`.
    pub fn eval_type_flex(&mut self, ty: &surface::Type, is_flex: bool) -> Ty<'tcx> {
        let t = self.eval_type(ty);
        self.refine_flex(t, is_flex)
    }

    /// Refine a regional record's flexivity by a `[flex]` flag.
    pub fn refine_flex(&self, t: Ty<'tcx>, is_flex: bool) -> Ty<'tcx> {
        let refined = if is_flex {
            Flexivity::Flex
        } else {
            Flexivity::Rigid
        };
        match t.kind() {
            TyKind::Record {
                def,
                args,
                flex: Flexivity::Regional,
            } => self.tcx.mk_record(*def, args, refined),
            TyKind::Nullable(inner) => {
                if let TyKind::Record {
                    def,
                    args,
                    flex: Flexivity::Regional,
                } = inner.kind()
                {
                    let inner = self.tcx.mk_record(*def, args, refined);
                    self.tcx.mk_nullable(inner)
                } else {
                    t
                }
            }
            _ => t,
        }
    }

    /// Freeze a value escaping a region: peel any `Nullable` wrapper and turn a
    /// `Flex` (or still-unpinned `Regional`) record *head* into `Rigid`, the
    /// materializable frozen form.
    ///
    /// It re-colors only the head (through `Nullable` wrappers); it does *not*
    /// descend into a record's fields or type arguments. That is sound, not a
    /// shortcut: a `[field]` link is colored `Regional` (= inherit the
    /// container's flexivity), never independently `Flex`, so once the head is
    /// `Rigid` every link reads as frozen. The other field kinds — primitive,
    /// shared/value rc (`Irrelevant`), already-rigid rc — are flex-invariant.
    pub fn freeze_region(&self, t: Ty<'tcx>) -> Ty<'tcx> {
        match t.kind() {
            TyKind::Record {
                def,
                args,
                flex: Flexivity::Regional | Flexivity::Flex,
            } => self.tcx.mk_record(*def, args, Flexivity::Rigid),
            TyKind::Nullable(inner) => {
                let inner = self.freeze_region(*inner);
                self.tcx.mk_nullable(inner)
            }
            _ => t,
        }
    }
}

/// Is `t` *concretely* a non-pointer-like type — i.e. one that cannot be the
/// inner of a `Nullable` (which needs a pointer-like inner to encode the null
/// case)? Conservative: only the by-value scalars and a nested `Nullable` are
/// rejected; `Record`/`Closure`/`Array`/`Cell` are valid inners, and
/// `Generic`/`Hole`/`Bottom` are deferred (resolved/checked later) so this never
/// reports a false positive on a not-yet-ground type.
/// Whether `t` qualifies as an array element on the scalar side: a plain
/// scalar, or a generic/hole deferred to the monomorphization-time check.
pub(crate) fn is_plain_scalar_or_deferred(t: Ty<'_>) -> bool {
    matches!(
        t.kind(),
        TyKind::Int(_)
            | TyKind::Fp(_)
            | TyKind::Bool
            | TyKind::Char
            | TyKind::Generic(_)
            | TyKind::Hole(_)
            | TyKind::Bottom
    )
}

/// Why `t` cannot be an array element, or `None` when it can: a plain
/// scalar, or a shared rc box — a `[shared]` record (struct or enum), whose
/// payload slot is one pointer the acquire/drop expansions already walk.
/// Value records (member-wise rc traffic through a view), regional records
/// (region lifetimes), cells, closures, and `Arc` (atomic context through a
/// plain view) stay rejected. Generic/hole/bottom defer to the
/// monomorphization-time check, exactly like [`arc_inner_rejection`].
pub fn array_element_rejection<'tcx>(
    cap_of: impl FnOnce(DefId) -> Option<DefaultCap>,
    t: Ty<'tcx>,
) -> Option<&'static str> {
    if is_plain_scalar_or_deferred(t) {
        return None;
    }
    match t.kind() {
        TyKind::Record { def, flex, .. } => match flex {
            Flexivity::Flex | Flexivity::Rigid => Some("a regional record"),
            _ => match cap_of(*def) {
                Some(DefaultCap::Shared) | None => None,
                Some(DefaultCap::Value) => Some("a `[value]` record"),
                Some(DefaultCap::Regional) => Some("a `[regional]` record"),
            },
        },
        TyKind::Arc(_) => Some("an `Arc`"),
        TyKind::Cell { .. } => Some("a cell"),
        TyKind::Tuple(_) => {
            Some("an inline tuple (arrays currently require scalar or shared-record elements)")
        }
        TyKind::Closure { .. } => Some("a closure"),
        TyKind::Array { .. } => Some("a nested rc array"),
        TyKind::DevArray { .. } => Some("a device array"),
        TyKind::Tensor { .. } => Some("a tensor"),
        TyKind::Token | TyKind::Future(_) => Some("a kernel SSA value"),
        TyKind::Str => Some("a string"),
        _ => Some("not a scalar or `[shared]` record"),
    }
}

/// Why `t` cannot be the inner of an `Arc`, or `None` when it can — a shared
/// rc box: a `[shared]` record, an array, or a closure — or when the answer
/// must wait (generic/hole/bottom). `cap_of` looks up a record's default
/// capability; an unknown record defers rather than rejecting. Shared between
/// `ty_eval` (a concrete annotation) and monomorphization (the deferred half:
/// a generic inner is first grounded by an instantiation, e.g. a trampoline's
/// explicit type arguments).
/// Why `t` cannot be the element of an `Atomic` cell, or `None` when it can
/// (or when it is a generic/hole, deferred to the monomorphization
/// backstop): the dialect stores the element as an inline atomic scalar, so
/// only integer and floating-point primitives qualify (every supported fp
/// width is a power of two).
pub fn atomic_cell_element_rejection(t: Ty<'_>) -> Option<&'static str> {
    match *t.kind() {
        TyKind::Int(IntTy::Signed(w) | IntTy::Unsigned(w)) => {
            if w.is_power_of_two() && w >= 8 {
                None
            } else {
                Some("an integer whose width is not a power of two of at least 8 bits")
            }
        }
        TyKind::Fp(_) => None,
        TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => None,
        TyKind::Bool => Some("`bool` (use an 8-bit integer)"),
        TyKind::Char => Some("`char` (use a 32-bit integer)"),
        _ => Some("not an integer or floating-point primitive"),
    }
}

/// Why `t` cannot sit in a lock-guarded cell's slot, or `None` when it can
/// (or when the answer waits for monomorphization): the slot is a memref
/// element, so it stores a primitive or an rc pointer — inline aggregates
/// and nullable wrappers do not qualify. (The `Sync` element bound is
/// checked separately; this is only the storage-shape half.)
/// Inline tuple slots are immutable values. Live regional references cannot
/// be stored in them; deferred generic slots are checked again after grounding.
pub fn tuple_element_rejection(ty: Ty<'_>) -> Option<&'static str> {
    let unfrozen = match ty.kind() {
        TyKind::Record {
            flex: Flexivity::Flex | Flexivity::Regional,
            ..
        } => true,
        TyKind::Nullable(inner) => tuple_element_rejection(*inner).is_some(),
        TyKind::Tuple(fields) => fields.iter().any(|&t| tuple_element_rejection(t).is_some()),
        _ => false,
    };
    unfrozen.then_some("a tuple cannot store an unfrozen regional value; freeze the element before constructing the tuple")
}

pub fn lock_cell_slot_rejection<'tcx>(
    cap_of: impl FnOnce(DefId) -> Option<DefaultCap>,
    t: Ty<'tcx>,
) -> Option<&'static str> {
    match *t.kind() {
        TyKind::Record { def, .. } => match cap_of(def) {
            Some(DefaultCap::Value) => {
                Some("a `[value]` record, which is stored inline rather than as a pointer")
            }
            _ => None,
        },
        TyKind::Tuple(_) => {
            Some("an inline tuple, which is stored inline rather than as a pointer")
        }
        TyKind::Nullable(_) => Some("a nullable, whose null case has no pointer to store"),
        TyKind::Str => Some("a `str`"),
        TyKind::Unit => Some("the unit type"),
        _ => None,
    }
}

pub fn arc_inner_rejection<'tcx>(
    cap_of: impl FnOnce(DefId) -> Option<DefaultCap>,
    t: Ty<'tcx>,
) -> Option<&'static str> {
    match t.kind() {
        TyKind::Record { def, .. } => match cap_of(*def) {
            Some(DefaultCap::Shared) | None => None,
            Some(DefaultCap::Value) => Some("a `[value]` record"),
            Some(DefaultCap::Regional) => Some("a `[regional]` record"),
        },
        // An array or a closure is one shared rc box exactly like a
        // `[shared]` record, so it takes the atomic coloring the same way.
        TyKind::Array { .. } | TyKind::DevArray { .. } | TyKind::Closure { .. } => None,
        TyKind::Tensor { .. } => Some("a tensor is scope-confined and cannot be arc'd"),
        TyKind::Future(_) | TyKind::Token => {
            Some("a kernel SSA value is scope-confined and cannot be arc'd")
        }
        TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => None,
        // A cell is a shared box too, but synchronization is the cell's own
        // axis (plain vs exclusive, and the atomic flavor at the MLIR level)
        // — `Arc` must not stack a second discipline on top.
        TyKind::Cell { .. } => Some("a cell"),
        TyKind::Arc(_) => Some("already an `Arc`"),
        TyKind::Nullable(_) => Some("a nullable"),
        // Scalars, `str`, unit: not rc-managed boxes at all.
        _ => Some("not an rc box"),
    }
}

fn is_concretely_non_pointer(t: Ty<'_>) -> bool {
    matches!(
        t.kind(),
        TyKind::Tuple(_)
            | TyKind::Token
            | TyKind::Future(_)
            | TyKind::Tensor { .. }
            | TyKind::DefaultPlacement
            | TyKind::Int(_)
            | TyKind::Fp(_)
            | TyKind::Bool
            | TyKind::Str
            | TyKind::Char
            | TyKind::Unit
            | TyKind::Nullable(_)
    )
}

//===------------------------------------------------------------------===//
// Structural `Sync` over the elaborator's tables
//===------------------------------------------------------------------===//

/// [`SyncEnv`] over the elaborator: record capabilities from the collected
/// table, member types instantiated through the inference context.
pub(crate) struct ElabSyncEnv<'e, 'a, 'tcx> {
    pub el: &'e Elaborator<'a, 'tcx>,
}

impl<'tcx> SyncEnv<'tcx> for ElabSyncEnv<'_, '_, 'tcx> {
    fn default_cap(&self, def: DefId) -> Option<DefaultCap> {
        self.el.records.get(&def).map(|r| r.default_cap)
    }

    fn members(&self, def: DefId, args: &'tcx [Ty<'tcx>]) -> Option<Vec<(String, Ty<'tcx>)>> {
        let rec = self.el.records.get(&def)?;
        let fields = rec.fields.as_ref()?;
        let inst = Instantiation::from_pairs(
            rec.ty_params
                .iter()
                .map(|(_, g)| *g)
                .zip(args.iter().copied()),
        );
        let subst = |t: Ty<'tcx>| self.el.infer.instantiate_ty(t, &inst);
        Some(match fields {
            RecordFields::Named(fs) => fs
                .iter()
                .map(|(n, t, _, _)| (self.el.sym(*n).to_owned(), subst(*t)))
                .collect(),
            RecordFields::Unnamed(fs) => fs
                .iter()
                .enumerate()
                .map(|(i, (t, _, _))| (i.to_string(), subst(*t)))
                .collect(),
            RecordFields::Variants(vs) => vs
                .iter()
                .flat_map(|v| {
                    let vname = self.el.sym(v.name);
                    v.fields
                        .iter()
                        .enumerate()
                        .map(move |(i, t)| (format!("{vname}.{i}"), *t))
                })
                .map(|(label, t)| (label, subst(t)))
                .collect(),
            // The foreign payload is invisible; `foreign` refutes before
            // members are ever consulted.
            RecordFields::Opaque => Vec::new(),
        })
    }

    fn member_record_defs(&self, def: DefId) -> Option<Vec<DefId>> {
        let rec = self.el.records.get(&def)?;
        let fields = rec.fields.as_ref()?;
        let mut out = Vec::new();
        let mut walk = |t: Ty<'tcx>| crate::semi::traits::sync::collect_record_defs(t, &mut out);
        match fields {
            RecordFields::Named(fs) => fs.iter().for_each(|(_, t, _, _)| walk(*t)),
            RecordFields::Unnamed(fs) => fs.iter().for_each(|(t, _, _)| walk(*t)),
            RecordFields::Variants(vs) => vs
                .iter()
                .for_each(|v| v.fields.iter().for_each(|t| walk(*t))),
            RecordFields::Opaque => {}
        }
        Some(out)
    }

    fn foreign(&self, def: DefId) -> bool {
        self.el.records.get(&def).is_some_and(|r| r.ffi.is_some())
    }
}

impl<'a, 'tcx> Elaborator<'a, 'tcx> {
    /// The recursive group (SCC) of `def` over the record member-reference
    /// graph — the defs whose bare shared links re-color under an `Arc` of
    /// any group member (§3.3). Empty when fields are not populated yet (the
    /// promotion then simply does not fire; mono re-derives it ground).
    pub(crate) fn arc_recursive_group(&self, def: DefId) -> rustc_hash::FxHashSet<DefId> {
        crate::semi::traits::sync::recursive_group(&ElabSyncEnv { el: self }, def)
            .unwrap_or_default()
    }

    /// §3.3 SCC promotion of a member/field type read or written through an
    /// arc'd box: a bare shared record head in `group` becomes its own `Arc`
    /// (through a `Nullable` wrapper); everything else is untouched. This is
    /// the typing half of the whole-subtree rule — representationally the
    /// declaration stays bare and the box's atomic context derives the links.
    pub(crate) fn arc_promote_member_ty(
        &self,
        group: &rustc_hash::FxHashSet<DefId>,
        ty: Ty<'tcx>,
    ) -> Ty<'tcx> {
        match *ty.kind() {
            TyKind::Record { def, .. }
                if group.contains(&def)
                    && self
                        .records
                        .get(&def)
                        .is_some_and(|r| r.default_cap == DefaultCap::Shared) =>
            {
                self.tcx.mk_arc(ty)
            }
            TyKind::Nullable(inner) => {
                let promoted = self.arc_promote_member_ty(group, inner);
                if promoted == inner {
                    ty
                } else {
                    self.tcx.mk_nullable(promoted)
                }
            }
            _ => ty,
        }
    }

    /// Report an ill-formed `Arc<inner>` whose kind is admissible but whose
    /// contents are not `Sync`. Call only where records are complete (the
    /// post-populate sweep and body checking); a blocked verdict defers to
    /// the monomorphization backstop.
    /// Report an ill-formed cell type per its kind's element bound
    /// (thread-safety design §4.2). `Atomic` demands an integer or
    /// floating-point primitive; the lock-guarded kinds demand a
    /// pointer-or-primitive slot shape and a `Sync` element. Concrete
    /// violations report here; generic elements defer to the
    /// monomorphization backstop. Plain and exclusive cells have no bound.
    pub(crate) fn report_cell_wf(
        &mut self,
        elem: Ty<'tcx>,
        kind: crate::semi::ty::CellKind,
        span: Option<surface::Span>,
    ) {
        use crate::semi::traits::sync::sync_verdict;
        use crate::semi::ty::CellKind;
        if crate::semi::ctxt::contains_kernel_value(elem) {
            self.error(span, "a cell cannot contain a scope-confined kernel value");
            return;
        }
        match kind {
            CellKind::Plain | CellKind::Exclusive => {}
            CellKind::Atomic => {
                if let Some(what) = atomic_cell_element_rejection(elem) {
                    self.error(
                        span,
                        format!(
                            "`Atomic<{}>` is ill-formed: the element is {what}",
                            self.ty_display(elem)
                        ),
                    );
                }
            }
            CellKind::Mutex | CellKind::Flatlock | CellKind::Rwlock => {
                let name = kind.surface_name();
                if let Some(what) = lock_cell_slot_rejection(
                    |def| self.records.get(&def).map(|r| r.default_cap),
                    elem,
                ) {
                    self.error(
                        span,
                        format!(
                            "`{name}<{}>` is ill-formed: the element is {what}",
                            self.ty_display(elem)
                        ),
                    );
                    return;
                }
                if let SyncVerdict::NotSync(w) = sync_verdict(&ElabSyncEnv { el: self }, elem) {
                    self.error(
                        span,
                        format!(
                            "`{name}<{}>` is ill-formed: {}; the element of a \
                             lock-guarded cell must be `Sync` — the lock protects \
                             the stored slot, while clones of the element leave \
                             the critical section",
                            self.ty_display(elem),
                            w.describe()
                        ),
                    );
                }
            }
        }
    }

    pub(crate) fn report_arc_wf(&mut self, inner: Ty<'tcx>, span: Option<surface::Span>) {
        if self.arc_inner_rejection(inner).is_some() {
            // The kind gate owns this error at its own site.
            return;
        }
        if let SyncVerdict::NotSync(w) = wf_arc(&ElabSyncEnv { el: self }, inner) {
            self.error(
                span,
                format!(
                    "`Arc<{}>` is ill-formed: {}; every member of an `Arc` inner must be `Sync`",
                    self.ty_display(inner),
                    w.describe()
                ),
            );
        }
    }

    /// Walk a signature or member type and wf-check every `Arc` node inside
    /// it. Function prototypes are scanned before record fields populate, so
    /// their annotation-site checks blocked; the post-populate sweep re-runs
    /// them through this walker (mono's `check_arc_inners` is the ground
    /// backstop for what still blocks here, e.g. generic inners).
    pub(crate) fn sweep_arc_wf(&mut self, ty: Ty<'tcx>, span: Option<surface::Span>) {
        match *ty.kind() {
            TyKind::DevArray {
                elem, placement, ..
            } => {
                self.sweep_arc_wf(elem, span);
                self.sweep_arc_wf(placement, span);
            }
            TyKind::Arc(inner) => {
                self.report_arc_wf(inner, span);
                self.sweep_arc_wf(inner, span);
            }
            TyKind::Record { args, .. } | TyKind::Tuple(args) => {
                for &arg in args {
                    self.sweep_arc_wf(arg, span);
                }
            }
            TyKind::Closure { params, ret } => {
                for &p in params {
                    self.sweep_arc_wf(p, span);
                }
                self.sweep_arc_wf(ret, span);
            }
            TyKind::Nullable(inner) => self.sweep_arc_wf(inner, span),
            TyKind::Cell { elem, kind } => {
                self.report_cell_wf(elem, kind, span);
                self.sweep_arc_wf(elem, span);
            }
            TyKind::Future(ps) => {
                for &p in ps {
                    if future_payload_rejection(p).is_some() {
                        self.error(span, "`Future` payload must be a Tensor type");
                    }
                    self.sweep_arc_wf(p, span);
                }
            }
            TyKind::Array { elem, .. } | TyKind::Tensor { elem, .. } => {
                self.sweep_arc_wf(elem, span)
            }
            TyKind::ConstSeq(elem) => {
                self.report_const_seq_wf(elem, span);
                self.sweep_arc_wf(elem, span);
            }
            _ => {}
        }
    }

    /// Report an ill-formed `ConstSeq<elem>`: the element must be a constant
    /// type ([`Elaborator::check_const_ty`]), which needs record members, so
    /// this runs once they populate (like [`Self::report_cell_wf`]). A
    /// generic part defers the check to monomorphization. An element that is
    /// itself a sequence is reported by that sequence's own check.
    pub(crate) fn report_const_seq_wf(&mut self, elem: Ty<'tcx>, span: Option<surface::Span>) {
        if matches!(elem.kind(), TyKind::ConstSeq(_)) {
            return;
        }
        if let Err(part) = self.check_const_ty(elem)
            && !matches!(
                part.ty.kind(),
                TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom
            )
        {
            let found = self.non_const_ty_display(elem, &part);
            self.error(
                span,
                format!(
                    "`ConstSeq` requires a constant element type ({}), found {found}",
                    crate::semi::const_eval::CONST_TYS
                ),
            );
        }
    }
}

/// Collect source dimension holes recursively, including those hidden in
/// function types, structural products, and applications of named type formers.
fn inferred_dimension_spans(ty: &surface::Type, out: &mut Vec<surface::Span>) {
    match ty.kind() {
        TypeKind::TypeArray(elem, extents) => {
            inferred_dimension_spans(&elem, out);
            out.extend(extents.iter().filter_map(|extent| match extent {
                surface::ArrayExtent::Infer(span) => Some(*span),
                _ => None,
            }));
        }
        TypeKind::TypeTuple(fields) => {
            for field in fields {
                inferred_dimension_spans(&field, out);
            }
        }
        TypeKind::TypeExpr(_, args) => {
            for arg in args {
                inferred_dimension_spans(&arg, out);
            }
        }
        TypeKind::TypeArrow(args, ret) => {
            for arg in args {
                inferred_dimension_spans(&arg, out);
            }
            inferred_dimension_spans(&ret, out);
        }
        TypeKind::TypeIntegral(_)
        | TypeKind::TypeFp(_)
        | TypeKind::TypeBool
        | TypeKind::TypeStr
        | TypeKind::TypeChar
        | TypeKind::TypeUnit => {}
    }
}

/// Preserve the canonical type representation for fully specified shapes.
/// Inference dimensions live only in InferCtxt; they are never encoded as a
/// special integer extent or confused with the runtime dynamic marker.
fn known_extents(dims: &[Dim]) -> Option<Vec<u64>> {
    dims.iter()
        .map(|dim| match dim {
            Dim::Known(value) => Some(*value),
            _ => None,
        })
        .collect()
}

/// PJRT scalar dtypes supported by the frontend. Generic elements are checked after substitution.
pub fn device_element_supported(ty: Ty<'_>) -> bool {
    matches!(
        ty.kind(),
        TyKind::Int(IntTy::Signed(8 | 16 | 32 | 64) | IntTy::Unsigned(8 | 16 | 32 | 64))
            | TyKind::Bool
            | TyKind::Fp(
                crate::semi::ty::FpTy::Ieee(16 | 32 | 64) | crate::semi::ty::FpTy::BFloat16
            )
            | TyKind::Generic(_)
            | TyKind::Hole(_)
            | TyKind::Bottom
    )
}

/// StableHLO futures contain ranked tensors, never tokens, futures, or host
/// values. Generic payloads defer this obligation until substitution.
pub fn future_payload_rejection(payload: Ty<'_>) -> Option<&'static str> {
    match payload.kind() {
        TyKind::Tensor { .. } | TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => None,
        _ => Some("future payload must be a Tensor type"),
    }
}

/// Tensor elements have a native numeric MLIR representation. Unknown generic
/// elements are checked again after substitution; pointers and aggregates never
/// become tensor elements. `f8` has no specified encoding in the source language.
pub fn tensor_element_rejection(elem: Ty<'_>) -> Option<&'static str> {
    match elem.kind() {
        TyKind::Int(IntTy::Signed(8 | 16 | 32 | 64) | IntTy::Unsigned(8 | 16 | 32 | 64))
        | TyKind::Bool
        | TyKind::Fp(FpTy::Ieee(16 | 32 | 64) | FpTy::BFloat16)
        | TyKind::Generic(_)
        | TyKind::Hole(_)
        | TyKind::Bottom => None,
        _ => Some("a supported integer, floating-point, or bool scalar is required"),
    }
}

/// Types that can cross an internal kernel/helper boundary. Unknown generic
/// arguments are checked again after specialization; host resources never
/// acquire kernel semantics through an identity function. Keep this exhaustive
/// so adding an SSA type requires an explicit decision here.
pub fn kernel_type_rejection(ty: Ty<'_>) -> Option<&'static str> {
    match *ty.kind() {
        TyKind::Tuple(fields) => fields.iter().find_map(|&f| kernel_type_rejection(f)),
        TyKind::Token => None,
        TyKind::Future(payloads) => {
            if payloads.is_empty() {
                Some("Future requires one or more Tensor payloads")
            } else {
                payloads.iter().find_map(|&payload| {
                    future_payload_rejection(payload).or_else(|| kernel_type_rejection(payload))
                })
            }
        }
        TyKind::Tensor { elem, dims } => {
            if dims
                .iter()
                .any(|&d| d != crate::semi::ty::DYNAMIC_EXTENT && d > i64::MAX as u64)
            {
                Some("tensor dimensions must fit signed 64-bit MLIR extents")
            } else {
                tensor_element_rejection(elem)
            }
        }
        TyKind::Int(_) | TyKind::Fp(_) | TyKind::Bool => tensor_element_rejection(ty),
        TyKind::Unit | TyKind::Generic(_) | TyKind::Hole(_) | TyKind::Bottom => None,
        TyKind::Record { .. }
        | TyKind::Str
        | TyKind::Char
        | TyKind::Closure { .. }
        | TyKind::Array { .. }
        | TyKind::DefaultPlacement
        | TyKind::DevArray { .. }
        | TyKind::Cell { .. }
        | TyKind::Nullable(_)
        | TyKind::ConstSeq(_)
        | TyKind::Arc(_) => Some(
            "kernel values must be tensors, futures, tokens, supported numeric/bool scalars, or unit",
        ),
    }
}

#[cfg(test)]
mod dimension_annotation_tests {
    use super::*;

    #[test]
    fn local_dimension_annotations_elaborate_without_exporting_holes() {
        let source =
            include_str!("../../../../tests/integration/frontend/shape_annotation_types.rr");
        crate::with_tcx(|tcx| {
            let interner = std::sync::Arc::new(reussir_syntax::new_threaded_interner());
            let parsed = reussir_syntax::parse_with_interner(source, interner.clone());
            assert!(parsed.ok(), "{:?}", parsed.errors);
            let program = surface::program(&parsed.root);
            let elab = crate::semi::elaborate(tcx, &program, &interner);
            assert!(!elab.has_errors(), "{:#?}", elab.reports);
        });
    }

    #[test]
    fn declaration_holes_are_found_inside_all_type_constructors() {
        let source = "fn f(x: (Tensor<[f32; _, ?]>, ([i32; _],) -> DevArray<[f32; 4, _]>)) {}";
        let parsed = reussir_syntax::parse(source);
        assert!(parsed.ok(), "{:?}", parsed.errors);
        let program = surface::program(&parsed.root);
        let surface::StmtKind::Function(function) = program[0].kind() else {
            panic!("expected function");
        };
        let mut holes = Vec::new();
        inferred_dimension_spans(&function.params[0].1, &mut holes);
        assert_eq!(holes.len(), 3);
        for span in holes {
            assert_eq!(&source[span.start as usize..span.end as usize], "_");
        }
    }

    #[test]
    fn runtime_dimensions_and_fixed_shapes_are_valid_declaration_syntax() {
        let source = "fn f(x: (Tensor<[f32; ?, 4]>, [i32; 2], DevArray<[f32; ?, ?]>)) {}";
        let parsed = reussir_syntax::parse(source);
        assert!(parsed.ok(), "{:?}", parsed.errors);
        let program = surface::program(&parsed.root);
        let surface::StmtKind::Function(function) = program[0].kind() else {
            panic!("expected function");
        };
        let mut holes = Vec::new();
        inferred_dimension_spans(&function.params[0].1, &mut holes);
        assert!(holes.is_empty());
    }
}
