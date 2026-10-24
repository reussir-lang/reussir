//! Language items: the declarations the compiler itself must know.
//!
//! A lang item ties a name the compiler wires into checking (a numeric
//! tower trait, an operator trait, the `Ordering` enum) to an actual
//! declaration. The declaration comes from one of two places, in
//! precedence order:
//!
//! 1. **Source or a loaded interface** carrying `#[lang("…")]` — how the
//!    `core` package hands the compiler its declarations, complete with
//!    real source locations for diagnostics, debug info, and the LSP.
//! 2. **The compiler's fallback table** (`builtin_traits!`) — what a
//!    session gets when no `core` is compiled against: unit tests, bare
//!    `rrc file.rr`, the REPL today. Fallback items carry no source
//!    location until their declarations migrate into `core`.
//!
//! The wired numeric/marker tower is fallback-registered in every session
//! for now; migrated items will suppress their fallback entry instead
//! (a later PR in this series).

use rustc_hash::FxHashMap;

use crate::feature::Feature;
use crate::semi::traits::TraitId;
use crate::semi::traits::builtins::Builtins;
use crate::semi::ty::DefId;

/// A `core::intrinsic` operation, as a lang item: the marker on the
/// prototype `core` declares for it, so the operation has a real
/// declaration site (locations for diagnostics, debug info, the LSP)
/// while checking and lowering stay wired in the compiler.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IntrinsicItem {
    Panic,
    PjrtLaunch,
    Math(crate::intrinsic::MathFn),
    Array(crate::intrinsic::ArrayFn),
    Tensor(crate::intrinsic::TensorFn),
    Cell(crate::intrinsic::CellFn),
    Str(crate::intrinsic::StrFn),
    ConstSeq(crate::intrinsic::ConstSeqFn),
    Pjrt(crate::intrinsic::PjrtFn),
}

impl IntrinsicItem {
    pub fn family(self) -> &'static str {
        match self {
            IntrinsicItem::Panic => "panic",
            IntrinsicItem::PjrtLaunch => "pjrt",
            IntrinsicItem::Math(_) => "math",
            IntrinsicItem::Array(_) => "array",
            IntrinsicItem::Tensor(_) => "tensor",
            IntrinsicItem::Cell(_) => "cell",
            IntrinsicItem::Str(_) => "str",
            IntrinsicItem::ConstSeq(_) => "const_seq",
            IntrinsicItem::Pjrt(_) => "pjrt",
        }
    }

    pub fn op_name(self) -> &'static str {
        match self {
            IntrinsicItem::Panic => "panic",
            IntrinsicItem::PjrtLaunch => "launch",
            IntrinsicItem::Math(f) => f.as_str(),
            IntrinsicItem::Array(f) => f.as_str(),
            IntrinsicItem::Tensor(f) => f.as_str(),
            IntrinsicItem::Cell(f) => f.as_str(),
            IntrinsicItem::Str(f) => f.as_str(),
            IntrinsicItem::ConstSeq(f) => f.as_str(),
            IntrinsicItem::Pjrt(f) => f.as_str(),
        }
    }

    /// The optional feature a program needs to call this intrinsic.
    pub fn required_feature(self) -> Option<Feature> {
        match self {
            IntrinsicItem::PjrtLaunch | IntrinsicItem::Tensor(_) => Some(Feature::OpenXla),
            _ => None,
        }
    }

    pub(crate) fn from_parts(family: &str, name: &str) -> Option<IntrinsicItem> {
        match family {
            "panic" if name == "panic" => Some(IntrinsicItem::Panic),
            "pjrt" if name == "launch" => Some(IntrinsicItem::PjrtLaunch),
            "math" => crate::intrinsic::MathFn::parse(name).map(IntrinsicItem::Math),
            "array" => crate::intrinsic::ArrayFn::parse(name).map(IntrinsicItem::Array),
            "tensor" => crate::intrinsic::TensorFn::parse(name).map(IntrinsicItem::Tensor),
            "cell" => crate::intrinsic::CellFn::parse(name).map(IntrinsicItem::Cell),
            "str" => crate::intrinsic::StrFn::parse(name).map(IntrinsicItem::Str),
            "const_seq" => crate::intrinsic::ConstSeqFn::parse(name).map(IntrinsicItem::ConstSeq),
            "pjrt" => crate::intrinsic::PjrtFn::parse(name).map(IntrinsicItem::Pjrt),
            _ => None,
        }
    }
}

/// A built-in type *former* — `Arc`, `Nullable`, `ConstSeq`, `Tensor`, and the cell family —
/// as a lang item: the marker on the anchor record `core` declares for
/// it. The former itself stays wired (its canonical
/// `core::intrinsic::…` spelling and bare prelude name resolve in
/// `ty_eval` by spelling, never through the anchor); the anchor supplies
/// the declaration site for diagnostics, debug info, and the LSP.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FormerItem {
    Tensor,
    TensorFuture,
    TensorToken,
    Arc,
    Nullable,
    ConstSeq,
    DevArray,
    DefaultPlacement,
    /// One per [`crate::semi::ty::CellKind`].
    Cell,
    RefCell,
    Atomic,
    Mutex,
    FlatLock,
    RwLock,
}

impl FormerItem {
    pub const ALL: [FormerItem; 14] = [
        FormerItem::Tensor,
        FormerItem::TensorFuture,
        FormerItem::TensorToken,
        FormerItem::Arc,
        FormerItem::Nullable,
        FormerItem::ConstSeq,
        FormerItem::DevArray,
        FormerItem::DefaultPlacement,
        FormerItem::Cell,
        FormerItem::RefCell,
        FormerItem::Atomic,
        FormerItem::Mutex,
        FormerItem::FlatLock,
        FormerItem::RwLock,
    ];

    /// Declaration-anchor arity. Future declares its minimum payload; the
    /// intrinsic source former accepts one or more payload arguments.
    pub fn arity(self) -> usize {
        match self {
            FormerItem::DevArray => 2,
            FormerItem::DefaultPlacement | FormerItem::TensorToken => 0,
            _ => 1,
        }
    }

    /// The optional feature a program needs to name this former.
    pub fn required_feature(self) -> Option<Feature> {
        match self {
            FormerItem::TensorFuture | FormerItem::TensorToken => Some(Feature::OpenXla),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            FormerItem::Tensor => "tensor",
            FormerItem::TensorFuture => "tensor_future",
            FormerItem::TensorToken => "tensor_token",
            FormerItem::Arc => "arc",
            FormerItem::Nullable => "nullable",
            FormerItem::ConstSeq => "const_seq",
            FormerItem::DevArray => "dev_array",
            FormerItem::DefaultPlacement => "default_placement",
            FormerItem::Cell => "cell",
            FormerItem::RefCell => "ref_cell",
            FormerItem::Atomic => "atomic",
            FormerItem::Mutex => "mutex",
            FormerItem::FlatLock => "flat_lock",
            FormerItem::RwLock => "rw_lock",
        }
    }
}

/// Every language item the compiler recognizes. Closed: an unknown
/// `#[lang("…")]` name is a diagnostic, so the set is versioned with the
/// compiler.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum LangItem {
    /// The numeric/marker tower — wired, fallback-registered today.
    Num,
    Integral,
    FloatingPoint,
    PtrLike,
    Sync,
    /// The comparison tower — declared by `core` (a later PR in this
    /// series); the slots exist so its source can land without a compiler
    /// release in between.
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    /// `core::cmp::Ordering`, the result of `Ord::cmp`.
    Ordering,
    /// A `core::intrinsic` operation's prototype
    /// (`#[lang("core::intrinsic::math::sqrt")]`).
    Intrinsic(IntrinsicItem),
    /// A built-in type former's anchor record (`#[lang("arc")]`).
    Former(FormerItem),
}

/// What kind of declaration a lang item must be.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LangItemKind {
    Trait,
    Record,
    Function,
}

impl LangItem {
    /// The wired numeric/marker tower, in `Builtins` field order — the
    /// items whose [`LangItems`] fields repoint when a declaration binds.
    pub const TOWER: [LangItem; 5] = [
        LangItem::Num,
        LangItem::Integral,
        LangItem::FloatingPoint,
        LangItem::PtrLike,
        LangItem::Sync,
    ];

    /// The fixed (non-intrinsic) items, for enumeration.
    pub const FIXED: [LangItem; 10] = [
        LangItem::Num,
        LangItem::Integral,
        LangItem::FloatingPoint,
        LangItem::PtrLike,
        LangItem::Sync,
        LangItem::PartialEq,
        LangItem::Eq,
        LangItem::PartialOrd,
        LangItem::Ord,
        LangItem::Ordering,
    ];

    /// The attribute spelling: `#[lang("partial_eq")]`,
    /// `#[lang("core::intrinsic::math::sqrt")]`.
    pub fn name(self) -> String {
        match self {
            LangItem::Num => "num".into(),
            LangItem::Integral => "integral".into(),
            LangItem::FloatingPoint => "floating_point".into(),
            LangItem::PtrLike => "ptr_like".into(),
            LangItem::Sync => "sync".into(),
            LangItem::PartialEq => "partial_eq".into(),
            LangItem::Eq => "eq".into(),
            LangItem::PartialOrd => "partial_ord".into(),
            LangItem::Ord => "ord".into(),
            LangItem::Ordering => "ordering".into(),
            LangItem::Intrinsic(op) => {
                format!("core::intrinsic::{}::{}", op.family(), op.op_name())
            }
            LangItem::Former(f) => f.name().into(),
        }
    }

    pub fn from_name(name: &str) -> Option<LangItem> {
        if let Some(rest) = name.strip_prefix("core::intrinsic::") {
            let (family, op) = rest.split_once("::")?;
            return IntrinsicItem::from_parts(family, op).map(LangItem::Intrinsic);
        }
        if let Some(former) = FormerItem::ALL.into_iter().find(|f| f.name() == name) {
            return Some(LangItem::Former(former));
        }
        Self::FIXED.into_iter().find(|item| item.name() == name)
    }

    /// The comparison tower's traits, in declaration (and fallback
    /// registration) order. `Ordering` is a record, not part of this set.
    pub const CMP_TRAITS: [LangItem; 4] = [
        LangItem::PartialEq,
        LangItem::Eq,
        LangItem::PartialOrd,
        LangItem::Ord,
    ];

    pub fn is_cmp_trait(self) -> bool {
        Self::CMP_TRAITS.contains(&self)
    }

    pub fn kind(self) -> LangItemKind {
        match self {
            LangItem::Ordering | LangItem::Former(_) => LangItemKind::Record,
            LangItem::Intrinsic(_) => LangItemKind::Function,
            _ => LangItemKind::Trait,
        }
    }
}

/// The session's lang-item registry: the wired tower ids plus every
/// `#[lang]`-declared item. Lives on the elaborator; declarations are
/// insertion-ordered so the REPL checkpoint is a plain length and a
/// rejected batch retracts exactly its own (the same truncate-and-retain
/// scheme every other elaborator table uses).
pub struct LangItems {
    pub num: TraitId,
    pub integral: TraitId,
    pub floating_point: TraitId,
    pub ptr_like: TraitId,
    /// Marker (auto) trait: a value may be reached from any thread,
    /// concurrently; answered structurally, never by impl search.
    pub sync: TraitId,
    /// The construction-time builtin ids, in [`LangItem::TOWER`] order —
    /// retained after the wired fields repoint, so bare-name resolution
    /// can recognize (and substitute) a builtin hit.
    builtin: [TraitId; 5],
    declared: Vec<(LangItem, DefId)>,
}

impl LangItems {
    /// A registry over the fallback-registered tower.
    pub fn new(fallback: Builtins) -> Self {
        LangItems {
            num: fallback.num,
            integral: fallback.integral,
            floating_point: fallback.floating_point,
            ptr_like: fallback.ptr_like,
            sync: fallback.sync,
            builtin: [
                fallback.num,
                fallback.integral,
                fallback.floating_point,
                fallback.ptr_like,
                fallback.sync,
            ],
            declared: Vec::new(),
        }
    }

    /// Point a wired tower field at `id` — the declared trait (`core`'s)
    /// takes over from the construction builtin, so everything the checker
    /// wires through the field (literal bounds, arithmetic obligations,
    /// defaulting, the structural `Sync` answer, the `Num ⊴ PartialOrd`
    /// edge) follows the declaration.
    pub(crate) fn repoint(&mut self, item: LangItem, id: TraitId) {
        match item {
            LangItem::Num => self.num = id,
            LangItem::Integral => self.integral = id,
            LangItem::FloatingPoint => self.floating_point = id,
            LangItem::PtrLike => self.ptr_like = id,
            LangItem::Sync => self.sync = id,
            _ => {}
        }
    }

    /// The wired fields, in [`LangItem::TOWER`] order — the REPL
    /// checkpoint's snapshot (a rejected batch that repointed them must
    /// restore the builtins).
    pub(crate) fn fields(&self) -> [TraitId; 5] {
        [
            self.num,
            self.integral,
            self.floating_point,
            self.ptr_like,
            self.sync,
        ]
    }

    pub(crate) fn restore_fields(&mut self, fields: [TraitId; 5]) {
        [
            self.num,
            self.integral,
            self.floating_point,
            self.ptr_like,
            self.sync,
        ] = fields;
    }

    /// Substitute a bare-name hit on a *construction builtin* with the
    /// declared tower item, when one is bound: `T: Num` keeps resolving at
    /// the crate root in plain sessions, and resolves to `core`'s
    /// declaration the moment an interface (or the batch itself) binds it.
    /// Module-local shadows are their own defs and pass through untouched.
    pub fn override_builtin(&self, def: DefId, db: &crate::semi::traits::TraitDb<'_>) -> DefId {
        LangItem::TOWER
            .iter()
            .zip(self.builtin)
            .find(|&(_, builtin_id)| db.trait_def(builtin_id).def == def)
            .and_then(|(&item, _)| self.get(item))
            .unwrap_or(def)
    }

    /// Bind `item` to `def`. `Err` returns the prior declaration for the
    /// duplicate diagnostic (re-declaring the *same* def is idempotent —
    /// the same interface loaded twice).
    pub fn declare(&mut self, item: LangItem, def: DefId) -> Result<(), DefId> {
        match self.get(item) {
            None => {
                self.declared.push((item, def));
                Ok(())
            }
            Some(prior) if prior == def => Ok(()),
            Some(prior) => Err(prior),
        }
    }

    /// The declared def of `item`, if any source/interface declared it.
    pub fn get(&self, item: LangItem) -> Option<DefId> {
        self.declared
            .iter()
            .find(|&&(i, _)| i == item)
            .map(|&(_, def)| def)
    }

    /// Every `#[lang]`-declared binding, def-keyed — the serialization
    /// view (`.rri` emission re-prints the markers; the wired fallback
    /// tower is compiler-provided and never serializes).
    pub fn declared_by_def(&self) -> FxHashMap<DefId, LangItem> {
        self.declared
            .iter()
            .map(|&(item, def)| (def, item))
            .collect()
    }

    /// Whether `def` is a built-in type former's anchor record — a
    /// declaration site, not a nominal type (the former resolves by
    /// spelling; the anchor cannot be constructed).
    pub fn is_former_anchor(&self, def: DefId) -> bool {
        matches!(self.item_of(def), Some(LangItem::Former(_)))
    }

    /// The lang item bound to `def`, if any.
    pub fn item_of(&self, def: DefId) -> Option<LangItem> {
        self.declared
            .iter()
            .find(|&&(_, d)| d == def)
            .map(|&(item, _)| item)
    }

    /// Whether `def` is a bound intrinsic prototype — a location anchor,
    /// not a callable value (calls to its path are intercepted as the
    /// intrinsic itself).
    pub fn is_intrinsic_def(&self, def: DefId) -> bool {
        self.declared
            .iter()
            .any(|&(item, d)| d == def && matches!(item, LangItem::Intrinsic(_)))
    }

    /// The checkpoint counter, in the elaborator's truncate-and-retain
    /// scheme.
    pub(crate) fn len(&self) -> usize {
        self.declared.len()
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.declared.truncate(len);
    }
}

#[cfg(test)]
mod async_former_tests {
    use super::{FormerItem, LangItem};

    #[test]
    fn tensor_async_formers_have_family_scoped_names() {
        assert_eq!(FormerItem::TensorFuture.name(), "tensor_future");
        assert_eq!(FormerItem::TensorToken.name(), "tensor_token");
        assert_eq!(FormerItem::TensorFuture.arity(), 1);
        assert_eq!(FormerItem::TensorToken.arity(), 0);
        assert_eq!(
            LangItem::from_name("tensor_future"),
            Some(LangItem::Former(FormerItem::TensorFuture))
        );
        assert_eq!(
            LangItem::from_name("tensor_token"),
            Some(LangItem::Former(FormerItem::TensorToken))
        );
        assert_eq!(LangItem::from_name("future"), None);
        assert_eq!(LangItem::from_name("token"), None);
    }
}
