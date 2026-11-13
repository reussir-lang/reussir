//! The built-in `core::intrinsic` operation families.
//!
//! Reussir source reaches these as `core::intrinsic::<family>::<name>(…)` — the
//! package name `core` is reserved, so the path cannot collide with user code.
//! [`IntrinsicOp`] is the resolved family-independent representation carried by
//! HIR and MIR; each family defines its own names, checking, ownership contract,
//! and code generation.

/// The shape of a math intrinsic's value arguments (the fast-math flag is an
/// extra trailing argument in the surface form, not counted here).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MathKind {
    /// One float in, same float out (`sqrt`, `sin`, …).
    Unary,
    /// One float in, `bool` out (`isnan`, …).
    Check,
    /// Two floats in, same float out (`powf`, `atan2`, `copysign`).
    Binary,
    /// Fused multiply-add: three floats in, same float out.
    Fma,
    /// Float base and `i32` exponent in, float out.
    Fpowi,
    /// One integer in, same integer out (`ctpop`, `ctlz`, `cttz`). Bounded
    /// by `Integral` rather than `FloatingPoint`, and takes no fast-math
    /// flag — the MLIR integer math ops have no such attribute.
    IntUnary,
}

macro_rules! math_fns {
    ($($variant:ident => $name:literal : $kind:ident),* $(,)?) => {
        /// A surfaced math intrinsic; [`Self::as_str`] is both the surface
        /// name and the MLIR `math.<name>` mnemonic.
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
        pub enum MathFn {
            $($variant),*
        }

        impl MathFn {
            /// Parse a surface / textual-IR name.
            pub fn parse(name: &str) -> Option<MathFn> {
                match name {
                    $($name => Some(MathFn::$variant),)*
                    _ => None,
                }
            }

            /// The surface name, also the `math.<name>` op mnemonic.
            pub fn as_str(self) -> &'static str {
                match self {
                    $(MathFn::$variant => $name),*
                }
            }

            pub fn kind(self) -> MathKind {
                match self {
                    $(MathFn::$variant => MathKind::$kind),*
                }
            }

            /// Every surfaced math intrinsic, for coverage checks.
            pub const ALL: &'static [MathFn] = &[$(MathFn::$variant),*];
        }
    };
}

math_fns! {
    Absf => "absf": Unary,
    Acos => "acos": Unary,
    Acosh => "acosh": Unary,
    Asin => "asin": Unary,
    Asinh => "asinh": Unary,
    Atan => "atan": Unary,
    Atanh => "atanh": Unary,
    Cbrt => "cbrt": Unary,
    Ceil => "ceil": Unary,
    Cos => "cos": Unary,
    Cosh => "cosh": Unary,
    Erf => "erf": Unary,
    Erfc => "erfc": Unary,
    Exp => "exp": Unary,
    Exp2 => "exp2": Unary,
    Expm1 => "expm1": Unary,
    Floor => "floor": Unary,
    Log10 => "log10": Unary,
    Log1p => "log1p": Unary,
    Log2 => "log2": Unary,
    Round => "round": Unary,
    Roundeven => "roundeven": Unary,
    Rsqrt => "rsqrt": Unary,
    Sin => "sin": Unary,
    Sinh => "sinh": Unary,
    Sqrt => "sqrt": Unary,
    Tan => "tan": Unary,
    Tanh => "tanh": Unary,
    Trunc => "trunc": Unary,
    Isfinite => "isfinite": Check,
    Isinf => "isinf": Check,
    Isnan => "isnan": Check,
    Isnormal => "isnormal": Check,
    Atan2 => "atan2": Binary,
    Copysign => "copysign": Binary,
    Powf => "powf": Binary,
    Fma => "fma": Fma,
    Fpowi => "fpowi": Fpowi,
    Ctpop => "ctpop": IntUnary,
    Ctlz => "ctlz": IntUnary,
    Cttz => "cttz": IntUnary,
}

impl MathKind {
    /// How many *value* arguments the intrinsic takes (the constant fast-math
    /// flag is one more).
    pub fn value_args(self) -> usize {
        match self {
            MathKind::Unary | MathKind::Check | MathKind::IntUnary => 1,
            MathKind::Binary | MathKind::Fpowi => 2,
            MathKind::Fma => 3,
        }
    }
}

/// A fast-math flag set, the constant trailing `i32` argument of every math
/// intrinsic. Bit-compatible with MLIR's `arith.fastmath` bits; `0` is none
/// and `127` (all bits) is `fast`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FastMath(pub u32);

impl FastMath {
    pub const MAX: u32 = 127;

    const NAMES: [(u32, &'static str); 7] = [
        (1, "reassoc"),
        (2, "nnan"),
        (4, "ninf"),
        (8, "nsz"),
        (16, "arcp"),
        (32, "contract"),
        (64, "afn"),
    ];

    /// The MLIR `#arith.fastmath<…>` attribute text, or `None` for an empty
    /// flag set (MLIR's default — no attribute needed).
    pub fn mlir_attr(self) -> Option<String> {
        if self.0 == 0 {
            return None;
        }
        if self.0 == Self::MAX {
            return Some("#arith.fastmath<fast>".to_string());
        }
        let names: Vec<&str> = Self::NAMES
            .iter()
            .filter(|(bit, _)| self.0 & bit != 0)
            .map(|&(_, name)| name)
            .collect();
        Some(format!("#arith.fastmath<{}>", names.join(",")))
    }
}

/// A built-in operation on a statically shaped array (`TyKind::Array`, issue
/// #344), spelled `core::intrinsic::array::<fn>`. Unlike [`IntrinsicOp`],
/// these carry their own HIR/MIR node (`ExprKind::ArrayOp`) because
/// `Tabulate`/`Fold` hold an inline kernel body, which the generic
/// `Intrinsic` node cannot.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ArrayFn {
    /// `splat<[T; e…]>(v)`: every element is `v`. Value operands: `[v]`.
    Splat,
    /// `tabulate<[T; e…]>(|i, …| e)`: element at each index tuple is the
    /// kernel applied to the (row-major) indices. No value operands.
    Tabulate,
    /// `get(a, i, …)`: checked element read. Value operands: `[a, i…]`; `a`
    /// is borrowed, not consumed.
    Get,
    /// `set(a, i, …, v)`: checked element write, consuming `a` and returning
    /// the updated array (copy-on-write when shared). Value operands:
    /// `[a, i…, v]`.
    Set,
    /// `fold(a, init, |acc, x| e)`: row-major reduction. Value operands:
    /// `[a, init]`; `a` is borrowed, not consumed.
    Fold,
    /// `dim(a, k)`: the `k`-th extent as `u64`. Value operands: `[a, k]`;
    /// `a` is borrowed, not consumed. The interesting case is a dynamic
    /// dimension (`[T; ?, 4]`), whose extent lives in the box header; on a
    /// fully static array it folds to a constant.
    Dim,
    /// `unsafe_data_ptr(a)`: the pointer to the first element. Value
    /// operands: `[a]`; `a` is borrowed, not consumed, and the pointer does
    /// not keep it alive.
    UnsafeDataPtr,
}

/// A built-in operation on a shared mutable [`Cell`](crate::semi::ty::TyKind::Cell),
/// spelled `core::intrinsic::cell::<fn>` and dispatched on the operand's
/// cell kind. `alloc`/`get`/`set` work on every kind; `rmw` on every kind
/// but a plain `Cell` (the exclusive in-use region, an atomic CAS-retry, or
/// a lock's critical section); `in_use` observes the exclusive guard flag;
/// `rdlock` runs a read transaction over an `RwLock` cell. The operation
/// itself does not record the kind — operand and result types carry it, the
/// checker enforces the matrix, and the dialect verifier re-checks it after
/// lowering.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum CellFn {
    /// `alloc(value)`: move `value` into a fresh cell.
    Alloc,
    /// `get(cell)`: clone the value currently stored in `cell`.
    Get,
    /// `set(cell, value)`: replace and drop the old value.
    Set,
    /// `rmw(cell, update)`: move the value through `update` and store its
    /// result. Every kind but a plain `Cell` and an `Atomic` cell (the
    /// atomic CAS-retry region must be effect-free, which a closure call is
    /// not; dedicated atomic arithmetic intrinsics are future work).
    Rmw,
    /// `in_use(cell)`: whether the element is currently moved out into an
    /// active `rmw` updater. Exclusive cells (`RefCell`) only.
    InUse,
    /// `rdlock(cell, reader)`: run `reader` over a clone of the element
    /// inside the shared read lock. `RwLock` cells only.
    Rdlock,
}

impl CellFn {
    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<CellFn> {
        match name {
            "alloc" => Some(CellFn::Alloc),
            "get" => Some(CellFn::Get),
            "set" => Some(CellFn::Set),
            "rmw" => Some(CellFn::Rmw),
            "in_use" => Some(CellFn::InUse),
            "rdlock" => Some(CellFn::Rdlock),
            _ => None,
        }
    }

    /// The surface name, also used by the textual IR.
    pub fn as_str(self) -> &'static str {
        match self {
            CellFn::Alloc => "alloc",
            CellFn::Get => "get",
            CellFn::Set => "set",
            CellFn::Rmw => "rmw",
            CellFn::InUse => "in_use",
            CellFn::Rdlock => "rdlock",
        }
    }

    /// Whether this operation is available on a cell of `kind` — the §4.2
    /// access matrix. Lowering re-derives the kind from the monomorphized
    /// operand type; the dialect verifier re-checks after lowering.
    pub fn supported_on(self, kind: crate::semi::ty::CellKind) -> bool {
        use crate::semi::ty::CellKind;
        match self {
            CellFn::Alloc | CellFn::Get | CellFn::Set => true,
            CellFn::Rmw => kind != CellKind::Plain && kind != CellKind::Atomic,
            CellFn::InUse => kind == CellKind::Exclusive,
            CellFn::Rdlock => kind == CellKind::Rwlock,
        }
    }

    /// Which operands support this operation, for diagnostics.
    pub fn requirement(self) -> &'static str {
        match self {
            CellFn::Alloc | CellFn::Get | CellFn::Set => "any cell",
            CellFn::Rmw => {
                "an exclusive or lock-guarded cell (`RefCell`, `Mutex`, \
                 `FlatLock`, or `RwLock`)"
            }
            CellFn::InUse => "an exclusive cell (`RefCell`)",
            CellFn::Rdlock => "an `RwLock` cell",
        }
    }
}

impl ArrayFn {
    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<ArrayFn> {
        match name {
            "splat" => Some(ArrayFn::Splat),
            "tabulate" => Some(ArrayFn::Tabulate),
            "get" => Some(ArrayFn::Get),
            "set" => Some(ArrayFn::Set),
            "fold" => Some(ArrayFn::Fold),
            "dim" => Some(ArrayFn::Dim),
            "unsafe_data_ptr" => Some(ArrayFn::UnsafeDataPtr),
            _ => None,
        }
    }

    /// The surface name, also used by the textual IR (`array#<name>`).
    pub fn as_str(self) -> &'static str {
        match self {
            ArrayFn::Splat => "splat",
            ArrayFn::Tabulate => "tabulate",
            ArrayFn::Get => "get",
            ArrayFn::Set => "set",
            ArrayFn::Fold => "fold",
            ArrayFn::Dim => "dim",
            ArrayFn::UnsafeDataPtr => "unsafe_data_ptr",
        }
    }

    /// Whether the op takes an inline kernel body.
    pub fn has_kernel(self) -> bool {
        matches!(self, ArrayFn::Tabulate | ArrayFn::Fold)
    }
}

/// A tensor operation spelled `core::intrinsic::tensor::<fn>` and represented
/// by the same `TensorOp` node in HIR and MIR. Kernel operations are validated
/// against the pinned StableHLO semantics; the original host boundary operations
/// retain their separate confinement and lowering contract.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TensorFn {
    /// `of(a)`: view an array as a tensor. Value operands: `[a]`; consumes
    /// the reference (the tensor owns the rooted dup).
    Of,
    /// `materialize(t)`: build a fresh array from the tensor's value.
    /// Value operands: `[t]`.
    Materialize,
    /// `dim(t, k)`: the `k`-th extent as `u64`. Value operands: `[t, k]`.
    Dim,
    /// Kernel-only dynamic slice; result type supplies static slice sizes.
    DynamicSlice,
    /// Kernel-only static, unit-stride slice with literal starting offsets.
    Slice,
    /// Cross-replica permutation with literal source/target pairs.
    CollectivePermute,
    /// Resolve a single-result StableHLO future inside a kernel.
    AsyncDone,
    Fill,
    DynamicFill,
    Iota,
    DynamicIota,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Abs,
    Min,
    Max,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Select,
    Dot,
    Transpose,
}

impl TensorFn {
    pub fn is_kernel(self) -> bool {
        !matches!(self, Self::Of | Self::Materialize | Self::Dim)
    }
    pub fn is_basic(self) -> bool {
        self.is_kernel()
            && !matches!(
                self,
                Self::DynamicSlice | Self::Slice | Self::CollectivePermute | Self::AsyncDone
            )
    }
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            Self::Eq | Self::Ne | Self::Lt | Self::Le | Self::Gt | Self::Ge
        )
    }
    pub fn arity(self) -> usize {
        match self {
            Self::Fill | Self::Iota | Self::Neg | Self::Abs | Self::Transpose => 1,
            Self::Select => 3,
            _ => 2,
        }
    }

    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<TensorFn> {
        match name {
            "of" => Some(TensorFn::Of),
            "materialize" => Some(TensorFn::Materialize),
            "dim" => Some(TensorFn::Dim),
            "dynamic_slice" => Some(TensorFn::DynamicSlice),
            "slice" => Some(TensorFn::Slice),
            "collective_permute" => Some(TensorFn::CollectivePermute),
            "async_done" => Some(TensorFn::AsyncDone),
            "fill" => Some(TensorFn::Fill),
            "dynamic_fill" => Some(TensorFn::DynamicFill),
            "iota" => Some(TensorFn::Iota),
            "dynamic_iota" => Some(TensorFn::DynamicIota),
            "add" => Some(TensorFn::Add),
            "sub" => Some(TensorFn::Sub),
            "mul" => Some(TensorFn::Mul),
            "div" => Some(TensorFn::Div),
            "rem" => Some(TensorFn::Rem),
            "neg" => Some(TensorFn::Neg),
            "abs" => Some(TensorFn::Abs),
            "min" => Some(TensorFn::Min),
            "max" => Some(TensorFn::Max),
            "eq" => Some(TensorFn::Eq),
            "ne" => Some(TensorFn::Ne),
            "lt" => Some(TensorFn::Lt),
            "le" => Some(TensorFn::Le),
            "gt" => Some(TensorFn::Gt),
            "ge" => Some(TensorFn::Ge),
            "select" => Some(TensorFn::Select),
            "dot" => Some(TensorFn::Dot),
            "transpose" => Some(TensorFn::Transpose),
            _ => None,
        }
    }

    /// The surface name, also used by the textual IR (`tensor#<name>`).
    pub fn as_str(self) -> &'static str {
        match self {
            TensorFn::Of => "of",
            TensorFn::Materialize => "materialize",
            TensorFn::Dim => "dim",
            TensorFn::DynamicSlice => "dynamic_slice",
            TensorFn::Slice => "slice",
            TensorFn::CollectivePermute => "collective_permute",
            TensorFn::AsyncDone => "async_done",
            TensorFn::Fill => "fill",
            TensorFn::DynamicFill => "dynamic_fill",
            TensorFn::Iota => "iota",
            TensorFn::DynamicIota => "dynamic_iota",
            TensorFn::Add => "add",
            TensorFn::Sub => "sub",
            TensorFn::Mul => "mul",
            TensorFn::Div => "div",
            TensorFn::Rem => "rem",
            TensorFn::Neg => "neg",
            TensorFn::Abs => "abs",
            TensorFn::Min => "min",
            TensorFn::Max => "max",
            TensorFn::Eq => "eq",
            TensorFn::Ne => "ne",
            TensorFn::Lt => "lt",
            TensorFn::Le => "le",
            TensorFn::Gt => "gt",
            TensorFn::Ge => "ge",
            TensorFn::Select => "select",
            TensorFn::Dot => "dot",
            TensorFn::Transpose => "transpose",
        }
    }
}

/// A resolved intrinsic operation: which family, which op within it, and the
/// family's compile-time immediates. One generic `Intrinsic` IR node carries
/// this through HIR/MIR, so the node itself — and its traversal, ownership, and
/// monomorphization — stay family-agnostic. Adding a family (`arith`, `atomic`,
/// …) adds a variant here plus its own checking and codegen; the shared IR
/// plumbing is untouched.
///
/// The textual IR spells an op as `intrinsic#<family>#<name>#<imm>(args…)`,
/// where `<imm>` is the family's packed immediate ([`Self::imm`]); [`Self::parse`]
/// rebuilds the typed op from that triple.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntrinsicOp {
    /// `core::object::<func>`; borrows one RC-managed object.
    Object {
        func: ObjectFn,
    },
    /// `core::intrinsic::panic::panic()`: aborts execution and inhabits the
    /// contextually expected result type.
    Panic,
    /// `core::intrinsic::math::<func>`, with an `arith.fastmath` flag.
    Math {
        func: MathFn,
        flag: u32,
    },
    /// `core::intrinsic::cell::<func>`; cell operations have no immediates
    /// and dispatch on the operand's cell flavor rather than encoding it.
    Cell {
        func: CellFn,
    },
    /// `core::intrinsic::str::<func>`; string operations have no immediates.
    Str {
        func: StrFn,
    },
    /// `core::intrinsic::const_seq::<func>`; no immediates.
    ConstSeq {
        func: ConstSeqFn,
    },
    Pjrt {
        func: PjrtFn,
    },
    /// `core::intrinsic::bigint::<func>`; no immediates.
    BigInt {
        func: BigIntFn,
    },
    /// `core::simd::<func>`, an operation on vectors.
    Simd {
        func: SimdFn,
    },
    /// `core::ptr::<func>`. A load or store carries how it accesses memory,
    /// and an offset what it assumes of the arithmetic.
    Ptr {
        func: PtrFn,
        access: PtrAccess,
        arith: PtrArithFlag,
    },
}

/// Operations on vectors (`Simd<...>`). `V` is a vector type.
///
/// A lanewise operation may take one more operand than its table entry
/// says: a mask of the shape of its vectors. A lane whose mask bit is clear
/// keeps the lane of the first operand, or is false in a comparison, and
/// never traps.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum SimdFn {
    /// `splat<V>(x)`: every lane is `x`.
    Splat,
    /// `step<V>()`: lanes 0, 1, 2, ... of a one-dimensional integer vector.
    Step,
    /// `vscale() -> u64`: the factor of scalable extents on this machine.
    Vscale,
    /// `lanes<V>() -> u64`: the number of lanes of `V`; elaborates to a
    /// constant, times `vscale()` for a scalable `V`.
    Lanes,
    /// `from_elements<V>(x...)`: one argument per lane, in row-major order;
    /// `V` is fixed.
    FromElements,
    /// `extract(v, i...)`: the lane at the indices, or the sub-vector when
    /// fewer indices than dimensions are given. Only an index into the last
    /// dimension may be a run-time value.
    Extract,
    /// `insert(v, x, i...)`: `v` with that lane or sub-vector replaced.
    Insert,
    /// `add sub mul div rem min max`: lanewise arithmetic on two vectors of
    /// one numeric type; signedness comes from the lane type.
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Min,
    Max,
    /// `neg abs`: lanewise on one numeric vector.
    Neg,
    Abs,
    /// `and or xor not`: lanewise on integer vectors and on masks.
    And,
    Or,
    Xor,
    Not,
    /// `shl shr`: lanewise shifts of integer vectors; `shr` is arithmetic
    /// for signed lanes.
    Shl,
    Shr,
    /// `fma(a, b, c)`: lanewise `a * b + c` on floating-point vectors.
    Fma,
    /// `eq ne lt le gt ge`: lanewise comparison; the result is a mask of
    /// the same shape. `eq` and `ne` also compare masks.
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    /// `select(m, a, b)`: `a` where the mask is set, else `b`.
    Select,
    /// `mask_first<M>(n...)`: the mask whose leading `n` lanes are set, with
    /// one count per dimension.
    MaskFirst,
    /// `any(m)`, `all(m)`: whether some or every lane of a mask is set.
    Any,
    All,
    /// `count(m) -> u64`: the number of set lanes.
    Count,
    /// `first_set(m) -> u64`: the index of the first set lane of a
    /// one-dimensional mask, or its number of lanes when none is set.
    FirstSet,
}

/// The lanes a lanewise vector operation works on.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SimdLanes {
    /// Integers and floating-point numbers.
    Numeric,
    /// Integers.
    Integer,
    /// Integers and `bool`.
    Bits,
    /// Floating-point numbers.
    Float,
    /// Any lane type.
    Any,
}

impl SimdLanes {
    pub fn describe(self) -> &'static str {
        match self {
            SimdLanes::Numeric => "integer or floating-point",
            SimdLanes::Integer => "integer",
            SimdLanes::Bits => "integer or `bool`",
            SimdLanes::Float => "floating-point",
            SimdLanes::Any => "any",
        }
    }
}

impl SimdFn {
    pub const ALL: &'static [SimdFn] = &[
        SimdFn::Splat,
        SimdFn::Step,
        SimdFn::Vscale,
        SimdFn::Lanes,
        SimdFn::FromElements,
        SimdFn::Extract,
        SimdFn::Insert,
        SimdFn::Add,
        SimdFn::Sub,
        SimdFn::Mul,
        SimdFn::Div,
        SimdFn::Rem,
        SimdFn::Neg,
        SimdFn::Min,
        SimdFn::Max,
        SimdFn::Abs,
        SimdFn::And,
        SimdFn::Or,
        SimdFn::Xor,
        SimdFn::Not,
        SimdFn::Shl,
        SimdFn::Shr,
        SimdFn::Fma,
        SimdFn::Eq,
        SimdFn::Ne,
        SimdFn::Lt,
        SimdFn::Le,
        SimdFn::Gt,
        SimdFn::Ge,
        SimdFn::Select,
        SimdFn::MaskFirst,
        SimdFn::Any,
        SimdFn::All,
        SimdFn::Count,
        SimdFn::FirstSet,
    ];

    pub fn parse(name: &str) -> Option<SimdFn> {
        SimdFn::ALL.iter().copied().find(|f| f.as_str() == name)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            SimdFn::Splat => "splat",
            SimdFn::Step => "step",
            SimdFn::Vscale => "vscale",
            SimdFn::Lanes => "lanes",
            SimdFn::FromElements => "from_elements",
            SimdFn::Extract => "extract",
            SimdFn::Insert => "insert",
            SimdFn::Add => "add",
            SimdFn::Sub => "sub",
            SimdFn::Mul => "mul",
            SimdFn::Div => "div",
            SimdFn::Rem => "rem",
            SimdFn::Neg => "neg",
            SimdFn::Min => "min",
            SimdFn::Max => "max",
            SimdFn::Abs => "abs",
            SimdFn::And => "and",
            SimdFn::Or => "or",
            SimdFn::Xor => "xor",
            SimdFn::Not => "not",
            SimdFn::Shl => "shl",
            SimdFn::Shr => "shr",
            SimdFn::Fma => "fma",
            SimdFn::Eq => "eq",
            SimdFn::Ne => "ne",
            SimdFn::Lt => "lt",
            SimdFn::Le => "le",
            SimdFn::Gt => "gt",
            SimdFn::Ge => "ge",
            SimdFn::Select => "select",
            SimdFn::MaskFirst => "mask_first",
            SimdFn::Any => "any",
            SimdFn::All => "all",
            SimdFn::Count => "count",
            SimdFn::FirstSet => "first_set",
        }
    }

    /// For an operation applied lane by lane to vectors of one type: the
    /// number of operands, the lanes it accepts, and whether it yields a
    /// mask and not a vector of that type.
    pub fn lanewise(self) -> Option<(usize, SimdLanes, bool)> {
        use SimdFn::*;
        Some(match self {
            Add | Sub | Mul | Div | Rem | Min | Max => (2, SimdLanes::Numeric, false),
            Neg | Abs => (1, SimdLanes::Numeric, false),
            And | Or | Xor => (2, SimdLanes::Bits, false),
            Not => (1, SimdLanes::Bits, false),
            Shl | Shr => (2, SimdLanes::Integer, false),
            Fma => (3, SimdLanes::Float, false),
            Eq | Ne => (2, SimdLanes::Any, true),
            Lt | Le | Gt | Ge => (2, SimdLanes::Numeric, true),
            _ => return None,
        })
    }

    /// Whether the function survives elaboration; `lanes` does not.
    pub fn is_lowered(self) -> bool {
        self != SimdFn::Lanes
    }
}

/// Operations on opaque pointers. `Ptr<A>` and `ConstPtr<A>` share them,
/// except that only a `Ptr<A>` is stored through. The types read, written
/// and measured are the integers, the floating-point numbers, `bool`,
/// `char` and the pointers themselves.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PtrFn {
    /// `offset(p, bytes: i64, flag)`: the pointer `bytes` bytes after `p`,
    /// under the constant [`PtrArithFlag`] `flag`.
    Offset,
    /// `type_offset<T>() -> u64`: the distance in bytes between two
    /// consecutive values of `T` in memory.
    TypeOffset,
    /// `advance<T>(p, count: i64, flag)`:
    /// `offset(p, count * type_offset<T>(), flag)`.
    Advance,
    /// `mask(p, mask: u64)`: `p` with the address bits clear in `mask`
    /// cleared.
    Mask,
    /// `addr(p) -> u64`: the address of `p`. The provenance of `p` is not
    /// exposed.
    Addr,
    /// `expose_addr(p) -> u64`: the address of `p`, exposing its provenance
    /// to a later `from_addr`.
    ExposeAddr,
    /// `from_addr<P>(addr: u64) -> P`: the pointer of type `P` to `addr`,
    /// with the provenance exposed for that address.
    FromAddr,
    /// `with_provenance(p, addr: u64)`: the pointer to `addr` with the
    /// provenance of `p`.
    WithProvenance,
    /// `as_const(p)`: `p` as a `ConstPtr`.
    AsConst,
    /// `as_mut(p)`: `p` as a `Ptr`.
    AsMut,
    /// `launder_invariant_group(p)`: `p` in a new invariant group.
    LaunderInvariantGroup,
    /// `strip_invariant_group(p)`: `p` in no invariant group.
    StripInvariantGroup,
    /// `diff(p, q) -> i64`: the distance in bytes from `q` to `p`.
    Diff,
    /// `load<T>(p) -> T`: read a `T`.
    Load,
    /// `store<T>(p, value: T)`: write a `T`.
    Store,
    /// `load_extended<T>(p, volatile, nontemporal, invariant,
    /// invariant_group, align) -> T`: a load with constant access flags.
    LoadExtended,
    /// `store_extended<T>(p, value, volatile, nontemporal, invariant_group,
    /// align)`: a store with constant access flags.
    StoreExtended,
}

impl PtrFn {
    pub const ALL: &'static [PtrFn] = &[
        PtrFn::Offset,
        PtrFn::TypeOffset,
        PtrFn::Advance,
        PtrFn::Mask,
        PtrFn::Addr,
        PtrFn::ExposeAddr,
        PtrFn::FromAddr,
        PtrFn::WithProvenance,
        PtrFn::AsConst,
        PtrFn::AsMut,
        PtrFn::LaunderInvariantGroup,
        PtrFn::StripInvariantGroup,
        PtrFn::Diff,
        PtrFn::Load,
        PtrFn::Store,
        PtrFn::LoadExtended,
        PtrFn::StoreExtended,
    ];

    pub fn parse(name: &str) -> Option<PtrFn> {
        PtrFn::ALL.iter().copied().find(|f| f.as_str() == name)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            PtrFn::Offset => "offset",
            PtrFn::TypeOffset => "type_offset",
            PtrFn::Advance => "advance",
            PtrFn::Mask => "mask",
            PtrFn::Addr => "addr",
            PtrFn::ExposeAddr => "expose_addr",
            PtrFn::FromAddr => "from_addr",
            PtrFn::WithProvenance => "with_provenance",
            PtrFn::AsConst => "as_const",
            PtrFn::AsMut => "as_mut",
            PtrFn::LaunderInvariantGroup => "launder_invariant_group",
            PtrFn::StripInvariantGroup => "strip_invariant_group",
            PtrFn::Diff => "diff",
            PtrFn::Load => "load",
            PtrFn::Store => "store",
            PtrFn::LoadExtended => "load_extended",
            PtrFn::StoreExtended => "store_extended",
        }
    }
}

/// What a pointer offset assumes, as the flags of `ptr.ptr_add` and an LLVM
/// `getelementptr`. The variants are those of `core::ptr::PtrArithFlag`, in
/// its declaration order.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum PtrArithFlag {
    /// The offset wraps.
    #[default]
    None,
    /// The offset does not wrap when added to the address as a signed value.
    Nusw,
    /// The offset does not wrap when added as an unsigned value.
    Nuw,
    /// `Nusw`, and the pointer stays within the bounds of its object.
    Inbounds,
}

impl PtrArithFlag {
    pub const ALL: [PtrArithFlag; 4] = [
        PtrArithFlag::None,
        PtrArithFlag::Nusw,
        PtrArithFlag::Nuw,
        PtrArithFlag::Inbounds,
    ];

    pub fn bits(self) -> u32 {
        self as u32
    }

    pub fn from_bits(bits: u32) -> Option<PtrArithFlag> {
        PtrArithFlag::ALL.get(usize::try_from(bits).ok()?).copied()
    }
}

/// How a load or store accesses memory, as the LLVM instruction flags of
/// the same names. An alignment of zero is the natural alignment of the
/// accessed type.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct PtrAccess {
    pub volatile: bool,
    pub nontemporal: bool,
    pub invariant: bool,
    pub invariant_group: bool,
    pub align: u64,
}

impl PtrAccess {
    /// The largest alignment an access may state, which is the largest the
    /// LLVM dialect's `load` and `store` builders take.
    pub const MAX_ALIGN: u64 = 1 << 31;

    /// The packed form printed in the textual IR: the four flags in the low
    /// bits, then one more than the base-two logarithm of the alignment.
    pub fn bits(self) -> u32 {
        let align = match self.align {
            0 => 0,
            n => n.trailing_zeros() + 1,
        };
        u32::from(self.volatile)
            | u32::from(self.nontemporal) << 1
            | u32::from(self.invariant) << 2
            | u32::from(self.invariant_group) << 3
            | align << 8
    }

    pub fn from_bits(bits: u32) -> Option<PtrAccess> {
        let align = bits >> 8;
        (bits & 0xf0 == 0 && align <= 32).then(|| PtrAccess {
            volatile: bits & 1 != 0,
            nontemporal: bits & 2 != 0,
            invariant: bits & 4 != 0,
            invariant_group: bits & 8 != 0,
            align: if align == 0 { 0 } else { 1 << (align - 1) },
        })
    }
}

/// Synchronous host/device operations. Transfers borrow their input and return
/// fresh owned arrays; allocation initializes every logical element.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PjrtFn {
    Alloc,
    Upload,
    Download,
    Dim,
}
impl PjrtFn {
    pub fn parse(name: &str) -> Option<Self> {
        Some(match name {
            "alloc" => Self::Alloc,
            "upload" => Self::Upload,
            "download" => Self::Download,
            "dim" => Self::Dim,
            _ => return None,
        })
    }
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Alloc => "alloc",
            Self::Upload => "upload",
            Self::Download => "download",
            Self::Dim => "dim",
        }
    }
}

/// A surfaced `str` intrinsic; [`Self::as_str`] is both the surface name
/// and the `reussir.str.<name>` mnemonic family it lowers through.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum StrFn {
    /// `len(s)`: the byte length.
    Len,
    /// `byte_at(s, index)`: the byte at `index`, `0` when out of bounds.
    ByteAt,
    /// `slice(s, offset)`: the suffix from byte `offset`, clamped to empty
    /// when the offset passes the end.
    Slice,
    /// `unsafe_data_ptr(s)`: the pointer to the first byte.
    UnsafeDataPtr,
}

impl StrFn {
    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<StrFn> {
        match name {
            "len" => Some(StrFn::Len),
            "byte_at" => Some(StrFn::ByteAt),
            "slice" => Some(StrFn::Slice),
            "unsafe_data_ptr" => Some(StrFn::UnsafeDataPtr),
            _ => None,
        }
    }

    /// The surface name, also used by the textual IR.
    pub fn as_str(self) -> &'static str {
        match self {
            StrFn::Len => "len",
            StrFn::ByteAt => "byte_at",
            StrFn::Slice => "slice",
            StrFn::UnsafeDataPtr => "unsafe_data_ptr",
        }
    }
}

/// A surfaced `ConstSeq` intrinsic; [`Self::as_str`] is both the surface
/// name and the `reussir.const_seq.<name>` op it lowers to.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ConstSeqFn {
    /// `len(seq)`: the number of elements.
    Len,
    /// `get(seq, index)`: the element at `index`; panics when out of bounds.
    Get,
}

impl ConstSeqFn {
    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<ConstSeqFn> {
        match name {
            "len" => Some(ConstSeqFn::Len),
            "get" => Some(ConstSeqFn::Get),
            _ => None,
        }
    }

    /// The surface name, also used by the textual IR.
    pub fn as_str(self) -> &'static str {
        match self {
            ConstSeqFn::Len => "len",
            ConstSeqFn::Get => "get",
        }
    }
}

/// A surfaced `bigint` intrinsic over the built-in integers `Nat` and `Int`.
/// Each lowers to the rcgmp operation its name spells; [`Self::as_str`] is
/// the surface name.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum BigIntFn {
    /// `gcdi(a, b) -> Nat`: the greatest common divisor of the magnitudes.
    Gcdi,
    /// `ipowi(base, exp: u64)`: `base` to the power `exp`.
    Ipowi,
    /// `log2i(x) -> u64`: the base-2 logarithm of the magnitude, rounded
    /// down; `0` for zero.
    Log2i,
    /// `testbiti(x, i: u64) -> bool`: bit `i` of the infinite two's
    /// complement.
    Testbiti,
    /// `divsi_exact(a, b)`: the quotient of an exact division; `a` must be a
    /// multiple of `b`.
    DivsiExact,
    /// `edivsi(a, b)`: the Euclidean quotient.
    Edivsi,
    /// `emodsi(a, b)`: the Euclidean remainder, never negative.
    Emodsi,
    /// `floordivsi(a, b)`: the quotient rounded toward negative infinity.
    Floordivsi,
    /// `floormodsi(a, b)`: the remainder with the sign of `b`.
    Floormodsi,
    /// `absi(x) -> Nat`: the magnitude.
    Absi,
    /// `trunci_overflow(x) -> (U, bool)`: the low bits of `x` at the
    /// fixed-width integer `U`, and whether `x` does not fit `U`.
    TrunciOverflow,
    /// `sitofp(x) -> f64`: `x` rounded to the nearest `f64`, ties to even.
    Sitofp,
    /// `fptosi(value: f64) -> Int`: `value` truncated toward zero; a NaN or
    /// an infinity terminates the program.
    Fptosi,
    /// `limb_count(x) -> u64`: the number of 64-bit limbs of the magnitude.
    LimbCount,
    /// `limb(x, i: u64) -> u64`: limb `i` of the magnitude, least significant
    /// first, or zero past the last one.
    Limb,
}

impl BigIntFn {
    pub const ALL: &[BigIntFn] = &[
        BigIntFn::Gcdi,
        BigIntFn::Ipowi,
        BigIntFn::Log2i,
        BigIntFn::Testbiti,
        BigIntFn::DivsiExact,
        BigIntFn::Edivsi,
        BigIntFn::Emodsi,
        BigIntFn::Floordivsi,
        BigIntFn::Floormodsi,
        BigIntFn::Absi,
        BigIntFn::TrunciOverflow,
        BigIntFn::Sitofp,
        BigIntFn::Fptosi,
        BigIntFn::LimbCount,
        BigIntFn::Limb,
    ];

    /// Parse a surface / textual-IR name.
    pub fn parse(name: &str) -> Option<BigIntFn> {
        BigIntFn::ALL.iter().copied().find(|f| f.as_str() == name)
    }

    /// The surface name, also used by the textual IR.
    pub fn as_str(self) -> &'static str {
        match self {
            BigIntFn::Gcdi => "gcdi",
            BigIntFn::Ipowi => "ipowi",
            BigIntFn::Log2i => "log2i",
            BigIntFn::Testbiti => "testbiti",
            BigIntFn::DivsiExact => "divsi_exact",
            BigIntFn::Edivsi => "edivsi",
            BigIntFn::Emodsi => "emodsi",
            BigIntFn::Floordivsi => "floordivsi",
            BigIntFn::Floormodsi => "floormodsi",
            BigIntFn::Absi => "absi",
            BigIntFn::TrunciOverflow => "trunci_overflow",
            BigIntFn::Sitofp => "sitofp",
            BigIntFn::Fptosi => "fptosi",
            BigIntFn::LimbCount => "limb_count",
            BigIntFn::Limb => "limb",
        }
    }

    /// Whether the operation borrows its integer operand (the first) rather
    /// than consuming it. Every other integer operand is consumed.
    pub fn borrows_operand(self) -> bool {
        matches!(
            self,
            BigIntFn::Log2i
                | BigIntFn::Testbiti
                | BigIntFn::TrunciOverflow
                | BigIntFn::Sitofp
                | BigIntFn::LimbCount
                | BigIntFn::Limb
        )
    }

    /// Whether the operation takes a `Nat` or an `Int` as its first operand;
    /// only `fptosi`, which makes one from a float, does not.
    pub fn takes_integer(self) -> bool {
        self != BigIntFn::Fptosi
    }
}

/// Operations on an object's reference count.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ObjectFn {
    IsUnique,
    UnsafeInc,
    UnsafeDec,
}

impl ObjectFn {
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "is_unique" => Some(Self::IsUnique),
            "unsafe_inc" => Some(Self::UnsafeInc),
            "unsafe_dec" => Some(Self::UnsafeDec),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::IsUnique => "is_unique",
            Self::UnsafeInc => "unsafe_inc",
            Self::UnsafeDec => "unsafe_dec",
        }
    }
}

impl IntrinsicOp {
    /// The family segment under `core::intrinsic::` (`math`, …).
    pub fn family(self) -> &'static str {
        match self {
            IntrinsicOp::Object { .. } => "object",
            IntrinsicOp::Panic => "panic",
            IntrinsicOp::Math { .. } => "math",
            IntrinsicOp::Cell { .. } => "cell",
            IntrinsicOp::Str { .. } => "str",
            IntrinsicOp::ConstSeq { .. } => "const_seq",
            IntrinsicOp::Pjrt { .. } => "pjrt",
            IntrinsicOp::BigInt { .. } => "bigint",
            IntrinsicOp::Ptr { .. } => "ptr",
            IntrinsicOp::Simd { .. } => "simd",
        }
    }

    /// The op's name within its family (also the dialect mnemonic).
    pub fn name(self) -> &'static str {
        match self {
            IntrinsicOp::Object { func } => func.as_str(),
            IntrinsicOp::Panic => "panic",
            IntrinsicOp::Math { func, .. } => func.as_str(),
            IntrinsicOp::Cell { func, .. } => func.as_str(),
            IntrinsicOp::Str { func, .. } => func.as_str(),
            IntrinsicOp::ConstSeq { func } => func.as_str(),
            IntrinsicOp::Pjrt { func } => func.as_str(),
            IntrinsicOp::BigInt { func } => func.as_str(),
            IntrinsicOp::Ptr { func, .. } => func.as_str(),
            IntrinsicOp::Simd { func } => func.as_str(),
        }
    }

    /// The family's packed immediate, as printed in the textual IR.
    pub fn imm(self) -> u32 {
        match self {
            IntrinsicOp::Object { .. } => 0,
            IntrinsicOp::Panic => 0,
            IntrinsicOp::Math { flag, .. } => flag,
            IntrinsicOp::Cell { .. } => 0,
            IntrinsicOp::Str { .. } => 0,
            IntrinsicOp::ConstSeq { .. } => 0,
            IntrinsicOp::Pjrt { .. } => 0,
            IntrinsicOp::BigInt { .. } => 0,
            IntrinsicOp::Ptr {
                func: PtrFn::Offset,
                arith,
                ..
            } => arith.bits(),
            IntrinsicOp::Ptr { access, .. } => access.bits(),
            IntrinsicOp::Simd { .. } => 0,
        }
    }

    /// Rebuild a typed op from a textual-IR `(family, name, imm)` triple.
    /// `None` if the family or name is unknown.
    pub fn parse(family: &str, name: &str, imm: u32) -> Option<IntrinsicOp> {
        match family {
            "object" if imm == 0 => Some(IntrinsicOp::Object {
                func: ObjectFn::parse(name)?,
            }),
            "panic" if name == "panic" && imm == 0 => Some(IntrinsicOp::Panic),
            "math" => Some(IntrinsicOp::Math {
                func: MathFn::parse(name)?,
                flag: imm,
            }),
            "cell" if imm == 0 => Some(IntrinsicOp::Cell {
                func: CellFn::parse(name)?,
            }),
            "pjrt" if imm == 0 => Some(IntrinsicOp::Pjrt {
                func: PjrtFn::parse(name)?,
            }),
            "str" if imm == 0 => Some(IntrinsicOp::Str {
                func: StrFn::parse(name)?,
            }),
            "const_seq" if imm == 0 => Some(IntrinsicOp::ConstSeq {
                func: ConstSeqFn::parse(name)?,
            }),
            "bigint" if imm == 0 => Some(IntrinsicOp::BigInt {
                func: BigIntFn::parse(name)?,
            }),
            "simd" if imm == 0 => {
                let func = SimdFn::parse(name)?;
                func.is_lowered().then_some(IntrinsicOp::Simd { func })
            }
            "ptr" => {
                let func = PtrFn::parse(name)?;
                let plain = PtrAccess::default();
                let (access, arith) = match func {
                    PtrFn::Load => (PtrAccess::from_bits(imm)?, PtrArithFlag::None),
                    PtrFn::Store => {
                        let access = PtrAccess::from_bits(imm)?;
                        (!access.invariant).then_some((access, PtrArithFlag::None))?
                    }
                    PtrFn::Offset => (plain, PtrArithFlag::from_bits(imm)?),
                    PtrFn::Mask
                    | PtrFn::Addr
                    | PtrFn::ExposeAddr
                    | PtrFn::FromAddr
                    | PtrFn::WithProvenance
                    | PtrFn::AsConst
                    | PtrFn::AsMut
                    | PtrFn::LaunderInvariantGroup
                    | PtrFn::StripInvariantGroup
                    | PtrFn::Diff => (imm == 0).then_some((plain, PtrArithFlag::None))?,
                    // The other surface operations elaborate to these.
                    PtrFn::TypeOffset
                    | PtrFn::Advance
                    | PtrFn::LoadExtended
                    | PtrFn::StoreExtended => return None,
                };
                Some(IntrinsicOp::Ptr {
                    func,
                    access,
                    arith,
                })
            }
            _ => None,
        }
    }
}

/// Validate transfer signatures at textual-IR and code-generation boundaries.
/// Source inference may retain a generic whole-shape element until substitution.
pub fn validate_pjrt(
    op: PjrtFn,
    result: crate::semi::ty::Ty<'_>,
    args: &[crate::semi::ty::Ty<'_>],
) -> Result<(), String> {
    use crate::semi::ty::{DYNAMIC_EXTENT, IntTy, TyKind};
    let invalid = || {
        Err(format!(
            "invalid pjrt::{} operand or result types",
            op.as_str()
        ))
    };
    let is_extent =
        |ty: crate::semi::ty::Ty<'_>| matches!(ty.kind(), TyKind::Int(IntTy::Unsigned(64)));
    let device = if matches!(op, PjrtFn::Alloc | PjrtFn::Upload) {
        result
    } else {
        let Some(&device) = args.first() else {
            return invalid();
        };
        device
    };
    let TyKind::DevArray { elem, dims, .. } = *device.kind() else {
        return invalid();
    };
    if !crate::semi::ty_eval::device_element_supported(elem) {
        return invalid();
    }
    match op {
        PjrtFn::Alloc => {
            let count = dims.iter().filter(|&&d| d == DYNAMIC_EXTENT).count();
            if args.len() == count + 1
                && args[count] == elem
                && args[..count].iter().all(|&t| is_extent(t))
            {
                Ok(())
            } else {
                invalid()
            }
        }
        PjrtFn::Dim => {
            if args.len() == 2 && is_extent(args[1]) && is_extent(result) {
                Ok(())
            } else {
                invalid()
            }
        }
        PjrtFn::Upload | PjrtFn::Download => {
            if args.len() != 1 {
                return invalid();
            }
            let host = if op == PjrtFn::Upload {
                args[0]
            } else {
                result
            };
            let valid = if dims.is_empty() {
                host == elem
            } else {
                matches!(host.kind(), TyKind::Array { elem: h, dims: d } if *h == elem && *d == dims)
            };
            if valid { Ok(()) } else { invalid() }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intrinsic_op_round_trips_through_textual_triple() {
        for op in [
            IntrinsicOp::Panic,
            IntrinsicOp::Pjrt {
                func: PjrtFn::Alloc,
            },
            IntrinsicOp::Pjrt {
                func: PjrtFn::Upload,
            },
            IntrinsicOp::Pjrt {
                func: PjrtFn::Download,
            },
            IntrinsicOp::Pjrt { func: PjrtFn::Dim },
            IntrinsicOp::Math {
                func: MathFn::Sqrt,
                flag: 0,
            },
            IntrinsicOp::Math {
                func: MathFn::Fma,
                flag: 127,
            },
            IntrinsicOp::Cell {
                func: CellFn::Alloc,
            },
            IntrinsicOp::Cell { func: CellFn::Rmw },
            IntrinsicOp::Cell {
                func: CellFn::InUse,
            },
            IntrinsicOp::ConstSeq {
                func: ConstSeqFn::Len,
            },
            IntrinsicOp::ConstSeq {
                func: ConstSeqFn::Get,
            },
        ] {
            assert_eq!(
                IntrinsicOp::parse(op.family(), op.name(), op.imm()),
                Some(op)
            );
        }
        for func in [ObjectFn::IsUnique, ObjectFn::UnsafeInc, ObjectFn::UnsafeDec] {
            let op = IntrinsicOp::Object { func };
            assert_eq!(
                IntrinsicOp::parse(op.family(), op.name(), op.imm()),
                Some(op)
            );
        }
        for func in [
            PtrFn::Offset,
            PtrFn::Mask,
            PtrFn::Addr,
            PtrFn::ExposeAddr,
            PtrFn::FromAddr,
            PtrFn::WithProvenance,
            PtrFn::AsConst,
            PtrFn::AsMut,
            PtrFn::LaunderInvariantGroup,
            PtrFn::StripInvariantGroup,
            PtrFn::Diff,
            PtrFn::Load,
            PtrFn::Store,
        ] {
            let op = IntrinsicOp::Ptr {
                func,
                access: PtrAccess::default(),
                arith: PtrArithFlag::None,
            };
            assert_eq!(
                IntrinsicOp::parse(op.family(), op.name(), op.imm()),
                Some(op)
            );
        }
        for flags in 0..16u32 {
            for log2 in 0..=32u32 {
                let access = PtrAccess::from_bits(flags | log2 << 8).expect("valid access");
                assert_eq!(access.bits(), flags | log2 << 8);
                assert!(access.align <= PtrAccess::MAX_ALIGN);
            }
        }
        assert_eq!(PtrAccess::from_bits(33 << 8), None);
        assert_eq!(PtrAccess::from_bits(1 << 4), None);
        let access = PtrAccess {
            volatile: true,
            nontemporal: false,
            invariant: true,
            invariant_group: true,
            align: 4096,
        };
        let load = IntrinsicOp::Ptr {
            func: PtrFn::Load,
            access,
            arith: PtrArithFlag::None,
        };
        for arith in PtrArithFlag::ALL {
            let offset = IntrinsicOp::Ptr {
                func: PtrFn::Offset,
                access: PtrAccess::default(),
                arith,
            };
            assert_eq!(
                IntrinsicOp::parse("ptr", "offset", offset.imm()),
                Some(offset)
            );
        }
        assert_eq!(IntrinsicOp::parse("ptr", "offset", 4), None);
        for &func in SimdFn::ALL {
            let op = IntrinsicOp::Simd { func };
            assert_eq!(
                IntrinsicOp::parse(op.family(), op.name(), op.imm()),
                func.is_lowered().then_some(op)
            );
        }
        assert_eq!(IntrinsicOp::parse("simd", "splat", 1), None);
        assert_eq!(IntrinsicOp::parse("ptr", "as_const", 1), None);
        assert_eq!(IntrinsicOp::parse("ptr", "load", load.imm()), Some(load));
        assert_eq!(IntrinsicOp::parse("ptr", "store", load.imm()), None);
        assert_eq!(IntrinsicOp::parse("ptr", "advance", 0), None);
        assert_eq!(IntrinsicOp::parse("object", "is_unique", 1), None);
        assert_eq!(IntrinsicOp::parse("object", "unknown", 0), None);
        assert_eq!(IntrinsicOp::parse("pjrt", "upload", 1), None);
        assert_eq!(IntrinsicOp::parse("pjrt", "unknown", 0), None);
        assert_eq!(IntrinsicOp::parse("math", "nope", 0), None);
        assert_eq!(IntrinsicOp::parse("panic", "panic", 1), None);
        assert_eq!(IntrinsicOp::parse("panic", "abort", 0), None);
        assert_eq!(IntrinsicOp::parse("cell", "get", 1), None);
        assert!(
            IntrinsicOp::parse("cell", "in_use", 0).is_some(),
            "one textual family; the operand type carries the flavor"
        );
        assert_eq!(IntrinsicOp::parse("refcell", "in_use", 0), None);
        assert_eq!(IntrinsicOp::parse("atomic", "sqrt", 0), None);
    }

    #[test]
    fn names_round_trip() {
        for f in [
            MathFn::Sqrt,
            MathFn::Isnan,
            MathFn::Powf,
            MathFn::Fma,
            MathFn::Fpowi,
        ] {
            assert_eq!(MathFn::parse(f.as_str()), Some(f));
        }
        assert_eq!(MathFn::parse("log"), None);
        assert_eq!(MathFn::parse("sincos"), None);
        for f in [
            CellFn::Alloc,
            CellFn::Get,
            CellFn::Set,
            CellFn::Rmw,
            CellFn::InUse,
        ] {
            assert_eq!(CellFn::parse(f.as_str()), Some(f));
        }
    }

    #[test]
    fn fastmath_attr_spellings() {
        assert_eq!(FastMath(0).mlir_attr(), None);
        assert_eq!(
            FastMath(127).mlir_attr().as_deref(),
            Some("#arith.fastmath<fast>")
        );
        assert_eq!(
            FastMath(6).mlir_attr().as_deref(),
            Some("#arith.fastmath<nnan,ninf>")
        );
        assert_eq!(
            FastMath(1).mlir_attr().as_deref(),
            Some("#arith.fastmath<reassoc>")
        );
    }
}
