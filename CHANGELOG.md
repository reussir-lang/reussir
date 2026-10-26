# Changelog

## Unreleased

### Language

- `&&` and `||` now short-circuit. Elaboration desugars them into the
  conditional — `l && r` to `if l { r } else { false }` and `l || r` to
  `if l { true } else { r }` — so the right operand becomes a branch body
  and lowers to `scf.if` like every other conditional. They previously
  elaborated to `ArithOp::And`/`Or` alongside the bitwise operators and
  emitted `arith.andi`/`arith.ori`, which evaluate both operands: a guard
  such as `d != 0 && n / d > 1` divided by zero, and an early-exit
  traversal written with `||` visited every node. The eager `ArithOp`
  forms stay reachable from hand-written HIR/MIR text, where they keep
  their bitwise meaning.
- Record fields now carry visibility: fields default to private and accept a
  leading `pub` marker. A private field is accessible (projection,
  assignment, construction) only from the record's defining module and its
  descendant modules, inside a package and across packages alike; the marker
  round-trips through HIR text and ships in `.rri` interfaces.
  Enum-variant payloads are unaffected — variants stay
  exactly as visible as their enum.

- `char` casts to any integer type: `as` on a `char` source reads the
  code point as an unsigned 32-bit value, truncating to narrower targets
  (`'\u{1F600}' as u8` is `0`) and zero-extending to wider ones. Nothing
  casts *to* `char` — an arbitrary integer is not necessarily a scalar
  value, and the FFI boundary relies on every `char` being one. This
  retires `std::hash`'s `char_code` FFI shim: `write_char` now casts
  natively.
- `str` is now usable, not just constructible (see `docs/design/str.md`:
  `str` is exactly the immutable static literal; growable strings stay
  outside the language behind the FFI). String literals gain the read
  intrinsics `core::intrinsic::str::{len, byte_at, slice}` (byte
  granularity, checked/clamped), the full builtin comparison tower
  (`==`/`!=`/orderings/`cmp` via new `reussir.str.equal` and
  `reussir.str.compare` ops, expanded inline in `convert-to-std`: `scf`
  fast paths — `str.ref_eq` view identity and the length gate — around a
  `memcmp` call through the `str.unsafe_memcmp` residue), record-field
  placement (`str` now carries a data layout and is trivially copyable),
  and FFI crossing: a `str` parameter or return renders as the runtime's
  `#[repr(C)]` `collections::string::Str<'static>`, bit-identical to the
  lowered `{ptr, len}` pair. With `std::hash::Hash for str` (a runtime
  content digest fed to the caller's hasher) a string literal keys every
  container: the hash containers hash it on the Reussir side, and the
  Rust-backed ordered containers order the runtime `Str` byte-wise,
  matching `str.compare`.

### Standard library

- `std::string::String`: the growable counterpart to `str`, wrapping the
  runtime's rc-boxed copy-on-write Rust `String` through the polymorphic
  FFI (the same raw-handle pattern as the `cow` collections). Initial
  surface: `new`, `from_str`, `len`, `is_empty`, `push_char`, `push_str`,
  `eq_str`, and `PartialEq`/`Eq`. The buffer is always UTF-8, so
  `push_str` must not be fed a `str::slice` that splits a multi-byte
  character.
- `WavlSet` and `WavlMap` now carry the same interface as the hash
  containers, plus the operations only an ordered container can offer.
  Shared with `HashSet`/`HashMap`: `singleton`, `clear`, `get`/`get_entry`,
  `take`/`remove_entry`, `insert_result`, `insert_with`, `adjust`, `update`,
  `alter`, `keys`, `values`, `filter`, `filter_map`, `map`, `map_values`,
  `map_keys`, `map_entries`, `count_if`, `any`, `all`, `find`, the set and
  map algebra (`union`, `union_with`, `intersection`, `intersection_with`,
  `difference`, `symmetric_difference`), the containment relations, and
  `PartialEq`/`Eq`. Ordered-only: `min`/`max` gain `pop_min`/`pop_max`,
  the neighbour queries `floor`, `ceiling`, `predecessor`, `successor`
  (`*_entry` on the map), the half-open window queries `range` and
  `range_to_list`, the descending traversals `to_list_desc` and
  `fold_right`, and `depth`, which reports the tree height so the balance
  bound is observable from outside the module.
- The ordered containers' `any`, `all`, and `find` stop at the first
  decisive element rather than folding the whole container, and `find`
  returns the least match rather than an unspecified one.
- New `std::collections::cow` module: copy-on-write `HashMap` and
  `HashSet` backed by the runtime's hashbrown table
  (`reussir_rt::collections::{hash_map, hash_set}`, built on
  `hashbrown::HashTable`). Keys hash on the Reussir side with
  `FastHasher` and the digest crosses the FFI boundary with the key, so
  the runtime never hashes: each entry caches its digest, growth and the
  copy-on-write clone reuse it, and key equality — bridged to the
  Reussir `Eq` impl — is consulted only when the cached digests match.
  Core operations for both containers: `new`, `with_seed`, `singleton`,
  `len`, `is_empty`, `clear`, `insert`, `get`/`contains`, `remove`.
  Prefer them for linear build-query-discard usage; retained versions
  pay a full-table clone per divergence, where the `pure` tries share.
- `std::hash` implements `Hash` for the primitives (`u8`–`u64`,
  `i8`–`i64`, `bool`, `char`): each scalar hashes as its own typed
  write, so primitives feed hashers, `write` chains, and hash-container
  key positions directly.
- `std::collections::cow` grows five more containers over the runtime's
  Rust structures (`reussir_rt::collections::{vec, vec_deque, btree_map,
  btree_set, binary_heap}`): `Vec` (push/pop/get/set/insert_at/
  remove_at/last), `VecDeque` (push/pop at both ends, front/back/get),
  `BTreeMap` and `BTreeSet` (insert/get/contains/remove plus
  first/last), and `BinaryHeap` (push/pop/peek — Rust's max-heap;
  reverse the `Ord` for a min-heap). The ordered containers compare
  through the `Ord` bridge; the sequences need no bounds beyond
  FFI-crossability. All follow the module's copy-on-write contract:
  in-place when uniquely owned, one clone when shared, and empty pops
  and lookup misses never clone.

### Compiler and runtime

- Trait impls with method bodies are now accepted for builtin types
  (`impl Hash for u64 { … }`) when the trait is local to the package,
  taking the ordinary impl path with the scalar as the self type; dot
  dispatch on scalars and generic-bound dispatch select them like any
  impl. The intrinsic method-less special form stays reserved to lang
  traits declared by the current package, and implementing an extern
  trait for a builtin now reports the ordinary orphan violation. Member
  paths name the scalar head by its spelling
  (`std::hash::Hash::u64::hash`), which the interface grammar accepts
  as path segments.
- Interface loading registers a dependency's traits before its records,
  so a record binder bounding an interface-local trait
  (`struct Holder<T : Digest>`) no longer crashes every downstream
  compile of that dependency.
- Debug builds no longer fail LLVM verification on cross-package
  monomorphized instances: once a function carries a `DISubprogram`,
  location-less synthesized ops in its body are stamped with the
  conventional line-0 location.

- `rrc --instrument-nonlinear-ffi` (and the matching
  `instrument_nonlinear_ffi` profile knob in `rene`) instruments non-linear
  ffi/array usage: the new `reussir-instrument-nonlinear-ffi` pass guards
  every FFI-import call that consumes an rc'd `ffi_object`/`array` argument
  with a reference-count check, and a count other than one calls the new
  runtime entry point `__reussir_report_nonlinear_usage(file, line,
  col)`, which reports the call's source location on stderr. A still-shared
  value at the consuming boundary is the signature that the foreign side
  degrades to copy-on-write instead of updating in place; the check is
  purely diagnostic and runs after all rc optimization passes, so the
  observed count is the final one.
- `rrc --emit executable` and `--emit dynlib` now compile
  position-independent code when no `--relocation-mode` is given, as
  `rene` already did. Their link step makes a position-independent
  executable or a shared object, but the code was static on ELF: a table of
  code addresses (a closure vtable, or a retcon table of a recursive drop)
  in `.rodata` gave the executable text relocations (`DT_TEXTREL`), and lld
  refused to link it. An explicit `--relocation-mode` is kept, and the other
  outputs keep the target's default.

### Tooling

- `rene` keeps what every build has in common in a per-user cache instead
  of each build directory, starting with the sources of the bundled
  `core`, which every build now reads from one place. The cache is
  `$XDG_CACHE_HOME/rene` (`~/.cache/rene`) on Linux,
  `~/Library/Caches/rene` on macOS, and `%LOCALAPPDATA%\rene` on Windows;
  `RENE_CACHE_DIR` overrides it. Entries are keyed by their contents and
  built under an OS file lock, which a concurrent rene waits on with a
  message, and a ledger inside the cache records which running rene uses
  which entry (docs/design/cache-ledger.md). A cache rene cannot use
  safely, such as one it cannot write or one on a network file system,
  fails the command with an error naming the cache and suggesting
  `RENE_CACHE_DIR` on a local disk. Because core's sources move, an
  existing build directory rebuilds core once.
- The `reussir-rt` bake uses the toolchain the package's directory
  selects, wherever rene runs. rene asks the selected `rustc` for its
  sysroot from the package's directory, so the project's
  `rust-toolchain.toml` decides, and then runs that toolchain's `rustc` and
  (unless one is configured) its `cargo` by path; `rrc` gets the same
  `rustc`.
- The `reussir-rt` bake lives in the per-user cache too, one per toolchain,
  target, allocator backend, and rustflags environment, which every build
  on the machine shares: a new project's first build skips the cargo bake
  when an earlier build used the same toolchain and target. Cargo's outputs
  stay inside the bake whatever `CARGO_TARGET_DIR` or the cargo
  configuration says.
- The bundled `core` is compiled once per toolchain, target, and profile
  on the machine: its interface and archive live in the per-user cache,
  and a build directory that needs them copies them in instead of
  compiling `core` again. A second rene compiling the same `core` waits for
  the first and says so. A `core` a package brings by path is compiled in
  each build directory, like any other dependency.

## 0.1.0

The first tagged release of Reussir: an MLIR-based compiler framework for
token-based memory reuse in RC-managed functional programs, together with the
toolchain built on it.

### Language

- An ownership-aware functional surface language: records (`struct`/`enum`)
  with `value`/`shared`/`regional` capabilities, pattern matching with
  exhaustiveness checking, closures, generics with monomorphization, and
  modules with `mod`/`import`.
- Region-based local mutation: `regional` records and functions, `[field]`
  links, flex/rigid flexivity tracking, and region freezing.
- Polymorphic FFI against Rust (`#[ffi]`, raw MLIR bodies), `extern`
  monomorphization, and cross-package interfaces (`.rri`).
- Prelude intrinsics: cells (with synchronized variants), `Arc`, `Nullable`
  links, fixed-size multidimensional arrays, math intrinsics, and `#[main]`
  entry points.

### Compiler and runtime

- `rrc`, the clang-style pipeline driver over the stage chain
  `.rr → hir → mir → mlir → LLVM IR → objects/executables`, re-entrant at
  every dump; machine-target selection (`--target-triple`, `--target-cpu`,
  `--target-features`) including `wasm32-wasip1[-threads]`.
- The Reussir MLIR dialect: RC-managed objects as first-class IR, token-based
  reuse analysis across branches and regions, inc/dec cancellation, closure
  whole-program devirtualization, and LLVM lowering with debug info.
- `reussir-rt`, the Rust runtime for RC objects and regions, with
  sanitizer-instrumented builds (ASan/LSan/MSan/TSan) and Miri-checked core.

### Tooling

- `rene`, the package manager: Nickel manifests, path dependencies with
  pubgrub version solving, cross-package builds with a process pool (`-j`),
  profiles, machine targets (`--target`), product selection
  (`--bin`/`--lib`), freshness tracking, and JSON introspection
  (`rene inspect`).
- `rrepl`, the JIT-backed REPL with TUI, script mode, and state inspection.
- MLIR tools (`reussir-opt`, `reussir-translate`, `reussir-llvm-opt`) and
  LLDB pretty-printers.

### Platforms

Linux (x86_64, aarch64), macOS, and Windows (MSVC), on LLVM/MLIR 22.
