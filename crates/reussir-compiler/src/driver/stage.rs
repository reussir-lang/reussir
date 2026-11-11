//! The stage chain and the emission knobs riding on it: where the pipeline
//! can be entered and left, and the value-enum flavors of the codegen options
//! (`--lto`, `--runtime-linkage`, `--sanitizer`,
//! `--variant-encoding`).

use std::path::Path;

use reussir_backend::llvm::LtoMode;
use reussir_backend::pipeline::VariantEncoding as BackendVariantEncoding;
use reussir_codegen::lower::Sanitizer;

use crate::OutputKind;

/// A stage on the compilation chain, ordered from source to object so a target
/// can be compared against the input (`>=` means "reachable going forward").
///
/// Deriving [`palc::ValueEnum`] gives `--emit`/`--from` their value parsing and a
/// kebab-case [`Display`] (`MlirLlvm` → `mlir-llvm`, `LlvmIr` → `llvm-ir`, …).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, palc::ValueEnum)]
pub(crate) enum Stage {
    /// Reussir source (`.rr`).
    Rr,
    /// Elaborated, still-polymorphic HIR (`.hir`).
    Hir,
    /// The package interface (`.rri`): the HIR reduced to the export closure
    /// behind a versioned header — what a foreign package needs to
    /// type-check against this one and monomorphize its generics locally.
    /// Emit-only, and only from a package: a plain dump carries no
    /// authoritative package name for the header. See `docs/design/rri.md`.
    Rri,
    /// Monomorphized ground MIR (`.mir`).
    Mir,
    /// The Reussir MLIR dialect, before the lowering pipeline (`.mlir`).
    Mlir,
    /// MLIR after the lowering pipeline — the LLVM dialect (`--emit mlir-llvm`).
    MlirLlvm,
    /// LLVM IR text (`.ll`).
    LlvmIr,
    /// Target assembly (`.s`).
    Asm,
    /// A relocatable object file (`.o`).
    Obj,
    /// A static library archiving the codegen units (`.a`/`.lib`).
    Staticlib,
    /// A linked dynamic library (`.so`/`.dylib`/`.dll`) exporting the
    /// program's `extern` trampolines.
    Dynlib,
    /// A linked executable (`.exe` on Windows), entered through the program's
    /// `#[main]` function.
    Executable,
}

/// Whether the emitted code is packaged for link-time optimization, and how.
/// A static library's members become bitcode the consumer's linker optimizes
/// across, instead of finished native objects; for the two linked targets the
/// driver's own link step hands that bitcode to the linker.
#[derive(Clone, Copy, PartialEq, Eq, palc::ValueEnum)]
pub(crate) enum Lto {
    /// No LTO: each codegen unit is compiled to native code here.
    None,
    /// ThinLTO: bitcode plus a module summary index, so the linker imports
    /// across modules without merging them.
    Thin,
    /// Full ("fat") LTO: bitcode the linker merges into one module.
    Fat,
}

/// How a linked target (`--emit executable`/`dynlib`) carries the Reussir
/// runtime.
#[derive(Clone, Copy, PartialEq, Eq, palc::ValueEnum)]
pub(crate) enum RuntimeLinkage {
    /// Bundle the runtime's static archive into the product. Self-contained —
    /// the result has no load-time dependency on a Reussir library — and the
    /// default.
    Static,
    /// Link against the shared runtime library. One runtime per process no
    /// matter how many Reussir products load into it, but the library must be
    /// found at load time (rpath or the platform's search path).
    Dynamic,
}

impl From<Lto> for LtoMode {
    fn from(lto: Lto) -> Self {
        match lto {
            Lto::None => LtoMode::None,
            Lto::Thin => LtoMode::Thin,
            Lto::Fat => LtoMode::Fat,
        }
    }
}

/// CLI surface for [`Sanitizer`]: the kinds a build may declare via
/// `--sanitizer` (matching `REUSSIR_RUNTIME_SANITIZERS` plus `undefined`).
#[derive(Clone, Copy, PartialEq, Eq, palc::ValueEnum)]
pub(crate) enum SanitizerCli {
    /// AddressSanitizer: annotates functions `sanitize_address`.
    Address,
    /// LeakSanitizer: no function attribute (pure runtime mechanism).
    Leak,
    /// MemorySanitizer: annotates functions `sanitize_memory`.
    Memory,
    /// ThreadSanitizer: annotates functions `sanitize_thread`.
    Thread,
    /// UndefinedBehaviorSanitizer: no function attribute (UBSan checks are
    /// emitted by the C/C++ frontend, not an attribute-driven LLVM pass).
    Undefined,
}

impl From<SanitizerCli> for Sanitizer {
    fn from(kind: SanitizerCli) -> Self {
        match kind {
            SanitizerCli::Address => Sanitizer::Address,
            SanitizerCli::Leak => Sanitizer::Leak,
            SanitizerCli::Memory => Sanitizer::Memory,
            SanitizerCli::Thread => Sanitizer::Thread,
            SanitizerCli::Undefined => Sanitizer::Undefined,
        }
    }
}

/// CLI encoding selection, including automatic target-based selection.
#[derive(Clone, Copy, PartialEq, Eq, palc::ValueEnum)]
pub(crate) enum VariantEncoding {
    /// TBI on aarch64, immortal nullary constructors elsewhere.
    Auto,
    /// Encode constructor tags in the pointer's top byte (aarch64).
    Tbi,
    /// Represent nullary constructors by immortal dummy-box addresses.
    ImmortalNullary,
    /// Heap-box every constructor.
    Boxed,
}

impl VariantEncoding {
    pub(crate) fn resolve(self, triple: &str) -> Result<BackendVariantEncoding, String> {
        match self {
            Self::Auto if triple.starts_with("aarch64") => Ok(BackendVariantEncoding::Tbi),
            Self::Auto | Self::ImmortalNullary => Ok(BackendVariantEncoding::ImmortalNullary),
            Self::Tbi if triple.starts_with("aarch64") => Ok(BackendVariantEncoding::Tbi),
            Self::Tbi => Err("--variant-encoding tbi requires an aarch64 target".into()),
            Self::Boxed => Ok(BackendVariantEncoding::Boxed),
        }
    }
}

impl Stage {
    /// Whether this stage can be *read* as an input (it has a parser). The two
    /// derived MLIR/assembly forms and the object file are outputs only.
    pub(crate) fn is_input(self) -> bool {
        matches!(
            self,
            Stage::Rr | Stage::Hir | Stage::Mir | Stage::Mlir | Stage::LlvmIr
        )
    }

    /// The stage a file extension denotes, if any.
    pub(crate) fn from_extension(path: &Path) -> Option<Stage> {
        Some(match path.extension().and_then(|e| e.to_str())? {
            "rr" => Stage::Rr,
            "hir" => Stage::Hir,
            "rri" => Stage::Rri,
            "mir" => Stage::Mir,
            "mlir" => Stage::Mlir,
            "mlir-llvm" => Stage::MlirLlvm,
            "ll" => Stage::LlvmIr,
            "s" => Stage::Asm,
            "o" => Stage::Obj,
            // `.lib` is MSVC's static library; a MinGW/Unix `.a` is the same
            // stage. Dynamic libraries are named per platform.
            "a" | "lib" => Stage::Staticlib,
            "so" | "dylib" | "dll" => Stage::Dynlib,
            "exe" => Stage::Executable,
            _ => return None,
        })
    }

    /// The artifact each codegen unit is emitted as. For a static library and
    /// the two linked targets that is their *intermediates*: finished objects
    /// normally, bitcode under LTO — which the archive then collects, or the
    /// link step hands to the linker.
    pub(crate) fn output_kind(self, lto: Lto) -> Option<OutputKind> {
        match self {
            Stage::LlvmIr => Some(OutputKind::LlvmIr),
            Stage::Asm => Some(OutputKind::Assembly),
            Stage::Obj => Some(OutputKind::Object),
            Stage::Staticlib | Stage::Dynlib | Stage::Executable => Some(match lto {
                Lto::None => OutputKind::Object,
                packaged => OutputKind::Bitcode(packaged.into()),
            }),
            _ => None,
        }
    }

    /// Whether this stage is a linked product — the two targets the driver
    /// finishes with a link step rather than a file emitter.
    pub(crate) fn is_linked(self) -> bool {
        matches!(self, Stage::Dynlib | Stage::Executable)
    }
}
