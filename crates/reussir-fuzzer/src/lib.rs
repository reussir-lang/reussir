//! Entrypoint and mutational fuzzing harness for static-library Reussir test suites.
//!
//! A Reussir test suite compiled as a static library exports `reussir_fuzz_init`
//! and calls `test::fuzz::init(suite)`. During initialization, the Reussir side
//! derives its [`spec::SuiteSpec`], serializes it to bytes, and registers its
//! `TestTree` handle and `reussir_fuzz_run` trampoline via
//! [`__reussir_fuzzer_register`].
//!
//! The Rust binary links the static library and invokes [`fuzz_target!`]:
//!
//! ```ignore
//! reussir_fuzzer::fuzz_target!();
//! ```

use std::ffi::{OsString, c_void};
use std::path::PathBuf;
use std::process::ExitCode;
use std::sync::Mutex;

use palc::Parser;

pub mod mutator;
pub mod spec;

use mutator::Rng;
use spec::{CaseSpec, SuiteSpec};

/// Function pointer that increments the reference count of a bridged Reussir `TestTree`.
pub type CloneTreeFn = unsafe extern "C" fn(*mut c_void) -> *mut c_void;

/// Function pointer that decrements the reference count of a bridged Reussir `TestTree`.
pub type DropTreeFn = unsafe extern "C" fn(*mut c_void);

/// Function pointer that executes one suite-level byte buffer against a bridged
/// Reussir `TestTree` (consuming one reference to `tree`).
pub type RunFuzzFn = unsafe extern "C" fn(*mut c_void, *const u8, usize) -> i32;

struct RawRegistration {
    tree: usize,
    clone_fn: CloneTreeFn,
    drop_fn: DropTreeFn,
    run_fn: RunFuzzFn,
    spec_bytes: Vec<u8>,
}

static REGISTERED: Mutex<Option<RawRegistration>> = Mutex::new(None);

/// FFI callback invoked by `test::fuzz::init` during `reussir_fuzz_init`.
///
/// # Safety
///
/// `tree` must be a valid owned pointer to the Reussir `TestTree` managed by
/// `clone_fn` and `drop_fn`, and `spec_ptr` must be valid for `spec_len` bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __reussir_fuzzer_register(
    tree: *mut c_void,
    clone_fn: CloneTreeFn,
    drop_fn: DropTreeFn,
    run_fn: RunFuzzFn,
    spec_ptr: *const u8,
    spec_len: usize,
) {
    let spec_bytes = if spec_len == 0 {
        Vec::new()
    } else {
        unsafe { std::slice::from_raw_parts(spec_ptr, spec_len) }.to_vec()
    };
    let mut guard = REGISTERED
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(prev) = guard.take() {
        unsafe { (prev.drop_fn)(prev.tree as *mut c_void) };
    }
    *guard = Some(RawRegistration {
        tree: tree as usize,
        clone_fn,
        drop_fn,
        run_fn,
        spec_bytes,
    });
}

/// Active fuzzing session holding the initialized Reussir `TestTree` handle and
/// its deserialized [`SuiteSpec`].
pub struct FuzzSession {
    tree: *mut c_void,
    clone_fn: CloneTreeFn,
    drop_fn: DropTreeFn,
    run_fn: RunFuzzFn,
    spec_bytes: Vec<u8>,
    spec: SuiteSpec,
}

impl FuzzSession {
    /// Initializes a fuzzing session by invoking the Reussir `reussir_fuzz_init`
    /// entry symbol and decoding the registered [`SuiteSpec`].
    pub fn initialize(init_fn: unsafe extern "C" fn()) -> Result<Self, String> {
        unsafe { init_fn() };
        let reg = REGISTERED
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
            .ok_or_else(|| {
                "reussir_fuzz_init completed without calling test::fuzz::init".to_owned()
            })?;
        let Some(spec) = SuiteSpec::from_bytes(&reg.spec_bytes) else {
            unsafe { (reg.drop_fn)(reg.tree as *mut c_void) };
            return Err("failed to decode SuiteSpec registered by test::fuzz::init".to_owned());
        };
        Ok(Self {
            tree: reg.tree as *mut c_void,
            clone_fn: reg.clone_fn,
            drop_fn: reg.drop_fn,
            run_fn: reg.run_fn,
            spec_bytes: reg.spec_bytes,
            spec,
        })
    }

    /// Returns the deserialized [`SuiteSpec`] for the registered `TestTree`.
    pub const fn spec(&self) -> &SuiteSpec {
        &self.spec
    }

    /// Returns the raw serialized `"RTSP"` v1 bytes for the [`SuiteSpec`].
    pub fn spec_bytes(&self) -> &[u8] {
        &self.spec_bytes
    }

    /// Feeds one suite-level byte buffer into the Reussir test runner (`reussir_fuzz_run`).
    ///
    /// Returns `0` on pass, `1` on test failure, and `-1` on malformed input.
    pub fn run_bytes(&self, bytes: &[u8]) -> i32 {
        let cloned = unsafe { (self.clone_fn)(self.tree) };
        unsafe { (self.run_fn)(cloned, bytes.as_ptr(), bytes.len()) }
    }
}

impl Drop for FuzzSession {
    fn drop(&mut self) {
        if !self.tree.is_null() {
            unsafe { (self.drop_fn)(self.tree) };
            self.tree = std::ptr::null_mut();
        }
    }
}

/// Command-line interface for a `reussir-fuzzer` target binary.
#[derive(Parser, Debug)]
#[command(name = "reussir-fuzzer")]
pub struct FuzzCli {
    /// Run only test cases whose `/`-joined path contains `<PATTERN>`.
    #[arg(short = 'm', long = "match")]
    pub matches: Vec<String>,

    /// Run only test cases whose `/`-joined path equals `<PATH>`.
    #[arg(short = 'e', long = "exact")]
    pub exact: Vec<String>,

    /// Skip test cases whose `/`-joined path contains `<PATTERN>`.
    #[arg(short = 's', long = "skip")]
    pub skip: Vec<String>,

    /// Number of fuzzing iterations per property test case.
    #[arg(short = 'n', long = "iterations", default_value_t = 100)]
    pub iterations: u64,

    /// Deterministic 64-bit PRNG seed (uses OS entropy when omitted).
    #[arg(long = "seed")]
    pub seed: Option<u64>,

    /// List matching test cases and their input specs without running them.
    #[arg(short = 'l', long = "list", default_value_t = false)]
    pub list: bool,

    /// Write the binary SuiteSpec (`RTSP` v1) to `<PATH>`.
    #[arg(long = "dump-spec")]
    pub dump_spec: Option<PathBuf>,
}

impl FuzzCli {
    fn selects(&self, case: &CaseSpec) -> bool {
        let joined = case.joined_path();
        if self.skip.iter().any(|pat| joined.contains(pat)) {
            return false;
        }
        if self.matches.is_empty() && self.exact.is_empty() {
            return true;
        }
        self.matches.iter().any(|pat| joined.contains(pat)) || self.exact.contains(&joined)
    }
}

/// Runs the fuzzer entrypoint using process command-line arguments.
pub fn run_fuzzer(init_fn: unsafe extern "C" fn()) -> ExitCode {
    run_fuzzer_with_args(init_fn, std::env::args_os())
}

/// Runs the fuzzer entrypoint with an explicit argument iterator.
pub fn run_fuzzer_with_args(
    init_fn: unsafe extern "C" fn(),
    args: impl IntoIterator<Item = OsString>,
) -> ExitCode {
    let cli = match FuzzCli::try_parse_from(args) {
        Ok(cli) => cli,
        Err(err) => match err.try_into_help() {
            Ok(help) => {
                println!("{help}");
                return ExitCode::SUCCESS;
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::from(2);
            }
        },
    };

    let session = match FuzzSession::initialize(init_fn) {
        Ok(session) => session,
        Err(err) => {
            eprintln!("error: {err}");
            return ExitCode::from(2);
        }
    };

    if let Some(path) = &cli.dump_spec
        && let Err(err) = std::fs::write(path, session.spec_bytes())
    {
        eprintln!("error: failed to write spec to `{}`: {err}", path.display());
        return ExitCode::from(2);
    }

    let selected: Vec<(usize, &CaseSpec)> = session
        .spec()
        .cases
        .iter()
        .enumerate()
        .filter(|(_, case)| cli.selects(case))
        .collect();

    if cli.list {
        for (_, case) in &selected {
            println!("{case}");
        }
        return ExitCode::SUCCESS;
    }

    let (seed, mut rng) = match cli.seed {
        Some(seed) => (seed, Rng::from_seed(seed)),
        None => Rng::from_entropy(),
    };

    let iterations = cli.iterations.max(1);
    let mut examples = 0usize;
    let mut failures = 0usize;

    for (case_idx, case) in selected {
        examples += 1;
        let path = case.joined_path();
        if case.input.is_unit() {
            let input = session.spec().generate_for_case(case_idx, &mut rng);
            let rc = session.run_bytes(&input);
            if rc == 0 {
                println!("✓ {path}");
            } else {
                println!("✗ {path}");
                failures += 1;
            }
            continue;
        }

        let mut corpus: Vec<Vec<u8>> = Vec::with_capacity(16);
        let mut failed = false;
        for step in 0..iterations {
            let candidate = if corpus.is_empty() || step < 4 || (rng.next_u64() & 1) == 0 {
                session.spec().generate_for_case(case_idx, &mut rng)
            } else {
                let base_idx = (rng.next_u64() as usize) % corpus.len();
                let mut mutated = corpus[base_idx].clone();
                session
                    .spec()
                    .mutate_for_case(case_idx, &mut mutated, &mut rng);
                mutated
            };

            let rc = session.run_bytes(&candidate);
            if rc == 1 {
                println!("✗ {path} (failed at iteration {}, seed {seed})", step + 1);
                failures += 1;
                failed = true;
                break;
            } else if rc == 0 && corpus.len() < 32 {
                corpus.push(candidate);
            }
        }

        if !failed {
            println!("✓ {path} ({iterations} fuzz runs)");
        }
    }

    println!(
        "\n{} example{}, {} failure{} (seed: {seed})",
        examples,
        if examples == 1 { "" } else { "s" },
        failures,
        if failures == 1 { "" } else { "s" },
    );

    if failures == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

/// Defines the `main` entrypoint for a fuzzed Reussir static library target.
///
/// By default, this calls the `reussir_fuzz_init` C trampoline exported by the
/// Reussir test archive; an alternative symbol name may be passed explicitly.
#[macro_export]
macro_rules! fuzz_target {
    () => {
        $crate::fuzz_target!(reussir_fuzz_init);
    };
    ($init:ident) => {
        extern crate reussir_rt;

        fn main() -> ::std::process::ExitCode {
            unsafe extern "C" {
                fn $init();
            }
            $crate::run_fuzzer($init)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spec::{Charset, FloatWidth, IntWidth, ValueSpec};

    #[test]
    fn value_spec_and_suite_spec_round_trip_bytes() {
        let complex = ValueSpec::Tuple(vec![
            ValueSpec::Unit,
            ValueSpec::Bool,
            ValueSpec::Int {
                width: IntWidth::I64,
                min: -100,
                max: 250,
            },
            ValueSpec::UInt {
                width: IntWidth::I16,
                min: 10,
                max: 500,
            },
            ValueSpec::Float {
                width: FloatWidth::F64,
                min: -1.5,
                max: 2.5,
                allow_nan: true,
                allow_inf: false,
            },
            ValueSpec::Char(Charset::Identifier),
            ValueSpec::String {
                charset: Charset::Alphanumeric,
                min_len: 1,
                max_len: 12,
            },
            ValueSpec::Array {
                elem: Box::new(ValueSpec::Int {
                    width: IntWidth::I32,
                    min: 0,
                    max: 99,
                }),
                min_len: 2,
                max_len: 8,
            },
        ]);
        let encoded = complex.to_bytes();
        assert_eq!(ValueSpec::from_bytes(&encoded).as_ref(), Some(&complex));

        let suite = SuiteSpec {
            cases: vec![
                CaseSpec {
                    path: vec!["math".into(), "unit".into()],
                    input: ValueSpec::Unit,
                },
                CaseSpec {
                    path: vec!["math".into(), "prop".into()],
                    input: complex,
                },
            ],
        };
        let suite_bytes = suite.to_bytes();
        assert_eq!(SuiteSpec::from_bytes(&suite_bytes).as_ref(), Some(&suite));
    }

    #[test]
    fn mutator_preserves_case_selector_prefix() {
        let suite = SuiteSpec {
            cases: vec![
                CaseSpec {
                    path: vec!["suite".into(), "first".into()],
                    input: ValueSpec::Unit,
                },
                CaseSpec {
                    path: vec!["suite".into(), "second".into()],
                    input: ValueSpec::Array {
                        elem: Box::new(ValueSpec::Int {
                            width: IntWidth::I64,
                            min: -50,
                            max: 50,
                        }),
                        min_len: 1,
                        max_len: 6,
                    },
                },
            ],
        };
        let mut rng = Rng::from_seed(42);
        let mut buf = suite.generate_for_case(1, &mut rng);
        assert_eq!(u64::from_le_bytes(buf[..8].try_into().unwrap()), 1);
        for _ in 0..32 {
            suite.mutate_for_case(1, &mut buf, &mut rng);
            assert_eq!(u64::from_le_bytes(buf[..8].try_into().unwrap()), 1);
        }
    }
}
