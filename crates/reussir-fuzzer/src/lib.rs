//! Bridge between static-library Reussir fuzz targets, [`libfuzzer_sys`], and [`mutatis`].
//!
//! A Reussir test suite compiled for fuzzing exports `reussir_fuzz_init` and
//! calls `test::fuzz::init(suite)`. During initialization, the Reussir side
//! derives its [`spec::SuiteSpec`], serializes it to bytes, and registers its
//! `TestTree` handle and `reussir_fuzz_run` trampoline via
//! [`__reussir_fuzzer_register`].
//!
//! The fuzzer binary links the target archive and invokes [`fuzz_target!`]:
//!
//! ```ignore
//! #![no_main]
//! reussir_fuzzer::fuzz_target!();
//! ```

use std::cell::{Cell, OnceCell, RefCell};
use std::ffi::c_void;

pub use libfuzzer_sys;
pub use mutatis;

mod budget;
#[cfg(windows)]
mod coff_sections;
pub mod mutator;
mod selection;
pub mod spec;

pub use budget::InputBudgetError;
use spec::SuiteSpec;

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

thread_local! {
    static REGISTERED: RefCell<Option<RawRegistration>> = const { RefCell::new(None) };
    static SESSION: OnceCell<FuzzSession> = const { OnceCell::new() };
    static UNIT_CASES_CHECKED: Cell<bool> = const { Cell::new(false) };
}

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
    let previous = REGISTERED.replace(Some(RawRegistration {
        tree: tree as usize,
        clone_fn,
        drop_fn,
        run_fn,
        spec_bytes,
    }));
    if let Some(prev) = previous {
        unsafe { (prev.drop_fn)(prev.tree as *mut c_void) };
    }
}

/// Active fuzzing session holding the initialized Reussir `TestTree` handle and
/// its deserialized [`SuiteSpec`]. The owned tree stays on its initializing thread.
///
/// ```compile_fail
/// fn move_session(session: reussir_fuzzer::FuzzSession) {
///     std::thread::spawn(move || session.run_bytes(&[]));
/// }
/// ```
pub struct FuzzSession {
    tree: *mut c_void,
    clone_fn: CloneTreeFn,
    drop_fn: DropTreeFn,
    run_fn: RunFuzzFn,
    spec_bytes: Vec<u8>,
    spec: SuiteSpec,
    selection: selection::CaseSelection,
    budget_checked: Cell<bool>,
}

impl FuzzSession {
    /// Initializes a fuzzing session by invoking the Reussir `reussir_fuzz_init`
    /// entry symbol and decoding the registered [`SuiteSpec`].
    pub fn initialize(init_fn: unsafe extern "C" fn()) -> Result<Self, String> {
        unsafe { init_fn() };
        let reg = REGISTERED.take().ok_or_else(|| {
            "reussir_fuzz_init completed without calling test::fuzz::init".to_owned()
        })?;
        let Some(spec) = SuiteSpec::from_bytes(&reg.spec_bytes) else {
            unsafe { (reg.drop_fn)(reg.tree as *mut c_void) };
            return Err("failed to decode SuiteSpec registered by test::fuzz::init".to_owned());
        };
        let filter = std::env::var("REUSSIR_FUZZ_FILTER")
            .ok()
            .filter(|s| !s.is_empty());
        let selection = selection::CaseSelection::new(&spec, filter.as_deref());
        Ok(Self {
            tree: reg.tree as *mut c_void,
            clone_fn: reg.clone_fn,
            drop_fn: reg.drop_fn,
            run_fn: reg.run_fn,
            spec_bytes: reg.spec_bytes,
            spec,
            selection,
            budget_checked: Cell::new(false),
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

    /// Returns whether the case at `case_idx` is enabled under `REUSSIR_FUZZ_FILTER`.
    pub fn selects_case(&self, case_idx: usize) -> bool {
        self.selection.contains(case_idx)
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

/// Initializes libFuzzer and the registered Reussir [`FuzzSession`].
#[doc(hidden)]
pub fn libfuzzer_initialize(
    argc: *const isize,
    argv: *const *const *const u8,
    init_fn: unsafe extern "C" fn(),
) -> isize {
    libfuzzer_sys::initialize(argc, argv);
    let session = match FuzzSession::initialize(init_fn) {
        Ok(session) => session,
        Err(err) => {
            eprintln!("reussir-fuzzer initialization error: {err}");
            std::process::abort();
        }
    };
    let explicit_budget = std::env::args()
        .filter_map(|arg| {
            arg.strip_prefix("-max_len=")
                .and_then(|value| value.parse::<usize>().ok())
        })
        .next_back()
        .filter(|&limit| limit != 0);
    if let Some(budget) = explicit_budget {
        if let Err(error) = session
            .spec()
            .validate_input_budget(&session.selection.fuzz, budget)
        {
            eprintln!("reussir-fuzzer initialization error: {error}");
            std::process::abort();
        }
        session.budget_checked.set(true);
    }
    if let Ok(path) = std::env::var("REUSSIR_FUZZ_DUMP_SPEC")
        && !path.is_empty()
        && let Err(err) = std::fs::write(&path, session.spec_bytes())
    {
        eprintln!("reussir-fuzzer: failed to dump SuiteSpec to `{path}`: {err}");
        std::process::abort();
    }
    if std::env::var("REUSSIR_FUZZ_LIST").ok().as_deref() == Some("1") {
        for (idx, case) in session.spec().cases.iter().enumerate() {
            if session.selects_case(idx) {
                println!("{case}");
            }
        }
    }
    SESSION.with(|slot| {
        let _ = slot.set(session);
    });
    0
}

/// Executes one libFuzzer input against the active Reussir [`FuzzSession`].
#[doc(hidden)]
pub fn libfuzzer_test_one_input(bytes: &[u8]) -> libfuzzer_sys::Corpus {
    SESSION.with(|slot| {
        let Some(session) = slot.get() else {
            return libfuzzer_sys::Corpus::Reject;
        };
        if session.spec().cases.is_empty() {
            return libfuzzer_sys::Corpus::Reject;
        }

        // Run unit test cases once on the first invocation so deterministic cases in
        // a mixed TestTree are always exercised.
        if !UNIT_CASES_CHECKED.replace(true) {
            for (idx, case) in session.spec().cases.iter().enumerate() {
                if case.input.is_unit() && session.selects_case(idx) {
                    let buf = (idx as u64).to_le_bytes();
                    if session.run_bytes(&buf) == 1 {
                        panic!("Reussir unit test case `{}` failed", case.joined_path());
                    }
                }
            }
        }

        if bytes.len() < 8 {
            return libfuzzer_sys::Corpus::Reject;
        }
        let raw_idx = u64::from_le_bytes(bytes[..8].try_into().unwrap());
        let case_idx = (raw_idx as usize) % session.spec().cases.len();
        if !session.selects_case(case_idx) {
            return libfuzzer_sys::Corpus::Reject;
        }

        match session.run_bytes(bytes) {
            0 => libfuzzer_sys::Corpus::Keep,
            -1 => libfuzzer_sys::Corpus::Reject,
            _ => panic!(
                "Reussir fuzz target `{}` failed",
                session.spec().cases[case_idx].joined_path()
            ),
        }
    })
}

/// Mutates `data[..size]` in place within `max_size` using [`mutatis`] and
/// `libfuzzer-sys`'s `LLVMFuzzerMutate`.
#[doc(hidden)]
pub fn libfuzzer_custom_mutator(
    data: &mut [u8],
    size: usize,
    max_size: usize,
    seed: u32,
    raw_mutate: mutator::RawMutateFn,
) -> usize {
    SESSION.with(|slot| {
        let Some(session) = slot.get() else {
            return 0;
        };
        // With automatic max_len, libFuzzer resolves the corpus-derived budget
        // before the first custom mutation callback.
        if !session.budget_checked.replace(true)
            && let Err(error) = session
                .spec()
                .validate_input_budget(&session.selection.fuzz, max_size)
        {
            eprintln!("reussir-fuzzer input budget error: {error}");
            std::process::abort();
        }
        session.spec().mutate_buffer_selected(
            data,
            size,
            max_size,
            seed,
            Some(raw_mutate),
            &session.selection.fuzz,
        )
    })
}

/// Defines the libFuzzer entrypoints (`LLVMFuzzerInitialize`,
/// `rust_fuzzer_test_input`, and `LLVMFuzzerCustomMutator`) for a fuzzed
/// Reussir static library target.
///
/// The crate invoking this macro should be compiled with `#![no_main]` so
/// libFuzzer supplies the process `main` entrypoint.
#[macro_export]
macro_rules! fuzz_target {
    () => {
        $crate::fuzz_target!(reussir_fuzz_init);
    };
    ($init:ident) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn LLVMFuzzerInitialize(
            argc: *const isize,
            argv: *const *const *const u8,
        ) -> isize {
            unsafe extern "C" {
                fn $init();
            }
            $crate::libfuzzer_initialize(argc, argv, $init)
        }

        #[unsafe(no_mangle)]
        #[allow(improper_ctypes_definitions)]
        pub extern "C" fn rust_fuzzer_test_input(bytes: &[u8]) -> i32 {
            if let Some(path) = $crate::libfuzzer_sys::rust_libfuzzer_debug_path() {
                use ::std::io::Write;
                let mut file = ::std::fs::File::create(path)
                    .expect("failed to create `RUST_LIBFUZZER_DEBUG_PATH` file");
                let _ = writeln!(&mut file, "{bytes:?}");
                return 0;
            }
            $crate::libfuzzer_test_one_input(bytes).to_libfuzzer_code()
        }

        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn LLVMFuzzerCustomMutator(
            data: *mut u8,
            size: usize,
            max_size: usize,
            seed: ::std::os::raw::c_uint,
        ) -> usize {
            let len = ::std::cmp::max(max_size, size);
            let data_slice = unsafe { ::std::slice::from_raw_parts_mut(data, len) };
            let new_size = $crate::libfuzzer_custom_mutator(
                data_slice,
                size,
                max_size,
                seed as u32,
                $crate::libfuzzer_sys::fuzzer_mutate,
            );
            ::std::cmp::min(new_size, max_size)
        }
    };
}

#[cfg(test)]
mod tests {
    use mutatis::Session;

    use crate::spec::{CaseSpec, Charset, FloatWidth, IntWidth, SuiteSpec, ValueSpec};

    /// The unit-test binary links `libfuzzer_sys`, whose `LLVMFuzzerTestOneInput`
    /// calls the hook [`fuzz_target!`] defines in fuzzer binaries. COFF links
    /// resolve that reference even though the tests never run it.
    #[unsafe(no_mangle)]
    #[allow(improper_ctypes_definitions)]
    extern "C" fn rust_fuzzer_test_input(_bytes: &[u8]) -> i32 {
        0
    }

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
    fn mutatis_mutator_preserves_constraints_and_shrinks() {
        let suite = SuiteSpec {
            cases: vec![
                CaseSpec {
                    path: vec!["suite".into(), "first".into()],
                    input: ValueSpec::Unit,
                },
                CaseSpec {
                    path: vec!["suite".into(), "second".into()],
                    input: ValueSpec::Tuple(vec![
                        ValueSpec::Int {
                            width: IntWidth::I64,
                            min: -50,
                            max: 50,
                        },
                        ValueSpec::String {
                            charset: Charset::Identifier,
                            min_len: 2,
                            max_len: 8,
                        },
                        ValueSpec::Array {
                            elem: Box::new(ValueSpec::UInt {
                                width: IntWidth::I32,
                                min: 5,
                                max: 25,
                            }),
                            min_len: 1,
                            max_len: 6,
                        },
                    ]),
                },
            ],
        };

        let mut session = Session::new().seed(42);
        let mut buf = suite.generate_for_case(1, &mut session);
        assert_eq!(u64::from_le_bytes(buf[..8].try_into().unwrap()), 1);

        for seed in 1..=32 {
            suite.mutate_for_case(1, &mut buf, &mut session);
            assert_eq!(u64::from_le_bytes(buf[..8].try_into().unwrap()), 1);
            let decoded = suite.decode_input(&buf).expect("valid suite input");
            assert_eq!(decoded.case_idx, 1);

            let mut raw = vec![0u8; 256];
            let cur_len = buf.len().min(raw.len());
            raw[..cur_len].copy_from_slice(&buf[..cur_len]);
            let next_len = suite.mutate_buffer(&mut raw, cur_len, 256, seed, None);
            assert!(next_len >= 8);
            assert!(suite.decode_input(&raw[..next_len]).is_some());
        }
    }
}
