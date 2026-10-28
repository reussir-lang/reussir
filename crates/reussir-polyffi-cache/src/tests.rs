use super::*;
use std::{
    path::Path,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};

struct CacheHandle(*mut Cache);
impl CacheHandle {
    fn open(path: &Path) -> Self {
        let path = path.to_str().unwrap().as_bytes();
        let mut cache = ptr::null_mut();
        let mut error = ptr::null_mut();
        let status = unsafe {
            reussir_polyffi_cache_open(
                path.as_ptr(),
                path.len(),
                ptr::null(),
                0,
                &mut cache,
                &mut error,
            )
        };
        let error = ErrorHandle(error);
        assert_eq!(status, OK, "{}", error.message());
        assert!(error.0.is_null());
        Self(cache)
    }
    fn get(&self, key: &Key) -> (u32, BlobHandle, ErrorHandle) {
        let mut blob = ptr::null_mut();
        let mut error = ptr::null_mut();
        let status = unsafe { reussir_polyffi_cache_get(self.0, key, &mut blob, &mut error) };
        (status, BlobHandle(blob), ErrorHandle(error))
    }
    fn produce(&self, key: &Key, state: &State) -> (u32, BlobHandle, ErrorHandle) {
        let mut blob = ptr::null_mut();
        let mut error = ptr::null_mut();
        let status = unsafe {
            reussir_polyffi_cache_get_or_produce(
                self.0,
                key,
                Some(producer),
                ptr::from_ref(state).cast_mut().cast(),
                &mut blob,
                &mut error,
            )
        };
        (status, BlobHandle(blob), ErrorHandle(error))
    }
    fn evict(&self, key: &Key) {
        let mut error = ptr::null_mut();
        let status = unsafe { reussir_polyffi_cache_evict(self.0, key, &mut error) };
        let error = ErrorHandle(error);
        assert_eq!(status, OK, "{}", error.message());
    }
}
impl Drop for CacheHandle {
    fn drop(&mut self) {
        unsafe { reussir_polyffi_cache_destroy(self.0) };
    }
}
struct BlobHandle(*mut Blob);
impl BlobHandle {
    fn bytes(&self) -> &[u8] {
        let mut size = 0;
        let data = unsafe { reussir_polyffi_cache_blob_data(self.0, &mut size) };
        unsafe { std::slice::from_raw_parts(data, size) }
    }
}
impl Drop for BlobHandle {
    fn drop(&mut self) {
        unsafe { reussir_polyffi_cache_blob_destroy(self.0) };
    }
}
struct ErrorHandle(*mut CacheError);
impl ErrorHandle {
    fn message(&self) -> String {
        let mut size = 0;
        let data = unsafe { reussir_polyffi_cache_error_message(self.0, &mut size) };
        String::from_utf8_lossy(unsafe { std::slice::from_raw_parts(data, size) }).into_owned()
    }
}
impl Drop for ErrorHandle {
    fn drop(&mut self) {
        unsafe { reussir_polyffi_cache_error_destroy(self.0) };
    }
}
struct State {
    calls: AtomicUsize,
    bytes: &'static [u8],
    persist: u8,
    failure: u32,
    delay: bool,
}
impl State {
    fn new(bytes: &'static [u8]) -> Self {
        Self {
            calls: AtomicUsize::new(0),
            bytes,
            persist: 1,
            failure: 0,
            delay: false,
        }
    }
}
unsafe extern "C" fn producer(user_data: *mut c_void, output: *mut Produced) -> u32 {
    let state = unsafe { &*user_data.cast::<State>() };
    state.calls.fetch_add(1, Ordering::SeqCst);
    if state.delay {
        std::thread::sleep(Duration::from_millis(30));
    }
    if state.failure != 0 {
        return state.failure;
    }
    unsafe {
        *output = Produced {
            data: state.bytes.as_ptr(),
            size: state.bytes.len(),
            persist: state.persist,
        };
    }
    0
}
fn key(recipe: &[u8]) -> Key {
    let mut key = Key { bytes: [0; 32] };
    let status = unsafe {
        reussir_polyffi_cache_key_from_parts(
            b"test".as_ptr(),
            4,
            recipe.as_ptr(),
            recipe.len(),
            &mut key,
            ptr::null_mut(),
        )
    };
    assert_eq!(status, OK);
    assert_eq!(
        key.bytes,
        *compcache::Key::from_parts("test", recipe).as_bytes()
    );
    key
}
fn successful(result: (u32, BlobHandle, ErrorHandle)) -> BlobHandle {
    let (status, blob, error) = result;
    assert_eq!(status, OK, "{}", error.message());
    assert!(error.0.is_null(), "{}", error.message());
    assert!(!blob.0.is_null());
    blob
}

#[test]
fn opaque_bytes_are_verbatim_database_blobs_and_hits_skip_the_producer() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let key = key(b"opaque");
    let (status, blob, error) = cache.get(&key);
    assert_eq!(status, MISS);
    assert!(blob.0.is_null());
    assert!(error.0.is_null());
    let state = State::new(b"\0\xffopaque bytes\0");
    let cold = successful(cache.produce(&key, &state));
    assert_eq!(cold.bytes(), state.bytes);
    let mut must_not_run = State::new(b"wrong");
    must_not_run.failure = 31;
    assert_eq!(
        successful(cache.produce(&key, &must_not_run)).bytes(),
        state.bytes
    );
    assert_eq!(must_not_run.calls.load(Ordering::SeqCst), 0);
    let db = rusqlite::Connection::open(directory.path().join("cache.db")).unwrap();
    let (payload, external): (Vec<u8>, Option<Vec<u8>>) = db
        .query_row(
            "SELECT payload, object_key FROM entries WHERE kind=0",
            [],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .unwrap();
    assert_eq!(payload, state.bytes);
    assert!(external.is_none());
    assert!(!directory.path().join("objects").exists());
    assert_eq!(
        db.query_row("SELECT count(*) FROM entries WHERE kind=1", [], |row| row
            .get::<_, i64>(
            0
        ))
        .unwrap(),
        0
    );
}

#[test]
fn bypass_observes_other_handles_and_blobs_outlive_eviction_and_cache_drop() {
    let directory = tempfile::tempdir().unwrap();
    let first = CacheHandle::open(directory.path());
    let second = CacheHandle::open(directory.path());
    let key = key(b"shared");
    let retained = successful(first.produce(&key, &State::new(b"old")));
    second.evict(&key);
    assert_eq!(first.get(&key).0, MISS);
    successful(second.produce(&key, &State::new(b"new")));
    assert_eq!(successful(first.get(&key)).bytes(), b"new");
    drop(first);
    drop(second);
    assert_eq!(retained.bytes(), b"old");
}

#[test]
fn transient_results_are_not_admitted_or_persisted() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let mut state = State::new(b"temporary");
    state.persist = 0;
    let key = key(b"transient");
    let retained = successful(cache.produce(&key, &state));
    assert_eq!(retained.bytes(), b"temporary");
    assert_eq!(cache.get(&key).0, MISS);
    successful(cache.produce(&key, &state));
    assert_eq!(state.calls.load(Ordering::SeqCst), 2);
}

#[test]
fn producer_failures_are_typed_and_not_retried_or_published() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let key = key(b"failure");
    let mut failed = State::new(b"unused");
    failed.failure = 71;
    let (status, blob, error) = cache.produce(&key, &failed);
    assert_eq!(status, PRODUCER_ERROR);
    assert!(blob.0.is_null());
    assert_eq!(
        unsafe { reussir_polyffi_cache_error_kind(error.0) },
        PRODUCER_ERROR
    );
    assert!(error.message().contains("71"));
    assert_eq!(failed.calls.load(Ordering::SeqCst), 1);
    assert_eq!(cache.get(&key).0, MISS);
    successful(cache.produce(&key, &State::new(b"retry by caller")));
}

#[test]
fn persistence_failure_returns_the_usable_blob_and_a_warning() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let db = rusqlite::Connection::open(directory.path().join("cache.db")).unwrap();
    db.execute_batch("CREATE TRIGGER reject_blob BEFORE INSERT ON entries WHEN NEW.kind=0 BEGIN SELECT RAISE(FAIL, 'test write failure'); END;").unwrap();
    let state = State::new(b"still usable");
    let key = key(b"write-failure");
    let (status, blob, error) = cache.produce(&key, &state);
    assert_eq!(status, OK);
    assert_eq!(blob.bytes(), state.bytes);
    assert_eq!(
        unsafe { reussir_polyffi_cache_error_kind(error.0) },
        CACHE_ERROR
    );
    assert!(error.message().contains("test write failure"));
    assert_eq!(state.calls.load(Ordering::SeqCst), 1);
    assert_eq!(cache.get(&key).0, MISS);
}

#[test]
fn concurrent_requests_coalesce_without_retaining_payloads() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let address = cache.0 as usize;
    let mut state = State::new(b"one production");
    state.delay = true;
    let key = key(b"concurrent");
    std::thread::scope(|scope| {
        let threads: Vec<_> = (0..8)
            .map(|_| {
                let state = &state;
                let key = &key;
                scope.spawn(move || {
                    // Borrow the shared handle without transferring its ownership.
                    let cache = std::mem::ManuallyDrop::new(CacheHandle(address as *mut Cache));
                    assert_eq!(
                        successful(cache.produce(key, state)).bytes(),
                        b"one production"
                    );
                })
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
    });
    assert_eq!(state.calls.load(Ordering::SeqCst), 1);
}

#[test]
fn empty_bytes_and_null_destruction_are_supported() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let key = key(b"empty");
    assert!(
        successful(cache.produce(&key, &State::new(b"")))
            .bytes()
            .is_empty()
    );
    assert!(successful(cache.get(&key)).bytes().is_empty());
    unsafe {
        reussir_polyffi_cache_destroy(ptr::null_mut());
        reussir_polyffi_cache_blob_destroy(ptr::null_mut());
        reussir_polyffi_cache_error_destroy(ptr::null_mut());
        assert_eq!(reussir_polyffi_cache_error_kind(ptr::null()), OK);
    }
}

#[test]
fn bad_arguments_and_open_errors_do_not_return_handles() {
    let mut cache = ptr::null_mut();
    let mut error = ptr::null_mut();
    let status = unsafe {
        reussir_polyffi_cache_open(ptr::null(), 1, ptr::null(), 0, &mut cache, &mut error)
    };
    let error = ErrorHandle(error);
    assert_eq!(status, INVALID_ARGUMENT);
    assert!(cache.is_null());
    assert!(error.message().contains("byte span"));
    let directory = tempfile::tempdir().unwrap();
    let file = directory.path().join("regular-file");
    std::fs::write(&file, b"x").unwrap();
    let path = file.to_str().unwrap().as_bytes();
    let mut error = ptr::null_mut();
    let status = unsafe {
        reussir_polyffi_cache_open(
            path.as_ptr(),
            path.len(),
            ptr::null(),
            0,
            &mut cache,
            &mut error,
        )
    };
    let error = ErrorHandle(error);
    assert_eq!(status, CACHE_ERROR);
    assert!(cache.is_null());
    assert!(error.message().contains("regular-file"));
}

#[test]
fn internal_panics_are_contained_at_the_boundary() {
    let mut error = ptr::null_mut();
    let status = unsafe { boundary(&mut error, || panic!("contained test panic")) };
    let error = ErrorHandle(error);
    assert_eq!(status, PANIC);
    assert_eq!(unsafe { reussir_polyffi_cache_error_kind(error.0) }, PANIC);
}

#[test]
fn subprocess_worker() {
    let Some(directory) = std::env::var_os("REUSSIR_POLYFFI_TEST_ROOT") else {
        return;
    };
    let cache = CacheHandle::open(Path::new(&directory));
    let mut state = State::new(b"persisted across processes");
    let warm = std::env::var_os("REUSSIR_POLYFFI_TEST_WARM").is_some();
    if warm {
        state.failure = 81;
    }
    assert_eq!(
        successful(cache.produce(&key(b"process"), &state)).bytes(),
        state.bytes
    );
    assert_eq!(state.calls.load(Ordering::SeqCst), usize::from(!warm));
}

#[test]
fn blobs_persist_across_processes() {
    let directory = tempfile::tempdir().unwrap();
    for warm in [false, true] {
        let mut child = std::process::Command::new(std::env::current_exe().unwrap());
        child
            .args(["--exact", "tests::subprocess_worker"])
            .env("REUSSIR_POLYFFI_TEST_ROOT", directory.path());
        if warm {
            child.env("REUSSIR_POLYFFI_TEST_WARM", "1");
        }
        let output = child.output().unwrap();
        assert!(
            output.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn null_arguments_and_malformed_producer_output_are_rejected_without_publication() {
    let directory = tempfile::tempdir().unwrap();
    let cache = CacheHandle::open(directory.path());
    let key = key(b"invalid-arguments");
    let invalid = |status, error| {
        let error = ErrorHandle(error);
        assert_eq!(status, INVALID_ARGUMENT, "{}", error.message());
        assert_eq!(
            unsafe { reussir_polyffi_cache_error_kind(error.0) },
            INVALID_ARGUMENT
        );
    };
    let mut blob = ptr::null_mut();
    let mut error = ptr::null_mut();
    invalid(
        unsafe { reussir_polyffi_cache_get(ptr::null(), &key, &mut blob, &mut error) },
        error,
    );
    assert!(blob.is_null());
    invalid(
        unsafe { reussir_polyffi_cache_get(cache.0, ptr::null(), &mut blob, &mut error) },
        error,
    );
    assert!(blob.is_null());
    invalid(
        unsafe { reussir_polyffi_cache_get(cache.0, &key, ptr::null_mut(), &mut error) },
        error,
    );
    invalid(
        unsafe {
            reussir_polyffi_cache_get_or_produce(
                cache.0,
                &key,
                None,
                ptr::null_mut(),
                &mut blob,
                &mut error,
            )
        },
        error,
    );
    assert!(blob.is_null());

    unsafe extern "C" fn malformed(context: *mut c_void, output: *mut Produced) -> u32 {
        let mode = unsafe { *context.cast::<u8>() };
        unsafe {
            *output = Produced {
                data: ptr::null(),
                size: usize::from(mode == 0),
                persist: if mode == 0 { 1 } else { 2 },
            };
        }
        0
    }
    for mode in [0u8, 1] {
        let status = unsafe {
            reussir_polyffi_cache_get_or_produce(
                cache.0,
                &key,
                Some(malformed),
                ptr::from_ref(&mode).cast_mut().cast(),
                &mut blob,
                &mut error,
            )
        };
        invalid(status, error);
        assert!(blob.is_null());
        assert_eq!(cache.get(&key).0, MISS);
    }
}
