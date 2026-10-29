use super::*;
use std::path::Path;

#[derive(Default)]
struct Request {
    input: &'static [u8],
    output: Vec<u8>,
    calls: usize,
    diagnostics: usize,
    fail: bool,
}
unsafe extern "C" fn produce(context: *mut c_void, output: *mut Produced) -> bool {
    let request = unsafe { &mut *context.cast::<Request>() };
    request.calls += 1;
    unsafe {
        *output = Produced {
            data: request.input.as_ptr(),
            size: request.input.len(),
        }
    };
    !request.fail
}
unsafe extern "C" fn receive(data: *const u8, size: usize, context: *mut c_void) {
    let request = unsafe { &mut *context.cast::<Request>() };
    request.output = unsafe { std::slice::from_raw_parts(data, size) }.to_vec();
}
unsafe extern "C" fn diagnostic(_: *const u8, _: usize, context: *mut c_void) {
    unsafe { &mut *context.cast::<Request>() }.diagnostics += 1;
}
fn open(directory: &Path) -> *mut Cache {
    Box::into_raw(Box::new(Cache(
        StoreBuilder::new().path(directory).open().unwrap(),
    )))
}

impl Request {
    fn run(&mut self, cache: *mut Cache, key: u8) -> u32 {
        self.output.clear();
        unsafe {
            reussir_polyffi_cache_get_or_produce(
                cache,
                &[key; 32],
                produce,
                receive,
                diagnostic,
                ptr::from_mut(self).cast(),
            )
        }
    }
}

#[test]
fn hits_skip_production_and_read_verbatim_database_bytes() {
    let directory = tempfile::tempdir().unwrap();
    let mut request = Request {
        input: b"\0BC\xff",
        ..Default::default()
    };
    let cache = open(directory.path());
    assert!(!cache.is_null());
    assert_eq!(request.run(cache, 1), 0);
    assert_eq!(request.run(cache, 1), 0);
    assert_eq!(request.calls, 1);
    let db = rusqlite::Connection::open(directory.path().join("cache.db")).unwrap();
    let (payload, external): (Vec<u8>, Option<Vec<u8>>) = db
        .query_row(
            "SELECT payload, object_key FROM entries WHERE kind=0",
            [],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .unwrap();
    assert_eq!(payload, request.input);
    assert!(external.is_none());
    assert!(!directory.path().join("objects").exists());
    // An external update is visible immediately: there is no resident byte cache.
    db.execute(
        "UPDATE entries SET payload=?1 WHERE kind=0",
        [b"new!".as_slice()],
    )
    .unwrap();
    assert_eq!(request.run(cache, 1), 0);
    assert_eq!(request.output, b"new!");
    unsafe { reussir_polyffi_cache_close(cache) };
    // The callback copied its borrowed bytes; they survive closing the store.
    assert_eq!(request.output, b"new!");
    request.fail = true;
    let reopened = open(directory.path());
    assert!(!reopened.is_null());
    assert_eq!(request.run(reopened, 1), 0);
    assert_eq!(request.calls, 1);
    assert_eq!(request.output, b"new!");
    unsafe { reussir_polyffi_cache_close(reopened) };
}

#[test]
fn producer_failures_are_not_cached_and_successful_retries_are() {
    let directory = tempfile::tempdir().unwrap();
    let mut request = Request {
        input: b"bitcode",
        fail: true,
        ..Default::default()
    };
    let cache = open(directory.path());
    assert!(!cache.is_null());
    assert_eq!(request.run(cache, 2), 2);
    assert_eq!(request.calls, 1);
    assert!(request.output.is_empty());
    request.fail = false;
    assert_eq!(request.run(cache, 2), 0);
    assert_eq!(request.run(cache, 2), 0);
    assert_eq!(request.calls, 2);
    assert_eq!(request.output, request.input);
    let db = rusqlite::Connection::open(directory.path().join("cache.db")).unwrap();
    assert_eq!(
        db.query_row("SELECT count(*) FROM entries", [], |row| row
            .get::<_, i64>(0))
            .unwrap(),
        1
    );
    unsafe { reussir_polyffi_cache_close(cache) };
}

#[test]
fn cache_failures_report_diagnostics_and_preserve_produced_bytes() {
    let directory = tempfile::tempdir().unwrap();
    let mut request = Request {
        input: b"usable",
        ..Default::default()
    };
    let cache = open(directory.path());
    assert!(!cache.is_null());
    let db = rusqlite::Connection::open(directory.path().join("cache.db")).unwrap();
    db.execute_batch("CREATE TRIGGER reject_blob BEFORE INSERT ON entries BEGIN SELECT RAISE(FAIL, 'write failure'); END;").unwrap();
    assert_eq!(request.run(cache, 3), 0);
    assert_eq!(request.output, request.input);
    assert_eq!(request.calls, 1);
    assert_eq!(request.diagnostics, 1);
    unsafe { reussir_polyffi_cache_close(cache) };
}
