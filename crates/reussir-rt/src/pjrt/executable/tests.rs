use super::*;
use std::{
    ptr,
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

#[derive(Default)]
struct State {
    compiles: AtomicUsize,
    loads: AtomicUsize,
    destroys: AtomicUsize,
    clients_destroyed: AtomicUsize,
    failures: AtomicUsize,
    serialize_failure: std::sync::atomic::AtomicBool,
    rendezvous: Mutex<Option<Arc<std::sync::Barrier>>>,
    programs: Mutex<Vec<(Vec<u8>, Vec<u8>)>>,
}

unsafe fn state(client: *mut PJRT_Client) -> &'static State {
    unsafe { &*client.cast::<Arc<State>>() }
}

unsafe extern "C" fn compile(args: *mut PJRT_Client_Compile_Args) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    let state = unsafe { state(args.client) };
    let program = unsafe { &*args.program };
    assert_eq!(program.struct_size, PJRT_Program_STRUCT_SIZE as usize);
    assert_eq!(
        unsafe { std::slice::from_raw_parts(program.format.cast::<u8>(), program.format_size) },
        b"mlir"
    );
    state.programs.lock().unwrap().push((
        unsafe { std::slice::from_raw_parts(program.code.cast::<u8>(), program.code_size) }
            .to_vec(),
        unsafe {
            std::slice::from_raw_parts(args.compile_options.cast::<u8>(), args.compile_options_size)
        }
        .to_vec(),
    ));
    state.compiles.fetch_add(1, Ordering::SeqCst);
    let barrier = state.rendezvous.lock().unwrap().clone();
    if let Some(barrier) = barrier {
        barrier.wait();
    }
    #[allow(deprecated)] // Keep tests compatible with the bundled stable runtime.
    if state
        .failures
        .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |n| n.checked_sub(1))
        .is_ok()
    {
        return Box::into_raw(Box::new(0u8)).cast();
    }
    args.executable = Box::into_raw(Box::new(args.client)).cast();
    ptr::null_mut()
}

unsafe extern "C" fn destroy(args: *mut PJRT_LoadedExecutable_Destroy_Args) -> *mut PJRT_Error {
    let client = unsafe { Box::from_raw((*args).executable.cast::<*mut PJRT_Client>()) };
    unsafe { state(*client) }
        .destroys
        .fetch_add(1, Ordering::SeqCst);
    ptr::null_mut()
}

unsafe extern "C" fn destroy_client(args: *mut PJRT_Client_Destroy_Args) -> *mut PJRT_Error {
    let state = unsafe { Box::from_raw((*args).client.cast::<Arc<State>>()) };
    state.clients_destroyed.fetch_add(1, Ordering::SeqCst);
    ptr::null_mut()
}

unsafe extern "C" fn get_executable(
    args: *mut PJRT_LoadedExecutable_GetExecutable_Args,
) -> *mut PJRT_Error {
    unsafe {
        (*args).executable = (*args).loaded_executable.cast();
    }
    ptr::null_mut()
}
unsafe extern "C" fn destroy_executable(_: *mut PJRT_Executable_Destroy_Args) -> *mut PJRT_Error {
    ptr::null_mut()
}
unsafe extern "C" fn delete_serialized(bytes: *mut PJRT_SerializedExecutable) {
    drop(unsafe { Box::from_raw(bytes.cast::<Vec<u8>>()) });
}
unsafe extern "C" fn serialize(args: *mut PJRT_Executable_Serialize_Args) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    let client = unsafe { *args.executable.cast::<*mut PJRT_Client>() };
    if unsafe { state(client) }
        .serialize_failure
        .swap(false, Ordering::SeqCst)
    {
        return Box::into_raw(Box::new(0u8)).cast();
    }
    let bytes = Box::new(b"serialized executable".to_vec());
    args.serialized_bytes = bytes.as_ptr().cast();
    args.serialized_bytes_size = bytes.len();
    args.serialized_executable = Box::into_raw(bytes).cast();
    args.serialized_executable_deleter = Some(delete_serialized);
    ptr::null_mut()
}
unsafe extern "C" fn deserialize(
    args: *mut PJRT_Executable_DeserializeAndLoad_Args,
) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    let bytes = unsafe {
        std::slice::from_raw_parts(
            args.serialized_executable.cast::<u8>(),
            args.serialized_executable_size,
        )
    };
    if bytes != b"serialized executable" {
        return Box::into_raw(Box::new(0u8)).cast();
    }
    unsafe { state(args.client) }
        .loads
        .fetch_add(1, Ordering::SeqCst);
    args.loaded_executable = Box::into_raw(Box::new(args.client)).cast();
    ptr::null_mut()
}
unsafe extern "C" fn error_message(args: *mut PJRT_Error_Message_Args) {
    unsafe {
        (*args).message = c"compile failed".as_ptr();
        (*args).message_size = 14;
    }
}
unsafe extern "C" fn error_code(args: *mut PJRT_Error_GetCode_Args) -> *mut PJRT_Error {
    unsafe { (*args).code = PJRT_Error_Code_PJRT_Error_Code_INTERNAL };
    ptr::null_mut()
}
unsafe extern "C" fn error_destroy(args: *mut PJRT_Error_Destroy_Args) {
    drop(unsafe { Box::from_raw((*args).error.cast::<u8>()) });
}

// Every test keeps its API allocation alive until all executables are released.
fn api() -> Box<PJRT_Api> {
    Box::new(PJRT_Api {
        PJRT_Client_Compile: Some(compile),
        PJRT_Client_Destroy: Some(destroy_client),
        PJRT_LoadedExecutable_Destroy: Some(destroy),
        PJRT_LoadedExecutable_GetExecutable: Some(get_executable),
        PJRT_Executable_Destroy: Some(destroy_executable),
        PJRT_Executable_Serialize: Some(serialize),
        PJRT_Executable_DeserializeAndLoad: Some(deserialize),
        PJRT_Error_Message: Some(error_message),
        PJRT_Error_GetCode: Some(error_code),
        PJRT_Error_Destroy: Some(error_destroy),
        ..Default::default()
    })
}
struct CacheConfig {
    config: compcache::Config,
    directory: std::path::PathBuf,
    temporary: Option<Arc<tempfile::TempDir>>,
}
struct TestCache {
    inner: CompilationCache,
    _directory: Option<Arc<tempfile::TempDir>>,
}
impl std::ops::Deref for TestCache {
    type Target = CompilationCache;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
fn cache(api: &PJRT_Api, config: &CacheConfig, namespace: [u8; 32]) -> (TestCache, Arc<State>) {
    let state = Arc::new(State::default());
    let native = Box::into_raw(Box::new(state.clone())).cast::<PJRT_Client>();
    // SAFETY: Client is repr(transparent) over a non-null PJRT_Client pointer.
    let client: Client = unsafe { std::mem::transmute(NonNull::new(native).unwrap()) };
    let owner = Arc::new(CompilationClient {
        api: Api(NonNull::from(api)),
        client,
    });
    let builder = StoreBuilder::new()
        .path(&config.directory)
        .config(config.config.clone());
    let inner = CompilationCache::open(owner, namespace, config.config.clone(), builder).unwrap();
    (
        TestCache {
            inner,
            _directory: config.temporary.clone(),
        },
        state,
    )
}
fn cache_config(entries: usize) -> CacheConfig {
    let directory = Arc::new(tempfile::tempdir().unwrap());
    let mut config = compcache::Config::default();
    config.memory.weight_capacity = entries as u64 * 128;
    config.memory.estimated_items_capacity = entries;
    config.memory.shards = Some(1);
    CacheConfig {
        config,
        directory: directory.path().to_owned(),
        temporary: Some(directory),
    }
}
fn disk_config(directory: &Path) -> CacheConfig {
    let mut config = cache_config(2);
    config.directory = directory.to_owned();
    config.temporary = None;
    config
}
fn request(cache: &CompilationCache, code: &[u8], options: &[u8]) -> Result<Arc<Executable>> {
    let checksum = blake3::hash(code).to_hex();
    let digest = verify_checksum(code, checksum.as_bytes())?;
    cache.get_or_build(code, digest, options, None)
}

#[test]
fn verification_exact_keys_and_retry() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(128), [0; 32]);
    let code = b"ML\xefR debug locations";
    let checksum = blake3::hash(code).to_hex();
    let first = request(&cache, code, b"opts").unwrap();
    let hit = request(&cache, code, b"opts").unwrap();
    assert!(Arc::ptr_eq(&first, &hit));
    assert!(verify_checksum(b"changed", checksum.as_bytes()).is_err());
    assert!(verify_checksum(code, checksum.to_uppercase().as_bytes()).is_ok());
    for bad in [&b""[..], &b"0"[..], &[b'g'; 64], &[b'0'; 65]] {
        assert!(verify_checksum(code, bad).is_err());
    }
    request(&cache, code, b"different opts").unwrap();
    request(&cache, b"different debug locations", b"opts").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 3);
    assert_eq!(
        state.programs.lock().unwrap()[0],
        (code.to_vec(), b"opts".to_vec())
    );
    state.failures.store(1, Ordering::SeqCst);
    assert_eq!(
        request(&cache, b"retry", b"").unwrap_err().message(),
        "compile failed"
    );
    request(&cache, b"retry", b"").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 5);
}

#[test]
fn concurrent_requests_coalesce_and_distinct_keys_progress() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(128), [0; 32]);
    std::thread::scope(|scope| {
        let workers: Vec<_> = (0..8)
            .map(|_| scope.spawn(|| request(&cache, b"kernel", b"").unwrap()))
            .collect();
        let handles: Vec<_> = workers.into_iter().map(|w| w.join().unwrap()).collect();
        assert!(handles.iter().all(|h| Arc::ptr_eq(h, &handles[0])));
    });
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    *state.rendezvous.lock().unwrap() = Some(Arc::new(std::sync::Barrier::new(2)));
    std::thread::scope(|scope| {
        for options in [b"one", b"two"] {
            let cache = &cache;
            scope.spawn(move || request(cache, b"kernel", options).unwrap());
        }
    });
    assert_eq!(state.compiles.load(Ordering::SeqCst), 3);
}

#[test]
fn eviction_and_cache_drop_preserve_live_executable_and_client() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(1), [0; 32]);
    let retained = request(&cache, b"kernel", b"").unwrap();
    for i in 0u8..10 {
        request(&cache, &[i], b"").unwrap();
    }
    assert!(cache.memory.len() <= 1);
    drop(cache);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 10);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 0);
    drop(retained);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 11);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 1);
}

#[test]
fn shutdown_releases_cache_ownership_and_is_idempotent() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(1), [0; 32]);
    for i in 0u8..20 {
        request(&cache, &[i], b"").unwrap();
    }
    cache.shutdown();
    assert_eq!(state.destroys.load(Ordering::SeqCst), 20);
    cache.shutdown();
    drop(cache);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 20);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 1);
}

#[test]
fn disk_restores_across_clients_and_namespaces_isolate() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let config = disk_config(dir.path());
    let (first, state) = cache(&api, &config, [1; 32]);
    request(&first, b"kernel", b"opts").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    // A second client opens the same root while the first is still alive.
    // Writes are durable before shutdown and native handles stay client-local.
    let (second, state) = cache(&api, &config, [1; 32]);
    request(&second, b"kernel", b"opts").unwrap();
    assert_eq!(state.loads.load(Ordering::SeqCst), 1);
    assert_eq!(state.compiles.load(Ordering::SeqCst), 0);
    request(&second, b"kernel", b"different options").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    drop(second);
    drop(first);
    let (third, state) = cache(&api, &config, [2; 32]);
    request(&third, b"kernel", b"opts").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
}

#[test]
fn bad_artifacts_recompile_and_replace() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let (cache, state) = cache(&api, &disk_config(dir.path()), [0; 32]);
    let key = cache
        .artifact_key(&CacheKey {
            digest: blake3::hash(b"kernel"),
            options: vec![],
            donation: None,
        })
        .unwrap();
    let artifacts = compcache::StoreBuilder::<compcache::Bypass, NoExtraData>::new()
        .path(cache.store.root())
        .open()
        .unwrap();
    for bad in [b"corrupt".to_vec(), {
        let mut bytes = blake3::hash(b"incompatible").as_bytes().to_vec();
        bytes.extend_from_slice(b"incompatible");
        bytes
    }] {
        drop(cache.memory.clear());
        artifacts.force_evict_blob(&key).unwrap();
        artifacts
            .get_or_insert_blob(key, || Ok(bad.clone()))
            .unwrap();
        request(&cache, b"kernel", b"").unwrap();
    }
    assert_eq!(state.compiles.load(Ordering::SeqCst), 2);
}

#[test]
fn serialization_failure_preserves_the_compiled_payload() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(2), [0; 32]);
    state.serialize_failure.store(true, Ordering::SeqCst);
    let first = request(&cache, b"kernel", b"").unwrap();
    let hit = request(&cache, b"kernel", b"").unwrap();
    assert!(Arc::ptr_eq(&first, &hit));
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    let key = cache
        .artifact_key(&CacheKey {
            digest: blake3::hash(b"kernel"),
            options: vec![],
            donation: None,
        })
        .unwrap();
    assert!(
        cache
            .store
            .status(compcache::EntryKind::Blob, &key)
            .unwrap()
            .is_none()
    );
}

#[test]
fn persistent_cache_across_processes() {
    if let Some(directory) = std::env::var_os("REUSSIR_PJRT_TEST_CACHE") {
        let api = api();
        let (cache, state) = cache(&api, &disk_config(Path::new(&directory)), [3; 32]);
        request(&cache, b"kernel", b"options").unwrap();
        let warm = std::env::var_os("REUSSIR_PJRT_TEST_WARM").is_some();
        assert_eq!(state.compiles.load(Ordering::SeqCst), usize::from(!warm));
        assert_eq!(state.loads.load(Ordering::SeqCst), usize::from(warm));
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    for warm in [false, true] {
        let mut child = std::process::Command::new(std::env::current_exe().unwrap());
        child
            .args([
                "--exact",
                "pjrt::executable::tests::persistent_cache_across_processes",
                "--nocapture",
            ])
            .env("REUSSIR_PJRT_TEST_CACHE", directory.path());
        if warm {
            child.env("REUSSIR_PJRT_TEST_WARM", "1");
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
fn pruning_preserves_caller_owned_executable() {
    let api = api();
    let (cache, state) = cache(&api, &cache_config(2), [0; 32]);
    let retained = request(&cache, b"kernel", b"").unwrap();
    std::thread::sleep(std::time::Duration::from_millis(1100));
    let deadline = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs() as i64;
    assert_eq!(cache.store.prune_before(deadline).unwrap().removed, 1);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 0);
    drop(cache);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 0);
    drop(retained);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 1);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 1);
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn real_plugin_serializes_and_restores_without_compiling() {
    let context = crate::pjrt::context::get().unwrap();
    let owner = context.executables.owner.clone();
    let plugin = std::env::var_os("REUSSIR_PJRT_PLUGIN").unwrap();
    let namespace = owner.namespace(Path::new(&plugin)).unwrap();
    let directory = tempfile::tempdir().unwrap();
    let config = disk_config(directory.path());
    let options = include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../reussir-pjrt-sys/tests/fixtures/compile_options.pb"
    ));
    let code = br#"module {
      func.func @main() -> tensor<i32> {
        %c = stablehlo.constant dense<42> : tensor<i32> loc("kernel.rr":3:5)
        return %c : tensor<i32>
      }
    }"#;
    let digest = blake3::hash(code);
    let first = CompilationCache::open(
        owner.clone(),
        namespace,
        config.config.clone(),
        StoreBuilder::new()
            .path(&config.directory)
            .config(config.config.clone()),
    )
    .unwrap();
    request(&first, code, options).unwrap();
    drop(first);
    let second = CompilationCache::open(
        owner,
        namespace,
        config.config.clone(),
        StoreBuilder::new()
            .path(&config.directory)
            .config(config.config.clone()),
    )
    .unwrap();
    // Exercise the internal cache layer after checksum verification: supplying
    // uncompileable code here makes any unexpected compilation fail the test.
    second
        .get_or_build(b"must restore the artifact", digest, options, None)
        .unwrap();
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn real_plugin_persists_across_processes() {
    if let Some(phase) = std::env::var_os("REUSSIR_PJRT_REAL_PHASE") {
        let context = crate::pjrt::context::get().unwrap();
        let code = br#"module { func.func @main() -> tensor<i32> {
          %v = stablehlo.constant dense<42> : tensor<i32>
          return %v : tensor<i32>
        }}"#;
        let options = include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../reussir-pjrt-sys/tests/fixtures/compile_options.pb"
        ));
        let input = if phase == "warm" {
            &b"must load the persisted executable"[..]
        } else {
            &code[..]
        };
        // A warm miss tries to compile invalid code and fails, proving native
        // artifact reuse rather than merely obtaining the right output again.
        context
            .executables
            .get_or_build(input, blake3::hash(code), options, None)
            .unwrap();
        return;
    }
    let directory = tempfile::tempdir().unwrap();
    for phase in ["cold", "warm"] {
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "pjrt::executable::tests::real_plugin_persists_across_processes",
                "--ignored",
                "--nocapture",
            ])
            .env("REUSSIR_PJRT_REAL_PHASE", phase)
            .env("REUSSIR_CACHE_DIR", directory.path().join("cache"))
            .env("REUSSIR_CONFIG_DIR", directory.path().join("config"))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{phase}: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn refined_programs_are_keyed_by_argument_shapes() {
    use reussir_stablehlo::Argument;
    let api = api();
    let (cache, _) = cache(&api, &cache_config(128), [0; 32]);
    let code = br#"module { func.func @main(%x: tensor<?xf32>) -> tensor<?xf32> {
        %y = stablehlo.add %x, %x : tensor<?xf32>
        return %y : tensor<?xf32>
    }}"#;
    let checksum = blake3::hash(code).to_hex();
    let refine = |extent| {
        cache
            .refine_program(
                code,
                checksum.as_bytes(),
                vec![Argument::Shape(vec![extent])],
            )
            .unwrap()
    };
    let three = refine(3);
    assert_eq!(three.result_types[0].dimensions, [3]);
    assert!(Arc::ptr_eq(&three, &refine(3)));
    assert_eq!(refine(5).result_types[0].dimensions, [5]);
}

#[test]
fn refined_and_donated_programs_are_cached_separately() {
    use reussir_stablehlo::{Argument, Donation};
    let api = api();
    let (cache, state) = cache(&api, &cache_config(128), [0; 32]);
    let code = br#"module { func.func @main(%a: tensor<?xf32>) -> tensor<?xf32> {
        %v = stablehlo.add %a, %a : tensor<?xf32>
        return %v : tensor<?xf32>
    }}"#;
    let checksum = blake3::hash(code).to_hex();
    let prepare = || {
        cache
            .refine_program(code, checksum.as_bytes(), vec![Argument::Shape(vec![2048])])
            .unwrap()
    };
    let first = prepare();
    assert!(Arc::ptr_eq(&first, &prepare()));
    let donation = Donation {
        mapping: vec![0],
        donors: vec![0],
        aliases: vec![],
    };
    cache.compile_refined(&first.bytecode, b"", None).unwrap();
    let donated = cache
        .compile_refined(&first.bytecode, b"", Some(donation.clone()))
        .unwrap();
    assert!(Arc::ptr_eq(
        &donated,
        &cache
            .compile_refined(&first.bytecode, b"", Some(donation))
            .unwrap()
    ));
    assert_eq!(state.compiles.load(Ordering::SeqCst), 2);
    let programs = state.programs.lock().unwrap();
    assert_ne!(programs[0].0, programs[1].0);
}
