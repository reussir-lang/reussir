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
fn cache(
    api: &PJRT_Api,
    config: &CacheConfig,
    namespace: [u8; 32],
) -> (CompilationCache, Arc<State>) {
    let state = Arc::new(State::default());
    let native = Box::into_raw(Box::new(state.clone())).cast::<PJRT_Client>();
    // SAFETY: Client is repr(transparent) over a non-null PJRT_Client pointer.
    let client: Client = unsafe { std::mem::transmute(NonNull::new(native).unwrap()) };
    let owner = Arc::new(CompilationClient {
        api: Api(NonNull::from(api)),
        client,
    });
    (
        CompilationCache::new(owner, config, Some(namespace)).unwrap(),
        state,
    )
}
fn memory_config(entries: usize) -> CacheConfig {
    let mut config = CacheConfig::default();
    config.memory.loaded_entries = entries;
    config.disk.enabled = false;
    config
}
fn disk_config(directory: &Path) -> CacheConfig {
    let mut config = memory_config(2);
    config.disk.enabled = true;
    config.disk.directory = Some(directory.to_owned());
    config.disk.block_bytes = 64 * 1024;
    config.disk.capacity_bytes = 1024 * 1024;
    config.disk.buffer_bytes = 64 * 1024;
    config
}
fn request(cache: &CompilationCache, code: &[u8], options: &[u8]) -> Result<Arc<Executable>> {
    let checksum = blake3::hash(code).to_hex();
    let digest = verify_checksum(code, checksum.as_bytes())?;
    cache.get_or_compile(code, digest, options)
}

#[test]
fn verification_exact_keys_and_retry() {
    let api = api();
    let (cache, state) = cache(&api, &memory_config(128), [0; 32]);
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
        request(&cache, b"retry", b"").unwrap_err().message,
        "compile failed"
    );
    request(&cache, b"retry", b"").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 5);
}

#[test]
fn concurrent_requests_coalesce_and_distinct_keys_progress() {
    let api = api();
    let (cache, state) = cache(&api, &memory_config(128), [0; 32]);
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
    let (cache, state) = cache(&api, &memory_config(1), [0; 32]);
    let retained = request(&cache, b"kernel", b"").unwrap();
    for i in 0u8..10 {
        request(&cache, &[i], b"").unwrap();
    }
    assert!(cache.entries.usage() <= 1);
    drop(cache);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 10);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 0);
    drop(retained);
    assert_eq!(state.destroys.load(Ordering::SeqCst), 11);
    assert_eq!(state.clients_destroyed.load(Ordering::SeqCst), 1);
}

#[test]
fn disk_restores_across_clients_and_namespaces_isolate() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let config = disk_config(dir.path());
    let (first, state) = cache(&api, &config, [1; 32]);
    assert!(first.artifacts.is_some());
    request(&first, b"kernel", b"opts").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    let (locked, _) = cache(&api, &config, [1; 32]);
    assert!(locked.artifacts.is_none());
    drop(locked);
    drop(first); // flush_on_shutdown persists a hot, never-evicted artifact.
    let (second, state) = cache(&api, &config, [1; 32]);
    request(&second, b"kernel", b"opts").unwrap();
    assert_eq!(state.loads.load(Ordering::SeqCst), 1);
    assert_eq!(state.compiles.load(Ordering::SeqCst), 0);
    request(&second, b"kernel", b"different options").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
    drop(second);
    let (third, state) = cache(&api, &config, [2; 32]);
    request(&third, b"kernel", b"opts").unwrap();
    assert_eq!(state.compiles.load(Ordering::SeqCst), 1);
}

#[test]
fn bad_artifacts_recompile_and_replace() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let (cache, state) = cache(&api, &disk_config(dir.path()), [0; 32]);
    let mut key = vec![0; 32];
    key.extend_from_slice(blake3::hash(b"kernel").as_bytes());
    let artifacts = cache.artifacts.as_ref().unwrap();
    for bad in [b"corrupt".to_vec(), {
        let mut bytes = blake3::hash(b"incompatible").as_bytes().to_vec();
        bytes.extend_from_slice(b"incompatible");
        bytes
    }] {
        cache.entries.clear();
        artifacts.cache.insert(key.clone(), bad);
        request(&cache, b"kernel", b"").unwrap();
    }
    assert_eq!(state.compiles.load(Ordering::SeqCst), 2);
}

#[test]
fn shrinking_disk_capacity_removes_old_partitions() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let mut config = disk_config(dir.path());
    let (first, _) = cache(&api, &config, [0; 32]);
    request(&first, b"kernel", b"").unwrap();
    drop(first);
    config.disk.capacity_bytes /= 2;
    let (second, _) = cache(&api, &config, [0; 32]);
    assert!(second.artifacts.is_some());
    let bytes: u64 = std::fs::read_dir(dir.path().join("data"))
        .unwrap()
        .map(|entry| entry.unwrap().metadata().unwrap().len())
        .sum();
    assert!(bytes <= config.disk.capacity_bytes as u64);
}

#[test]
fn synchronous_api_works_inside_tokio() {
    let api = api();
    let runtime = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    runtime.block_on(async {
        let (cache, _) = cache(&api, &memory_config(2), [0; 32]);
        request(&cache, b"kernel", b"").unwrap();
    });
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn real_plugin_serializes_and_restores_without_compiling() {
    let context = context::get().unwrap();
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
    let first = CompilationCache::new(owner.clone(), &config, Some(namespace)).unwrap();
    request(&first, code, options).unwrap();
    drop(first);
    let second = CompilationCache::new(owner, &config, Some(namespace)).unwrap();
    // Exercise the internal cache layer after checksum verification: supplying
    // uncompileable code here makes any unexpected compilation fail the test.
    second
        .get_or_compile(b"must restore the artifact", digest, options)
        .unwrap();
}

#[test]
fn write_policies_and_shutdown_flush_control_persistence() {
    use super::super::cache_config::WritePolicy;
    let api = api();
    for (policy, flush, persisted) in [
        (WritePolicy::OnEviction, false, false),
        (WritePolicy::OnEviction, true, true),
        (WritePolicy::OnInsertion, false, true),
    ] {
        let dir = tempfile::tempdir().unwrap();
        let mut config = disk_config(dir.path());
        config.disk.write_policy = policy;
        config.disk.flush_on_shutdown = flush;
        let (first, _) = cache(&api, &config, [0; 32]);
        request(&first, b"kernel", b"").unwrap();
        drop(first);
        let (second, state) = cache(&api, &config, [0; 32]);
        request(&second, b"kernel", b"").unwrap();
        assert_eq!(state.loads.load(Ordering::SeqCst), usize::from(persisted));
    }
}

#[test]
fn artifact_memory_eviction_writes_back_within_disk_capacity() {
    let api = api();
    let dir = tempfile::tempdir().unwrap();
    let mut config = disk_config(dir.path());
    config.disk.buffer_bytes = 8192;
    config.disk.flush_on_shutdown = false;
    let (first, _) = cache(&api, &config, [0; 32]);
    request(&first, b"kernel", b"").unwrap();
    let artifacts = first.artifacts.as_ref().unwrap();
    for i in 0u8..20 {
        artifacts.cache.insert(vec![i; 64], vec![i; 2048]);
        first.wait(artifacts.cache.storage().wait());
    }
    assert!(artifacts.cache.memory().usage() <= config.disk.buffer_bytes);
    drop(first);
    let (second, state) = cache(&api, &config, [0; 32]);
    request(&second, b"kernel", b"").unwrap();
    assert_eq!(state.loads.load(Ordering::SeqCst), 1);
}
