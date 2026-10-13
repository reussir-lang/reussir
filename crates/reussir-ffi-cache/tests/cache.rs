use reussir_ffi_cache::{Cache, Config, ffi::*};
use std::{fs, ptr, slice, sync::Arc};

fn config(path: &std::path::Path) -> Config {
    Config {
        directory: path.into(),
        io_buffer_bytes: 256 * 1024,
        disk_bytes: 1024 * 1024,
        block_bytes: 256 * 1024,
    }
}

#[test]
fn misses_and_binary_values_survive_close_and_reopen() {
    let dir = tempfile::tempdir().unwrap();
    let cfg = config(dir.path());
    let cache = Cache::open(cfg.clone()).unwrap();
    assert_eq!(cache.get(b"missing").unwrap(), None);
    cache.insert(b"key\0", b"bitcode\0\xff").unwrap();
    cache.insert(b"empty", b"").unwrap();
    assert_eq!(
        cache.get(b"key\0").unwrap(),
        Some(b"bitcode\0\xff".to_vec())
    );
    cache.close().unwrap();
    let cache = Cache::open(cfg).unwrap();
    assert_eq!(
        cache.get(b"key\0").unwrap(),
        Some(b"bitcode\0\xff".to_vec())
    );
    assert_eq!(cache.get(b"empty").unwrap(), Some(Vec::new()));
    cache.close().unwrap();
}

#[test]
fn directory_lock_is_exclusive_and_drop_flushes_and_releases_it() {
    let dir = tempfile::tempdir().unwrap();
    let cfg = config(dir.path());
    let first = Cache::open(cfg.clone()).unwrap();
    assert!(Cache::open(cfg.clone()).is_err());
    first.insert(b"persist", b"on drop").unwrap();
    drop(first);
    let reopened = Cache::open(cfg).unwrap();
    assert_eq!(reopened.get(b"persist").unwrap(), Some(b"on drop".to_vec()));
}

#[test]
fn invalid_config_and_io_failures_are_errors() {
    let dir = tempfile::tempdir().unwrap();
    for field in 0..4 {
        let mut cfg = config(dir.path());
        match field {
            0 => cfg.directory.clear(),
            1 => cfg.io_buffer_bytes = 0,
            2 => cfg.block_bytes = 0,
            3 => cfg.disk_bytes = cfg.block_bytes * 3,
            _ => unreachable!(),
        }
        assert!(Cache::open(cfg).is_err());
    }
    let file = dir.path().join("file");
    fs::write(&file, "not a directory").unwrap();
    assert!(Cache::open(config(&file)).is_err());
}

#[test]
fn entries_obey_io_buffer_and_block_budgets_and_resize_resets_storage() {
    let dir = tempfile::tempdir().unwrap();
    let mut cfg = config(dir.path());
    let cache = Cache::open(cfg.clone()).unwrap();
    assert!(cache.insert(b"big", &vec![0; cfg.block_bytes]).is_err());
    cache.insert(b"key", b"value").unwrap();
    cache.close().unwrap();
    cfg.disk_bytes /= 2;
    cfg.block_bytes /= 2;
    let cache = Cache::open(cfg.clone()).unwrap();
    assert_eq!(cache.get(b"key").unwrap(), None);
    cache.close().unwrap();
    let bytes: u64 = fs::read_dir(dir.path().join("data"))
        .unwrap()
        .map(|entry| entry.unwrap().metadata().unwrap().len())
        .sum();
    assert!(
        bytes <= cfg.disk_bytes as u64,
        "{bytes} exceeds disk capacity"
    );
}

#[test]
fn caller_threads_and_existing_tokio_runtime_are_supported() {
    let dir = tempfile::tempdir().unwrap();
    let cache = Arc::new(Cache::open(config(dir.path())).unwrap());
    std::thread::scope(|scope| {
        for index in 0u8..4 {
            let cache = cache.clone();
            scope.spawn(move || {
                cache.insert(&[index], &[index; 128]).unwrap();
                assert_eq!(cache.get(&[index]).unwrap(), Some(vec![index; 128]));
            });
        }
    });
    drop(cache);
    let runtime = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    runtime.block_on(async {
        let cache = Cache::open(config(dir.path())).unwrap();
        cache.insert(b"async caller", b"works").unwrap();
        assert_eq!(cache.get(b"async caller").unwrap(), Some(b"works".to_vec()));
        drop(cache);
    });
}

#[test]
fn corrupted_disk_does_not_return_corrupted_values() {
    let dir = tempfile::tempdir().unwrap();
    let cfg = config(dir.path());
    let cache = Cache::open(cfg.clone()).unwrap();
    let value = b"uniquely recognizable bitcode payload\0\xff";
    cache.insert(b"corrupt", value).unwrap();
    cache.close().unwrap();
    let cache = Cache::open(cfg).unwrap();
    assert_eq!(cache.get(b"corrupt").unwrap(), Some(value.to_vec()));
    let mut changed = false;
    for entry in fs::read_dir(dir.path().join("data")).unwrap() {
        let path = entry.unwrap().path();
        let mut data = fs::read(&path).unwrap();
        if let Some(offset) = data.windows(value.len()).position(|bytes| bytes == value) {
            data[offset] ^= 0xff;
            fs::write(path, data).unwrap();
            changed = true;
        }
    }
    assert!(changed, "test must corrupt a persisted payload");
    // The earlier successful lookup must not hide disk corruption behind a
    // retained memory value. Every completed lookup reads the backing store.
    assert!(!matches!(cache.get(b"corrupt"), Ok(Some(_))));
}

#[test]
fn c_api_owns_inputs_and_results_and_clears_misses() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap().as_bytes();
    unsafe {
        let cache = reussir_ffi_cache_open(path.as_ptr(), path.len(), 262144, 1048576, 262144);
        assert!(!cache.is_null());
        let value = b"owned value".to_vec();
        assert_eq!(
            reussir_ffi_cache_insert(cache, b"k".as_ptr(), 1, value.as_ptr(), value.len()),
            0
        );
        drop(value);
        let mut result = ptr::null_mut();
        assert_eq!(
            reussir_ffi_cache_get(cache, b"k".as_ptr(), 1, &mut result),
            1
        );
        assert!(!result.is_null());
        assert_eq!(reussir_ffi_cache_close(cache), 0);
        assert_eq!(
            slice::from_raw_parts(
                reussir_ffi_cache_buffer_data(result),
                reussir_ffi_cache_buffer_size(result)
            ),
            b"owned value"
        );
        reussir_ffi_cache_buffer_free(result);
        let cache = reussir_ffi_cache_open(path.as_ptr(), path.len(), 262144, 1048576, 262144);
        assert!(!cache.is_null());
        assert_eq!(
            reussir_ffi_cache_get(cache, b"x".as_ptr(), 1, &mut result),
            0
        );
        assert!(result.is_null());
        assert_eq!(
            reussir_ffi_cache_insert(cache, ptr::null(), 0, ptr::null(), 0),
            0
        );
        assert_eq!(reussir_ffi_cache_get(cache, ptr::null(), 0, &mut result), 1);
        assert_eq!(reussir_ffi_cache_buffer_size(result), 0);
        reussir_ffi_cache_buffer_free(result);
        assert_eq!(reussir_ffi_cache_close(cache), 0);
    }
}

#[test]
fn c_api_rejects_invalid_arguments_and_null_cleanup_is_safe() {
    unsafe {
        assert!(reussir_ffi_cache_open(ptr::null(), 0, 0, 0, 0).is_null());
        let mut result = ptr::dangling_mut();
        assert_eq!(
            reussir_ffi_cache_get(ptr::null(), ptr::null(), 0, &mut result),
            -1
        );
        assert!(result.is_null());
        assert_eq!(
            reussir_ffi_cache_get(ptr::null(), ptr::null(), 1, ptr::null_mut()),
            -1
        );
        assert_eq!(
            reussir_ffi_cache_insert(ptr::null(), ptr::null(), 0, ptr::null(), 0),
            -1
        );
        assert!(reussir_ffi_cache_buffer_data(ptr::null()).is_null());
        assert_eq!(reussir_ffi_cache_buffer_size(ptr::null()), 0);
        reussir_ffi_cache_buffer_free(ptr::null_mut());
        assert_eq!(reussir_ffi_cache_close(ptr::null_mut()), 0);
    }
}
