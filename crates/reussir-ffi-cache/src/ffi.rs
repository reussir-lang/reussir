//! Panic-contained C ABI. See `include/Reussir/RustCompiler/Cache.h`.

use crate::{Cache, Config};
use std::{
    ffi::c_int,
    panic::{AssertUnwindSafe, catch_unwind},
    path::PathBuf,
    ptr, slice,
};

/// Opaque handle, only created and consumed by this module.
pub struct ReussirFfiCache(Cache);
/// Owned bytes; independent of the cache that returned them.
pub struct ReussirFfiCacheBuffer(Vec<u8>);

fn guard<T>(fallback: T, operation: impl FnOnce() -> T) -> T {
    catch_unwind(AssertUnwindSafe(operation)).unwrap_or(fallback)
}

unsafe fn bytes<'a>(data: *const u8, len: usize) -> Option<&'a [u8]> {
    if len == 0 {
        Some(&[])
    } else if data.is_null() || len > isize::MAX as usize {
        None
    } else {
        Some(unsafe { slice::from_raw_parts(data, len) })
    }
}

fn path(bytes: &[u8]) -> Option<PathBuf> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        Some(std::ffi::OsStr::from_bytes(bytes).into())
    }
    #[cfg(not(unix))]
    {
        std::str::from_utf8(bytes).ok().map(PathBuf::from)
    }
}

/// Opens a cache. Zero capacities select defaults; null means failure.
///
/// # Safety
/// `directory` must identify `directory_len` readable bytes, or be null for zero
/// length. The resulting handle must be closed exactly once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_open(
    directory: *const u8,
    directory_len: usize,
    memory_bytes: usize,
    disk_bytes: usize,
    block_bytes: usize,
) -> *mut ReussirFfiCache {
    guard(ptr::null_mut(), || {
        let Some(directory) = (unsafe { bytes(directory, directory_len) }).and_then(path) else {
            return ptr::null_mut();
        };
        let mut config = Config::new(directory);
        if memory_bytes != 0 {
            config.memory_bytes = memory_bytes;
        }
        if disk_bytes != 0 {
            config.disk_bytes = disk_bytes;
        }
        if block_bytes != 0 {
            config.block_bytes = block_bytes;
        }
        Cache::open(config)
            .map(|cache| Box::into_raw(Box::new(ReussirFfiCache(cache))))
            .unwrap_or(ptr::null_mut())
    })
}

/// Returns 1 for a hit, 0 for a miss, or -1 on error. Clears `output` first.
///
/// # Safety
/// `cache` must be live or null, `key` must identify `key_len` readable bytes
/// (null is allowed for zero length), and `output` must be writable or null.
/// A hit transfers one buffer that must be freed exactly once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_get(
    cache: *const ReussirFfiCache,
    key: *const u8,
    key_len: usize,
    output: *mut *mut ReussirFfiCacheBuffer,
) -> c_int {
    guard(-1, || {
        if output.is_null() {
            return -1;
        }
        unsafe { output.write(ptr::null_mut()) };
        let (Some(cache), Some(key)) = (unsafe { cache.as_ref() }, unsafe { bytes(key, key_len) })
        else {
            return -1;
        };
        match cache.0.get(key) {
            Ok(Some(value)) => {
                unsafe { output.write(Box::into_raw(Box::new(ReussirFfiCacheBuffer(value)))) };
                1
            }
            Ok(None) => 0,
            Err(_) => -1,
        }
    })
}

/// Copies the entry; returns 0 on admission, -1 on error or an oversized entry.
///
/// # Safety
/// `cache` must be live or null. Key/value pointers must identify their readable
/// lengths; null is allowed for zero length. No pointer is retained.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_insert(
    cache: *const ReussirFfiCache,
    key: *const u8,
    key_len: usize,
    value: *const u8,
    value_len: usize,
) -> c_int {
    guard(-1, || {
        let (Some(cache), Some(key), Some(value)) = (
            unsafe { cache.as_ref() },
            unsafe { bytes(key, key_len) },
            unsafe { bytes(value, value_len) },
        ) else {
            return -1;
        };
        cache.0.insert(key, value).map(|()| 0).unwrap_or(-1)
    })
}

/// # Safety
/// `buffer` must be live or null; returned bytes live until buffer_free.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_buffer_data(
    buffer: *const ReussirFfiCacheBuffer,
) -> *const u8 {
    guard(ptr::null(), || {
        unsafe { buffer.as_ref() }.map_or(ptr::null(), |buffer| buffer.0.as_ptr())
    })
}

/// # Safety
/// `buffer` must be live or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_buffer_size(
    buffer: *const ReussirFfiCacheBuffer,
) -> usize {
    guard(0, || {
        unsafe { buffer.as_ref() }.map_or(0, |buffer| buffer.0.len())
    })
}

/// # Safety
/// `buffer` must be null or an unfreed buffer from get. No reads may overlap free.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_buffer_free(buffer: *mut ReussirFfiCacheBuffer) {
    guard((), || {
        if !buffer.is_null() {
            drop(unsafe { Box::from_raw(buffer) });
        }
    });
}

/// Flushes, joins workers and releases the lock. Null is a successful no-op.
/// Returns 0 on success, -1 on error; consumes the handle either way.
///
/// # Safety
/// `cache` must be null or an unclosed handle from open. No other calls may use
/// the handle concurrently with close or after it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_ffi_cache_close(cache: *mut ReussirFfiCache) -> c_int {
    guard(-1, || {
        if cache.is_null() {
            return 0;
        }
        let ReussirFfiCache(cache) = *unsafe { Box::from_raw(cache) };
        cache.close().map(|()| 0).unwrap_or(-1)
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn panic_boundary_returns_failure() {
        assert_eq!(super::guard(-1, || panic!("boundary test")), -1);
    }
}
