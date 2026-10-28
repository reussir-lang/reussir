//! Database-only cache bindings. Byte callbacks are synchronous and borrowed.

use compcache::{Bypass, Key, NoExtraData, Store, StoreBuilder};
use std::{
    ffi::{CStr, c_char, c_void},
    path::Path,
    ptr,
    sync::Arc,
};

pub struct Cache(Store<Bypass, NoExtraData>);
pub type Bytes = unsafe extern "C" fn(*const u8, usize, *mut c_void);
pub type Producer = unsafe extern "C" fn(*mut c_void, *mut Produced) -> bool;

#[repr(C)]
pub struct Produced {
    pub data: *const u8,
    pub size: usize,
    pub persist: bool,
}

#[derive(Debug, thiserror::Error)]
#[error("cache producer failed")]
struct ProducerFailed;

unsafe fn report(error: &dyn std::error::Error, diagnostic: Bytes, context: *mut c_void) {
    let message = error.to_string();
    unsafe { diagnostic(message.as_ptr(), message.len(), context) };
}

/// Open a store once for a batch of requests; return null on failure.
/// # Safety
/// Directory is a live NUL-terminated path (native bytes on Unix, UTF-8 on
/// Windows). The diagnostic callback must not unwind.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_open(
    directory: *const c_char,
    diagnostic: Bytes,
    context: *mut c_void,
) -> *mut Cache {
    let directory = unsafe { CStr::from_ptr(directory) };
    #[cfg(unix)]
    let path = {
        use std::os::unix::ffi::OsStrExt;
        Path::new(std::ffi::OsStr::from_bytes(directory.to_bytes()))
    };
    #[cfg(not(unix))]
    let path = match directory.to_str() {
        Ok(path) => Path::new(path),
        Err(error) => {
            unsafe { report(&error, diagnostic, context) };
            return ptr::null_mut();
        }
    };
    match StoreBuilder::<Bypass, NoExtraData>::new().path(path).open() {
        Ok(store) => Box::into_raw(Box::new(Cache(store))),
        Err(error) => {
            unsafe { report(&error, diagnostic, context) };
            ptr::null_mut()
        }
    }
}

/// Return cached bytes or produce them under compcache's per-key lock.
/// Returns 0 on success, 1 on cache failure, or 2 on producer failure.
/// # Safety
/// Cache and the 32-byte key are live. Callbacks must not unwind or recursively
/// request the same key. Successful producer bytes remain live until this call
/// returns. Result and diagnostic bytes are borrowed only during their callback.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_get_or_produce(
    cache: *const Cache,
    key: *const [u8; 32],
    producer: Producer,
    result: Bytes,
    diagnostic: Bytes,
    context: *mut c_void,
) -> u32 {
    let cached =
        unsafe { &*cache }
            .0
            .get_or_insert_payload(Key::from_bytes(unsafe { *key }), || {
                let mut produced = Produced {
                    data: ptr::null(),
                    size: 0,
                    persist: true,
                };
                if !unsafe { producer(context, &mut produced) } {
                    return Err(Box::new(ProducerFailed) as compcache::ProducerError);
                }
                let bytes = if produced.size == 0 {
                    &[]
                } else {
                    unsafe { std::slice::from_raw_parts(produced.data, produced.size) }
                };
                Ok(compcache::Produced {
                    payload: Arc::<[u8]>::from(bytes),
                    serialized: Ok(produced.persist.then(|| bytes.to_vec())),
                })
            });
    match cached {
        Ok(cached) => {
            if let Some(error) = cached.cache_error {
                unsafe { report(&error, diagnostic, context) };
            }
            unsafe { result(cached.payload.as_ptr(), cached.payload.len(), context) };
            0
        }
        Err(error) => {
            unsafe { report(&error, diagnostic, context) };
            if matches!(error, compcache::Error::Producer(_)) {
                2
            } else {
                1
            }
        }
    }
}

/// Close a store after its requests finish. Null is harmless.
/// # Safety
/// A nonnull cache is an owned handle from open, closed exactly once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_close(cache: *mut Cache) {
    if !cache.is_null() {
        drop(unsafe { Box::from_raw(cache) });
    }
}

#[cfg(test)]
mod tests;
