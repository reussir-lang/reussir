//! C bindings for compcache's database-only opaque byte cache.
//!
//! The crate defines no compiler policy. See `include/reussir_polyffi_cache.h`
//! for the ABI, pointer contracts, ownership, and callback requirements.

use compcache::{Bypass, NoExtraData, Store, StoreBuilder};
use std::{
    ffi::c_void,
    panic::{AssertUnwindSafe, catch_unwind},
    path::PathBuf,
    ptr,
    sync::Arc,
};

pub const OK: u32 = 0;
pub const MISS: u32 = 1;
pub const INVALID_ARGUMENT: u32 = 2;
pub const CACHE_ERROR: u32 = 3;
pub const PRODUCER_ERROR: u32 = 4;
pub const PANIC: u32 = 5;

pub struct Cache {
    store: Store<Bypass, NoExtraData>,
}
pub struct Blob {
    bytes: Arc<[u8]>,
}
pub struct CacheError {
    cause: Error,
    message: String,
}
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Key {
    pub bytes: [u8; 32],
}
#[repr(C)]
pub struct Produced {
    pub data: *const u8,
    pub size: usize,
    pub persist: u8,
}
pub type Producer = unsafe extern "C" fn(*mut c_void, *mut Produced) -> u32;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("invalid argument: {0}")]
    Invalid(&'static str),
    #[error("cache operation failed")]
    Cache(#[source] compcache::Error),
    #[error("producer returned failure code {0}")]
    Producer(u32),
    #[error("Rust panic in cache binding")]
    Panic,
}
impl Error {
    fn kind(&self) -> u32 {
        match self {
            Self::Invalid(_) => INVALID_ARGUMENT,
            Self::Cache(_) => CACHE_ERROR,
            Self::Producer(_) => PRODUCER_ERROR,
            Self::Panic => PANIC,
        }
    }
}
impl From<compcache::Error> for Error {
    fn from(error: compcache::Error) -> Self {
        match error {
            compcache::Error::Producer(source) => match source.downcast::<Self>() {
                Ok(error) => *error,
                Err(source) => Self::Cache(compcache::Error::Producer(source)),
            },
            error => Self::Cache(error),
        }
    }
}
fn owned_error(cause: Error) -> *mut CacheError {
    use std::error::Error as _;
    let mut message = cause.to_string();
    let mut source = cause.source();
    while let Some(error) = source {
        message.push_str(": ");
        message.push_str(&error.to_string());
        source = error.source();
    }
    Box::into_raw(Box::new(CacheError { cause, message }))
}
unsafe fn report(output: *mut *mut CacheError, error: Error) {
    if !output.is_null() {
        unsafe { *output = owned_error(error) };
    }
}
unsafe fn boundary(error: *mut *mut CacheError, run: impl FnOnce() -> Result<u32, Error>) -> u32 {
    if !error.is_null() {
        unsafe { *error = ptr::null_mut() };
    }
    match catch_unwind(AssertUnwindSafe(run)).unwrap_or(Err(Error::Panic)) {
        Ok(status) => status,
        Err(cause) => {
            let kind = cause.kind();
            unsafe { report(error, cause) };
            kind
        }
    }
}
unsafe fn span<'a>(data: *const u8, size: usize) -> Result<&'a [u8], Error> {
    if size == 0 {
        return Ok(&[]);
    }
    if data.is_null() || size > isize::MAX as usize {
        return Err(Error::Invalid("invalid byte span"));
    }
    Ok(unsafe { std::slice::from_raw_parts(data, size) })
}
unsafe fn required<'a, T>(value: *const T) -> Result<&'a T, Error> {
    unsafe { value.as_ref() }.ok_or(Error::Invalid("null required pointer"))
}
unsafe fn output<T>(value: *mut *mut T) -> Result<(), Error> {
    if value.is_null() {
        return Err(Error::Invalid("null output pointer"));
    }
    unsafe { *value = ptr::null_mut() };
    Ok(())
}
fn path(bytes: &[u8]) -> Result<PathBuf, Error> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        Ok(std::ffi::OsStr::from_bytes(bytes).into())
    }
    #[cfg(not(unix))]
    {
        Ok(std::str::from_utf8(bytes)
            .map_err(|_| Error::Invalid("path must be UTF-8"))?
            .into())
    }
}

/// Construct a key using compcache's opaque binary recipe API.
/// # Safety
/// Pointers obey the contracts in the C header; result addresses writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_key_from_parts(
    domain: *const u8,
    domain_size: usize,
    recipe: *const u8,
    recipe_size: usize,
    result: *mut Key,
    error: *mut *mut CacheError,
) -> u32 {
    unsafe {
        boundary(error, || {
            if result.is_null() {
                return Err(Error::Invalid("null key output"));
            }
            let domain = std::str::from_utf8(span(domain, domain_size)?)
                .map_err(|_| Error::Invalid("domain must be UTF-8"))?;
            *result = Key {
                bytes: *compcache::Key::from_parts(domain, span(recipe, recipe_size)?).as_bytes(),
            };
            Ok(OK)
        })
    }
}
/// Open an explicit database-only cache directory.
/// # Safety
/// Pointers obey the C header; output pointers address writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_open(
    directory: *const u8,
    directory_size: usize,
    config_path: *const u8,
    config_path_size: usize,
    result: *mut *mut Cache,
    error: *mut *mut CacheError,
) -> u32 {
    unsafe {
        boundary(error, || {
            output(result)?;
            let directory = span(directory, directory_size)?;
            if directory.is_empty() {
                return Err(Error::Invalid("empty cache directory"));
            }
            let configuration = span(config_path, config_path_size)?;
            let config = if configuration.is_empty() {
                compcache::Config::default()
            } else {
                compcache::Config::from_nickel(path(configuration)?)?
            };
            let store = StoreBuilder::<Bypass, NoExtraData>::new()
                .path(path(directory)?)
                .config(config)
                .open()?;
            *result = Box::into_raw(Box::new(Cache { store }));
            Ok(OK)
        })
    }
}
/// Destroy a cache handle after its active operations finish. Null is harmless.
/// # Safety
/// A nonnull pointer is an owned, live handle from cache_open, destroyed once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_destroy(cache: *mut Cache) {
    if !cache.is_null() {
        drop(unsafe { Box::from_raw(cache) });
    }
}
/// Read opaque bytes; MISS returns no blob and no error.
/// # Safety
/// Pointers obey the ownership, lifetime, and output contracts in the C header.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_get(
    cache: *const Cache,
    key: *const Key,
    result: *mut *mut Blob,
    error: *mut *mut CacheError,
) -> u32 {
    unsafe {
        boundary(error, || {
            output(result)?;
            let cache = required(cache)?;
            let key = compcache::Key::from_bytes(required(key)?.bytes);
            match cache.store.get_blob(&key)? {
                Some(bytes) => {
                    *result = Box::into_raw(Box::new(Blob { bytes }));
                    Ok(OK)
                }
                None => Ok(MISS),
            }
        })
    }
}
/// Produce opaque bytes only on a miss, using compcache's existing key lease.
/// # Safety
/// Pointers and callbacks obey the C header. Callbacks must not unwind; their
/// returned spans remain valid until this function returns.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_get_or_produce(
    cache: *const Cache,
    key: *const Key,
    producer: Option<Producer>,
    user_data: *mut c_void,
    result: *mut *mut Blob,
    error: *mut *mut CacheError,
) -> u32 {
    unsafe {
        boundary(error, || {
            output(result)?;
            let cache = required(cache)?;
            let key = compcache::Key::from_bytes(required(key)?.bytes);
            let producer = producer.ok_or(Error::Invalid("null producer callback"))?;
            let cached = cache.store.get_or_insert_payload(key, || {
                let mut produced = Produced {
                    data: ptr::null(),
                    size: 0,
                    persist: 1,
                };
                let status = producer(user_data, &mut produced);
                if status != 0 {
                    return Err(Box::new(Error::Producer(status)) as compcache::ProducerError);
                }
                if produced.persist > 1 {
                    return Err(Box::new(Error::Invalid("persist must be zero or one"))
                        as compcache::ProducerError);
                }
                let bytes = span(produced.data, produced.size)?;
                Ok(compcache::Produced {
                    payload: Arc::<[u8]>::from(bytes),
                    serialized: Ok((produced.persist != 0).then(|| bytes.to_vec())),
                })
            })?;
            *result = Box::into_raw(Box::new(Blob {
                bytes: cached.payload,
            }));
            if let Some(warning) = cached.cache_error {
                report(error, Error::Cache(warning));
            }
            Ok(OK)
        })
    }
}
/// Evict persisted bytes without invalidating outstanding blob handles.
/// # Safety
/// Pointers obey the handle and key lifetime contracts in the C header.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_evict(
    cache: *const Cache,
    key: *const Key,
    error: *mut *mut CacheError,
) -> u32 {
    unsafe {
        boundary(error, || {
            required(cache)?
                .store
                .force_evict_blob(&compcache::Key::from_bytes(required(key)?.bytes))?;
            Ok(OK)
        })
    }
}
/// Borrow a blob's byte span; null returns an empty span.
/// # Safety
/// Nonnull blob is live; nonnull size points to writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_blob_data(
    blob: *const Blob,
    size: *mut usize,
) -> *const u8 {
    let bytes = unsafe { blob.as_ref() }
        .map(|blob| blob.bytes.as_ref())
        .unwrap_or(&[]);
    if !size.is_null() {
        unsafe { *size = bytes.len() };
    }
    bytes.as_ptr()
}
/// Destroy an owned blob; null is harmless.
/// # Safety
/// Nonnull blob is an owned live handle and is destroyed exactly once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_blob_destroy(blob: *mut Blob) {
    if !blob.is_null() {
        drop(unsafe { Box::from_raw(blob) });
    }
}
/// Return the category of a live error handle, or OK for null.
/// # Safety
/// A nonnull error points to a live error handle.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_error_kind(error: *const CacheError) -> u32 {
    unsafe { error.as_ref() }.map_or(OK, |error| error.cause.kind())
}
/// Borrow an error's UTF-8 message; null returns an empty span.
/// # Safety
/// Nonnull error is live; nonnull size points to writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_error_message(
    error: *const CacheError,
    size: *mut usize,
) -> *const u8 {
    let bytes = unsafe { error.as_ref() }
        .map(|error| error.message.as_bytes())
        .unwrap_or(&[]);
    if !size.is_null() {
        unsafe { *size = bytes.len() };
    }
    bytes.as_ptr()
}
/// Destroy an owned error; null is harmless.
/// # Safety
/// Nonnull error is an owned live handle and is destroyed exactly once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn reussir_polyffi_cache_error_destroy(error: *mut CacheError) {
    if !error.is_null() {
        drop(unsafe { Box::from_raw(error) });
    }
}

#[cfg(test)]
mod tests;
