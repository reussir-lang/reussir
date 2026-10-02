use std::{
    ffi::OsStr,
    mem,
    sync::{Arc, OnceLock},
};

use super::{
    Error, Result,
    api::{Api, call},
    cache_config::CacheConfig,
    client::{Client, Device},
    executable::{CompilationCache, CompilationClient},
    sys::*,
};

/// Process-lifetime PjRt client owned by the runtime, never passed to language code.
pub(super) struct Context {
    pub(super) api: Api,
    pub(super) client: Client,
    devices: Vec<Device>,
    pub(super) executables: CompilationCache,
}

// SAFETY: PjRtClient is thread-safe; the API table and device handles are
// immutable after setup and owned by the resident plugin/client. The singleton
// never destroys the client while generated code may still hold an array or
// executable. The compilation cache synchronizes access to its entries.
unsafe impl Send for Context {}
unsafe impl Sync for Context {}

static CONTEXT: OnceLock<Result<Context>> = OnceLock::new();

extern "C" fn close_cache() {
    if let Some(Ok(context)) = CONTEXT.get() {
        // C/C++ hosts may have already run this thread's TLS destructors.
        // Foyer/Tokio shutdown needs fresh TLS even though its workers are live.
        if let Ok(worker) = std::thread::Builder::new()
            .name("reussir-pjrt-shutdown".into())
            .spawn(move || context.executables.close())
        {
            let _ = worker.join();
        }
    }
}

pub(super) fn get() -> Result<&'static Context> {
    CONTEXT
        .get_or_init(|| {
            let path = std::env::var_os("REUSSIR_PJRT_PLUGIN").ok_or_else(|| {
                Error::local("set REUSSIR_PJRT_PLUGIN to the runtime's PjRt plugin")
            })?;
            // Plugin selection is runtime configuration, just like selecting a
            // native shared library through the platform loader's search path.
            let context = unsafe { Context::load(path) }?;
            // The process-lifetime singleton is not dropped at exit.
            if unsafe { libc::atexit(close_cache) } != 0 {
                return Err(Error::local("could not register PJRT cache shutdown"));
            }
            Ok(context)
        })
        .as_ref()
        .map_err(Clone::clone)
}

impl Context {
    /// Load and initialize a plugin, then create a client with default options.
    /// Plugins stay resident for the process lifetime, as in XLA's loader;
    /// plugin globals and worker threads can outlive individual clients.
    ///
    /// # Safety
    /// `path` must identify a trusted, ABI-conforming native PjRt plugin. Plugin
    /// initialization must not race with a separate loader outside this module.
    unsafe fn load(path: impl AsRef<OsStr>) -> Result<Self> {
        let config = CacheConfig::load()?;
        let path = path.as_ref();
        let library = unsafe { libloading::Library::new(path) }
            .map_err(|error| Error::local(error.to_string()))?;
        let get =
            unsafe { library.get::<unsafe extern "C" fn() -> *const PJRT_Api>(b"GetPjrtApi\0") }
                .map_err(|error| Error::local(error.to_string()))?;
        let raw = unsafe { get() };
        mem::forget(library);
        let api = unsafe { Api::new(raw) }?;

        call!(api, PJRT_Plugin_Initialize {})?;
        let client = Client::create(api)?;
        let owner = Arc::new(CompilationClient { api, client });
        let namespace = if config.disk.enabled {
            match owner.namespace(std::path::Path::new(path)) {
                Ok(namespace) => Some(namespace),
                Err(error) => {
                    tracing::warn!(%error, "PJRT disk cache disabled");
                    None
                }
            }
        } else {
            None
        };
        let executables = CompilationCache::new(owner, &config, namespace)?;
        let mut context = Self {
            api,
            client,
            devices: Vec::new(),
            executables,
        };
        context.devices = client.addressable_devices(api)?;
        tracing::debug!(
            ?client,
            devices = context.devices.len(),
            "created PjRt context"
        );
        Ok(context)
    }

    pub(super) fn device(&self, index: usize) -> Result<Device> {
        self.devices
            .get(index)
            .copied()
            .ok_or_else(|| Error::local("PjRt device index out of range"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
    fn shared_context_initializes_once() {
        let workers: Vec<_> = (0..4)
            .map(|_| std::thread::spawn(|| get().unwrap()))
            .collect();
        let context = get().unwrap();
        for worker in workers {
            assert!(std::ptr::eq(context, worker.join().unwrap()));
        }
    }
}
