use std::{ffi::OsStr, mem, sync::OnceLock};

use super::{
    Error, Result,
    api::{Api, call},
    client::{Client, Device},
    sys::*,
};

/// Process-lifetime PjRt client owned by the runtime, never passed to language code.
pub(super) struct Context {
    pub(super) api: Api,
    pub(super) client: Client,
    devices: Vec<Device>,
}

// SAFETY: PjRtClient is thread-safe; the API table and device handles are
// immutable after setup and owned by the resident plugin/client. The singleton
// never destroys the client while generated code may still hold an array.
unsafe impl Send for Context {}
unsafe impl Sync for Context {}

pub(super) fn get() -> Result<&'static Context> {
    static CONTEXT: OnceLock<Result<Context>> = OnceLock::new();
    CONTEXT
        .get_or_init(|| {
            let path = std::env::var_os("REUSSIR_PJRT_PLUGIN").ok_or_else(|| {
                Error::local("set REUSSIR_PJRT_PLUGIN to the runtime's PjRt plugin")
            })?;
            // Plugin selection is runtime configuration, just like selecting a
            // native shared library through the platform loader's search path.
            unsafe { Context::load(path) }
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
        let mut context = Self {
            api,
            client,
            devices: Vec::new(),
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

impl Drop for Context {
    fn drop(&mut self) {
        if let Err(error) = unsafe { self.client.destroy(self.api) } {
            tracing::warn!(client = ?self.client, %error, "PjRt context cleanup failed");
        }
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
