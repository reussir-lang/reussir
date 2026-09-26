use std::ptr::NonNull;

use super::{
    Result,
    api::{Api, call, non_null},
    sys::*,
};

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub(super) struct Client(NonNull<PJRT_Client>);

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub(super) struct Device(NonNull<PJRT_Device>);

impl Client {
    pub(super) fn create(api: Api) -> Result<Self> {
        Ok(Self(non_null(
            call!(api, PJRT_Client_Create {})?.client,
            "client",
        )?))
    }

    pub(super) fn as_ptr(self) -> *mut PJRT_Client {
        self.0.as_ptr()
    }

    // Setup/teardown receive the API explicitly because the global context
    // has not been published yet. Device handles remain owned by the client.
    pub(super) fn addressable_devices(self, api: Api) -> Result<Vec<Device>> {
        let args = call!(
            api,
            PJRT_Client_AddressableDevices {
                client: self.as_ptr()
            }
        )?;
        if args.num_addressable_devices == 0 {
            return Ok(Vec::new());
        }
        let pointer = non_null(args.addressable_devices.cast_mut(), "device list")?;
        unsafe { std::slice::from_raw_parts(pointer.as_ptr(), args.num_addressable_devices) }
            .iter()
            .map(|&device| non_null(device, "device").map(Device))
            .collect()
    }

    /// Destroy a live client after all uses of it and its devices have ended.
    pub(super) unsafe fn destroy(self, api: Api) -> Result<()> {
        call!(
            api,
            PJRT_Client_Destroy {
                client: self.as_ptr()
            }
        )?;
        tracing::debug!(client = ?self, "destroyed PjRt context");
        Ok(())
    }
}

impl Device {
    pub(super) fn as_ptr(self) -> *mut PJRT_Device {
        self.0.as_ptr()
    }
}
