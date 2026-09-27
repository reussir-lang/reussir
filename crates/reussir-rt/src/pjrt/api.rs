use std::{mem, ptr::NonNull};

use super::{Error, Result, sys::*};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub(super) struct Api(pub(super) NonNull<PJRT_Api>);

/// A completion event consumed by waiting and destroying it.
#[repr(transparent)]
pub(super) struct Event(NonNull<PJRT_Event>);

pub(super) fn non_null<T>(pointer: *mut T, name: &str) -> Result<NonNull<T>> {
    NonNull::new(pointer).ok_or_else(|| Error::local(format!("PjRt returned null {name}")))
}

// All these fields are checked before constructing Api. Never form a reference
// to the entire PJRT_Api: compatible older plugins may have a shorter tail.
macro_rules! call {
    ($api:expr, $function:ident { $($field:ident $(: $value:expr)?),* $(,)? }) => {{
        let api = $api;
        let function = unsafe { (*api.0.as_ptr()).$function.unwrap() };
        paste::paste! {
            api.call(function, [<$function _Args>] {
                struct_size: [<$function _Args_STRUCT_SIZE>] as usize,
                $($field $(: $value)?,)*
                ..Default::default()
            })
        }
    }};
}

pub(super) use call;

impl Api {
    pub(super) unsafe fn new(raw: *const PJRT_Api) -> Result<Self> {
        let handle = non_null(raw.cast_mut(), "API table")?;
        let required = mem::offset_of!(PJRT_Api, PJRT_Client_CreateUninitializedBuffer)
            + mem::size_of::<PJRT_Client_CreateUninitializedBuffer>();
        if unsafe { (*handle.as_ptr()).struct_size } < required {
            return Err(Error::local(
                "PjRt API lacks the buffer-allocation API prefix",
            ));
        }
        if unsafe { (*handle.as_ptr()).pjrt_api_version.major_version } != PJRT_API_MAJOR as i32 {
            return Err(Error::local("incompatible PjRt API major version"));
        }
        macro_rules! require {
            ($($function:ident),* $(,)?) => { $(
                if unsafe { (*handle.as_ptr()).$function.is_none() } {
                    return Err(Error::local(concat!("missing ", stringify!($function))));
                }
            )* };
        }
        require!(
            PJRT_Error_Destroy,
            PJRT_Error_Message,
            PJRT_Error_GetCode,
            PJRT_Plugin_Initialize,
            PJRT_Client_Create,
            PJRT_Client_Destroy,
            PJRT_Client_AddressableDevices,
            PJRT_Client_CreateUninitializedBuffer,
            PJRT_Client_BufferFromHostBuffer,
            PJRT_Buffer_Destroy,
            PJRT_Buffer_CopyToDevice,
            PJRT_Buffer_ToHostBuffer,
            PJRT_Buffer_ReadyEvent,
            PJRT_Event_Await,
            PJRT_Event_Destroy,
        );
        Ok(Self(handle))
    }

    pub(super) fn call<A>(
        self,
        function: unsafe extern "C" fn(*mut A) -> *mut PJRT_Error,
        mut args: A,
    ) -> Result<A> {
        // SAFETY: only this module supplies the checked functions and their
        // corresponding C arguments; public raw host pointers have unsafe APIs.
        let error = unsafe { function(&mut args) };
        if error.is_null() {
            return Ok(args);
        }
        unsafe {
            let mut message = PJRT_Error_Message_Args {
                struct_size: PJRT_Error_Message_Args_STRUCT_SIZE as usize,
                error,
                ..Default::default()
            };
            (*self.0.as_ptr()).PJRT_Error_Message.unwrap()(&mut message);
            let message = if message.message_size == 0 {
                String::new()
            } else {
                String::from_utf8_lossy(std::slice::from_raw_parts(
                    message.message.cast(),
                    message.message_size,
                ))
                .into_owned()
            };
            let mut code = PJRT_Error_GetCode_Args {
                struct_size: PJRT_Error_GetCode_Args_STRUCT_SIZE as usize,
                error,
                ..Default::default()
            };
            let code_error = (*self.0.as_ptr()).PJRT_Error_GetCode.unwrap()(&mut code);
            let status = code_error.is_null().then_some(code.code);
            for error in [code_error, error]
                .into_iter()
                .filter(|error| !error.is_null())
            {
                (*self.0.as_ptr()).PJRT_Error_Destroy.unwrap()(&mut PJRT_Error_Destroy_Args {
                    struct_size: PJRT_Error_Destroy_Args_STRUCT_SIZE as usize,
                    error,
                    ..Default::default()
                });
            }
            Err(Error {
                code: status,
                message,
            })
        }
    }
}

impl Event {
    pub(super) fn new(raw: *mut PJRT_Event) -> Result<Self> {
        non_null(raw, "completion event").map(Self)
    }

    pub(super) fn wait(self, api: Api) -> Result<()> {
        let event = self.0.as_ptr();
        let ready = call!(api, PJRT_Event_Await { event });
        // Destroy the event even when the asynchronous operation failed.
        let destroyed = call!(api, PJRT_Event_Destroy { event });
        ready.and(destroyed).map(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    #[test]
    fn reject_incompatible_api_before_accessing_functions() {
        // This allocation contains only struct_size, not a full API table.
        let short = 0usize;
        unsafe {
            assert!(Api::new(ptr::null()).is_err());
            assert!(Api::new(ptr::from_ref(&short).cast()).is_err());
            let mut api = PJRT_Api {
                struct_size: PJRT_Api_STRUCT_SIZE as usize,
                ..Default::default()
            };
            api.pjrt_api_version.major_version = PJRT_API_MAJOR as i32 + 1;
            assert!(Api::new(&api).is_err());
            api.pjrt_api_version.major_version = PJRT_API_MAJOR as i32;
            assert!(Api::new(&api).is_err()); // Missing required function pointers.
        }
    }
}
