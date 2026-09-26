use reussir_pjrt_sys::*;
use std::ptr;

const TEXTURE: &[u8] = include_bytes!("fixtures/texture.mlir");
const OPTIONS: &[u8] = include_bytes!("fixtures/compile_options.pb");

// Test-only convenience for the C argument initialization convention.
macro_rules! args {
    ($name:ident { $($field:ident $(: $value:expr)?),* $(,)? }) => {
        paste::paste! {
            $name {
                struct_size: [<$name _STRUCT_SIZE>] as usize,
                $($field $(: $value)?,)*
                ..Default::default()
            }
        }
    };
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn compile_and_execute_embedded_texture() {
    let path = std::env::var_os("REUSSIR_PJRT_PLUGIN").expect("set REUSSIR_PJRT_PLUGIN");
    // SAFETY: the test explicitly loads a trusted native PjRt plugin. It stays
    // resident, as in XLA's loader: plugin globals can outlive client teardown.
    // Every C argument is sized; owned handles are destroyed below, and the
    // download allocation remains live until its completion event is awaited.
    unsafe {
        let library = Box::leak(Box::new(libloading::Library::new(path).unwrap()));
        let get = library
            .get::<unsafe extern "C" fn() -> *const PJRT_Api>(b"GetPjrtApi\0")
            .unwrap();
        let api = get();
        assert!(!api.is_null());
        // Read only fields in this checked prefix, not &*api: an older compatible
        // plugin need not allocate the newer tail of our generated PJRT_Api.
        assert!(
            (*api).struct_size
                >= std::mem::offset_of!(PJRT_Api, PJRT_Buffer_ReadyEvent)
                    + std::mem::size_of::<PJRT_Buffer_ReadyEvent>()
        );
        assert_eq!((*api).pjrt_api_version.major_version, PJRT_API_MAJOR as i32);

        macro_rules! call {
            ($function:ident, $args:expr) => {{
                let mut args = $args;
                let error = (*api).$function.expect(stringify!($function))(&mut args);
                if !error.is_null() {
                    let mut message = args!(PJRT_Error_Message_Args { error });
                    (*api).PJRT_Error_Message.unwrap()(&mut message);
                    let text = if message.message_size == 0 {
                        String::new()
                    } else {
                        String::from_utf8_lossy(std::slice::from_raw_parts(
                            message.message.cast(),
                            message.message_size,
                        ))
                        .into_owned()
                    };
                    (*api).PJRT_Error_Destroy.unwrap()(&mut args!(PJRT_Error_Destroy_Args {
                        error
                    }));
                    panic!("{}: {text}", stringify!($function));
                }
                args
            }};
        }
        macro_rules! await_event {
            ($event:expr) => {{
                let event = $event;
                assert!(!event.is_null());
                call!(PJRT_Event_Await, args!(PJRT_Event_Await_Args { event }));
                call!(PJRT_Event_Destroy, args!(PJRT_Event_Destroy_Args { event }));
            }};
        }

        call!(
            PJRT_Plugin_Initialize,
            args!(PJRT_Plugin_Initialize_Args {})
        );
        let client = call!(PJRT_Client_Create, args!(PJRT_Client_Create_Args {})).client;
        let program = args!(PJRT_Program {
            code: TEXTURE.as_ptr().cast_mut().cast(),
            code_size: TEXTURE.len(),
            format: c"mlir".as_ptr(),
            format_size: 4,
        });
        let executable = call!(
            PJRT_Client_Compile,
            args!(PJRT_Client_Compile_Args {
                client,
                program: &program,
                compile_options: OPTIONS.as_ptr().cast(),
                compile_options_size: OPTIONS.len(),
            })
        )
        .executable;

        let mut options = args!(PJRT_ExecuteOptions {});
        let inputs: [*mut PJRT_Buffer; 0] = [];
        let input_list = inputs.as_ptr();
        let mut output = ptr::null_mut();
        let output_list = &mut output as *mut *mut PJRT_Buffer;
        let mut complete = ptr::null_mut();
        call!(
            PJRT_LoadedExecutable_Execute,
            args!(PJRT_LoadedExecutable_Execute_Args {
                executable,
                options: &mut options,
                argument_lists: &input_list,
                num_devices: 1,
                num_args: 0,
                output_lists: &output_list,
                device_complete_events: &mut complete,
            })
        );
        await_event!(complete);

        let mut pixels = [0.0f32; 64];
        let download = call!(
            PJRT_Buffer_ToHostBuffer,
            args!(PJRT_Buffer_ToHostBuffer_Args {
                src: output,
                dst: pixels.as_mut_ptr().cast(),
                dst_size: std::mem::size_of_val(&pixels),
            })
        );
        await_event!(download.event);

        call!(
            PJRT_Buffer_Destroy,
            args!(PJRT_Buffer_Destroy_Args { buffer: output })
        );
        call!(
            PJRT_LoadedExecutable_Destroy,
            args!(PJRT_LoadedExecutable_Destroy_Args { executable })
        );
        call!(
            PJRT_Client_Destroy,
            args!(PJRT_Client_Destroy_Args { client })
        );
        for (i, pixel) in pixels.into_iter().enumerate() {
            assert_eq!(pixel, ((i / 8 + i % 8) % 2) as f32);
        }
    }
}
