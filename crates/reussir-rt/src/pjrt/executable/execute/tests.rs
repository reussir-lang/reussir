use super::*;
use crate::pjrt::{api::Api, client::Client, executable::CompilationClient, ffi};
use std::{cell::RefCell, ptr::NonNull, sync::Arc};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Failure {
    None,
    Metadata,
    Launch,
    Completion,
    NullOutput,
}

struct State {
    devices: usize,
    outputs: usize,
    failure: Failure,
    log: RefCell<Vec<&'static str>>,
    inputs: RefCell<Vec<Vec<*mut PJRT_Buffer>>>,
}

unsafe fn state<T>(pointer: *mut T) -> &'static State {
    unsafe { &*pointer.cast::<State>() }
}

unsafe extern "C" fn devices(
    args: *mut PJRT_LoadedExecutable_AddressableDevices_Args,
) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    args.num_addressable_devices = unsafe { state(args.executable) }.devices;
    ptr::null_mut()
}

unsafe extern "C" fn executable(
    args: *mut PJRT_LoadedExecutable_GetExecutable_Args,
) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    args.executable = args.loaded_executable.cast();
    ptr::null_mut()
}

unsafe extern "C" fn output_count(args: *mut PJRT_Executable_NumOutputs_Args) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    let state = unsafe { state(args.executable) };
    if state.failure == Failure::Metadata {
        return ptr::dangling_mut();
    }
    args.num_outputs = state.outputs;
    ptr::null_mut()
}

unsafe extern "C" fn destroy_executable(
    args: *mut PJRT_Executable_Destroy_Args,
) -> *mut PJRT_Error {
    unsafe { state((*args).executable) }
        .log
        .borrow_mut()
        .push("metadata destroyed");
    ptr::null_mut()
}

unsafe extern "C" fn execute(args: *mut PJRT_LoadedExecutable_Execute_Args) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    let state = unsafe { state(args.executable) };
    state.log.borrow_mut().push("execute");
    assert_eq!(
        args.struct_size,
        PJRT_LoadedExecutable_Execute_Args_STRUCT_SIZE as usize
    );
    assert_eq!(args.num_devices, state.devices);
    assert!(args.execute_device.is_null()); // Use the compiled assignment.
    let options = unsafe { &*args.options };
    assert_eq!(
        options.struct_size,
        PJRT_ExecuteOptions_STRUCT_SIZE as usize
    );
    let indices = unsafe {
        std::slice::from_raw_parts(
            options.non_donatable_input_indices,
            options.num_non_donatable_input_indices,
        )
    };
    assert_eq!(indices, (0..args.num_args as i64).collect::<Vec<_>>());
    *state.inputs.borrow_mut() = (0..args.num_devices)
        .map(|device| unsafe {
            std::slice::from_raw_parts(*args.argument_lists.add(device), args.num_args).to_vec()
        })
        .collect();
    if state.failure == Failure::Launch {
        return ptr::dangling_mut();
    }
    for device in 0..args.num_devices {
        let event = Box::into_raw(Box::new((ptr::from_ref(state), device))).cast();
        unsafe { args.device_complete_events.add(device).write(event) };
        for index in 0..state.outputs {
            if state.failure != Failure::NullOutput || device != 0 || index != 0 {
                let buffer = Box::into_raw(Box::new((ptr::from_ref(state), device, index))).cast();
                unsafe { (*args.output_lists.add(device)).add(index).write(buffer) };
            }
        }
    }
    ptr::null_mut()
}

unsafe extern "C" fn await_event(args: *mut PJRT_Event_Await_Args) -> *mut PJRT_Error {
    let (state, device) = unsafe { &*(*args).event.cast::<(*const State, usize)>() };
    let state = unsafe { &**state };
    state.log.borrow_mut().push("await");
    if state.failure == Failure::Completion && *device == 0 {
        ptr::dangling_mut()
    } else {
        ptr::null_mut()
    }
}

unsafe extern "C" fn destroy_event(args: *mut PJRT_Event_Destroy_Args) -> *mut PJRT_Error {
    let event = unsafe { Box::from_raw((*args).event.cast::<(*const State, usize)>()) };
    unsafe { &*event.0 }
        .log
        .borrow_mut()
        .push("event destroyed");
    ptr::null_mut()
}

unsafe extern "C" fn destroy_buffer(args: *mut PJRT_Buffer_Destroy_Args) -> *mut PJRT_Error {
    let buffer = unsafe { Box::from_raw((*args).buffer.cast::<(*const State, usize, usize)>()) };
    unsafe { &*buffer.0 }
        .log
        .borrow_mut()
        .push("buffer destroyed");
    ptr::null_mut()
}

unsafe extern "C" fn destroy_loaded(_: *mut PJRT_LoadedExecutable_Destroy_Args) -> *mut PJRT_Error {
    ptr::null_mut()
}

unsafe extern "C" fn destroy_client(_: *mut PJRT_Client_Destroy_Args) -> *mut PJRT_Error {
    ptr::null_mut()
}

unsafe extern "C" fn error_message(args: *mut PJRT_Error_Message_Args) {
    unsafe {
        (*args).message = c"execution failed".as_ptr();
        (*args).message_size = 16;
    }
}

unsafe extern "C" fn error_code(args: *mut PJRT_Error_GetCode_Args) -> *mut PJRT_Error {
    unsafe { (*args).code = PJRT_Error_Code_PJRT_Error_Code_INTERNAL };
    ptr::null_mut()
}

unsafe extern "C" fn error_destroy(_: *mut PJRT_Error_Destroy_Args) {}

// Drop the executable before its borrowed mock API and state allocations.
struct Fixture {
    executable: Arc<Executable>,
    _api: Box<PJRT_Api>,
    state: Box<State>,
}

impl Fixture {
    fn new(devices: usize, outputs: usize, failure: Failure) -> Self {
        let mut state = Box::new(State {
            devices,
            outputs,
            failure,
            log: RefCell::default(),
            inputs: RefCell::default(),
        });
        let api = Box::new(PJRT_Api {
            PJRT_LoadedExecutable_AddressableDevices: Some(self::devices),
            PJRT_LoadedExecutable_GetExecutable: Some(executable),
            PJRT_Executable_NumOutputs: Some(output_count),
            PJRT_Executable_Destroy: Some(destroy_executable),
            PJRT_LoadedExecutable_Execute: Some(execute),
            PJRT_LoadedExecutable_Destroy: Some(destroy_loaded),
            PJRT_Client_Destroy: Some(destroy_client),
            PJRT_Buffer_Destroy: Some(destroy_buffer),
            PJRT_Event_Await: Some(await_event),
            PJRT_Event_Destroy: Some(destroy_event),
            PJRT_Error_Message: Some(error_message),
            PJRT_Error_GetCode: Some(error_code),
            PJRT_Error_Destroy: Some(error_destroy),
            ..Default::default()
        });
        let native = NonNull::from(&mut *state);
        let owner = Arc::new(CompilationClient {
            api: Api(NonNull::from(&*api)),
            // Client is repr(transparent) over a non-null native pointer.
            client: unsafe { std::mem::transmute::<NonNull<PJRT_Client>, Client>(native.cast()) },
        });
        Self {
            executable: Arc::new(Executable {
                native: native.cast(),
                owner,
            }),
            _api: api,
            state,
        }
    }
}

#[test]
fn abi_accepts_logical_array_descriptors_and_packs_devices_internally() {
    #[repr(C)]
    struct Descriptor {
        shards: [std::mem::MaybeUninit<*mut PJRT_Buffer>; 2],
        offset: usize,
        extent: usize,
    }
    let fixture = Fixture::new(2, 2, Failure::None);
    let first = [
        ptr::without_provenance_mut(1),
        ptr::without_provenance_mut(2),
    ];
    let second = [
        ptr::without_provenance_mut(3),
        ptr::without_provenance_mut(4),
    ];
    let arrays = [first, second, first].map(|shards| Descriptor {
        shards: shards.map(std::mem::MaybeUninit::new),
        offset: 0,
        extent: 8,
    });
    // Three logical arguments, two devices; the third reuses the first array.
    let inputs = arrays.each_ref().map(|array| ptr::from_ref(array).cast());
    for _ in 0..2 {
        fixture.state.log.borrow_mut().clear();
        let mut outputs = [0; 2].map(|_| Descriptor {
            shards: [std::mem::MaybeUninit::<*mut PJRT_Buffer>::uninit(); 2],
            offset: 0,
            extent: 8,
        });
        let destinations = outputs.each_mut().map(|array| ptr::from_mut(array).cast());
        unsafe {
            ffi::__reussir_pjrt_executable_execute(
                Arc::as_ptr(&fixture.executable),
                inputs.as_ptr(),
                inputs.len(),
                destinations.as_ptr(),
                destinations.len(),
            )
        };
        assert_eq!(
            *fixture.state.inputs.borrow(),
            [
                vec![first[0], second[0], first[0]],
                vec![first[1], second[1], first[1]],
            ]
        );
        assert_eq!(
            *fixture.state.log.borrow(),
            [
                "metadata destroyed",
                "execute",
                "await",
                "event destroyed",
                "await",
                "event destroyed",
            ]
        );
        assert_eq!(Arc::strong_count(&fixture.executable), 1);
        for (result, array) in outputs.into_iter().enumerate() {
            assert_eq!(array.offset, 0);
            assert_eq!(array.extent, 8);
            for (device, shard) in array.shards.into_iter().enumerate() {
                let buffer =
                    unsafe { &*shard.assume_init().cast::<(*const State, usize, usize)>() };
                assert_eq!((buffer.1, buffer.2), (device, result));
                call!(
                    fixture.executable.owner.api,
                    PJRT_Buffer_Destroy {
                        buffer: unsafe { shard.assume_init() }
                    }
                )
                .unwrap();
            }
        }
        assert_eq!(
            fixture
                .state
                .log
                .borrow()
                .iter()
                .filter(|&&s| s == "buffer destroyed")
                .count(),
            4
        );
    }
}

#[test]
fn zero_outputs_still_wait_on_every_device_and_allow_null_lists() {
    let fixture = Fixture::new(2, 0, Failure::None);
    unsafe {
        ffi::__reussir_pjrt_executable_execute(
            Arc::as_ptr(&fixture.executable),
            ptr::null(),
            0,
            ptr::null(),
            0,
        )
    };
    assert_eq!(
        *fixture.state.log.borrow(),
        [
            "metadata destroyed",
            "execute",
            "await",
            "event destroyed",
            "await",
            "event destroyed"
        ]
    );
}

#[test]
fn rejects_output_count_and_non_addressable_execution_before_launch() {
    for (devices, slots) in [(1, 0), (2, 3), (0, 2)] {
        let fixture = Fixture::new(devices, 2, Failure::None);
        assert!(unsafe { fixture.executable.execute(&[], slots) }.is_err());
        assert!(!fixture.state.log.borrow().contains(&"execute"));
    }
}

#[test]
fn metadata_failure_releases_temporary_executable() {
    let fixture = Fixture::new(1, 1, Failure::Metadata);
    assert!(unsafe { fixture.executable.execute(&[], 1) }.is_err());
    assert_eq!(*fixture.state.log.borrow(), ["metadata destroyed"]);
}

#[test]
fn launch_failure_does_not_await_unpopulated_event() {
    let fixture = Fixture::new(1, 1, Failure::Launch);
    assert!(unsafe { fixture.executable.execute(&[], 1) }.is_err());
    assert_eq!(
        *fixture.state.log.borrow(),
        ["metadata destroyed", "execute"]
    );
}

#[test]
fn first_device_failure_still_waits_on_other_devices_before_cleanup() {
    let fixture = Fixture::new(2, 2, Failure::Completion);
    let error = unsafe { fixture.executable.execute(&[], 2) }.unwrap_err();
    assert_eq!(error.message, "execution failed");
    assert_eq!(
        *fixture.state.log.borrow(),
        [
            "metadata destroyed",
            "execute",
            "await",
            "event destroyed",
            "await",
            "event destroyed",
            "buffer destroyed",
            "buffer destroyed",
            "buffer destroyed",
            "buffer destroyed"
        ]
    );
}

#[test]
fn invalid_output_waits_before_releasing_other_outputs() {
    let fixture = Fixture::new(2, 2, Failure::NullOutput);
    assert!(unsafe { fixture.executable.execute(&[], 2) }.is_err());
    assert_eq!(
        *fixture.state.log.borrow(),
        [
            "metadata destroyed",
            "execute",
            "await",
            "event destroyed",
            "await",
            "event destroyed",
            "buffer destroyed",
            "buffer destroyed",
            "buffer destroyed"
        ]
    );
}
