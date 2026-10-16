use super::*;
use crate::pjrt::{api::Api, client::Client, executable::CompilationClient};
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
    device_handles: Vec<*mut PJRT_Device>,
    outputs: usize,
    failure: Failure,
    log: RefCell<Vec<&'static str>>,
    inputs: RefCell<Vec<Vec<*mut PJRT_Buffer>>>,
    non_donatable: RefCell<Vec<i64>>,
}

unsafe fn state<T>(pointer: *mut T) -> &'static State {
    unsafe { &*pointer.cast::<State>() }
}

unsafe extern "C" fn devices(
    args: *mut PJRT_LoadedExecutable_AddressableDevices_Args,
) -> *mut PJRT_Error {
    let args = unsafe { &mut *args };
    args.num_addressable_devices = unsafe { state(args.executable) }.devices;
    args.addressable_devices = unsafe { state(args.executable) }.device_handles.as_ptr();
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
    *state.non_donatable.borrow_mut() = indices.to_vec();
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

unsafe extern "C" fn destroy_loaded(
    args: *mut PJRT_LoadedExecutable_Destroy_Args,
) -> *mut PJRT_Error {
    unsafe { state((*args).executable) }
        .log
        .borrow_mut()
        .push("loaded destroyed");
    ptr::null_mut()
}

unsafe extern "C" fn destroy_client(args: *mut PJRT_Client_Destroy_Args) -> *mut PJRT_Error {
    unsafe { state((*args).client) }
        .log
        .borrow_mut()
        .push("client destroyed");
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
            device_handles: (1..=devices).map(ptr::without_provenance_mut).collect(),
            outputs,
            failure,
            log: RefCell::default(),
            inputs: RefCell::default(),
            non_donatable: RefCell::default(),
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
fn packs_logical_inputs_and_returns_outputs_by_device() {
    let fixture = Fixture::new(2, 2, Failure::None);
    let first = [
        ptr::without_provenance_mut(1),
        ptr::without_provenance_mut(2),
    ];
    let second = [
        ptr::without_provenance_mut(3),
        ptr::without_provenance_mut(4),
    ];
    // Three logical arguments, two devices; the third reuses the first array.
    let inputs = [first.as_ptr(), second.as_ptr(), first.as_ptr()];
    for _ in 0..2 {
        fixture.state.log.borrow_mut().clear();
        let outputs =
            unsafe { fixture.executable.clone().execute(&inputs, 2, &[0, 1, 2]) }.unwrap();
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
        for (result, shards) in outputs.into_iter().enumerate() {
            for (device, shard) in shards.into_iter().enumerate() {
                let buffer = unsafe { &*shard.cast::<(*const State, usize, usize)>() };
                assert_eq!((buffer.1, buffer.2), (device, result));
                call!(
                    fixture.executable.owner.api,
                    PJRT_Buffer_Destroy { buffer: shard }
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
fn zero_outputs_still_wait_on_every_device() {
    let fixture = Fixture::new(2, 0, Failure::None);
    unsafe { fixture.executable.clone().execute(&[], 0, &[]) }.unwrap();
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
        assert!(unsafe { fixture.executable.clone().execute(&[], slots, &[]) }.is_err());
        assert!(!fixture.state.log.borrow().contains(&"execute"));
    }
}

#[test]
fn metadata_failure_releases_temporary_executable() {
    let fixture = Fixture::new(1, 1, Failure::Metadata);
    assert!(unsafe { fixture.executable.clone().execute(&[], 1, &[]) }.is_err());
    assert_eq!(*fixture.state.log.borrow(), ["metadata destroyed"]);
}

#[test]
fn launch_failure_does_not_await_unpopulated_event() {
    let fixture = Fixture::new(1, 1, Failure::Launch);
    assert!(unsafe { fixture.executable.clone().execute(&[], 1, &[]) }.is_err());
    assert_eq!(
        *fixture.state.log.borrow(),
        ["metadata destroyed", "execute"]
    );
}

#[test]
fn first_device_failure_still_waits_on_other_devices_before_cleanup() {
    let fixture = Fixture::new(2, 2, Failure::Completion);
    let error = unsafe { fixture.executable.clone().execute(&[], 2, &[]) }.unwrap_err();
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
    assert!(unsafe { fixture.executable.clone().execute(&[], 2, &[]) }.is_err());
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

#[test]
fn success_drops_consumed_reference_after_every_device_finishes() {
    let Fixture {
        executable,
        _api,
        state,
    } = Fixture::new(2, 0, Failure::None);
    let weak = Arc::downgrade(&executable);
    unsafe { executable.execute(&[], 0, &[]) }.unwrap();
    assert!(weak.upgrade().is_none());
    assert_eq!(
        *state.log.borrow(),
        [
            "metadata destroyed",
            "execute",
            "await",
            "event destroyed",
            "await",
            "event destroyed",
            "loaded destroyed",
            "client destroyed"
        ]
    );
}

#[test]
fn failure_drops_consumed_reference_after_output_cleanup() {
    let Fixture {
        executable,
        _api,
        state,
    } = Fixture::new(2, 1, Failure::Completion);
    let weak = Arc::downgrade(&executable);
    assert!(unsafe { executable.execute(&[], 1, &[]) }.is_err());
    assert!(weak.upgrade().is_none());
    assert_eq!(
        *state.log.borrow(),
        [
            "metadata destroyed",
            "execute",
            "await",
            "event destroyed",
            "await",
            "event destroyed",
            "buffer destroyed",
            "buffer destroyed",
            "loaded destroyed",
            "client destroyed"
        ]
    );
}

#[test]
fn descriptor_placement_must_match_executable_before_accessing_shards() {
    let fixture = Fixture::new(2, 1, Failure::None);
    let expected = &fixture.state.device_handles;
    fixture.executable.validate_devices(expected).unwrap();
    for invalid in [&expected[..1], &[]] {
        assert!(
            fixture
                .executable
                .validate_devices(invalid)
                .unwrap_err()
                .message
                .contains("device count")
        );
    }
    let reversed = [expected[1], expected[0]];
    assert!(
        fixture
            .executable
            .validate_devices(&reversed)
            .unwrap_err()
            .message
            .contains("device order")
    );
    assert!(fixture.state.log.borrow().is_empty());
    assert!(fixture.state.inputs.borrow().is_empty());
}

#[test]
fn partial_donation_contract_reaches_every_device_unchanged() {
    let fixture = Fixture::new(2, 0, Failure::None);
    let shards = [
        ptr::without_provenance_mut(1),
        ptr::without_provenance_mut(2),
    ];
    let inputs = [shards.as_ptr(); 3];
    for denied in [&[0i64, 2][..], &[], &[0, 1, 2]] {
        unsafe { fixture.executable.clone().execute(&inputs, 0, denied) }.unwrap();
        assert_eq!(*fixture.state.non_donatable.borrow(), denied);
        assert_eq!(fixture.state.inputs.borrow().len(), 2);
    }
}

#[test]
fn malformed_donation_contract_is_rejected_before_execution() {
    let fixture = Fixture::new(1, 0, Failure::None);
    let shards = [ptr::without_provenance_mut(1)];
    let inputs = [shards.as_ptr(); 3];
    for denied in [&[-1i64][..], &[3], &[1, 1], &[2, 0]] {
        let error = unsafe { fixture.executable.clone().execute(&inputs, 0, denied) }.unwrap_err();
        assert!(error.message.contains("non-donatable indices"));
        assert!(!fixture.state.log.borrow().contains(&"execute"));
    }
}

#[test]
fn donated_execution_failure_is_never_retried() {
    for failure in [Failure::Launch, Failure::Completion, Failure::NullOutput] {
        let fixture = Fixture::new(2, 1, failure);
        let shards = [
            ptr::without_provenance_mut(1),
            ptr::without_provenance_mut(2),
        ];
        let inputs = [shards.as_ptr()];
        assert!(unsafe { fixture.executable.clone().execute(&inputs, 1, &[]) }.is_err());
        let log = fixture.state.log.borrow();
        assert_eq!(log.iter().filter(|entry| **entry == "execute").count(), 1);
        if failure != Failure::Launch {
            assert_eq!(log.iter().filter(|entry| **entry == "await").count(), 2);
            let destroyed = if failure == Failure::NullOutput { 1 } else { 2 };
            assert_eq!(
                log.iter()
                    .filter(|entry| **entry == "buffer destroyed")
                    .count(),
                destroyed
            );
        }
    }
}
