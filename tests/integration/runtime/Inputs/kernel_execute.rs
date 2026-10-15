use std::{ffi::c_void, mem::MaybeUninit, ptr};

unsafe extern "C" {
    fn __reussir_pjrt_compile(
        bytecode: *const u8,
        bytecode_size: usize,
        checksum: *const u8,
        options: *const u8,
        options_size: usize,
    ) -> *const c_void;
    fn __reussir_pjrt_executable_execute(
        executable: *const c_void,
        inputs: *const *const c_void,
        num_inputs: usize,
        outputs: *const *mut c_void,
        num_outputs: usize,
        non_donatable: *const i64,
        num_non_donatable: usize,
    );
    fn __reussir_pjrt_array_from_host(
        device: usize,
        element_type: u32,
        dims: *const i64,
        rank: usize,
        data: *const c_void,
        byte_strides: *const i64,
        options: *const c_void,
    ) -> *mut c_void;
    fn __reussir_pjrt_array_to_host(
        buffer: *mut c_void,
        data: *mut c_void,
        bytes: usize,
        layout: *const c_void,
    );
    fn __reussir_pjrt_array_deallocate(buffer: *mut c_void);
}

const OPTIONS: &[u8] =
    include_bytes!("../../../../crates/reussir-pjrt-sys/tests/fixtures/compile_options.pb");

unsafe fn compile(path: &str, options: &[u8]) -> *const c_void {
    let bytes = std::fs::read(path).unwrap();
    let digest = blake3::hash(&bytes).to_hex();
    unsafe {
        __reussir_pjrt_compile(
            bytes.as_ptr(),
            bytes.len(),
            digest.as_ptr(),
            options.as_ptr(),
            options.len(),
        )
    }
}

// Shared source-kernel execution harness. Arguments before `--` are rank-zero
// inputs; arguments after it are exact expected rank-zero outputs. Each is
// spelled bool:true, i64:-3, u64:18446744073709551615, or f32:1.5.
fn scalar(text: &str) -> (u32, Vec<u8>) {
    let (kind, value) = text.split_once(':').expect("typed scalar: KIND:VALUE");
    match kind {
        "bool" => (1, vec![u8::from(value.parse::<bool>().unwrap())]),
        "i64" => (5, value.parse::<i64>().unwrap().to_ne_bytes().to_vec()),
        "u64" => (9, value.parse::<u64>().unwrap().to_ne_bytes().to_vec()),
        "f32" => (11, value.parse::<f32>().unwrap().to_ne_bytes().to_vec()),
        _ => panic!("unsupported scalar kind: {kind}"),
    }
}
fn main() {
    let arguments: Vec<_> = std::env::args().skip(1).collect();
    let separator = arguments
        .iter()
        .position(|arg| arg == "--")
        .expect("expected -- before result values");
    assert!(separator > 0, "first argument must be MLIR bytecode path");
    let inputs: Vec<_> = arguments[1..separator]
        .iter()
        .map(|arg| scalar(arg))
        .collect();
    let expected: Vec<_> = arguments[separator + 1..]
        .iter()
        .map(|arg| scalar(arg).1)
        .collect();
    unsafe {
        let buffers: Vec<_> = inputs
            .iter()
            .map(|(dtype, bytes)| {
                __reussir_pjrt_array_from_host(
                    0,
                    *dtype,
                    ptr::null(),
                    0,
                    bytes.as_ptr().cast(),
                    ptr::null(),
                    ptr::null(),
                )
            })
            .collect();
        let inputs: Vec<*const c_void> = buffers
            .iter()
            .map(|buffer| ptr::from_ref(buffer).cast())
            .collect();
        let mut outputs = vec![MaybeUninit::<*mut c_void>::uninit(); expected.len()];
        let destinations: Vec<*mut c_void> = outputs
            .iter_mut()
            .map(|slot| ptr::from_mut(slot).cast())
            .collect();
        let non_donatable: Vec<_> = (0..buffers.len() as i64).collect();
        __reussir_pjrt_executable_execute(
            compile(&arguments[0], OPTIONS),
            inputs.as_ptr(),
            inputs.len(),
            destinations.as_ptr(),
            destinations.len(),
            non_donatable.as_ptr(),
            non_donatable.len(),
        );
        for (index, (slot, expected)) in outputs.into_iter().zip(expected).enumerate() {
            let buffer = slot.assume_init();
            let mut actual = vec![0u8; expected.len()];
            __reussir_pjrt_array_to_host(
                buffer,
                actual.as_mut_ptr().cast(),
                actual.len(),
                ptr::null(),
            );
            assert_eq!(actual, expected, "output {index}");
            __reussir_pjrt_array_deallocate(buffer);
        }
        for buffer in buffers {
            __reussir_pjrt_array_deallocate(buffer);
        }
    }
}
