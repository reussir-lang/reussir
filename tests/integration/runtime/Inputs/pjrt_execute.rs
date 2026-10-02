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
    );
    fn __reussir_pjrt_executable_release(executable: *const c_void);
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

unsafe fn upload(device: usize, values: &[f32; 4]) -> *mut c_void {
    // PJRT_Buffer_Type_F32 = 11 in the public PJRT C API.
    unsafe {
        __reussir_pjrt_array_from_host(
            device,
            11,
            [4i64].as_ptr(),
            1,
            values.as_ptr().cast(),
            ptr::null(),
            ptr::null(),
        )
    }
}

unsafe fn download(buffer: *mut c_void) -> [f32; 4] {
    let mut values = [0.0f32; 4];
    unsafe {
        __reussir_pjrt_array_to_host(
            buffer,
            values.as_mut_ptr().cast(),
            size_of_val(&values),
            ptr::null(),
        )
    };
    values
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let mode = args.get(3).map(String::as_str).unwrap_or("");
    if mode == "sharded" {
        unsafe { sharded(&args[1]) };
        return;
    }
    unsafe {
        let executable = compile(&args[1], OPTIONS);
        let lhs = upload(0, &[1.0, 2.0, 3.0, 4.0]);
        let rhs = upload(0, &[4.0, 3.0, 2.0, 1.0]);
        let mut outputs = [MaybeUninit::<*mut c_void>::uninit(); 2];
        let inputs = [ptr::from_ref(&lhs).cast(), ptr::from_ref(&rhs).cast()];
        let destinations = outputs.each_mut().map(|slot| ptr::from_mut(slot).cast());
        match mode {
            "wrong-output-count" | "wrong-input-count" => {
                __reussir_pjrt_executable_execute(
                    executable,
                    inputs.as_ptr(),
                    if mode == "wrong-input-count" { 1 } else { 2 },
                    destinations.as_ptr(),
                    if mode == "wrong-output-count" { 1 } else { 2 },
                );
                panic!("invalid invocation accepted");
            }
            "" => {}
            _ => panic!("unknown test mode"),
        }
        // Reusing the same buffers across calls must not consume their contents.
        for _ in 0..2 {
            __reussir_pjrt_executable_execute(
                executable,
                inputs.as_ptr(),
                2,
                destinations.as_ptr(),
                2,
            );
            let [sum, difference] = outputs.map(|output| output.assume_init());
            assert_eq!(download(sum), [5.0; 4]);
            assert_eq!(download(difference), [-3.0, -1.0, 1.0, 3.0]);
            __reussir_pjrt_array_deallocate(sum);
            __reussir_pjrt_array_deallocate(difference);
        }
        let repeated = [ptr::from_ref(&lhs).cast(), ptr::from_ref(&lhs).cast()];
        __reussir_pjrt_executable_execute(
            executable,
            repeated.as_ptr(),
            2,
            destinations.as_ptr(),
            2,
        );
        // Outputs remain owned after the caller releases its executable handle.
        __reussir_pjrt_executable_release(executable);
        let [sum, difference] = outputs.map(|output| output.assume_init());
        assert_eq!(download(sum), [2.0, 4.0, 6.0, 8.0]);
        assert_eq!(download(difference), [0.0; 4]);
        assert_eq!(download(lhs), [1.0, 2.0, 3.0, 4.0]);
        assert_eq!(download(rhs), [4.0, 3.0, 2.0, 1.0]);
        for buffer in [sum, difference, lhs, rhs] {
            __reussir_pjrt_array_deallocate(buffer);
        }
        let empty = compile(&args[2], OPTIONS);
        __reussir_pjrt_executable_execute(empty, ptr::null(), 0, ptr::null(), 0);
        __reussir_pjrt_executable_release(empty);
    }
}

// Matches the payload of a static MLIR target-array RC descriptor with d=2.
// FFI receives one pointer to this payload per logical argument/result.
#[repr(C)]
struct ShardedArray {
    shards: [*mut c_void; 2],
    offset: usize,
}

unsafe fn sharded(path: &str) {
    unsafe {
        let executable = compile(path, include_bytes!("pjrt_execute_sharded.pb"));
        // The compile fixture deliberately assigns partitions to devices [1, 0].
        let lhs = ShardedArray {
            shards: [
                upload(1, &[1.0, 2.0, 3.0, 4.0]),
                upload(0, &[5.0, 6.0, 7.0, 8.0]),
            ],
            offset: 0,
        };
        let rhs = ShardedArray {
            shards: [
                upload(1, &[8.0, 7.0, 6.0, 5.0]),
                upload(0, &[4.0, 3.0, 2.0, 1.0]),
            ],
            offset: 0,
        };
        let inputs = [ptr::from_ref(&lhs).cast(), ptr::from_ref(&rhs).cast()];
        for _ in 0..2 {
            let mut results = [0; 2].map(|_| ShardedArray {
                shards: [ptr::null_mut(); 2],
                offset: 0,
            });
            let outputs = results.each_mut().map(|array| ptr::from_mut(array).cast());
            __reussir_pjrt_executable_execute(executable, inputs.as_ptr(), 2, outputs.as_ptr(), 2);
            let [sum, reversed_difference] = &results;
            assert_eq!(download(sum.shards[0]), [9.0; 4]);
            assert_eq!(download(sum.shards[1]), [9.0; 4]);
            assert_eq!(
                download(reversed_difference.shards[0]),
                [7.0, 5.0, 3.0, 1.0]
            );
            assert_eq!(
                download(reversed_difference.shards[1]),
                [-1.0, -3.0, -5.0, -7.0]
            );
            for array in results {
                assert_eq!(array.offset, 0);
                for shard in array.shards {
                    __reussir_pjrt_array_deallocate(shard);
                }
            }
        }
        __reussir_pjrt_executable_release(executable);
        assert_eq!(download(lhs.shards[0]), [1.0, 2.0, 3.0, 4.0]);
        assert_eq!(download(lhs.shards[1]), [5.0, 6.0, 7.0, 8.0]);
        for shard in lhs.shards.into_iter().chain(rhs.shards) {
            __reussir_pjrt_array_deallocate(shard);
        }
    }
}
