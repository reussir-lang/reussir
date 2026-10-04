use std::ffi::c_void;

unsafe extern "C" {
    fn __reussir_pjrt_compile(
        bytecode: *const u8,
        bytecode_size: usize,
        checksum: *const u8,
        options: *const u8,
        options_size: usize,
    ) -> *mut c_void;
    fn __reussir_pjrt_executable_execute(
        executable: *mut c_void,
        inputs: *const *const c_void,
        num_inputs: usize,
        outputs: *const *mut c_void,
        num_outputs: usize,
    );
    fn __reussir_pjrt_array_deallocate(buffer: *mut c_void);
}

const OPTIONS: &[u8] =
    include_bytes!("../../../../crates/reussir-pjrt-sys/tests/fixtures/compile_options.pb");

// This fixture returns one scalar array. Execution consumes each reference
// acquired by compile, including cache hits from concurrent callers.
unsafe fn consume(executable: *mut c_void) {
    let mut output: *mut c_void = std::ptr::null_mut();
    let descriptor = std::ptr::from_mut(&mut output).cast();
    unsafe {
        __reussir_pjrt_executable_execute(executable, std::ptr::null(), 0, &descriptor, 1);
        __reussir_pjrt_array_deallocate(output);
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let mut bytecode = std::fs::read(&args[1]).unwrap();
    let checksum = blake3::hash(&bytecode).to_hex();
    let mode = args.get(2).map(String::as_str).unwrap_or("");
    // Inputs remain live for each call. Every result owns a strong reference.
    unsafe {
        if mode == "malformed" {
            __reussir_pjrt_compile(
                bytecode.as_ptr(),
                bytecode.len(),
                [b'g'; 64].as_ptr(),
                OPTIONS.as_ptr(),
                OPTIONS.len(),
            );
            panic!("malformed checksum was accepted");
        }
        let first = __reussir_pjrt_compile(
            bytecode.as_ptr(),
            bytecode.len(),
            checksum.as_ptr(),
            OPTIONS.as_ptr(),
            OPTIONS.len(),
        );
        match mode {
            "corrupt" => {
                // A populated cache must not bypass verification of this input.
                bytecode[0] ^= 1;
                __reussir_pjrt_compile(
                    bytecode.as_ptr(),
                    bytecode.len(),
                    checksum.as_ptr(),
                    OPTIONS.as_ptr(),
                    OPTIONS.len(),
                );
                panic!("corrupt payload was accepted");
            }
            "bad-options" => {
                let options = b"invalid protobuf";
                __reussir_pjrt_compile(
                    bytecode.as_ptr(),
                    bytecode.len(),
                    checksum.as_ptr(),
                    options.as_ptr(),
                    options.len(),
                );
                panic!("invalid options were accepted");
            }
            "" => {}
            _ => panic!("unknown test mode"),
        }
        std::thread::scope(|scope| {
            let workers: Vec<_> = (0..8)
                .map(|_| {
                    scope.spawn(|| {
                        let executable = __reussir_pjrt_compile(
                            bytecode.as_ptr(),
                            bytecode.len(),
                            checksum.as_ptr(),
                            OPTIONS.as_ptr(),
                            OPTIONS.len(),
                        );
                        let address = executable as usize;
                        consume(executable);
                        address
                    })
                })
                .collect();
            for worker in workers {
                assert_eq!(worker.join().unwrap(), first as usize);
            }
        });
        consume(first);
    }
}
