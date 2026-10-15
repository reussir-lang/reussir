use std::{ffi::c_void, ptr};

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


fn main() {
    let bytes=std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let digest=blake3::hash(&bytes).to_hex();
    unsafe {
        let input=__reussir_pjrt_array_from_host(0,11,[8i64].as_ptr(),1,[0f32,1.,2.,3.,4.,5.,6.,7.].as_ptr().cast(),ptr::null(),ptr::null());
        // The input is explicitly non-donatable and is reused across executions.
        for _ in 0..2 {
            let executable=__reussir_pjrt_compile(bytes.as_ptr(),bytes.len(),digest.as_ptr(),OPTIONS.as_ptr(),OPTIONS.len());
            let inputs=[ptr::from_ref(&input).cast()];
            let mut outputs=[ptr::null_mut::<c_void>();2];
            let destinations=outputs.each_mut().map(|s|ptr::from_mut(s).cast());
            __reussir_pjrt_executable_execute(executable,inputs.as_ptr(),1,destinations.as_ptr(),2,[0i64].as_ptr(),1);
            for (output,expected) in outputs.into_iter().zip([[0f32,1.,2.,3.,4.,5.,6.,7.],[0.;8]]) {
                let mut actual=[0f32;8];
                __reussir_pjrt_array_to_host(output,actual.as_mut_ptr().cast(),size_of_val(&actual),ptr::null());
                __reussir_pjrt_array_deallocate(output);
                assert_eq!(actual,expected);
            }
        }
        __reussir_pjrt_array_deallocate(input);
    }
}
