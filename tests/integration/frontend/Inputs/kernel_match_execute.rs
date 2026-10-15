//! Execute the source-generated kernel and download every flattened result.
use std::{ffi::c_void, ptr};
unsafe extern "C" {
    fn __reussir_pjrt_compile(bytes: *const u8, len: usize, digest: *const u8, options: *const u8, options_len: usize) -> *const c_void;
    fn __reussir_pjrt_executable_execute(executable: *const c_void, inputs: *const *const c_void, num_inputs: usize, outputs: *const *mut c_void, num_outputs: usize, non_donatable: *const i64, num_non_donatable: usize);
    fn __reussir_pjrt_array_to_host(buffer: *mut c_void, data: *mut c_void, bytes: usize, layout: *const c_void);
    fn __reussir_pjrt_array_deallocate(buffer: *mut c_void);
}
const OPTIONS: &[u8] = include_bytes!("../../../../crates/reussir-pjrt-sys/tests/fixtures/compile_options.pb");
fn main() {
    let bytes = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let digest = blake3::hash(&bytes).to_hex();
    let expected = [10i64, 20, 21, 30, 40, 50, 60, 70, 123];
    unsafe {
        let executable = __reussir_pjrt_compile(bytes.as_ptr(), bytes.len(), digest.as_ptr(), OPTIONS.as_ptr(), OPTIONS.len());
        let mut outputs = [ptr::null_mut::<c_void>(); 9];
        let destinations = outputs.each_mut().map(|slot| ptr::from_mut(slot).cast());
        __reussir_pjrt_executable_execute(executable, ptr::null(), 0, destinations.as_ptr(), destinations.len(), ptr::null(), 0);
        for (output, expected) in outputs.into_iter().zip(expected) {
            let mut actual = 0i64;
            __reussir_pjrt_array_to_host(output, ptr::from_mut(&mut actual).cast(), size_of_val(&actual), ptr::null());
            __reussir_pjrt_array_deallocate(output);
            assert_eq!(actual, expected);
        }
    }
}
