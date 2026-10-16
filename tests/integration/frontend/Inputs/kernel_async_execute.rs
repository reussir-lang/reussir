use std::{ffi::c_void, ptr};

#[path = "../../runtime/Inputs/pjrt_launch.rs"]
mod pjrt_launch;

unsafe extern "C" {
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

fn main() {
    let bytes=std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    unsafe {
        let input=__reussir_pjrt_array_from_host(0,11,[8i64].as_ptr(),1,[0f32,1.,2.,3.,4.,5.,6.,7.].as_ptr().cast(),ptr::null(),ptr::null());
        // The input is explicitly non-donatable and is reused across executions.
        for _ in 0..2 {
            let inputs=[pjrt_launch::Input{shards:&[input],dimensions:&[8]}];
            let outputs=pjrt_launch::launch(&bytes,pjrt_launch::OPTIONS,&[0],&inputs,2);
            for (shards,expected) in outputs.into_iter().zip([[0f32,1.,2.,3.,4.,5.,6.,7.],[0.;8]]) {
                let output=shards[0];
                let mut actual=[0f32;8];
                __reussir_pjrt_array_to_host(output,actual.as_mut_ptr().cast(),size_of_val(&actual),ptr::null());
                __reussir_pjrt_array_deallocate(output);
                assert_eq!(actual,expected);
            }
        }
        __reussir_pjrt_array_deallocate(input);
    }
}
