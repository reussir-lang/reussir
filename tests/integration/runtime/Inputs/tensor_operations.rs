use std::{ffi::c_void, ptr};

#[path = "pjrt_launch.rs"]
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
    let bytes = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let expected: &[&[f32]] = &[
        &[6., 12., 15., 30.],
        &[14., 32.],
        &[6., 12.],
        &[14.],
        &[1., 4., 2., 5., 3., 6.],
        &[0., 1., 0., 1., 0., 1.],
        &[0., 0., 0., 4., 5., 6.],
        &[3., 3., 3., 3., 3., 3.],
        &[0., 1., 2.],
    ];
    unsafe {
        let input = __reussir_pjrt_array_from_host(
            0,
            11,
            [2i64, 3].as_ptr(),
            2,
            [1f32, 2., 3., 4., 5., 6.].as_ptr().cast(),
            ptr::null(),
            ptr::null(),
        );
        for _ in 0..2 {
            let inputs = [pjrt_launch::Input {
                shards: &[input],
                dimensions: &[2, 3],
            }];
            let outputs =
                pjrt_launch::launch(&bytes, pjrt_launch::OPTIONS, &[0], &inputs, expected.len());
            for (shards, expected) in outputs.into_iter().zip(expected) {
                let output = shards[0];
                let mut actual = vec![0f32; expected.len()];
                __reussir_pjrt_array_to_host(
                    output,
                    actual.as_mut_ptr().cast(),
                    size_of_val(actual.as_slice()),
                    ptr::null(),
                );
                __reussir_pjrt_array_deallocate(output);
                assert_eq!(actual, *expected);
            }
        }
        __reussir_pjrt_array_deallocate(input);
    }
}
