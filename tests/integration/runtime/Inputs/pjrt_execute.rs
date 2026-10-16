use std::{ffi::c_void, ptr};

#[path = "pjrt_launch.rs"]
mod pjrt_launch;
use pjrt_launch::{Input, OPTIONS, launch};

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

// A single-device input of the four-element test vectors.
fn input(buffer: &*mut c_void) -> Input<'_> {
    Input {
        shards: std::slice::from_ref(buffer),
        dimensions: &[4],
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let program = std::fs::read(&args[1]).unwrap();
    let mode = args.get(3).map(String::as_str).unwrap_or("");
    if mode == "sharded" {
        unsafe { sharded(&program) };
        return;
    }
    unsafe {
        let lhs = upload(0, &[1.0, 2.0, 3.0, 4.0]);
        let rhs = upload(0, &[4.0, 3.0, 2.0, 1.0]);
        match mode {
            "wrong-output-count" => {
                launch(&program, OPTIONS, &[0], &[input(&lhs), input(&rhs)], 1);
                panic!("invalid launch accepted");
            }
            "wrong-input-count" => {
                launch(&program, OPTIONS, &[0], &[input(&lhs)], 2);
                panic!("invalid launch accepted");
            }
            "" => {}
            _ => panic!("unknown test mode"),
        }
        // Reusing the same buffers across launches must not consume them.
        for _ in 0..2 {
            let [sum, difference] =
                launch(&program, OPTIONS, &[0], &[input(&lhs), input(&rhs)], 2)
                    .try_into()
                    .unwrap();
            assert_eq!(download(sum[0]), [5.0; 4]);
            assert_eq!(download(difference[0]), [-3.0, -1.0, 1.0, 3.0]);
            __reussir_pjrt_array_deallocate(sum[0]);
            __reussir_pjrt_array_deallocate(difference[0]);
        }
        let [sum, difference] = launch(&program, OPTIONS, &[0], &[input(&lhs), input(&lhs)], 2)
            .try_into()
            .unwrap();
        assert_eq!(download(sum[0]), [2.0, 4.0, 6.0, 8.0]);
        assert_eq!(download(difference[0]), [0.0; 4]);
        assert_eq!(download(lhs), [1.0, 2.0, 3.0, 4.0]);
        assert_eq!(download(rhs), [4.0, 3.0, 2.0, 1.0]);
        for buffer in [sum[0], difference[0], lhs, rhs] {
            __reussir_pjrt_array_deallocate(buffer);
        }
        let empty = std::fs::read(&args[2]).unwrap();
        assert!(launch(&empty, OPTIONS, &[0], &[], 0).is_empty());
    }
}

unsafe fn sharded(program: &[u8]) {
    unsafe {
        // The compile options deliberately assign partitions to devices [1, 0].
        let devices = [1, 0];
        let lhs = [
            upload(1, &[1.0, 2.0, 3.0, 4.0]),
            upload(0, &[5.0, 6.0, 7.0, 8.0]),
        ];
        let rhs = [
            upload(1, &[8.0, 7.0, 6.0, 5.0]),
            upload(0, &[4.0, 3.0, 2.0, 1.0]),
        ];
        let inputs = [
            Input {
                shards: &lhs,
                dimensions: &[8],
            },
            Input {
                shards: &rhs,
                dimensions: &[8],
            },
        ];
        for _ in 0..2 {
            let options = include_bytes!("pjrt_execute_sharded.pb");
            let [sum, reversed_difference] = launch(program, options, &devices, &inputs, 2)
                .try_into()
                .unwrap();
            assert_eq!(download(sum[0]), [9.0; 4]);
            assert_eq!(download(sum[1]), [9.0; 4]);
            assert_eq!(download(reversed_difference[0]), [7.0, 5.0, 3.0, 1.0]);
            assert_eq!(download(reversed_difference[1]), [-1.0, -3.0, -5.0, -7.0]);
            for shard in sum.into_iter().chain(reversed_difference) {
                __reussir_pjrt_array_deallocate(shard);
            }
        }
        assert_eq!(download(lhs[0]), [1.0, 2.0, 3.0, 4.0]);
        assert_eq!(download(lhs[1]), [5.0, 6.0, 7.0, 8.0]);
        for shard in lhs.into_iter().chain(rhs) {
            __reussir_pjrt_array_deallocate(shard);
        }
    }
}
