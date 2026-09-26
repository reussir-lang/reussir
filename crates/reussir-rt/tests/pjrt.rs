#![cfg(feature = "pjrt")]

use reussir_pjrt_sys::*;
use reussir_rt::pjrt::ffi::*;

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin with two devices"]
fn compiler_abi_uses_native_buffer_handles() {
    // SAFETY: host arrays have the advertised shape/type and stay live until
    // transfer completion. Each opaque array handle is released once.
    unsafe {
        let dims = [2i64, 4];
        let mut input = [0.5f32, -1.0, 2.0, 3.25, 4.0, 5.5, 6.0, 7.0];
        let expected = input;
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_F32,
            dims.as_ptr(),
            dims.len(),
            input.as_ptr().cast(),
        );
        // The ABI handle is directly usable by PjRt itself, with no wrapper
        // allocation to unwrap. Query through the plugin's native API.
        let library =
            libloading::Library::new(std::env::var_os("REUSSIR_PJRT_PLUGIN").unwrap()).unwrap();
        let get_api = library
            .get::<unsafe extern "C" fn() -> *const PJRT_Api>(b"GetPjrtApi\0")
            .unwrap();
        let api = get_api();
        let mut shape = PJRT_Buffer_Dimensions_Args {
            struct_size: PJRT_Buffer_Dimensions_Args_STRUCT_SIZE as usize,
            buffer: array.as_ptr(),
            ..Default::default()
        };
        assert!(((*api).PJRT_Buffer_Dimensions.unwrap())(&mut shape).is_null());
        assert_eq!(std::slice::from_raw_parts(shape.dims, shape.num_dims), dims);
        input.fill(99.0);
        __reussir_pjrt_array_wait_ready(array);
        assert_eq!(__reussir_pjrt_array_host_size(array), size_of_val(&input));
        let copied = __reussir_pjrt_array_copy_to_device(array, 1);
        __reussir_pjrt_array_deallocate(array);
        __reussir_pjrt_array_wait_ready(copied);
        let mut output = [0.0f32; 8];
        __reussir_pjrt_array_to_host(copied, output.as_mut_ptr().cast(), size_of_val(&output));
        assert_eq!(output, expected);
        __reussir_pjrt_array_deallocate(copied);

        let scalar = 17i32;
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_S32,
            std::ptr::null(),
            0,
            (&scalar as *const i32).cast(),
        );
        let mut result = 0i32;
        __reussir_pjrt_array_to_host(
            array,
            (&mut result as *mut i32).cast(),
            size_of_val(&result),
        );
        assert_eq!(result, scalar);
        __reussir_pjrt_array_deallocate(array);
    }
    // The JIT sees these same addresses, without any context-management ABI.
    let symbols = reussir_rt::symbols::exported_symbols();
    assert!(
        symbols
            .iter()
            .any(|s| s.name == "__reussir_pjrt_array_allocate"
                && s.address == __reussir_pjrt_array_allocate as *const std::ffi::c_void)
    );
    assert!(
        !symbols
            .iter()
            .any(|s| s.name.starts_with("__reussir_pjrt_context_"))
    );
}
