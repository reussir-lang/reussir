#![cfg(feature = "pjrt")]

use reussir_pjrt_sys::*;
use reussir_rt::pjrt::ffi::*;

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn transfers_preserve_logical_values_across_layouts() {
    // Transpose a 3x2 allocation, then reverse its second logical dimension.
    // The logical 2x3 input is [[4, 2, 0], [5, 3, 1]].
    let mut input = [0i32, 1, 2, 3, 4, 5];
    let dims = [2, 3];
    let strides = [4, -8];
    let column_major = [0, 1];
    let row_major = [1, 0];
    let allocation = AllocationOptions {
        memory_kind: b"device".as_ptr(),
        memory_kind_size: 6,
        minor_to_major: row_major.as_ptr(),
        ..Default::default()
    };
    let row_layout = HostLayout {
        rank: 2,
        minor_to_major: row_major.as_ptr(),
        ..Default::default()
    };
    let column_layout = HostLayout {
        rank: 2,
        minor_to_major: column_major.as_ptr(),
        ..Default::default()
    };
    // SAFETY: the interior pointer and signed strides stay within input, and
    // each destination is writable for the queried size until return.
    unsafe {
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_S32,
            dims.as_ptr(),
            2,
            input.as_ptr().add(4).cast(),
            strides.as_ptr(),
            &allocation,
        );
        input.fill(99); // Upload has finished borrowing the source.
        let mut output = [0i32; 6];
        for (layout, expected) in [
            (&row_layout, [4, 2, 0, 5, 3, 1]),
            (&column_layout, [4, 5, 2, 3, 0, 1]),
        ] {
            let bytes = __reussir_pjrt_array_host_size(array, layout);
            assert_eq!(bytes, size_of_val(&output));
            __reussir_pjrt_array_to_host(array, output.as_mut_ptr().cast(), bytes, layout);
            assert_eq!(output, expected);
        }
        // Null explicitly inherits the row-major device layout.
        __reussir_pjrt_array_to_host(
            array,
            output.as_mut_ptr().cast(),
            size_of_val(&output),
            std::ptr::null(),
        );
        assert_eq!(output, [4, 2, 0, 5, 3, 1]);
        __reussir_pjrt_array_deallocate(array);
    }
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn upload_passes_nondefault_device_layout() {
    let dims = [2, 3];
    let order = [0, 1];
    let options = AllocationOptions {
        minor_to_major: order.as_ptr(),
        ..Default::default()
    };
    let input = [1i32; 6];
    unsafe {
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_S32,
            dims.as_ptr(),
            2,
            input.as_ptr().cast(),
            std::ptr::null(),
            &options,
        );
        let library =
            libloading::Library::new(std::env::var_os("REUSSIR_PJRT_PLUGIN").unwrap()).unwrap();
        let get_api = library
            .get::<unsafe extern "C" fn() -> *const PJRT_Api>(b"GetPjrtApi\0")
            .unwrap();
        let mut layout = PJRT_Buffer_GetMemoryLayout_Args {
            struct_size: PJRT_Buffer_GetMemoryLayout_Args_STRUCT_SIZE as usize,
            buffer: array.as_ptr(),
            ..Default::default()
        };
        assert!(((*get_api()).PJRT_Buffer_GetMemoryLayout.unwrap())(&mut layout).is_null());
        assert_eq!(
            layout.layout.type_,
            PJRT_Buffer_MemoryLayout_Type_PJRT_Buffer_MemoryLayout_Type_Tiled
        );
        let tiled = layout.layout.__bindgen_anon_1.tiled;
        assert_eq!(
            std::slice::from_raw_parts(tiled.minor_to_major, tiled.minor_to_major_size),
            order
        );
        __reussir_pjrt_array_deallocate(array);
    }
}

#[test]
#[ignore = "requires REUSSIR_PJRT_PLUGIN pointing to a trusted CPU plugin"]
fn empty_array_transfer_with_explicit_layout() {
    let dims = [0, 3];
    let order = [1, 0];
    let layout = HostLayout {
        rank: 2,
        minor_to_major: order.as_ptr(),
        ..Default::default()
    };
    let mut dummy = 42i32;
    unsafe {
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_S32,
            dims.as_ptr(),
            2,
            (&dummy as *const i32).cast(),
            std::ptr::null(),
            std::ptr::null(),
        );
        assert_eq!(__reussir_pjrt_array_host_size(array, &layout), 0);
        __reussir_pjrt_array_to_host(array, (&mut dummy as *mut i32).cast(), 0, &layout);
        assert_eq!(dummy, 42);
        __reussir_pjrt_array_deallocate(array);
    }
}

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
            std::ptr::null(),
            std::ptr::null(),
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
        assert_eq!(
            __reussir_pjrt_array_host_size(array, std::ptr::null()),
            size_of_val(&input)
        );
        let copied = __reussir_pjrt_array_copy_to_device(array, 1);
        __reussir_pjrt_array_deallocate(array);
        __reussir_pjrt_array_wait_ready(copied);
        let mut output = [0.0f32; 8];
        __reussir_pjrt_array_to_host(
            copied,
            output.as_mut_ptr().cast(),
            size_of_val(&output),
            std::ptr::null(),
        );
        assert_eq!(output, expected);
        __reussir_pjrt_array_deallocate(copied);

        let scalar = 17i32;
        let array = __reussir_pjrt_array_from_host(
            0,
            PJRT_Buffer_Type_PJRT_Buffer_Type_S32,
            std::ptr::null(),
            0,
            (&scalar as *const i32).cast(),
            std::ptr::null(),
            std::ptr::null(),
        );
        let mut result = 0i32;
        let scalar_layout = HostLayout::default();
        assert_eq!(
            __reussir_pjrt_array_host_size(array, &scalar_layout),
            size_of_val(&result)
        );
        __reussir_pjrt_array_to_host(
            array,
            (&mut result as *mut i32).cast(),
            size_of_val(&result),
            &scalar_layout,
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
