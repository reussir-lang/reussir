//! Runtime specialization using the pinned upstream StableHLO passes.
//! Shapes are logical global dimensions, never physical shard dimensions.
//! Shape values are an explicit host-known contract: ordinary scalar contents
//! are not inspected or specialized. Each call owns an independent MLIR context.
use reussir_stablehlo_sys as sys;
use std::{ffi::c_void, slice};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Argument {
    pub dimensions: Vec<i64>,
    pub shape_values: Option<Vec<i64>>,
}
impl Argument {
    pub fn shape(dimensions: impl Into<Vec<i64>>) -> Self {
        Self {
            dimensions: dimensions.into(),
            shape_values: None,
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Specialized {
    pub bytecode: Vec<u8>,
    pub output_shapes: Vec<Vec<i64>>,
    pub output_bytes: Vec<u64>,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidInput,
    RefinementFailed,
    ResidualDynamism,
    SerializationFailed,
}
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("StableHLO {kind:?}: {diagnostic}")]
pub struct Error {
    pub kind: ErrorKind,
    pub diagnostic: String,
}
#[derive(Default)]
struct Output {
    bytes: Vec<u8>,
    shapes: Vec<Vec<i64>>,
    sizes: Vec<u64>,
    diagnostic: Vec<u8>,
}
unsafe extern "C" fn bytes(data: *const u8, len: usize, state: *mut c_void) {
    if len != 0 {
        unsafe { &mut *state.cast::<Output>() }
            .bytes
            .extend_from_slice(unsafe { slice::from_raw_parts(data, len) });
    }
}
unsafe extern "C" fn shape(data: *const i64, len: usize, bytes: u64, state: *mut c_void) {
    let dims = if len == 0 {
        Vec::new()
    } else {
        unsafe { slice::from_raw_parts(data, len) }.to_vec()
    };
    unsafe { &mut *state.cast::<Output>() }.shapes.push(dims);
    unsafe { &mut *state.cast::<Output>() }.sizes.push(bytes);
}
unsafe extern "C" fn diagnostic(data: *const u8, len: usize, state: *mut c_void) {
    if len != 0 {
        unsafe { &mut *state.cast::<Output>() }
            .diagnostic
            .extend_from_slice(unsafe { slice::from_raw_parts(data, len) });
    }
}
pub fn specialize(program: &[u8], arguments: &[Argument]) -> Result<Specialized, Error> {
    let args: Vec<_> = arguments
        .iter()
        .map(|a| {
            let values = a.shape_values.as_deref().unwrap_or(&[]);
            sys::Argument {
                dimensions: a.dimensions.as_ptr(),
                rank: a.dimensions.len(),
                values: values.as_ptr(),
                value_count: values.len(),
                has_values: u8::from(a.shape_values.is_some()),
            }
        })
        .collect();
    let mut output = Output::default();
    // SAFETY: slices and callbacks remain live throughout this synchronous call;
    // the native implementation passes only borrowed buffers to the callbacks.
    let status = unsafe {
        sys::reussir_stablehlo_specialize(
            program.as_ptr(),
            program.len(),
            args.as_ptr(),
            args.len(),
            bytes,
            shape,
            diagnostic,
            (&mut output as *mut Output).cast(),
        )
    };
    if status != 0 {
        return Err(Error {
            kind: match status {
                1 => ErrorKind::InvalidInput,
                2 => ErrorKind::RefinementFailed,
                3 => ErrorKind::ResidualDynamism,
                _ => ErrorKind::SerializationFailed,
            },
            diagnostic: String::from_utf8_lossy(&output.diagnostic).into_owned(),
        });
    }
    Ok(Specialized {
        bytecode: output.bytes,
        output_shapes: output.shapes,
        output_bytes: output.sizes,
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    const ADD: &[u8] = br#"module {
      func.func @main(%x: tensor<?xf32>) -> tensor<?xf32> {
        %y = stablehlo.add %x, %x : tensor<?xf32>
        return %y : tensor<?xf32>
      }
    }"#;
    #[test]
    fn refines_and_roundtrips_bytecode() {
        for extent in [0, 3, 17] {
            let args = [Argument::shape(vec![extent])];
            let result = specialize(ADD, &args).unwrap();
            assert_eq!(result.output_shapes, [vec![extent]]);
            let again = specialize(&result.bytecode, &args).unwrap();
            assert_eq!(again.output_shapes, result.output_shapes);
        }
    }
    #[test]
    fn rejects_invalid_descriptors_and_modules() {
        assert_eq!(
            specialize(ADD, &[]).unwrap_err().kind,
            ErrorKind::InvalidInput
        );
        assert_eq!(
            specialize(ADD, &[Argument::shape(vec![-1])])
                .unwrap_err()
                .kind,
            ErrorKind::InvalidInput
        );
        assert_eq!(
            specialize(b"garbage", &[]).unwrap_err().kind,
            ErrorKind::InvalidInput
        );
        let static_code = specialize(ADD, &[Argument::shape(vec![3])]).unwrap();
        assert_eq!(
            specialize(&static_code.bytecode, &[Argument::shape(vec![4])])
                .unwrap_err()
                .kind,
            ErrorKind::InvalidInput
        );
    }
    #[test]
    fn only_explicit_values_resolve_dynamic_iota() {
        let code = br#"module {
          func.func @main(%shape: tensor<1xi64>) -> tensor<?xi32> {
            %x = stablehlo.dynamic_iota %shape, dim = 0 : (tensor<1xi64>) -> tensor<?xi32>
            return %x : tensor<?xi32>
          }
        }"#;
        assert_eq!(
            specialize(code, &[Argument::shape(vec![1])])
                .unwrap_err()
                .kind,
            ErrorKind::ResidualDynamism
        );
        let result = specialize(
            code,
            &[Argument {
                dimensions: vec![1],
                shape_values: Some(vec![7]),
            }],
        )
        .unwrap();
        assert_eq!(result.output_shapes, [vec![7]]);
    }
    #[test]
    fn contexts_are_independent_across_threads() {
        let threads: Vec<_> = (1..5)
            .map(|n| {
                std::thread::spawn(move || {
                    assert_eq!(
                        specialize(ADD, &[Argument::shape(vec![n])])
                            .unwrap()
                            .output_shapes,
                        [vec![n]]
                    );
                })
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
    }
}

/// One selected donation program, in original argument numbering. Mapping is a
/// compiler proof of identical operands, checked again against physical buffers
/// by the runtime before this transformation. No variants are enumerated here.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Selection {
    pub mapping: Vec<u32>,
    pub donors: Vec<u32>,
    pub aliases: Vec<[u32; 2]>,
}

pub fn select_program(program: &[u8], selection: &Selection) -> Result<Vec<u8>, Error> {
    let mut output = Output::default();
    // SAFETY: borrowed slices live throughout the synchronous call; pairs are
    // contiguous u32 values and callbacks copy all returned bytes.
    let status = unsafe {
        sys::reussir_stablehlo_select(
            program.as_ptr(),
            program.len(),
            selection.mapping.as_ptr(),
            selection.mapping.len(),
            selection.donors.as_ptr(),
            selection.donors.len(),
            selection.aliases.as_ptr().cast(),
            selection.aliases.len(),
            bytes,
            diagnostic,
            (&mut output as *mut Output).cast(),
        )
    };
    if status != 0 {
        return Err(Error {
            kind: ErrorKind::InvalidInput,
            diagnostic: String::from_utf8_lossy(&output.diagnostic).into_owned(),
        });
    }
    Ok(output.bytes)
}

#[cfg(test)]
mod selection_tests {
    use super::*;
    const ADD: &[u8] =
        br#"module { func.func @main(%a: tensor<4xf32>, %b: tensor<4xf32>) -> tensor<4xf32> {
        %v = stablehlo.add %a, %b : tensor<4xf32>
        return %v : tensor<4xf32>
    }}"#;
    #[test]
    fn selected_program_deduplicates_and_retains_result_shape() {
        let original =
            specialize(ADD, &[Argument::shape(vec![4]), Argument::shape(vec![4])]).unwrap();
        assert_eq!(original.output_bytes, [16]);
        let selected = select_program(
            &original.bytecode,
            &Selection {
                mapping: vec![0, 0],
                donors: vec![0],
                aliases: vec![],
            },
        )
        .unwrap();
        assert_eq!(
            specialize(&selected, &[Argument::shape(vec![4])])
                .unwrap()
                .output_shapes,
            [vec![4]]
        );
        assert!(
            specialize(
                &selected,
                &[Argument::shape(vec![4]), Argument::shape(vec![4])]
            )
            .is_err()
        );
    }
    #[test]
    fn explicit_aliases_and_invalid_mappings() {
        let selected = select_program(
            ADD,
            &Selection {
                mapping: vec![0, 1],
                donors: vec![1],
                aliases: vec![[1, 0]],
            },
        )
        .unwrap();
        assert!(
            specialize(
                &selected,
                &[Argument::shape(vec![4]), Argument::shape(vec![4])]
            )
            .is_ok()
        );
        for selection in [
            Selection {
                mapping: vec![1, 1],
                donors: vec![],
                aliases: vec![],
            },
            Selection {
                mapping: vec![0, 0],
                donors: vec![1],
                aliases: vec![],
            },
            Selection {
                mapping: vec![0, 1],
                donors: vec![0],
                aliases: vec![[0, 1]],
            },
        ] {
            assert!(select_program(ADD, &selection).is_err());
        }
    }
}

#[cfg(test)]
mod bounds_tests {
    use super::*;

    #[test]
    fn unused_arguments_still_enforce_dimension_bounds() {
        let program = br#"module {
          func.func @main(%x: tensor<?xf32, #stablehlo.type_extensions<bounds = [4]>>) -> tensor<i32> {
            %c = stablehlo.constant dense<1> : tensor<i32>
            return %c : tensor<i32>
          }
        }"#;
        for extent in [0, 4] {
            assert_eq!(
                specialize(program, &[Argument::shape(vec![extent])])
                    .unwrap()
                    .output_shapes,
                [Vec::<i64>::new()]
            );
        }
        let error = specialize(program, &[Argument::shape(vec![5])]).unwrap_err();
        assert_eq!(error.kind, ErrorKind::InvalidInput);
        assert!(error.diagnostic.contains("exceeds its declared bound"));
    }
}
