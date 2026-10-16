//! Shape refinement: make a dynamically shaped program static for one launch.

use crate::{Error, Output, collect_bytes, collect_diagnostic, ffi};
use std::ffi::c_void;

/// Describes one argument of `@main` at a launch.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Argument {
    /// Logical (global) dimensions, never per-shard dimensions.
    pub dimensions: Vec<i64>,
    /// Exact host-known contents of a static i32/i64 scalar or vector that the
    /// program uses as a shape. Device contents are never read to obtain them,
    /// and the caller must guarantee they match the launch's actual argument.
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

/// A static result type of the refined program.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResultType {
    pub dimensions: Vec<i64>,
    /// Element width in bits, or 0 if the element type has no fixed width.
    pub element_bits: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Refined {
    /// Verified MLIR bytecode with static shapes throughout.
    pub bytecode: Vec<u8>,
    pub result_types: Vec<ResultType>,
}

/// # Safety
/// `data` is valid for `rank` values and `output` is an [`Output::user_data`].
unsafe extern "C" fn collect_result_type(
    data: *const i64,
    rank: usize,
    element_bits: u32,
    output: *mut c_void,
) {
    let dimensions = if rank == 0 {
        Vec::new()
    } else {
        unsafe { std::slice::from_raw_parts(data, rank) }.to_vec()
    };
    unsafe { &mut *output.cast::<Output>() }
        .result_types
        .push(ResultType {
            dimensions,
            element_bits,
        });
}

/// Refine the dynamic shapes of `@main` in `program` (MLIR text or bytecode)
/// from one descriptor per argument, using the upstream StableHLO pipeline.
/// Dynamism that the descriptors cannot resolve is an error.
pub fn refine_shapes(program: &[u8], arguments: &[Argument]) -> Result<Refined, Error> {
    let descriptors: Vec<_> = arguments
        .iter()
        .map(|argument| {
            let values = argument.shape_values.as_deref().unwrap_or(&[]);
            ffi::Argument {
                dimensions: argument.dimensions.as_ptr(),
                rank: argument.dimensions.len(),
                values: values.as_ptr(),
                value_count: values.len(),
                has_values: u8::from(argument.shape_values.is_some()),
            }
        })
        .collect();
    let mut output = Output::default();
    // SAFETY: the slices outlive this synchronous call, and every callback
    // copies what it receives into `output`.
    let status = unsafe {
        ffi::reussir_stablehlo_refine_shapes(
            program.as_ptr(),
            program.len(),
            descriptors.as_ptr(),
            descriptors.len(),
            collect_bytes,
            collect_result_type,
            collect_diagnostic,
            output.user_data(),
        )
    };
    let output = output.finish(status)?;
    Ok(Refined {
        bytecode: output.bytes,
        result_types: output.result_types,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ErrorKind;

    const ADD: &[u8] = br#"module {
      func.func @main(%x: tensor<?xf32>) -> tensor<?xf32> {
        %y = stablehlo.add %x, %x : tensor<?xf32>
        return %y : tensor<?xf32>
      }
    }"#;

    fn dims(refined: &Refined) -> Vec<Vec<i64>> {
        refined
            .result_types
            .iter()
            .map(|ty| ty.dimensions.clone())
            .collect()
    }

    fn kind(program: &[u8], arguments: &[Argument]) -> ErrorKind {
        refine_shapes(program, arguments).unwrap_err().kind
    }

    #[test]
    fn refines_and_roundtrips_bytecode() {
        for extent in [0, 3, 17] {
            let arguments = [Argument::shape(vec![extent])];
            let refined = refine_shapes(ADD, &arguments).unwrap();
            assert_eq!(
                refined.result_types,
                [ResultType {
                    dimensions: vec![extent],
                    element_bits: 32,
                }]
            );
            let again = refine_shapes(&refined.bytecode, &arguments).unwrap();
            assert_eq!(again.result_types, refined.result_types);
        }
    }

    #[test]
    fn rejects_invalid_descriptors_and_programs() {
        assert_eq!(kind(ADD, &[]), ErrorKind::InvalidInput);
        assert_eq!(
            kind(ADD, &[Argument::shape(vec![-1])]),
            ErrorKind::InvalidInput
        );
        assert_eq!(kind(b"garbage", &[]), ErrorKind::InvalidInput);
        let refined = refine_shapes(ADD, &[Argument::shape(vec![3])]).unwrap();
        assert_eq!(
            kind(&refined.bytecode, &[Argument::shape(vec![4])]),
            ErrorKind::InvalidInput
        );
    }

    #[test]
    fn only_explicit_shape_values_resolve_dynamic_iota() {
        let program = br#"module {
          func.func @main(%shape: tensor<1xi64>) -> tensor<?xi32> {
            %x = stablehlo.dynamic_iota %shape, dim = 0 : (tensor<1xi64>) -> tensor<?xi32>
            return %x : tensor<?xi32>
          }
        }"#;
        assert_eq!(
            kind(program, &[Argument::shape(vec![1])]),
            ErrorKind::ResidualDynamism
        );
        let refined = refine_shapes(
            program,
            &[Argument {
                dimensions: vec![1],
                shape_values: Some(vec![7]),
            }],
        )
        .unwrap();
        assert_eq!(dims(&refined), [vec![7]]);
    }

    #[test]
    fn unused_arguments_still_enforce_bounds() {
        let program = br#"module {
          func.func @main(%x: tensor<?xf32, #stablehlo.type_extensions<bounds = [4]>>) -> tensor<i32> {
            %c = stablehlo.constant dense<1> : tensor<i32>
            return %c : tensor<i32>
          }
        }"#;
        for extent in [0, 4] {
            let refined = refine_shapes(program, &[Argument::shape(vec![extent])]).unwrap();
            assert_eq!(dims(&refined), [Vec::<i64>::new()]);
        }
        let error = refine_shapes(program, &[Argument::shape(vec![5])]).unwrap_err();
        assert_eq!(error.kind, ErrorKind::InvalidInput);
        assert!(error.diagnostic.contains("exceeds its declared bound"));
    }

    #[test]
    fn contexts_are_independent_across_threads() {
        let threads: Vec<_> = (1..5)
            .map(|n| {
                std::thread::spawn(move || {
                    let refined = refine_shapes(ADD, &[Argument::shape(vec![n])]).unwrap();
                    assert_eq!(dims(&refined), [vec![n]]);
                })
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
    }
}
