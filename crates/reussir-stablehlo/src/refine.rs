//! Shape refinement: make a dynamically shaped program static for one launch.

use crate::{Error, Output, collect_bytes, collect_diagnostic, ffi};
use std::ffi::c_void;

/// One argument of `@main` at a launch.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Argument {
    /// A runtime argument with these logical (global) dimensions, never
    /// per-shard ones.
    Shape(Vec<i64>),
    /// A rank-zero integer argument fixed to this value. The program is
    /// specialized on it and the argument is removed from `@main`.
    Constant(i64),
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

/// Specialize `@main` in `program` (MLIR text or bytecode) on its constant
/// arguments and refine its dynamic shapes from the shapes of the rest, using
/// the upstream StableHLO pipeline. The refined `@main` keeps only the runtime
/// arguments. Dynamism that neither constants nor argument shapes determine,
/// such as a result sized by a runtime argument's value, is an error.
pub fn refine_shapes(program: &[u8], arguments: &[Argument]) -> Result<Refined, Error> {
    let descriptors: Vec<_> = arguments
        .iter()
        .map(|argument| match argument {
            Argument::Shape(dimensions) => ffi::Argument {
                dimensions: dimensions.as_ptr(),
                rank: dimensions.len(),
                constant: 0,
                is_constant: 0,
            },
            &Argument::Constant(value) => ffi::Argument {
                dimensions: std::ptr::null(),
                rank: 0,
                constant: value,
                is_constant: 1,
            },
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
            let arguments = [Argument::Shape(vec![extent])];
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
            kind(ADD, &[Argument::Shape(vec![-1])]),
            ErrorKind::InvalidInput
        );
        assert_eq!(kind(b"garbage", &[]), ErrorKind::InvalidInput);
        let refined = refine_shapes(ADD, &[Argument::Shape(vec![3])]).unwrap();
        assert_eq!(
            kind(&refined.bytecode, &[Argument::Shape(vec![4])]),
            ErrorKind::InvalidInput
        );
    }

    #[test]
    fn constants_specialize_value_dependent_shapes() {
        let program = br#"module {
          func.func @main(%n: tensor<i32>, %x: tensor<?xf32>) -> tensor<?xi32> {
            %shape = stablehlo.reshape %n : (tensor<i32>) -> tensor<1xi32>
            %v = stablehlo.dynamic_iota %shape, dim = 0 : (tensor<1xi32>) -> tensor<?xi32>
            return %v : tensor<?xi32>
          }
        }"#;
        // Without the constant, the result size depends on a runtime value.
        let runtime = [Argument::Shape(vec![]), Argument::Shape(vec![2])];
        assert_eq!(kind(program, &runtime), ErrorKind::ResidualDynamism);
        let refined =
            refine_shapes(program, &[Argument::Constant(7), Argument::Shape(vec![2])]).unwrap();
        assert_eq!(dims(&refined), [vec![7]]);
        // The constant is no longer an argument of the refined program.
        let again = refine_shapes(&refined.bytecode, &[Argument::Shape(vec![2])]).unwrap();
        assert_eq!(dims(&again), [vec![7]]);
        assert_eq!(
            kind(
                program,
                &[Argument::Constant(1 << 40), Argument::Shape(vec![2])]
            ),
            ErrorKind::InvalidInput
        );
        assert_eq!(
            kind(program, &[Argument::Shape(vec![]), Argument::Constant(2)]),
            ErrorKind::InvalidInput
        );
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
            let refined = refine_shapes(program, &[Argument::Shape(vec![extent])]).unwrap();
            assert_eq!(dims(&refined), [Vec::<i64>::new()]);
        }
        let error = refine_shapes(program, &[Argument::Shape(vec![5])]).unwrap_err();
        assert_eq!(error.kind, ErrorKind::InvalidInput);
        assert!(error.diagnostic.contains("exceeds its declared bound"));
    }

    #[test]
    fn contexts_are_independent_across_threads() {
        let threads: Vec<_> = (1..5)
            .map(|n| {
                std::thread::spawn(move || {
                    let refined = refine_shapes(ADD, &[Argument::Shape(vec![n])]).unwrap();
                    assert_eq!(dims(&refined), [vec![n]]);
                })
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
    }
}
