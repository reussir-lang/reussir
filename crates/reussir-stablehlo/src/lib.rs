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
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidInput,
    RefinementFailed,
    ResidualDynamism,
    SerializationFailed,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub diagnostic: String,
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StableHLO {:?}: {}", self.kind, self.diagnostic)
    }
}
impl std::error::Error for Error {}
#[derive(Default)]
struct Output {
    bytes: Vec<u8>,
    shapes: Vec<Vec<i64>>,
    diagnostic: Vec<u8>,
}
unsafe extern "C" fn bytes(data: *const u8, len: usize, state: *mut c_void) {
    if len != 0 {
        unsafe { &mut *state.cast::<Output>() }
            .bytes
            .extend_from_slice(unsafe { slice::from_raw_parts(data, len) });
    }
}
unsafe extern "C" fn shape(data: *const i64, len: usize, state: *mut c_void) {
    let dims = if len == 0 {
        Vec::new()
    } else {
        unsafe { slice::from_raw_parts(data, len) }.to_vec()
    };
    unsafe { &mut *state.cast::<Output>() }.shapes.push(dims);
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
