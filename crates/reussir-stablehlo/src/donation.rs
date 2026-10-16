//! Donation rewrite: prepare a program for one choice of donated arguments.

use crate::{Error, Output, collect_bytes, collect_diagnostic, ffi};

/// One donation choice for `@main`, in original argument numbering.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Donation {
    /// `mapping[i]` is the argument that replaces argument `i`: `i` itself, or
    /// an earlier argument known to be the same buffer. Replaced arguments are
    /// erased from the signature.
    pub mapping: Vec<u32>,
    /// Retained arguments whose buffers the executable may reuse.
    pub donors: Vec<u32>,
    /// `[input, result]` pairs that must share a buffer; each input is a donor.
    pub aliases: Vec<[u32; 2]>,
}

/// Rewrite `program` (MLIR text or bytecode) for `donation`, returning
/// verified bytecode. This applies the choice; it does not decide whether
/// donating is permitted or profitable.
pub fn apply_donation(program: &[u8], donation: &Donation) -> Result<Vec<u8>, Error> {
    let mut output = Output::default();
    // SAFETY: the slices outlive this synchronous call, `[u32; 2]` pairs are
    // contiguous u32 values, and every callback copies what it receives.
    let status = unsafe {
        ffi::reussir_stablehlo_apply_donation(
            program.as_ptr(),
            program.len(),
            donation.mapping.as_ptr(),
            donation.mapping.len(),
            donation.donors.as_ptr(),
            donation.donors.len(),
            donation.aliases.as_ptr().cast(),
            donation.aliases.len(),
            collect_bytes,
            collect_diagnostic,
            output.user_data(),
        )
    };
    Ok(output.finish(status)?.bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Argument, refine_shapes};

    const ADD: &[u8] = br#"module {
      func.func @main(%a: tensor<4xf32>, %b: tensor<4xf32>) -> tensor<4xf32> {
        %v = stablehlo.add %a, %b : tensor<4xf32>
        return %v : tensor<4xf32>
      }
    }"#;

    fn donation(mapping: &[u32], donors: &[u32], aliases: &[[u32; 2]]) -> Donation {
        Donation {
            mapping: mapping.to_vec(),
            donors: donors.to_vec(),
            aliases: aliases.to_vec(),
        }
    }

    #[test]
    fn mapped_arguments_are_erased() {
        let rewritten = apply_donation(ADD, &donation(&[0, 0], &[0], &[])).unwrap();
        let one = [Argument::Shape(vec![4])];
        assert_eq!(
            refine_shapes(&rewritten, &one).unwrap().result_types[0].dimensions,
            [4]
        );
        assert!(refine_shapes(&rewritten, &[one[0].clone(), one[0].clone()]).is_err());
    }

    #[test]
    fn aliases_require_donors_and_earlier_representatives() {
        let rewritten = apply_donation(ADD, &donation(&[0, 1], &[1], &[[1, 0]])).unwrap();
        let two = [Argument::Shape(vec![4]), Argument::Shape(vec![4])];
        assert!(refine_shapes(&rewritten, &two).is_ok());
        for invalid in [
            donation(&[1, 1], &[], &[]),
            donation(&[0, 0], &[1], &[]),
            donation(&[0, 1], &[0], &[[0, 1]]),
            donation(&[0, 1], &[], &[[0, 0]]),
        ] {
            assert!(apply_donation(ADD, &invalid).is_err(), "{invalid:?}");
        }
    }
}
