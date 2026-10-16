//! Static permission is an input, not a profitability score. Runtime selection
//! checks complete physical shard identities and actual allocation sizes, then
//! builds exactly one selected program on a cache miss.
use super::{
    Error, Result,
    api::{call, non_null},
    context,
    sys::*,
};
use reussir_stablehlo::{Argument, Selection};
use std::collections::HashSet;

// A conservative compile/amortization floor, in total physical allocation bytes.
// This is a policy choice, not an ownership condition or a promise of XLA reuse.
const MIN_DONATION_BYTES: u64 = 4096;

#[derive(Debug)]
struct BufferFacts {
    handles: Vec<usize>,
    logical_bytes: u64,
    allocated_bytes: u64,
}

fn select(
    facts: &[BufferFacts],
    permissions: &[u8],
    mapping: &[u32],
    explicit: &[u32],
    aliases: &[[u32; 2]],
    output_bytes: &[u64],
    minimum: u64,
) -> Result<Selection> {
    let count = facts.len();
    if permissions.len() != count || mapping.len() != count {
        return Err(Error::local("donation permission/mapping count mismatch"));
    }
    for (i, &rep) in mapping.iter().enumerate() {
        let rep = rep as usize;
        if rep > i || mapping[rep] as usize != rep || facts[i].handles != facts[rep].handles {
            return Err(Error::local(
                "donation mapping does not match physical input identities",
            ));
        }
    }
    let distinct = |i: usize| {
        let handles: HashSet<_> = facts[i].handles.iter().copied().collect();
        // Check across every device, including duplicate handles within a donor.
        handles.len() == facts[i].handles.len()
            && !handles.contains(&0)
            && facts.iter().enumerate().all(|(j, other)| {
                mapping[j] as usize == i || other.handles.iter().all(|p| !handles.contains(p))
            })
    };
    let mut donors = Vec::new();
    if !explicit.is_empty() || !aliases.is_empty() {
        if mapping
            .iter()
            .enumerate()
            .any(|(i, &rep)| rep as usize != i)
        {
            return Err(Error::local(
                "explicit donation contracts cannot remap inputs",
            ));
        }
        for &i in explicit {
            let i = i as usize;
            if i >= count || permissions[i] == 0 || !distinct(i) || donors.contains(&(i as u32)) {
                return Err(Error::local(
                    "explicit donation requires unique ownership and distinct physical buffers",
                ));
            }
            donors.push(i as u32);
        }
        let mut outputs = HashSet::new();
        let mut inputs = HashSet::new();
        for &[input, output] in aliases {
            if !donors.contains(&input)
                || output as usize >= output_bytes.len()
                || !outputs.insert(output)
                || !inputs.insert(input)
            {
                return Err(Error::local("invalid explicit input/output alias"));
            }
        }
    } else {
        let mut candidates: Vec<_> = (0..count)
            .filter(|&i| {
                mapping[i] as usize == i
                    && mapping
                        .iter()
                        .enumerate()
                        .all(|(j, &rep)| rep as usize != i || permissions[j] != 0)
                    && facts[i].logical_bytes != 0
                    && facts[i].allocated_bytes >= minimum
                    && distinct(i)
            })
            .collect();
        candidates.sort_by_key(|&i| (std::cmp::Reverse(facts[i].allocated_bytes), i));
        let mut available = output_bytes.to_vec();
        for i in candidates {
            if let Some(output) = available
                .iter_mut()
                .find(|bytes| **bytes == facts[i].logical_bytes)
            {
                *output = 0;
                donors.push(i as u32);
            }
        }
    }
    donors.sort_unstable();
    // Keep the baseline signature when there is no profitable donation. Do not
    // manufacture a second bytecode form merely because inputs are duplicated.
    let mapping = if donors.is_empty() {
        (0..count as u32).collect()
    } else {
        mapping.to_vec()
    };
    Ok(Selection {
        mapping,
        donors,
        aliases: aliases.to_vec(),
    })
}

fn element_bytes(element: PJRT_Buffer_Type) -> Option<u64> {
    match element {
        1 | 2 | 6 => Some(1),
        3 | 7 | 10 | 13 => Some(2),
        4 | 8 | 11 => Some(4),
        5 | 9 | 12 | 14 => Some(8),
        15 => Some(16),
        _ => None,
    }
}

/// Inputs and shards are live borrowed buffers until synchronous execution ends.
#[allow(clippy::too_many_arguments)]
pub(super) unsafe fn launch(
    code: &[u8],
    checksum: &[u8],
    options: &[u8],
    arguments: Vec<Argument>,
    inputs: &[*const *mut PJRT_Buffer],
    devices: &[i32],
    permissions: &[u8],
    mapping: &[u32],
    explicit: &[u32],
    aliases: &[[u32; 2]],
    output_count: usize,
) -> Result<Vec<Vec<*mut PJRT_Buffer>>> {
    let context = context::get()?;
    if inputs.len() != arguments.len() || devices.is_empty() {
        return Err(Error::local("invalid launch input or device count"));
    }
    let expected = devices
        .iter()
        .map(|&device| {
            context
                .device(usize::try_from(device).map_err(|_| Error::local("negative device index"))?)
                .map(|device| device.as_ptr())
        })
        .collect::<Result<Vec<_>>>()?;
    let cache = &context.executables;
    let program = cache.specialize_program(code, checksum, arguments.clone())?;
    if program.output_shapes.len() != output_count {
        return Err(Error::local("specialized output count mismatch"));
    }
    let api = cache.owner.api;
    let mut facts = Vec::with_capacity(inputs.len());
    for (i, (&input, argument)) in inputs.iter().zip(&arguments).enumerate() {
        non_null(input.cast_mut(), "input descriptor")?;
        let row = unsafe { std::slice::from_raw_parts(input, devices.len()) };
        let mut fact = BufferFacts {
            handles: row.iter().map(|p| *p as usize).collect(),
            logical_bytes: 0,
            allocated_bytes: 0,
        };
        let permitted = permissions.get(i).copied().unwrap_or(0) != 0;
        let mut first_element = None;
        for (device, &buffer) in row.iter().enumerate() {
            non_null(buffer, "input shard")?;
            if !permitted {
                continue;
            }
            let element = call!(api, PJRT_Buffer_ElementType { buffer })?.type_;
            let physical =
                call!(api, PJRT_Buffer_OnDeviceSizeInBytes { buffer })?.on_device_size_in_bytes;
            fact.allocated_bytes = fact
                .allocated_bytes
                .checked_add(physical as u64)
                .ok_or_else(|| Error::local("physical allocation size overflow"))?;
            let logical = element_bytes(element)
                .and_then(|bytes| {
                    argument
                        .dimensions
                        .iter()
                        .try_fold(bytes, |n, &dim| n.checked_mul(u64::try_from(dim).ok()?))
                })
                .unwrap_or(0);
            if device == 0 {
                first_element = Some(element);
                fact.logical_bytes = logical;
            } else if first_element != Some(element) {
                return Err(Error::local("input shards have inconsistent element types"));
            }
        }
        facts.push(fact);
    }
    for (i, &rep) in mapping.iter().enumerate() {
        if arguments.get(rep as usize) != arguments.get(i) {
            return Err(Error::local(
                "deduplicated arguments have different specialization inputs",
            ));
        }
    }
    let selected = select(
        &facts,
        permissions,
        mapping,
        explicit,
        aliases,
        &program.output_bytes,
        MIN_DONATION_BYTES,
    )?;
    let kept: Vec<_> = selected
        .mapping
        .iter()
        .enumerate()
        .filter_map(|(i, &rep)| (i == rep as usize).then_some(i))
        .collect();
    let denied: Vec<_> = kept
        .iter()
        .enumerate()
        .filter_map(|(new, &old)| (!selected.donors.contains(&(old as u32))).then_some(new as i64))
        .collect();
    let remapped: Vec<_> = kept.iter().map(|&i| inputs[i]).collect();
    let transformation =
        (!selected.donors.is_empty() || !selected.aliases.is_empty()).then_some(selected);
    let executable = cache.compile_selected(&program.bytecode, options, transformation)?;
    executable.validate_devices(&expected)?;
    unsafe { executable.execute(&remapped, output_count, &denied) }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn fact(handles: &[usize], logical: u64, actual: u64) -> BufferFacts {
        BufferFacts {
            handles: handles.to_vec(),
            logical_bytes: logical,
            allocated_bytes: actual,
        }
    }
    #[test]
    fn permission_and_profitability_are_independent() {
        let facts = [fact(&[1], 8192, 8192), fact(&[2], 16, 16)];
        assert!(
            select(&facts, &[0, 1], &[0, 1], &[], &[], &[8192, 16], 4096)
                .unwrap()
                .donors
                .is_empty()
        );
        assert_eq!(
            select(&facts, &[1, 1], &[0, 1], &[], &[], &[8192, 16], 4096)
                .unwrap()
                .donors,
            [0]
        );
        assert_eq!(
            select(&facts, &[1, 1], &[0, 1], &[1], &[], &[16], 4096)
                .unwrap()
                .donors,
            [1]
        );
    }
    #[test]
    fn deduplication_and_all_device_alias_safety() {
        let facts = [fact(&[1, 2], 8192, 8192), fact(&[1, 2], 8192, 8192)];
        let selected = select(&facts, &[1, 1], &[0, 0], &[], &[], &[8192], 4096).unwrap();
        assert_eq!(selected.donors, [0]);
        assert_eq!(selected.mapping, [0, 0]);
        assert!(
            select(&facts, &[1, 0], &[0, 0], &[], &[], &[8192], 4096)
                .unwrap()
                .donors
                .is_empty()
        );
        let crossed = [fact(&[1, 2], 8192, 8192), fact(&[3, 1], 8192, 8192)];
        assert!(
            select(&crossed, &[1, 1], &[0, 1], &[], &[], &[8192], 4096)
                .unwrap()
                .donors
                .is_empty()
        );
        assert!(select(&crossed, &[1, 1], &[0, 1], &[0], &[], &[8192], 4096).is_err());
    }
    #[test]
    fn one_donor_per_output_and_invalid_proofs() {
        let facts = [fact(&[1], 8192, 16384), fact(&[2], 8192, 8192)];
        assert_eq!(
            select(&facts, &[1, 1], &[0, 1], &[], &[], &[8192], 4096)
                .unwrap()
                .donors,
            [0]
        );
        assert!(select(&facts, &[1, 1], &[0, 0], &[], &[], &[8192], 4096).is_err());
        assert!(select(&facts, &[0, 1], &[0, 1], &[0], &[], &[8192], 4096).is_err());
        assert!(select(&facts, &[1, 1], &[0, 1], &[0], &[[0, 2]], &[8192], 4096).is_err());
    }
}
