//! Launch-time donation. The compiler's ownership analysis decides which
//! inputs may be donated (permission), guarded by uniqueness checks, and which
//! inputs are provably the same buffer (mapping). This module decides whether
//! donating is worthwhile from the launch's actual buffers.
//!
//! Physical shard handles are compared across all devices, so an input aliased
//! on any device is never donated. An automatic donor needs permission, a
//! result of the same logical byte size, and at least [`MIN_DONATION_BYTES`] of
//! device allocation; larger inputs are considered first, and each result
//! takes at most one donor. When the plugin cannot report allocation sizes,
//! nothing is donated automatically. Explicit donors and aliases skip the size
//! policy but still fail on missing permission or physical aliasing.
//!
//! Only the chosen program is built on an executable-cache miss; no donor
//! subsets are enumerated. Donation lets PJRT reuse a buffer; it does not
//! guarantee that it will.
use super::{
    Buffer, Error, Result,
    api::{Api, non_null},
    context,
    sys::*,
};
use reussir_stablehlo::{Argument, Donation, ResultType};
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
) -> Result<Donation> {
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
    Ok(Donation {
        mapping,
        donors,
        aliases: aliases.to_vec(),
    })
}

// Bytes of a refined result, or 0 when unknown, which never matches a donor.
fn result_bytes(result: &ResultType) -> u64 {
    let element = u64::from(result.element_bits.div_ceil(8));
    result
        .dimensions
        .iter()
        .try_fold(element, |n, &dim| n.checked_mul(u64::try_from(dim).ok()?))
        .unwrap_or(0)
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

// Profitability queries are optional. A missing or failed query means there
// is insufficient information to justify automatic donation of this input.
fn allocation_info(api: Api, buffer: *mut PJRT_Buffer) -> Option<(PJRT_Buffer_Type, u64, u64)> {
    // Api validates a prefix containing these fields; avoid borrowing its tail.
    let element = unsafe { (*api.0.as_ptr()).PJRT_Buffer_ElementType }?;
    let size = unsafe { (*api.0.as_ptr()).PJRT_Buffer_OnDeviceSizeInBytes }?;
    let element = api
        .call(
            element,
            PJRT_Buffer_ElementType_Args {
                struct_size: PJRT_Buffer_ElementType_Args_STRUCT_SIZE as usize,
                buffer,
                ..Default::default()
            },
        )
        .ok()?
        .type_;
    let bytes = element_bytes(element)?;
    let size = api
        .call(
            size,
            PJRT_Buffer_OnDeviceSizeInBytes_Args {
                struct_size: PJRT_Buffer_OnDeviceSizeInBytes_Args_STRUCT_SIZE as usize,
                buffer,
                ..Default::default()
            },
        )
        .ok()?
        .on_device_size_in_bytes;
    Some((element, bytes, size as u64))
}

/// The launch inputs that remain arguments of the specialized program, in
/// order, with the donation contracts renumbered to their positions.
#[derive(Debug, PartialEq, Eq)]
struct RuntimeInputs {
    inputs: Vec<usize>,
    mapping: Vec<u32>,
    explicit: Vec<u32>,
    aliases: Vec<[u32; 2]>,
}

fn runtime_inputs(
    count: usize,
    constants: &[u32],
    mapping: &[u32],
    explicit: &[u32],
    aliases: &[[u32; 2]],
) -> Result<RuntimeInputs> {
    if !constants.windows(2).all(|pair| pair[0] < pair[1])
        || constants.last().is_some_and(|&last| last as usize >= count)
        || mapping.len() != count
        || mapping.iter().any(|&rep| rep as usize >= count)
    {
        return Err(Error::local("invalid constant inputs or donation mapping"));
    }
    let mut position = vec![None; count];
    let mut inputs = Vec::new();
    for (i, slot) in position.iter_mut().enumerate() {
        if constants.binary_search(&(i as u32)).is_err() {
            *slot = Some(inputs.len() as u32);
            inputs.push(i);
        }
    }
    // Inputs proven identical to a constant input map to the first of them.
    let mut constant_representatives = std::collections::HashMap::new();
    let mapping = inputs
        .iter()
        .map(|&i| {
            let rep = mapping[i] as usize;
            position[rep].unwrap_or_else(|| {
                *constant_representatives
                    .entry(rep)
                    .or_insert_with(|| position[i].unwrap())
            })
        })
        .collect();
    let renumber = |input: u32| {
        position
            .get(input as usize)
            .copied()
            .flatten()
            .ok_or_else(|| Error::local("constant inputs cannot be donated or aliased"))
    };
    Ok(RuntimeInputs {
        mapping,
        explicit: explicit
            .iter()
            .map(|&i| renumber(i))
            .collect::<Result<_>>()?,
        aliases: aliases
            .iter()
            .map(|&[input, output]| Ok([renumber(input)?, output]))
            .collect::<Result<_>>()?,
        inputs,
    })
}

/// Read each constant input's value from its first shard; every other input
/// contributes its logical shape.
///
/// # Safety
/// Each constant input descriptor addresses a live rank-zero buffer.
unsafe fn specialization(
    shapes: &[Vec<i64>],
    inputs: &[*const *mut PJRT_Buffer],
    constants: &[u32],
) -> Result<Vec<Argument>> {
    let mut arguments: Vec<_> = shapes.iter().cloned().map(Argument::Shape).collect();
    for &constant in constants {
        let descriptor = inputs[constant as usize];
        non_null(descriptor.cast_mut(), "input descriptor")?;
        let mut value = 0i64;
        unsafe {
            Buffer::new(*descriptor)?.to_host(
                std::ptr::from_mut(&mut value).cast(),
                size_of::<i64>(),
                None,
            )
        }?;
        arguments[constant as usize] = Argument::Constant(value);
    }
    Ok(arguments)
}

/// Inputs and shards are live borrowed buffers until synchronous execution
/// ends. `constants` lists, in increasing order, the inputs whose values the
/// kernel is specialized on; they are not passed to the executable.
#[allow(clippy::too_many_arguments)]
pub(super) unsafe fn launch(
    code: &[u8],
    checksum: &[u8],
    options: &[u8],
    shapes: Vec<Vec<i64>>,
    inputs: &[*const *mut PJRT_Buffer],
    constants: &[u32],
    devices: &[i32],
    permissions: &[u8],
    mapping: &[u32],
    explicit: &[u32],
    aliases: &[[u32; 2]],
    output_count: usize,
) -> Result<Vec<Vec<*mut PJRT_Buffer>>> {
    let context = context::get()?;
    if inputs.len() != shapes.len() || permissions.len() != inputs.len() || devices.is_empty() {
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
    let runtime = runtime_inputs(inputs.len(), constants, mapping, explicit, aliases)?;
    let cache = &context.executables;
    let arguments = unsafe { specialization(&shapes, inputs, constants) }?;
    let program = cache.refine_program(code, checksum, arguments)?;
    if program.result_types.len() != output_count {
        return Err(Error::local("refined result count mismatch"));
    }
    let api = cache.owner.api;
    let explicit = &runtime.explicit[..];
    let aliases = &runtime.aliases[..];
    let mut facts = Vec::with_capacity(runtime.inputs.len());
    for &i in &runtime.inputs {
        let input = inputs[i];
        non_null(input.cast_mut(), "input descriptor")?;
        let row = unsafe { std::slice::from_raw_parts(input, devices.len()) };
        let mut fact = BufferFacts {
            handles: row.iter().map(|p| *p as usize).collect(),
            logical_bytes: 0,
            allocated_bytes: 0,
        };
        let permitted = explicit.is_empty() && aliases.is_empty() && permissions[i] != 0;
        let mut first_element = None;
        for (device, &buffer) in row.iter().enumerate() {
            non_null(buffer, "input shard")?;
            if !permitted {
                continue;
            }
            let Some((element, element_bytes, physical)) = allocation_info(api, buffer) else {
                fact.logical_bytes = 0;
                fact.allocated_bytes = 0;
                break;
            };
            fact.allocated_bytes = fact
                .allocated_bytes
                .checked_add(physical)
                .ok_or_else(|| Error::local("physical allocation size overflow"))?;
            let logical = Some(element_bytes)
                .and_then(|bytes| {
                    shapes[i]
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
    for (j, &rep) in runtime.mapping.iter().enumerate() {
        if shapes[runtime.inputs[rep as usize]] != shapes[runtime.inputs[j]] {
            return Err(Error::local("deduplicated arguments have different shapes"));
        }
    }
    let permissions: Vec<_> = runtime.inputs.iter().map(|&i| permissions[i]).collect();
    let output_bytes: Vec<_> = program.result_types.iter().map(result_bytes).collect();
    let selected = select(
        &facts,
        &permissions,
        &runtime.mapping,
        explicit,
        aliases,
        &output_bytes,
        MIN_DONATION_BYTES,
    )?;
    let kept: Vec<_> = selected
        .mapping
        .iter()
        .enumerate()
        .filter_map(|(j, &rep)| (j == rep as usize).then_some(j))
        .collect();
    let denied: Vec<_> = kept
        .iter()
        .enumerate()
        .filter_map(|(new, &old)| (!selected.donors.contains(&(old as u32))).then_some(new as i64))
        .collect();
    let remapped: Vec<_> = kept.iter().map(|&j| inputs[runtime.inputs[j]]).collect();
    let transformation =
        (!selected.donors.is_empty() || !selected.aliases.is_empty()).then_some(selected);
    let executable = cache.compile_refined(&program.bytecode, options, transformation)?;
    context::register_compiled_shutdown()?;
    executable.validate_devices(&expected)?;
    unsafe { executable.execute(&remapped, output_count, &denied) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_inputs_leave_the_program_and_renumber_contracts() {
        let plain = runtime_inputs(2, &[], &[0, 0], &[], &[]).unwrap();
        assert_eq!((plain.inputs, plain.mapping), (vec![0, 1], vec![0, 0]));
        // Input 2 is the same array as constant input 1, so it becomes its
        // own representative; input 3 then maps to it.
        let mixed = runtime_inputs(4, &[1], &[0, 1, 1, 1], &[3], &[[3, 0]]).unwrap();
        assert_eq!(
            mixed,
            RuntimeInputs {
                inputs: vec![0, 2, 3],
                mapping: vec![0, 1, 1],
                explicit: vec![2],
                aliases: vec![[2, 0]],
            }
        );
        for (constants, explicit) in [(&[1, 0][..], &[][..]), (&[2], &[]), (&[0], &[0])] {
            assert!(runtime_inputs(2, constants, &[0, 1], explicit, &[]).is_err());
        }
    }
    #[test]
    fn missing_profitability_queries_disable_automatic_donation() {
        unsafe extern "C" fn element(args: *mut PJRT_Buffer_ElementType_Args) -> *mut PJRT_Error {
            unsafe { (*args).type_ = 11 }; // PJRT F32.
            std::ptr::null_mut()
        }
        unsafe extern "C" fn size(
            args: *mut PJRT_Buffer_OnDeviceSizeInBytes_Args,
        ) -> *mut PJRT_Error {
            unsafe { (*args).on_device_size_in_bytes = 8192 };
            std::ptr::null_mut()
        }
        let mut table = PJRT_Api::default();
        // Mock callbacks do not dereference their buffer argument.
        let info = |table: &mut PJRT_Api| {
            allocation_info(Api(std::ptr::NonNull::from(table)), std::ptr::null_mut())
        };
        assert_eq!(info(&mut table), None);
        table.PJRT_Buffer_ElementType = Some(element);
        assert_eq!(info(&mut table), None);
        table.PJRT_Buffer_OnDeviceSizeInBytes = Some(size);
        assert_eq!(info(&mut table), Some((11, 4, 8192)));
        table.PJRT_Buffer_ElementType = None;
        assert_eq!(info(&mut table), None);
    }

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
