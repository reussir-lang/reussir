//! Even rectangular host/device shard transfers. Backend downloads use dense
//! temporary storage because PJRT plugins need not support strided downloads.
use super::{Buffer, Error, Result, context, layout::Layout};
use std::{ffi::c_void, mem::MaybeUninit};

pub(super) struct Shard {
    pub dims: Vec<i64>,
    global: Vec<usize>,
    origin: Vec<usize>,
}

impl Shard {
    pub fn new(global: &[i64], counts: &[i64], coordinates: &[i64]) -> Result<Self> {
        if global.len() != counts.len() || global.len() != coordinates.len() {
            return Err(Error::local("array transfer shard rank mismatch"));
        }
        let mut result = Self {
            dims: Vec::new(),
            global: Vec::new(),
            origin: Vec::new(),
        };
        for ((&size, &count), &coordinate) in global.iter().zip(counts).zip(coordinates) {
            if size < 0 || count <= 0 || coordinate < 0 || coordinate >= count || size % count != 0
            {
                return Err(Error::local(
                    "array transfer requires nonnegative extents divisible by shard counts and valid coordinates",
                ));
            }
            let local = size / count;
            result.dims.push(local);
            result.global.push(
                usize::try_from(size)
                    .map_err(|_| Error::local("array transfer extent overflow"))?,
            );
            result.origin.push(
                usize::try_from(local * coordinate)
                    .map_err(|_| Error::local("array transfer origin overflow"))?,
            );
        }
        Ok(result)
    }

    pub fn byte_offset(&self, strides: &[i64]) -> Result<isize> {
        if strides.len() != self.dims.len() {
            return Err(Error::local("array transfer stride rank mismatch"));
        }
        // An empty view has no logical first element: retain the caller's pointer.
        if self.dims.contains(&0) {
            return Ok(0);
        }
        let offset = self
            .origin
            .iter()
            .zip(strides)
            .try_fold(0i64, |offset, (&origin, &stride)| {
                i64::try_from(origin)
                    .ok()?
                    .checked_mul(stride)?
                    .checked_add(offset)
            })
            .and_then(|offset| isize::try_from(offset).ok());
        offset.ok_or_else(|| Error::local("array transfer shard offset overflow"))
    }

    fn bytes(dims: impl Iterator<Item = usize>, element_bytes: usize) -> Result<usize> {
        let dims: Vec<_> = dims.collect();
        if element_bytes == 0 {
            return Err(Error::local("array transfer element size must be positive"));
        }
        if dims.contains(&0) {
            return Ok(0);
        }
        dims.into_iter()
            .try_fold(element_bytes, usize::checked_mul)
            .filter(|&bytes| bytes <= isize::MAX as usize)
            .ok_or_else(|| Error::local("array transfer byte size overflow"))
    }

    fn scatter(
        &self,
        source: &[u8],
        destination: &mut [MaybeUninit<u8>],
        element_bytes: usize,
    ) -> Result<()> {
        let expected = Self::bytes(self.dims.iter().map(|&dim| dim as usize), element_bytes)?;
        let total = Self::bytes(self.global.iter().copied(), element_bytes)?;
        if source.len() != expected || destination.len() != total {
            return Err(Error::local("array transfer host byte size mismatch"));
        }
        if expected == 0 {
            return Ok(());
        }
        // Copy contiguous rows rather than individual elements. All arithmetic
        // is bounded by the checked source/destination sizes above.
        let rank = self.dims.len();
        let row_bytes = self.dims.last().copied().unwrap_or(1) as usize * element_bytes;
        for (row, values) in source.chunks_exact(row_bytes).enumerate() {
            let mut remainder = row;
            let mut offset = self.origin.last().copied().unwrap_or(0);
            let mut stride = self.global.last().copied().unwrap_or(1);
            for dim in (0..rank.saturating_sub(1)).rev() {
                let index = remainder % self.dims[dim] as usize;
                remainder /= self.dims[dim] as usize;
                offset += (self.origin[dim] + index) * stride;
                stride *= self.global[dim];
            }
            let start = offset * element_bytes;
            // Destination host arrays may be uninitialized until all shards have
            // arrived; never form a reference to their contents as initialized u8.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    values.as_ptr(),
                    destination[start..start + row_bytes].as_mut_ptr().cast(),
                    row_bytes,
                );
            }
        }
        Ok(())
    }

    /// `data` points to exclusive dense row-major storage for the global shape.
    pub unsafe fn download(
        &self,
        buffer: Buffer,
        data: *mut c_void,
        element_bytes: usize,
    ) -> Result<()> {
        let total = Self::bytes(self.global.iter().copied(), element_bytes)?;
        let bytes = Self::bytes(self.dims.iter().map(|&dim| dim as usize), element_bytes)?;
        let mut actual = vec![0; self.dims.len()];
        unsafe {
            buffer.dimensions(context::get()?.api, actual.as_mut_ptr(), actual.len())?;
        }
        if actual != self.dims {
            return Err(Error::local("array transfer shard shape mismatch"));
        }
        let order: Vec<_> = (0..self.dims.len() as i64).rev().collect();
        let layout = Layout::new(&order, &[], &[])?;
        if unsafe { buffer.host_size(Some(&layout))? } != bytes {
            return Err(Error::local("array transfer host byte size mismatch"));
        }
        if data.is_null() {
            return Err(Error::local("array transfer destination must be non-null"));
        }
        let mut staging = vec![0u8; bytes];
        unsafe {
            buffer.to_host(staging.as_mut_ptr().cast(), bytes, Some(&layout))?;
        }
        self.scatter(
            &staging,
            unsafe { std::slice::from_raw_parts_mut(data.cast(), total) },
            element_bytes,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uninit_view(values: &mut [u8]) -> &mut [MaybeUninit<u8>] {
        // Every byte was initialized by the test, and scatter only writes bytes.
        unsafe { std::slice::from_raw_parts_mut(values.as_mut_ptr().cast(), values.len()) }
    }

    #[test]
    fn scatter_inner_dimension_and_nonzero_origin() {
        let shard = Shard::new(&[4, 6], &[2, 3], &[1, 1]).unwrap();
        let mut destination = [99; 24];
        shard
            .scatter(&[1, 2, 3, 4], uninit_view(&mut destination), 1)
            .unwrap();
        assert_eq!(&destination[14..16], &[1, 2]);
        assert_eq!(&destination[20..22], &[3, 4]);
        assert_eq!(destination.iter().filter(|&&x| x == 99).count(), 20);
    }

    #[test]
    fn scatter_scalar_empty_and_multibyte() {
        let scalar = Shard::new(&[], &[], &[]).unwrap();
        let mut destination = [0; 4];
        scalar
            .scatter(&[1, 2, 3, 4], uninit_view(&mut destination), 4)
            .unwrap();
        assert_eq!(destination, [1, 2, 3, 4]);
        let empty = Shard::new(&[0, i64::MAX], &[2, 1], &[1, 0]).unwrap();
        empty.scatter(&[], &mut [], 8).unwrap();
        assert_eq!(empty.byte_offset(&[i64::MAX, i64::MAX]).unwrap(), 0);
        let shard = Shard::new(&[2, 4], &[1, 2], &[0, 1]).unwrap();
        let mut destination = [0; 16];
        shard
            .scatter(&[1, 2, 3, 4, 5, 6, 7, 8], uninit_view(&mut destination), 2)
            .unwrap();
        assert_eq!(
            destination,
            [0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 5, 6, 7, 8]
        );
    }

    #[test]
    fn gathers_three_dimensional_tiles() {
        let mut destination = [0; 192];
        for a in 0..2 {
            for b in 0..3 {
                for c in 0..2 {
                    let shard = Shard::new(&[4, 6, 8], &[2, 3, 2], &[a, b, c]).unwrap();
                    let mut source = Vec::new();
                    for i in 0..2 {
                        for j in 0..2 {
                            for k in 0..4 {
                                source.push(((a * 2 + i) * 48 + (b * 2 + j) * 8 + c * 4 + k) as u8);
                            }
                        }
                    }
                    shard
                        .scatter(&source, uninit_view(&mut destination), 1)
                        .unwrap();
                }
            }
        }
        assert_eq!(destination, std::array::from_fn(|i| i as u8));
    }

    #[test]
    fn validates_shapes_sizes_and_signed_offsets() {
        assert!(Shard::new(&[3], &[2], &[0]).is_err());
        assert!(Shard::new(&[-2], &[2], &[0]).is_err());
        assert!(Shard::new(&[4], &[0], &[0]).is_err());
        assert!(Shard::new(&[4], &[2], &[2]).is_err());
        let shard = Shard::new(&[4, 6], &[2, 3], &[1, 1]).unwrap();
        assert_eq!(shard.byte_offset(&[-24, 4]).unwrap(), -40);
        assert!(shard.byte_offset(&[i64::MAX, 4]).is_err());
        assert!(
            shard
                .scatter(&[0; 3], uninit_view(&mut [0; 24]), 1)
                .is_err()
        );
        assert!(Shard::bytes([usize::MAX, 2].into_iter(), 4).is_err());
    }
}
