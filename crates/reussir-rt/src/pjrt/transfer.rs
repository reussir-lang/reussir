//! Rectangular host/device shard transfers using XLA padded physical tiles. Backend downloads use dense
//! temporary storage because PJRT plugins need not support strided downloads.
use super::{Buffer, Error, Result, context, layout::Layout};
use std::{ffi::c_void, mem::MaybeUninit};

pub(super) struct Shard {
    pub dims: Vec<i64>,
    global: Vec<usize>,
    origin: Vec<usize>,
    valid: Vec<usize>,
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
            valid: Vec::new(),
        };
        for ((&size, &count), &coordinate) in global.iter().zip(counts).zip(coordinates) {
            if size < 0 || count <= 0 || coordinate < 0 || coordinate >= count {
                return Err(Error::local(
                    "array transfer requires nonnegative extents, positive shard counts and valid coordinates",
                ));
            }
            // Match HloSharding::TileShape and TileOffsetForDevice. Avoid
            // overflowing size + count - 1 or forming an out-of-bounds pointer
            // for devices whose whole physical tile lies beyond the host shape.
            let local = size / count + i64::from(size % count != 0);
            let origin = ((local as u128) * (coordinate as u128)).min(size as u128) as i64;
            result.valid.push(
                usize::try_from(local.min(size - origin))
                    .map_err(|_| Error::local("array transfer extent overflow"))?,
            );
            result.dims.push(local);
            result.global.push(
                usize::try_from(size)
                    .map_err(|_| Error::local("array transfer extent overflow"))?,
            );
            result.origin.push(
                usize::try_from(origin)
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
        if self.valid.contains(&0) {
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

    pub fn needs_padding(&self) -> bool {
        self.valid
            .iter()
            .zip(&self.dims)
            .any(|(&valid, &physical)| valid as i64 != physical)
    }

    // Decode a physical row, rejecting rows wholly outside the logical domain.
    // The final coordinate stays zero; the caller copies the valid row prefix.
    fn row_coordinates(&self, mut row: usize, indices: &mut [usize]) -> bool {
        let rank = self.dims.len();
        for dim in (0..rank.saturating_sub(1)).rev() {
            indices[dim] = row % self.dims[dim] as usize;
            row /= self.dims[dim] as usize;
            if indices[dim] >= self.valid[dim] {
                return false;
            }
        }
        true
    }

    /// Pack an edge tile into zero-initialized dense physical storage. `data`
    /// addresses initialized global host storage with the supplied byte strides.
    pub unsafe fn pack(
        &self,
        data: *const c_void,
        strides: &[i64],
        element_bytes: usize,
    ) -> Result<Vec<u8>> {
        let bytes = Self::bytes(self.dims.iter().map(|&dim| dim as usize), element_bytes)?;
        if strides.len() != self.dims.len() || data.is_null() {
            return Err(Error::local(
                "array transfer requires source data and one stride per dimension",
            ));
        }
        let mut packed = vec![0; bytes];
        if self.valid.contains(&0) {
            return Ok(packed);
        }
        let row_bytes = self.dims.last().copied().unwrap_or(1) as usize * element_bytes;
        let valid_columns = self.valid.last().copied().unwrap_or(1);
        let mut indices = vec![0; self.dims.len()];
        for (row, output) in packed.chunks_exact_mut(row_bytes).enumerate() {
            if !self.row_coordinates(row, &mut indices) {
                continue;
            }
            let offset = self
                .origin
                .iter()
                .zip(&indices)
                .zip(strides)
                .try_fold(0i64, |offset, ((&origin, &index), &stride)| {
                    i64::try_from(origin + index)
                        .ok()?
                        .checked_mul(stride)?
                        .checked_add(offset)
                })
                .ok_or_else(|| Error::local("array transfer shard offset overflow"))?;
            let last_stride = strides.last().copied().unwrap_or(element_bytes as i64);
            // Preserve arbitrary signed strides, including reversed rows or columns.
            let (copies, copy_bytes, step) = if last_stride == element_bytes as i64 {
                (1, valid_columns * element_bytes, 0)
            } else {
                (valid_columns, element_bytes, last_stride)
            };
            for column in 0..copies {
                let source_offset = i64::try_from(column)
                    .ok()
                    .and_then(|column| column.checked_mul(step))
                    .and_then(|step| offset.checked_add(step))
                    .and_then(|offset| isize::try_from(offset).ok())
                    .ok_or_else(|| Error::local("array transfer shard offset overflow"))?;
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        data.cast::<u8>().offset(source_offset),
                        output[column * copy_bytes..].as_mut_ptr(),
                        copy_bytes,
                    );
                }
            }
        }
        Ok(packed)
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
        if self.valid.contains(&0) {
            return Ok(());
        }
        let rank = self.dims.len();
        let row_bytes = self.dims.last().copied().unwrap_or(1) as usize * element_bytes;
        let valid_bytes = self.valid.last().copied().unwrap_or(1) * element_bytes;
        let mut indices = vec![0; rank];
        for (row, values) in source.chunks_exact(row_bytes).enumerate() {
            if !self.row_coordinates(row, &mut indices) {
                continue;
            }
            let mut offset = self.origin.last().copied().unwrap_or(0);
            let mut stride = self.global.last().copied().unwrap_or(1);
            for dim in (0..rank.saturating_sub(1)).rev() {
                offset += (self.origin[dim] + indices[dim]) * stride;
                stride *= self.global[dim];
            }
            let start = offset * element_bytes;
            // Padding is never copied into host storage, which may remain
            // uninitialized outside the tiles already gathered.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    values.as_ptr(),
                    destination[start..start + valid_bytes].as_mut_ptr().cast(),
                    valid_bytes,
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
    fn packs_and_gathers_uneven_tiles_without_exposing_padding() {
        let source: Vec<u8> = (0..15).collect();
        let mut destination = [255; 15];
        for a in 0..2 {
            for b in 0..2 {
                let shard = Shard::new(&[3, 5], &[2, 2], &[a, b]).unwrap();
                assert_eq!(shard.dims, [2, 3]);
                let packed = unsafe {
                    shard
                        .pack(source.as_ptr().add(14).cast(), &[-5, -1], 1)
                        .unwrap()
                };
                if a == 1 && b == 1 {
                    assert_eq!(packed, [1, 0, 0, 0, 0, 0]);
                }
                shard
                    .scatter(&packed, uninit_view(&mut destination), 1)
                    .unwrap();
            }
        }
        assert_eq!(destination, std::array::from_fn(|i| 14 - i as u8));
    }

    #[test]
    fn wholly_padded_tiles_never_access_host_data() {
        let shard = Shard::new(&[1, 1], &[4, 4], &[3, 2]).unwrap();
        assert_eq!(shard.dims, [1, 1]);
        assert_eq!(shard.origin, [1, 1]);
        let source = [7u8];
        let packed = unsafe {
            shard
                .pack(source.as_ptr().cast(), &[i64::MAX, i64::MIN], 1)
                .unwrap()
        };
        assert_eq!(packed, [0]);
        assert_eq!(shard.byte_offset(&[i64::MAX, i64::MIN]).unwrap(), 0);
        let mut destination = [99];
        shard
            .scatter(&[200], uninit_view(&mut destination), 1)
            .unwrap();
        assert_eq!(destination, [99]);
        let extreme = Shard::new(&[i64::MAX], &[i64::MAX - 1], &[i64::MAX - 2]).unwrap();
        assert_eq!(extreme.origin[0] as i64, i64::MAX);
        assert_eq!(extreme.valid, [0]);
    }

    #[test]
    fn validates_shapes_sizes_and_signed_offsets() {
        assert_eq!(Shard::new(&[3], &[2], &[0]).unwrap().dims, [2]);
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
