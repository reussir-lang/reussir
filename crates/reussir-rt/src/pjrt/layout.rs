use super::{Error, Result, sys::*};

/// Borrowed allocation metadata. The compiler resolves XLA layout strings;
/// the runtime only translates the concrete layout to PjRt's C representation.
#[derive(Default)]
pub(super) struct Allocation<'a> {
    pub memory_kind: Option<&'a [u8]>,
    pub layout: Option<Layout<'a>>,
}

pub(super) struct Layout<'a> {
    minor_to_major: &'a [i64],
    tile_dims: &'a [i64],
    tile_dim_sizes: &'a [usize],
}

impl<'a> Layout<'a> {
    pub(super) fn new(
        minor_to_major: &'a [i64],
        tile_dims: &'a [i64],
        tile_dim_sizes: &'a [usize],
    ) -> Result<Self> {
        if tile_dim_sizes
            .iter()
            .try_fold(0usize, |n, &size| n.checked_add(size))
            != Some(tile_dims.len())
        {
            return Err(Error::local("PjRt tile dimensions do not match tile sizes"));
        }
        Ok(Self {
            minor_to_major,
            tile_dims,
            tile_dim_sizes,
        })
    }

    pub(super) fn as_pjrt(&self) -> PJRT_Buffer_MemoryLayout {
        let mut layout = PJRT_Buffer_MemoryLayout {
            struct_size: PJRT_Buffer_MemoryLayout_STRUCT_SIZE as usize,
            type_: PJRT_Buffer_MemoryLayout_Type_PJRT_Buffer_MemoryLayout_Type_Tiled,
            ..Default::default()
        };
        layout.__bindgen_anon_1.tiled = PJRT_Buffer_MemoryLayout_Tiled {
            struct_size: PJRT_Buffer_MemoryLayout_Tiled_STRUCT_SIZE as usize,
            minor_to_major: self.minor_to_major.as_ptr(),
            minor_to_major_size: self.minor_to_major.len(),
            tile_dims: self.tile_dims.as_ptr(),
            tile_dim_sizes: self.tile_dim_sizes.as_ptr(),
            num_tiles: self.tile_dim_sizes.len(),
            ..Default::default()
        };
        layout
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tiled_layout_borrows_the_full_description() {
        let order = [1, 0];
        let tiles = [8, 128, 2, 1];
        let sizes = [2, 2];
        let layout = Layout::new(&order, &tiles, &sizes).unwrap();
        let raw = layout.as_pjrt();
        let tiled = unsafe { raw.__bindgen_anon_1.tiled };
        assert_eq!(tiled.minor_to_major, order.as_ptr());
        assert_eq!(tiled.minor_to_major_size, 2);
        assert_eq!(tiled.tile_dims, tiles.as_ptr());
        assert_eq!(tiled.tile_dim_sizes, sizes.as_ptr());
        assert_eq!(tiled.num_tiles, 2);
        assert!(Layout::new(&order, &tiles, &[3]).is_err());
        assert!(Layout::new(&order, &tiles, &[usize::MAX, 1]).is_err());
    }
}
